
# -*- coding: utf-8 -*-
"""
Created on Mon Sep 29 19:10:43 2025

@author: anonymous
"""

import pandas as pd
import requests
import time
import json
from typing import Dict, List, Tuple, Optional, Set
import re
from anthropic import Anthropic
import logging
from datetime import datetime
from concurrent.futures import ThreadPoolExecutor, as_completed
import hashlib
from functools import lru_cache
import pickle
import os

# Setup logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class DualAddressGeocoder:
    """Complete geocoder with all methods for dual address datasets"""
    
    def __init__(self, here_api_key: str, claude_api_key: str, 
                 max_workers: int = 5, cache_dir: str = "geocode_cache",
                 existing_geocoded_file: str = None, retry_failed: bool = False):
        """
        Initialize the geocoder with support for dual addresses
        
        Args:
            here_api_key: HERE Maps API key
            claude_api_key: Anthropic Claude API key
            max_workers: Number of parallel workers
            cache_dir: Directory for caching results
            existing_geocoded_file: Path to existing geocoded addresses CSV
            retry_failed: If True, retry previously failed addresses with Claude
        """
        self.here_api_key = here_api_key
        self.claude_client = Anthropic(api_key=claude_api_key)
        self.max_workers = max_workers
        self.cache_dir = cache_dir
        self.retry_failed = retry_failed
        
        # Create cache directory if it doesn't exist
        os.makedirs(cache_dir, exist_ok=True)
        
        # Load persistent caches
        self.geocode_cache = self._load_cache('geocode_cache.pkl')
        self.claude_cache = self._load_cache('claude_cache.pkl')
        self.failed_cache = self._load_cache('failed_cache.pkl')
        
        # If retry_failed is True, we'll retry addresses in failed_cache with Claude
        if self.retry_failed:
            logger.info(f"Retry mode: Will retry {len(self.failed_cache)} previously failed addresses with Claude")
            # Create a separate cache for retry attempts
            self.retry_attempted = set()
        
        # Load existing geocoded addresses if provided
        if existing_geocoded_file and os.path.exists(existing_geocoded_file):
            self._load_existing_geocoded(existing_geocoded_file)
        
        # Track API calls
        self.api_call_counts = {
            'geopunt': 0,
            'here': 0,
            'overpass': 0,
            'claude': 0,
            'from_existing': 0,
            'retry_claude': 0
        }
        
        # Overpass endpoint
        self.overpass_endpoint = "https://overpass-api.de/api/interpreter"
        
        # Track failed Overpass municipalities
        self.overpass_failed_municipalities = set()
    
    def _load_existing_geocoded(self, filepath: str):
        """Load existing geocoded addresses into cache"""
        try:
            logger.info(f"Loading existing geocoded addresses from {filepath}")
            df_existing = pd.read_csv(filepath, sep=';', encoding='utf-8')
            
            loaded_count = 0
            for idx, row in df_existing.iterrows():
                # Only cache successfully geocoded addresses
                if row.get('geocode_status') in ['success', 'success_cached']:
                    cache_key = self.create_address_key(
                        self.clean_address_component(row.get('Straat')),
                        self.clean_address_component(row.get('Nr')),
                        self.clean_address_component(row.get('Postcode')),
                        self.clean_address_component(row.get('Gemeente'))
                    )
                    
                    self.geocode_cache[cache_key] = {
                        'latitude': row.get('latitude'),
                        'longitude': row.get('longitude'),
                        'geocode_method': row.get('geocode_method', 'from_existing'),
                        'geocode_status': 'success_cached'
                    }
                    loaded_count += 1
            
            logger.info(f"Loaded {loaded_count} successfully geocoded addresses from existing file")
            
        except Exception as e:
            logger.error(f"Error loading existing geocoded file: {e}")
    
    def _load_cache(self, filename: str) -> Dict:
        """Load cache from disk"""
        filepath = os.path.join(self.cache_dir, filename)
        if os.path.exists(filepath):
            try:
                with open(filepath, 'rb') as f:
                    logger.info(f"Loaded cache from {filename}")
                    return pickle.load(f)
            except:
                return {}
        return {}
    
    def _save_cache(self, cache: Dict, filename: str):
        """Save cache to disk"""
        filepath = os.path.join(self.cache_dir, filename)
        with open(filepath, 'wb') as f:
            pickle.dump(cache, f)
    
    def save_all_caches(self):
        """Save all caches to disk"""
        self._save_cache(self.geocode_cache, 'geocode_cache.pkl')
        self._save_cache(self.claude_cache, 'claude_cache.pkl')
        self._save_cache(self.failed_cache, 'failed_cache.pkl')
        logger.info("All caches saved to disk")
    
    def clean_address_component(self, value):
        """Clean address components"""
        if pd.isna(value) or value in ['-', '*', '%', '.', 'z/n', 'x', 'X']:
            return None
        value = str(value).strip()
        value = re.sub(r'\s+', ' ', value)
        return value if value else None
    
    def clean_house_number(self, nummer: str) -> str:
        """Clean house numbers by removing bus numbers and normalizing"""
        if not nummer or pd.isna(nummer):
            return None
            
        nummer = str(nummer).strip().lower()
        
        # Handle special cases that mean "no number"
        if nummer in ['z/n', 'zn', 'z.n.', 'x', '-', '*', '%', '.', 'nn', 'n/a']:
            return None
        
        # Extract just the house number from complex formats
        # Pattern 1: Number followed by parentheses
        match = re.match(r'^(\d+)\s*[\(\[].*?[\)\]]', nummer)
        if match:
            return match.group(1)
        
        # Pattern 2: Number followed by "bus" or "b"
        match = re.match(r'^(\d+)\s*(?:bus|b\.?)\s+\S+', nummer)
        if match:
            return match.group(1)
        
        # Pattern 3: Number with letter suffix
        match = re.match(r'^(\d+[a-z]?)\s*(?:bus|b\.?|\()', nummer)
        if match:
            return match.group(1).upper()
        
        # Pattern 4: Just a clean number possibly with letter
        match = re.match(r'^(\d+[a-z]?)$', nummer)
        if match:
            return match.group(1).upper()
        
        # If it starts with a number, extract it
        match = re.match(r'^(\d+)', nummer)
        if match:
            return match.group(1)
        
        return nummer if nummer else None
    
    def create_address_key(self, straat: str, nummer: str, postcode: str, gemeente: str) -> str:
        """Create a unique key for caching"""
        components = []
        for value in [straat, nummer, postcode, gemeente]:
            if value:
                components.append(str(value).lower())
            else:
                components.append('')
        return '|'.join(components)
    
    def extract_poi_name(self, straat: str, gemeente: str, aard: str = None) -> str:
        """Extract the actual POI name from a messy street field"""
        if not straat:
            return straat
            
        straat_lower = straat.lower()
        
        if 'ziekenhuis' in straat_lower:
            poi = re.sub(r'(laan|straat|weg|steenweg)\s*ziekenhuis', '', straat, flags=re.IGNORECASE)
            poi = re.sub(r'ziekenhuis', '', poi, flags=re.IGNORECASE).strip()
            if not poi:
                poi = f"Ziekenhuis {gemeente}"
            return poi
        
        if 'ocmw' in straat_lower:
            return f"OCMW {gemeente}"
        
        if 'station' in straat_lower or straat.strip().lower() == 'station':
            return f"Station {gemeente}"
        
        if 'kerk' in straat_lower:
            return straat.replace('kerk', 'kerk').strip()
        
        # Remove parenthetical additions
        if '(' in straat:
            straat = re.sub(r'\([^)]*\)', '', straat).strip()
        
        # Remove common street type suffixes
        street_types = ['straat', 'laan', 'weg', 'steenweg', 'plein', 'markt']
        for st in street_types:
            if straat_lower.endswith(st):
                straat = straat[:-len(st)].strip()
        
        return straat
    
    def geocode_street_only(self, straat: str, postcode: str, gemeente: str) -> Optional[Tuple[float, float]]:
        """Geocode just the street without house number - get street midpoint or centroid"""
        if not straat or not gemeente:
            return None
        
        try:
            # Method 1: Try Overpass to get the street as a way
            straat_clean = straat.replace('"', '\\"')
            
            overpass_query = f"""
            [out:json][timeout:5];
            area[name="{gemeente}"]["admin_level"~"8|9|10"]->.searchArea;
            (
              way["name"~"^{straat_clean}$",i]["highway"](area.searchArea);
              way["name"~"{straat_clean}",i]["highway"](area.searchArea);
            );
            out center 1;
            """
            
            response = requests.post(
                self.overpass_endpoint,
                data={'data': overpass_query},
                timeout=7,
                headers={'User-Agent': 'Flemish-Geocoder/1.0'}
            )
            
            if response.status_code == 200:
                data = response.json()
                if data.get('elements') and len(data['elements']) > 0:
                    element = data['elements'][0]
                    if 'center' in element:
                        self.api_call_counts['overpass'] += 1
                        return (element['center']['lat'], element['center']['lon'])
        except:
            pass
        
        # Method 2: Try Geopunt with just street name
        try:
            url = "https://geo.api.vlaanderen.be/geolocation/v4/Location"
            query_parts = [straat]
            
            if postcode:
                query_parts.append(postcode)
            if gemeente:
                query_parts.append(gemeente)
            
            query = ', '.join(query_parts)
            params = {'q': query, 'c': 1}
            
            response = requests.get(url, params=params, timeout=5)
            
            if response.status_code == 200:
                data = response.json()
                if data.get('LocationResult') and len(data['LocationResult']) > 0:
                    result = data['LocationResult'][0]
                    if 'Location' in result:
                        lat = result['Location']['Lat_WGS84']
                        lon = result['Location']['Lon_WGS84']
                        self.api_call_counts['geopunt'] += 1
                        return (lat, lon)
        except:
            pass
        
        return None
    
    def geocode_geopunt(self, straat: str, nummer: str, postcode: str, gemeente: str) -> Optional[Tuple[float, float]]:
        """Try geocoding with Geopunt API"""
        try:
            url = "https://geo.api.vlaanderen.be/geolocation/v4/Location"
            query_parts = []
            
            if straat:
                if nummer and nummer not in ['-', '1', '*']:
                    query_parts.append(f"{straat} {nummer}")
                else:
                    query_parts.append(straat)
            
            if postcode:
                query_parts.append(postcode)
            
            if gemeente:
                query_parts.append(gemeente)
            
            if not query_parts:
                return None
            
            query = ', '.join(query_parts)
            params = {'q': query, 'c': 1}
            
            response = requests.get(url, params=params, timeout=5)
            
            if response.status_code == 200:
                data = response.json()
                if data.get('LocationResult') and len(data['LocationResult']) > 0:
                    result = data['LocationResult'][0]
                    if 'Location' in result:
                        lat = result['Location']['Lat_WGS84']
                        lon = result['Location']['Lon_WGS84']
                        self.api_call_counts['geopunt'] += 1
                        return (lat, lon)
        except Exception as e:
            logger.debug(f"Geopunt error: {e}")
        
        return None
    
    def geocode_here(self, straat: str, nummer: str, postcode: str) -> Optional[Tuple[float, float]]:
        """Try geocoding with HERE API"""
        try:
            address_parts = []
            
            if straat:
                if nummer and nummer not in ['-', '1', '*']:
                    address_parts.append(f"{straat} {nummer}")
                else:
                    address_parts.append(straat)
            
            if postcode:
                address_parts.append(postcode)
            
            if not address_parts:
                return None
            
            address = ', '.join(address_parts)
            url = "https://geocode.search.hereapi.com/v1/geocode"
            params = {"q": address, "apiKey": self.here_api_key}
            
            response = requests.get(url, params=params, timeout=5)
            
            if response.status_code == 200:
                data = response.json()
                if data.get("items"):
                    lat = data["items"][0]["position"]["lat"]
                    lng = data["items"][0]["position"]["lng"]
                    self.api_call_counts['here'] += 1
                    return lat, lng
        except Exception as e:
            logger.debug(f"HERE error: {e}")
        
        return None
    
    def geocode_overpass_quick(self, name: str, gemeente: str) -> Optional[Tuple[float, float]]:
        """Quick Overpass query with strict timeout and no retries"""
        if gemeente in self.overpass_failed_municipalities:
            return None
        
        try:
            name_clean = name.replace('"', '\\"')
            
            overpass_query = f"""
            [out:json][timeout:10];
            area[name="{gemeente}"]["admin_level"~"8|9|10"]->.searchArea;
            (
              node["name"~"{name_clean}",i](area.searchArea);
              way["name"~"{name_clean}",i](area.searchArea);
            );
            out center 1;
            """
            
            response = requests.post(
                self.overpass_endpoint, 
                data={'data': overpass_query}, 
                timeout=12,
                headers={'User-Agent': 'Flemish-Geocoder/1.0'}
            )
            
            if response.status_code == 200:
                data = response.json()
                if data.get('elements') and len(data['elements']) > 0:
                    element = data['elements'][0]
                    
                    if element['type'] == 'node':
                        lat = element['lat']
                        lon = element['lon']
                    elif 'center' in element:
                        lat = element['center']['lat']
                        lon = element['center']['lon']
                    else:
                        return None
                    
                    self.api_call_counts['overpass'] += 1
                    return (lat, lon)
        
        except requests.exceptions.Timeout:
            self.overpass_failed_municipalities.add(gemeente)
            logger.debug(f"Overpass timeout for {gemeente}, skipping future attempts")
        except Exception as e:
            logger.debug(f"Overpass error: {e}")
        
        return None
    
    def generate_address_improvements(self, original_name: str, gemeente: str, aard: str = None) -> Dict[str, List[str]]:
        """Generate improved variations for addresses using Claude with specific examples"""
        cache_key = f"{original_name}|{gemeente}|{aard or ''}"
        
        if cache_key in self.claude_cache:
            return self.claude_cache[cache_key]
        
        try:
            poi_name = self.extract_poi_name(original_name, gemeente, aard)
            
            # Default structure
            variations = {
                'address': [],  # For Geopunt and HERE (need actual addresses)
                'osm': []       # For OSM/Overpass (need POI names)
            }
            
            # For simple transformations, don't call Claude
            original_lower = original_name.lower()
            if 'station' in original_lower:
                variations['address'] = [original_name]  # Keep original for address geocoders
                variations['osm'] = [f"Station {gemeente}", f"{gemeente} Station", "Station"]
                self.claude_cache[cache_key] = variations
                return variations
            
            if 'ziekenhuis' in original_lower or ' zh' in original_lower or 'az ' in original_lower:
                hospital_name = poi_name if poi_name != original_name else f"Ziekenhuis {gemeente}"
                variations['address'] = [original_name]  # Keep original for address geocoders
                variations['osm'] = [f"AZ {hospital_name}", hospital_name, f"Ziekenhuis {gemeente}", 
                                   f"AZ {gemeente}", f"Algemeen Ziekenhuis {gemeente}"]
                self.claude_cache[cache_key] = variations
                return variations
            
            if 'ocmw' in original_lower:
                variations['address'] = [original_name]  # Keep original for address geocoders
                variations['osm'] = [f"OCMW {gemeente}", f"Sociaal Huis {gemeente}", "OCMW", "Sociaal Huis"]
                self.claude_cache[cache_key] = variations
                return variations
            
            if 'wzc' in original_lower or 'woonzorgcentrum' in original_lower:
                # Extract WZC name
                wzc_name = re.sub(r'wzc\s+', '', original_name, flags=re.IGNORECASE).strip()
                variations['address'] = [original_name]
                variations['osm'] = [wzc_name, f"WZC {wzc_name}", f"Woonzorgcentrum {wzc_name}"]
                self.claude_cache[cache_key] = variations
                return variations
            
            # Call Claude for complex cases with improved prompt
            context = f"Location in {gemeente}, Flanders (Belgium)"
            if aard:
                aard_clean = aard.split('/')[0].strip().lower()
                context += f", type: {aard_clean}"
            
            prompt = f"""You are helping improve geocoding for Belgian/Flemish addresses. The address field might contain:
1. A messy street address that needs cleaning
2. A Point of Interest (POI) that's not actually a street

Input: "{original_name}"
Municipality: {gemeente}
Context: {context}

EXAMPLES OF IMPROVEMENTS:

Example 1 - Hospital hidden in street:
Input: "Sint Elisabeth ZH"
→ address: ["Sint Elisabeth ZH"]
→ osm: ["Sint-Elisabeth Ziekenhuis", "AZ Sint-Elisabeth", "Sint Elisabeth"]

Example 2 - WZC (nursing home):
Input: "WZC Verbert Verrijdt - Verberstraat"
→ address: ["Verberstraat"]
→ osm: ["WZC Verbert Verrijdt", "Verbert Verrijdt", "Woonzorgcentrum Verbert Verrijdt"]

Example 3 - Complex location:
Input: "GZA Zorg en Wonen Sint-Mathildis, Sportveldlaan"
→ address: ["Sportveldlaan"]
→ osm: ["GZA Sint-Mathildis", "Sint-Mathildis", "Zorg en Wonen Sint-Mathildis"]

Example 4 - Campus/Institution:
Input: "Jessa Ziekenhuis Campus Virga Jesse, Stadsomvaart"
→ address: ["Stadsomvaart"]
→ osm: ["Jessa Ziekenhuis", "Campus Virga Jesse", "Virga Jesse", "Jessa"]

Example 5 - Medical practice:
Input: "Dr. Dijana ELJUGA, Delacenseriestraat"
→ address: ["Delacenseriestraat"]
→ osm: ["Dr. Dijana Eljuga", "Dokter Eljuga"]

Now process the input and return JSON:
{{
  "address": ["cleaned_street_if_present"],
  "osm": ["poi_variation_1", "poi_variation_2", "poi_variation_3"]
}}

Focus on extracting the actual POI name for OSM, removing prefixes like WZC, AZ, Dr., etc. but also including variations WITH those prefixes."""

            response = self.claude_client.messages.create(
                model="claude-3-haiku-20240307",
                max_tokens=200,
                temperature=0.3,
                messages=[{"role": "user", "content": prompt}]
            )
            
            content = response.content[0].text.strip()
            json_match = re.search(r'\{.*\}', content, re.DOTALL)
            if json_match:
                service_variations = json.loads(json_match.group())
                
                # Process address variations (limit to 2)
                if 'address' in service_variations:
                    address_vars = service_variations['address'][:2]
                    address_vars = [v.replace('{gemeente}', gemeente) for v in address_vars]
                    variations['address'] = address_vars
                
                # Process OSM variations (limit to 4)
                if 'osm' in service_variations:
                    osm_vars = service_variations['osm'][:4]
                    osm_vars = [v.replace('{gemeente}', gemeente) for v in osm_vars]
                    variations['osm'] = osm_vars
                
                self.api_call_counts['claude'] += 1
            
            # Add original as fallback
            if not variations['address']:
                variations['address'] = [original_name]
            elif original_name not in variations['address']:
                variations['address'].append(original_name)
                
            if not variations['osm']:
                variations['osm'] = [poi_name] if poi_name != original_name else [original_name]
            
            self.claude_cache[cache_key] = variations
            return variations
        
        except Exception as e:
            logger.error(f"Claude API error: {e}")
            # Return fallback with original name
            fallback = {
                'address': [original_name],
                'osm': [poi_name if poi_name != original_name else original_name]
            }
            return fallback
    
    def geocode_address_complete(self, straat: str, nummer: str, postcode: str, gemeente: str, aard: str = None) -> Dict:
        """Complete geocoding with all methods - uses improved variations from Claude"""
        
        # Clean house number
        nummer_clean = self.clean_house_number(nummer)
        
        # Check if house number is missing or invalid
        has_valid_number = nummer_clean and nummer_clean not in ['%', '*', '.', '-', '1']
        
        # Create cache key with original nummer for consistency
        cache_key = self.create_address_key(straat, nummer, postcode, gemeente)
        
        # Check if we should retry a previously failed address
        should_retry = self.retry_failed and cache_key in self.failed_cache and cache_key not in self.retry_attempted
        
        # Check caches (unless we're retrying)
        if not should_retry:
            if cache_key in self.geocode_cache:
                result = self.geocode_cache[cache_key].copy()
                if result.get('geocode_method') == 'from_existing':
                    self.api_call_counts['from_existing'] += 1
                return result
            
            if cache_key in self.failed_cache:
                return {
                    'latitude': None,
                    'longitude': None,
                    'geocode_method': None,
                    'geocode_status': 'failed_cached'
                }
        
        result = {
            'latitude': None,
            'longitude': None,
            'geocode_method': None,
            'geocode_status': 'failed'
        }
        
        # If not retrying, try standard methods first
        if not should_retry:
            # PHASE 1: Direct geocoding (NO CLAUDE)
            
            # If no valid house number, try street-only geocoding first
            if not has_valid_number and straat:
                coords = self.geocode_street_only(straat, postcode, gemeente)
                if coords:
                    result.update({
                        'latitude': coords[0],
                        'longitude': coords[1],
                        'geocode_method': 'street_center',
                        'geocode_status': 'success'
                    })
                    self.geocode_cache[cache_key] = result
                    return result
            
            # Try Geopunt with cleaned house number
            coords = self.geocode_geopunt(straat, nummer_clean, postcode, gemeente)
            if coords:
                result.update({
                    'latitude': coords[0],
                    'longitude': coords[1],
                    'geocode_method': 'geopunt',
                    'geocode_status': 'success'
                })
                self.geocode_cache[cache_key] = result
                return result
            
            # Try HERE
            coords = self.geocode_here(straat, nummer_clean, postcode)
            if coords:
                result.update({
                    'latitude': coords[0],
                    'longitude': coords[1],
                    'geocode_method': 'here',
                    'geocode_status': 'success'
                })
                self.geocode_cache[cache_key] = result
                return result
            
            # Try Overpass
            if straat and gemeente and gemeente not in self.overpass_failed_municipalities:
                coords = self.geocode_overpass_quick(straat, gemeente)
                if coords:
                    result.update({
                        'latitude': coords[0],
                        'longitude': coords[1],
                        'geocode_method': 'overpass',
                        'geocode_status': 'success'
                    })
                    self.geocode_cache[cache_key] = result
                    return result
        
        # PHASE 2: Try Claude with improved variations (for failed or retry addresses)
        if straat and gemeente:
            if should_retry:
                logger.debug(f"Retrying previously failed address with Claude: {straat}, {gemeente}")
                self.retry_attempted.add(cache_key)
                self.api_call_counts['retry_claude'] += 1
            else:
                logger.debug(f"Using Claude for failed address: {straat}, {gemeente}")
            
            service_variations = self.generate_address_improvements(straat, gemeente, aard)
            
            # Try address variations with Geopunt and HERE
            for variation in service_variations.get('address', []):
                # Try Geopunt with address variation
                coords = self.geocode_geopunt(variation, nummer_clean, postcode, gemeente)
                if coords:
                    result.update({
                        'latitude': coords[0],
                        'longitude': coords[1],
                        'geocode_method': 'claude_geopunt' if not should_retry else 'retry_claude_geopunt',
                        'geocode_status': 'success'
                    })
                    self.geocode_cache[cache_key] = result
                    # Remove from failed cache if it was there
                    if cache_key in self.failed_cache:
                        del self.failed_cache[cache_key]
                    return result
                
                # Try HERE with address variation
                coords = self.geocode_here(variation, nummer_clean, postcode)
                if coords:
                    result.update({
                        'latitude': coords[0],
                        'longitude': coords[1],
                        'geocode_method': 'claude_here' if not should_retry else 'retry_claude_here',
                        'geocode_status': 'success'
                    })
                    self.geocode_cache[cache_key] = result
                    # Remove from failed cache if it was there
                    if cache_key in self.failed_cache:
                        del self.failed_cache[cache_key]
                    return result
            
            # Try OSM POI name variations with Overpass (no house number needed)
            if gemeente not in self.overpass_failed_municipalities:
                for variation in service_variations.get('osm', [])[:4]:  # Try up to 4 OSM variations
                    coords = self.geocode_overpass_quick(variation, gemeente)
                    if coords:
                        result.update({
                            'latitude': coords[0],
                            'longitude': coords[1],
                            'geocode_method': 'claude_street_center' if not should_retry else 'retry_claude_street_center',
                            'geocode_status': 'success'
                        })
                        self.geocode_cache[cache_key] = result
                        # Remove from failed cache if it was there
                        if cache_key in self.failed_cache:
                            del self.failed_cache[cache_key]
                        return result
        
        # Cache the failure
        self.failed_cache[cache_key] = True
        return result
    
    def process_batch_parallel(self, addresses: List[Dict]) -> List[Dict]:
        """Process a batch of addresses in parallel"""
        results = []
        
        with ThreadPoolExecutor(max_workers=self.max_workers) as executor:
            future_to_address = {
                executor.submit(self.geocode_address_complete, 
                               addr['Straat'], addr['Nr'], addr['Postcode'], 
                               addr['Gemeente'], addr.get('Aard')): addr 
                for addr in addresses
            }
            
            for future in as_completed(future_to_address):
                try:
                    result = future.result(timeout=30)
                    results.append(result)
                except Exception as e:
                    logger.error(f"Error processing address: {e}")
                    results.append({
                        'latitude': None,
                        'longitude': None,
                        'geocode_method': None,
                        'geocode_status': 'error'
                    })
        
        return results
    
    def process_dual_address_dataframe(self, df: pd.DataFrame, batch_size: int = 10) -> pd.DataFrame:
        """Process dataframe with dual addresses per row"""
        start_time = datetime.now()
        
        logger.info(f"Processing {len(df)} rows with dual addresses...")
        
        # Prepare columns for results
        result_columns = [
            'latitude_1', 'longitude_1', 'geocode_method_1', 'geocode_status_1',
            'latitude_2', 'longitude_2', 'geocode_method_2', 'geocode_status_2'
        ]
        
        for col in result_columns:
            df[col] = None
        
        # Collect all unique addresses from both address sets
        unique_addresses = []
        
        for idx, row in df.iterrows():
            # Address 1
            straat_1 = self.clean_address_component(row.get('Straat-1'))
            nr_1 = row.get('Nr-1')  # Don't clean yet, let geocode_address_complete handle it
            postcode_1 = self.clean_address_component(row.get('Postcode-1'))
            gemeente_1 = self.clean_address_component(row.get('Gemeente-1'))
            
            if straat_1 or gemeente_1:
                unique_addresses.append({
                    'Straat': straat_1,
                    'Nr': nr_1,
                    'Postcode': postcode_1,
                    'Gemeente': gemeente_1,
                    'Aard': row.get('Aard'),
                    'index': idx,
                    'address_num': 1
                })
            
            # Address 2
            straat_2 = self.clean_address_component(row.get('Straat-2'))
            nr_2 = row.get('Nr-2')  # Don't clean yet
            postcode_2 = self.clean_address_component(row.get('Postcode-2'))
            gemeente_2 = self.clean_address_component(row.get('Gemeente-2'))
            
            if straat_2 or gemeente_2:
                unique_addresses.append({
                    'Straat': straat_2,
                    'Nr': nr_2,
                    'Postcode': postcode_2,
                    'Gemeente': gemeente_2,
                    'Aard': row.get('Aard'),
                    'index': idx,
                    'address_num': 2
                })
        
        # Remove duplicates while preserving the index information
        unique_dict = {}
        for addr in unique_addresses:
            key = self.create_address_key(addr['Straat'], addr['Nr'], addr['Postcode'], addr['Gemeente'])
            if key not in unique_dict:
                unique_dict[key] = []
            unique_dict[key].append((addr['index'], addr['address_num']))
        
        logger.info(f"Found {len(unique_dict)} unique addresses across both address fields")
        
        # Check how many are already cached
        already_cached = sum(1 for key in unique_dict.keys() if key in self.geocode_cache)
        already_failed = sum(1 for key in unique_dict.keys() if key in self.failed_cache)
        to_process = len(unique_dict) - already_cached - already_failed
        
        logger.info(f"Already geocoded: {already_cached} addresses")
        logger.info(f"Already failed (cached): {already_failed} addresses")
        
        if self.retry_failed:
            logger.info(f"Retry mode enabled: Will retry {already_failed} failed addresses with Claude")
            to_process = to_process + already_failed
        
        logger.info(f"Need to geocode: {to_process} addresses")
        
        # Process unique addresses with progress tracking
        processed_count = 0
        total_to_process = len(unique_dict)
        
        for key, locations in unique_dict.items():
            processed_count += 1
            
            # Progress logging every 100 addresses
            if processed_count % 100 == 0:
                elapsed = (datetime.now() - start_time).total_seconds()
                rate = processed_count / elapsed if elapsed > 0 else 0
                remaining = (total_to_process - processed_count) / rate if rate > 0 else 0
                logger.info(f"Progress: {processed_count}/{total_to_process} addresses processed "
                          f"({processed_count/total_to_process*100:.1f}%) - "
                          f"Rate: {rate:.1f} addr/sec - "
                          f"Est. remaining: {remaining/60:.1f} minutes")
            
            # Get one address info for geocoding
            first_addr = next(addr for addr in unique_addresses 
                            if self.create_address_key(addr['Straat'], addr['Nr'], 
                                                     addr['Postcode'], addr['Gemeente']) == key)
            
            # Geocode the address
            result = self.geocode_address_complete(
                first_addr['Straat'], 
                first_addr['Nr'], 
                first_addr['Postcode'], 
                first_addr['Gemeente'],
                first_addr['Aard']
            )
            
            # Apply result to all instances of this address
            for idx, address_num in locations:
                if address_num == 1:
                    df.at[idx, 'latitude_1'] = result.get('latitude')
                    df.at[idx, 'longitude_1'] = result.get('longitude')
                    df.at[idx, 'geocode_method_1'] = result.get('geocode_method')
                    df.at[idx, 'geocode_status_1'] = result.get('geocode_status')
                else:
                    df.at[idx, 'latitude_2'] = result.get('latitude')
                    df.at[idx, 'longitude_2'] = result.get('longitude')
                    df.at[idx, 'geocode_method_2'] = result.get('geocode_method')
                    df.at[idx, 'geocode_status_2'] = result.get('geocode_status')
            
            # Save cache periodically
            if processed_count % 500 == 0:
                self.save_all_caches()
                logger.info(f"Cache saved at {processed_count} addresses")
        
        # Save final cache
        self.save_all_caches()
        
        processing_time = (datetime.now() - start_time).total_seconds()
        logger.info(f"Total processing time: {processing_time:.2f} seconds ({processing_time/60:.1f} minutes)")
        logger.info(f"Average time per address: {processing_time/total_to_process:.2f} seconds")
        
        # Log final API usage
        logger.info(f"API calls made: {self.api_call_counts}")
        
        return df
    
    def get_statistics(self, df: pd.DataFrame) -> Dict:
        """Get statistics for dual address geocoding"""
        total_address_1 = df['Straat-1'].notna().sum()
        total_address_2 = df['Straat-2'].notna().sum()
        
        success_1 = (df['geocode_status_1'] == 'success').sum() + \
                   (df['geocode_status_1'] == 'success_cached').sum()
        success_2 = (df['geocode_status_2'] == 'success').sum() + \
                   (df['geocode_status_2'] == 'success_cached').sum()
        
        # Count methods for address 1
        methods_1 = df[df['geocode_status_1'].isin(['success', 'success_cached'])]['geocode_method_1'].value_counts().to_dict()
        methods_2 = df[df['geocode_status_2'].isin(['success', 'success_cached'])]['geocode_method_2'].value_counts().to_dict()
        
        # Combine methods
        all_methods = {}
        for method, count in methods_1.items():
            all_methods[method] = all_methods.get(method, 0) + count
        for method, count in methods_2.items():
            all_methods[method] = all_methods.get(method, 0) + count
        
        stats = {
            'total_rows': len(df),
            'total_addresses': total_address_1 + total_address_2,
            'address_1': {
                'total': total_address_1,
                'success': success_1,
                'failed': total_address_1 - success_1,
                'success_rate': (success_1 / total_address_1 * 100) if total_address_1 > 0 else 0,
                'methods': methods_1
            },
            'address_2': {
                'total': total_address_2,
                'success': success_2,
                'failed': total_address_2 - success_2,
                'success_rate': (success_2 / total_address_2 * 100) if total_address_2 > 0 else 0,
                'methods': methods_2
            },
            'combined_methods': all_methods,
            'overall_success_rate': ((success_1 + success_2) / (total_address_1 + total_address_2) * 100) 
                                   if (total_address_1 + total_address_2) > 0 else 0,
            'api_calls': self.api_call_counts.copy(),
            'cache_size': len(self.geocode_cache),
            'failed_cache_size': len(self.failed_cache),
            'claude_cache_size': len(self.claude_cache)
        }
        
        return stats


def main():
    # Configuration
    HERE_API_KEY = ""
    CLAUDE_API_KEY = "" 
    
    # File paths
    input_file = r"C:\Users\Lars\OneDrive - UGent\03_Varia\Caroline\MTW2024_cleaned.csv"
    output_file = "geocoded_dual_addresses_2024.csv"
    existing_geocoded = r"C:\Users\Lars\OneDrive - UGent\03_Varia\Caroline\geocoded_addresses.csv"
    
    # Read the new dataset
    logger.info(f"Reading input file: {input_file}")
    df = pd.read_csv(input_file, delimiter=';', encoding='ANSI')
    
    # Initialize geocoder with retry_failed=True to retry previously failed addresses
    geocoder = DualAddressGeocoder(
        HERE_API_KEY,
        CLAUDE_API_KEY,
        max_workers=5,
        existing_geocoded_file=existing_geocoded,
        retry_failed=True  # This will retry failed addresses with Claude
    )
    
    # Process the dataframe
    df_geocoded = geocoder.process_dual_address_dataframe(df, batch_size=10)
    
    # Save results
    df_geocoded.to_csv(output_file, index=False, sep=';', encoding='utf-8')
    logger.info(f"Results saved to {output_file}")
    
    # Print statistics
    stats = geocoder.get_statistics(df_geocoded)
    
    print("\n" + "="*60)
    print("DUAL ADDRESS GEOCODING STATISTICS")
    print("="*60)
    print(f"Total rows processed: {stats['total_rows']:,}")
    print(f"Total addresses to geocode: {stats['total_addresses']:,}")
    
    print("\n--- Address Set 1 ---")
    print(f"  Total: {stats['address_1']['total']:,}")
    print(f"  Success: {stats['address_1']['success']:,} ({stats['address_1']['success_rate']:.1f}%)")
    print(f"  Failed: {stats['address_1']['failed']:,}")
    
    print("\n--- Address Set 2 ---")
    print(f"  Total: {stats['address_2']['total']:,}")
    print(f"  Success: {stats['address_2']['success']:,} ({stats['address_2']['success_rate']:.1f}%)")
    print(f"  Failed: {stats['address_2']['failed']:,}")
    
    print(f"\n--- Overall Success Rate: {stats['overall_success_rate']:.1f}% ---")
    
    print("\n--- API Usage ---")
    print(f"  From existing file: {stats['api_calls'].get('from_existing', 0):,}")
    print(f"  Geopunt calls: {stats['api_calls'].get('geopunt', 0):,}")
    print(f"  HERE calls: {stats['api_calls'].get('here', 0):,}")
    print(f"  Overpass calls: {stats['api_calls'].get('overpass', 0):,}")
    print(f"  Claude calls: {stats['api_calls'].get('claude', 0):,}")
    print(f"  Retry with Claude: {stats['api_calls'].get('retry_claude', 0):,}")
    
    print(f"\n--- Cache Statistics ---")
    print(f"  Total cached addresses: {stats['cache_size']:,}")
    print(f"  Failed addresses cached: {stats['failed_cache_size']:,}")
    print(f"  Claude variations cached: {stats['claude_cache_size']:,}")
    
    print("\n--- Methods Breakdown ---")
    for method, count in sorted(stats['combined_methods'].items(), key=lambda x: x[1], reverse=True):
        print(f"  {method}: {count:,}")


if __name__ == "__main__":
    main()