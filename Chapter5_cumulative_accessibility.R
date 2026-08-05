#Script of cumulative accessibility by PT
#2024
#We need to run the cumulative accessibility for a general Thursday (18 January 2024) 
#At different times: 6u - 9u - 12u - 15u - 18u - 21u 
#Total of 6 runs

# Packages 
pkgs <- c(
  'r5r', 'accessibility', 'rJavaEnv', 'ggplot2', 'mapview',
  'quantreg', 'dplyr', 'h3jsr', 'sf', 'data.table'
)
# install.packages(pkgs)

# Sufficient memory for Java / R5
options(java.parameters = "-Xmx8G")

# Libraries 
library(r5r)
library(h3jsr)
library(dplyr)
library(mapview)
library(ggplot2)
library(sf)
library(data.table)
library(tidyverse)
library(readr)

# ----------------------------
# 1) Loading networks (R5R)
# ----------------------------
# path_2022a <- system.file("extdata_Flanders_2022a", package = "r5r")
# path_2022b <- system.file("extdata_Flanders_2022b", package = "r5r")
# path_2023  <- system.file("extdata_Flanders_2023",  package = "r5r")
path_2024  <- system.file("extdata_Flanders_2024",  package = "r5r")
# path_2024  <- system.file("extdata_Flanders_2024_test_train",  package = "r5r")

# r5r_network_2022a <- build_network(data_path = path_2022a, verbose = FALSE)
# r5r_network_2022b <- build_network(data_path = path_2022b, verbose = FALSE)
# r5r_network_2023  <- build_network(data_path = path_2023,  verbose = FALSE)
r5r_network_2024 <- build_network(
  data_path = path_2024,
  overwrite = TRUE,
  verbose = TRUE
)

#---------------------
# 2) Select the unique origin points (analysis per Mobitwin user and not per Mobitwin trip)
#---------------------
#reading csv-files origin points
points_origin_2022 <- read.csv("Points_origin_2022.csv", sep=",")
points_origin_2023 <- read.csv("Points_origin_2023.csv", sep=",")
points_origin_2024 <- read.csv("Points_origin_2024.csv", sep=",")



#In case the coordinates are written with the scientific notation --> fix this first
library(readr)

points_origin_2022 <- read_delim(
  "Points_origin_2022.csv",
  delim = ",",
  locale = locale(
    decimal_mark = ".",
    grouping_mark = ""
  ),
  col_types = cols(
    lon = col_character(),
    lat = col_character(),
    .default = col_character()
  )
)
points_origin_2022 <- points_origin_2022 %>%
  mutate(
    lon_raw = parse_number(lon),
    lat_raw = parse_number(lat),
    
    lon = if_else(
      abs(lon_raw) > 180,
      lon_raw / 10^floor(log10(abs(lon_raw))),
      lon_raw
    ),
    
    lat = if_else(
      abs(lat_raw) > 90,
      lat_raw / 10^(floor(log10(abs(lat_raw))) - 1),
      lat_raw
    )
  )

points_origin_2023 <- read_delim(
  "Points_origin_2023.csv",
  delim = ",",
  locale = locale(
    decimal_mark = ".",
    grouping_mark = ""
  ),
  col_types = cols(
    lon = col_character(),
    lat = col_character(),
    .default = col_character()
  )
)
points_origin_2023 <- points_origin_2023 %>%
  mutate(
    lon_raw = parse_number(lon),
    lat_raw = parse_number(lat),
    
    lon = if_else(
      abs(lon_raw) > 180,
      lon_raw / 10^floor(log10(abs(lon_raw))),
      lon_raw
    ),
    
    lat = if_else(
      abs(lat_raw) > 90,
      lat_raw / 10^(floor(log10(abs(lat_raw))) - 1),
      lat_raw
    )
  )

points_origin_2024 <- read_delim(
  "Points_origin_2024.csv",
  delim = ",",
  locale = locale(
    decimal_mark = ".",
    grouping_mark = ""
  ),
  col_types = cols(
    lon = col_character(),
    lat = col_character(),
    .default = col_character()
  )
)
points_origin_2024 <- points_origin_2024 %>%
  mutate(
    lon_raw = parse_number(lon),
    lat_raw = parse_number(lat),
    
    lon = if_else(
      abs(lon_raw) > 180,
      lon_raw / 10^floor(log10(abs(lon_raw))),
      lon_raw
    ),
    
    lat = if_else(
      abs(lat_raw) > 90,
      lat_raw / 10^(floor(log10(abs(lat_raw))) - 1),
      lat_raw
    )
  )






#Changing ID to unique trip_ID
points_origin_2022 <- points_origin_2022 %>%
  mutate(Trip_ID = paste0(ID,"_","2022"))
points_origin_2023 <- points_origin_2023 %>%
  mutate(Trip_ID = paste0(ID,"_","2023"))
points_origin_2024 <- points_origin_2024 %>%
  mutate(Trip_ID = paste0(ID,"_","2024"))

#Combining to one data frame
all_origins <- rbind(points_origin_2022, points_origin_2023, points_origin_2024)

#Giving all trips a new unique ID-number
num_row <- nrow(all_origins)
temp_ID <- c(1:num_row)
all_origins <- cbind(temp_ID, all_origins)

#Selecting unique origin points
all_origins <- all_origins %>%
  mutate(lonlat = paste0(lon, "_", lat))
all_unique_origins <- all_origins %>%
  distinct(lonlat, .keep_all = TRUE)
num_row <- nrow(all_unique_origins)
new_ID <- c(1:num_row)
all_unique_origins <- cbind(new_ID, all_unique_origins)


# -----------------------------------------
# 3) Land use shapefiles 
# -----------------------------------------

#Make sure all the necessary shapefiles are in the folder of the working directory


#Hospitals
hospitals_path <- "Hospitals.shp"

hospitals_sf <- st_read(hospitals_path, quiet = TRUE)

#     To WGS84 & getting coordinates
if (is.na(st_crs(hospitals_sf))) {
  stop("CRS of 'hospitals' is unknown. Set CRS first.")
}
hospitals_sf <- st_transform(hospitals_sf, 4326)

#     Making clean ID column for hospitals
if (!("hospitals_id" %in% names(hospitals_sf))) {
  hospitals_sf$hospitals_id <- as.character(seq_len(nrow(hospitals_sf)))
} else {
  hospitals_sf$hospitals_id <- as.character(hospitals_sf$hospitals_id)
}

hospitals_df <- hospitals_sf |>
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2],
    opportunity = 1L          # every hospitals = 1 opportunity
  ) |>
  st_drop_geometry() |>
  select(id = hospitals_id, lon, lat, opportunity)


#Adult day care centres
adult_day_care_centres_path <- "Adult_day_care_centres.shp"

adult_day_care_centres_sf <- st_read(adult_day_care_centres_path, quiet = TRUE)

#     To WGS84 & getting coordinates
if (is.na(st_crs(adult_day_care_centres_sf))) {
  stop("CRS of 'adult_day_care_centres' is unknown. Set CRS first.")
}
adult_day_care_centres_sf <- st_transform(adult_day_care_centres_sf, 4326)

#     Making clean ID column for adult_day_care_centres
if (!("adult_day_care_centres_id" %in% names(adult_day_care_centres_sf))) {
  adult_day_care_centres_sf$adult_day_care_centres_id <- as.character(seq_len(nrow(adult_day_care_centres_sf)))
} else {
  adult_day_care_centres_sf$adult_day_care_centres_id <- as.character(adult_day_care_centres_sf$adult_day_care_centres_id)
}

adult_day_care_centres_df <- adult_day_care_centres_sf |>
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2],
    opportunity = 1L          # every adult_day_care_centres = 1 opportunity
  ) |>
  st_drop_geometry() |>
  select(id = adult_day_care_centres_id, lon, lat, opportunity)


#Socio-cultural activities
socio_cultural_activities_path <- "Socio-cultural amenities.shp"

socio_cultural_activities_sf <- st_read(socio_cultural_activities_path, quiet = TRUE)

#     To WGS84 & getting coordinates
if (is.na(st_crs(socio_cultural_activities_sf))) {
  stop("CRS of 'socio_cultural_activities' is unknown. Set CRS first.")
}
socio_cultural_activities_sf <- st_transform(socio_cultural_activities_sf, 4326)

#     Making clean ID column for socio_cultural_activities
if (!("socio_cultural_activities_id" %in% names(socio_cultural_activities_sf))) {
  socio_cultural_activities_sf$socio_cultural_activities_id <- as.character(seq_len(nrow(socio_cultural_activities_sf)))
} else {
  socio_cultural_activities_sf$socio_cultural_activities_id <- as.character(socio_cultural_activities_sf$socio_cultural_activities_id)
}

socio_cultural_activities_df <- socio_cultural_activities_sf |>
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2],
    opportunity = 1L          # every socio_cultural_activities = 1 opportunity
  ) |>
  st_drop_geometry() |>
  select(id = socio_cultural_activities_id, lon, lat, opportunity)


#Groceries
groceries_path <- "Groceries.shp"

groceries_sf <- st_read(groceries_path, quiet = TRUE)

#     To WGS84 & getting coordinates
if (is.na(st_crs(groceries_sf))) {
  stop("CRS of 'groceries' is unknown. Set CRS first.")
}
groceries_sf <- st_transform(groceries_sf, 4326)

#     Making clean ID column for groceries
if (!("groceries_id" %in% names(groceries_sf))) {
  groceries_sf$groceries_id <- as.character(seq_len(nrow(groceries_sf)))
} else {
  groceries_sf$groceries_id <- as.character(groceries_sf$groceries_id)
}

groceries_df <- groceries_sf |>
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2],
    opportunity = 1L          # every groceries = 1 opportunity
  ) |>
  st_drop_geometry() |>
  select(id = groceries_id, lon, lat, opportunity)


#Speech therapists
speech_therapists_path <- "Speech_therapists.shp"

speech_therapists_sf <- st_read(speech_therapists_path, quiet = TRUE)

#     To WGS84 & getting coordinates
if (is.na(st_crs(speech_therapists_sf))) {
  stop("CRS of 'speech_therapists' is unknown. Set CRS first.")
}
speech_therapists_sf <- st_transform(speech_therapists_sf, 4326)

#     Making clean ID column for speech_therapists
if (!("speech_therapists_id" %in% names(speech_therapists_sf))) {
  speech_therapists_sf$speech_therapists_id <- as.character(seq_len(nrow(speech_therapists_sf)))
} else {
  speech_therapists_sf$speech_therapists_id <- as.character(speech_therapists_sf$speech_therapists_id)
}

speech_therapists_df <- speech_therapists_sf |>
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2],
    opportunity = 1L          # every speech_therapists = 1 opportunity
  ) |>
  st_drop_geometry() |>
  select(id = speech_therapists_id, lon, lat, opportunity)


#Physiotherapists
physiotherapists_path <- "Physiotherapists.shp"

physiotherapists_sf <- st_read(physiotherapists_path, quiet = TRUE)

#     To WGS84 & getting coordinates
if (is.na(st_crs(physiotherapists_sf))) {
  stop("CRS of 'physiotherapists' is unknown. Set CRS first.")
}
physiotherapists_sf <- st_transform(physiotherapists_sf, 4326)

#     Making clean ID column for Physiotherapists
if (!("physiotherapists_id" %in% names(physiotherapists_sf))) {
  physiotherapists_sf$physiotherapists_id <- as.character(seq_len(nrow(physiotherapists_sf)))
} else {
  physiotherapists_sf$physiotherapists_id <- as.character(physiotherapists_sf$physiotherapists_id)
}

physiotherapists_df <- physiotherapists_sf |>
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2],
    opportunity = 1L          # every Physiotherapists = 1 opportunity
  ) |>
  st_drop_geometry() |>
  select(id = physiotherapists_id, lon, lat, opportunity)


#Dietitians
dietitians_path <- "Dieticians.shp"

dietitians_sf <- st_read(dietitians_path, quiet = TRUE)

#     To WGS84 & getting coordinates
if (is.na(st_crs(dietitians_sf))) {
  stop("CRS of 'dietitians' is unknown. Set CRS first.")
}
dietitians_sf <- st_transform(dietitians_sf, 4326)

#     Making clean ID column for dietitians
if (!("dietitians_id" %in% names(dietitians_sf))) {
  dietitians_sf$dietitians_id <- as.character(seq_len(nrow(dietitians_sf)))
} else {
  dietitians_sf$dietitians_id <- as.character(dietitians_sf$dietitians_id)
}

dietitians_df <- dietitians_sf |>
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2],
    opportunity = 1L          # every dietitians = 1 opportunity
  ) |>
  st_drop_geometry() |>
  select(id = dietitians_id, lon, lat, opportunity)


#Occupational therapists
occupational_therapists_path <- "Occupational_therapists.shp"

occupational_therapists_sf <- st_read(occupational_therapists_path, quiet = TRUE)

#     To WGS84 & getting coordinates
if (is.na(st_crs(occupational_therapists_sf))) {
  stop("CRS of 'occupational_therapists' is unknown. Set CRS first.")
}
occupational_therapists_sf <- st_transform(occupational_therapists_sf, 4326)

#     Making clean ID column for occupational_therapists
if (!("occupational_therapists_id" %in% names(occupational_therapists_sf))) {
  occupational_therapists_sf$occupational_therapists_id <- as.character(seq_len(nrow(occupational_therapists_sf)))
} else {
  occupational_therapists_sf$occupational_therapists_id <- as.character(occupational_therapists_sf$occupational_therapists_id)
}

occupational_therapists_df <- occupational_therapists_sf |>
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2],
    opportunity = 1L          # every occupational_therapists = 1 opportunity
  ) |>
  st_drop_geometry() |>
  select(id = occupational_therapists_id, lon, lat, opportunity)


#General practitioners 
general_practitioners_path <- "General_practitioners.shp"

general_practitioners_sf <- st_read(general_practitioners_path, quiet = TRUE)

#     To WGS84 & getting coordinates
if (is.na(st_crs(general_practitioners_sf))) {
  stop("CRS of 'general_practitioners' is unknown. Set CRS first.")
}
general_practitioners_sf <- st_transform(general_practitioners_sf, 4326)

#     Making clean ID column for general_practitioners
if (!("general_practitioners_id" %in% names(general_practitioners_sf))) {
  general_practitioners_sf$general_practitioners_id <- as.character(seq_len(nrow(general_practitioners_sf)))
} else {
  general_practitioners_sf$general_practitioners_id <- as.character(general_practitioners_sf$general_practitioners_id)
}

general_practitioners_df <- general_practitioners_sf |>
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2],
    opportunity = 1L          # every general_practitioners = 1 opportunity
  ) |>
  st_drop_geometry() |>
  select(id = general_practitioners_id, lon, lat, opportunity)


#Hairdressers
hairdressers_path <- "Hairdressers.shp"

hairdressers_sf <- st_read(hairdressers_path, quiet = TRUE)

#     To WGS84 & getting coordinates
if (is.na(st_crs(hairdressers_sf))) {
  stop("CRS of 'hairdressers' is unknown. Set CRS first.")
}
hairdressers_sf <- st_transform(hairdressers_sf, 4326)

#     Making clean ID column for hairdressers
if (!("hairdressers_id" %in% names(hairdressers_sf))) {
  hairdressers_sf$hairdressers_id <- as.character(seq_len(nrow(hairdressers_sf)))
} else {
  hairdressers_sf$hairdressers_id <- as.character(hairdressers_sf$hairdressers_id)
}

hairdressers_df <- hairdressers_sf |>
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2],
    opportunity = 1L          # every hairdressers = 1 opportunity
  ) |>
  st_drop_geometry() |>
  select(id = hairdressers_id, lon, lat, opportunity)


#Restaurants
restaurants_path <- "Restaurants.shp"

restaurants_sf <- st_read(restaurants_path, quiet = TRUE)

#     To WGS84 & getting coordinates
if (is.na(st_crs(restaurants_sf))) {
  stop("CRS of 'restaurants' is unknown. Set CRS first.")
}
restaurants_sf <- st_transform(restaurants_sf, 4326)

#     Making clean ID column for restaurants
if (!("restaurants_id" %in% names(restaurants_sf))) {
  restaurants_sf$restaurants_id <- as.character(seq_len(nrow(restaurants_sf)))
} else {
  restaurants_sf$restaurants_id <- as.character(restaurants_sf$restaurants_id)
}

restaurants_df <- restaurants_sf |>
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2],
    opportunity = 1L          # every restaurants = 1 opportunity
  ) |>
  st_drop_geometry() |>
  select(id = restaurants_id, lon, lat, opportunity)


#Dentists
dentists_path <- "Dentists.shp"

dentists_sf <- st_read(dentists_path, quiet = TRUE)

#     To WGS84 & getting coordinates
if (is.na(st_crs(dentists_sf))) {
  stop("CRS of 'dentists' is unknown. Set CRS first.")
}
dentists_sf <- st_transform(dentists_sf, 4326)

#     Making clean ID column for dentists
if (!("dentists_id" %in% names(dentists_sf))) {
  dentists_sf$dentists_id <- as.character(seq_len(nrow(dentists_sf)))
} else {
  dentists_sf$dentists_id <- as.character(dentists_sf$dentists_id)
}

dentists_df <- dentists_sf |>
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2],
    opportunity = 1L          # every dentists = 1 opportunity
  ) |>
  st_drop_geometry() |>
  select(id = dentists_id, lon, lat, opportunity)


#Town halls
town_halls_path <- "Town_halls.shp"

town_halls_sf <- st_read(town_halls_path, quiet = TRUE)

#     To WGS84 & getting coordinates
if (is.na(st_crs(town_halls_sf))) {
  stop("CRS of 'town_halls' is unknown. Set CRS first.")
}
town_halls_sf <- st_transform(town_halls_sf, 4326)

#     Making clean ID column for town_halls
if (!("town_halls_id" %in% names(town_halls_sf))) {
  town_halls_sf$town_halls_id <- as.character(seq_len(nrow(town_halls_sf)))
} else {
  town_halls_sf$town_halls_id <- as.character(town_halls_sf$town_halls_id)
}

town_halls_df <- town_halls_sf |>
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2],
    opportunity = 1L          # every town_halls = 1 opportunity
  ) |>
  st_drop_geometry() |>
  select(id = town_halls_id, lon, lat, opportunity)


#Local service centres
local_service_centres_path <- "Local service centres.shp"

local_service_centres_sf <- st_read(local_service_centres_path, quiet = TRUE)

#     To WGS84 & getting coordinates
if (is.na(st_crs(local_service_centres_sf))) {
  stop("CRS of 'local_service_centres' is unknown. Set CRS first.")
}
local_service_centres_sf <- st_transform(local_service_centres_sf, 4326)

#     Making clean ID column for local_service_centres
if (!("local_service_centres_id" %in% names(local_service_centres_sf))) {
  local_service_centres_sf$local_service_centres_id <- as.character(seq_len(nrow(local_service_centres_sf)))
} else {
  local_service_centres_sf$local_service_centres_id <- as.character(local_service_centres_sf$local_service_centres_id)
}

local_service_centres_df <- local_service_centres_sf |>
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2],
    opportunity = 1L          # every local_service_centres = 1 opportunity
  ) |>
  st_drop_geometry() |>
  select(id = local_service_centres_id, lon, lat, opportunity)


#Special needs education
special_needs_education_path <- "Special_needs_education.shp"

special_needs_education_sf <- st_read(special_needs_education_path, quiet = TRUE)

#     To WGS84 & getting coordinates
if (is.na(st_crs(special_needs_education_sf))) {
  stop("CRS of 'special_needs_education' is unknown. Set CRS first.")
}
special_needs_education_sf <- st_transform(special_needs_education_sf, 4326)

#     Making clean ID column for special_needs_education
if (!("special_needs_education_id" %in% names(special_needs_education_sf))) {
  special_needs_education_sf$special_needs_education_id <- as.character(seq_len(nrow(special_needs_education_sf)))
} else {
  special_needs_education_sf$special_needs_education_id <- as.character(special_needs_education_sf$special_needs_education_id)
}

special_needs_education_df <- special_needs_education_sf |>
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2],
    opportunity = 1L          # every special_needs_education = 1 opportunity
  ) |>
  st_drop_geometry() |>
  select(id = special_needs_education_id, lon, lat, opportunity)


#Crematoriums 
crematoriums_path <- "Crematoriums_Flanders.shp"

crematoriums_sf <- st_read(crematoriums_path, quiet = TRUE)

#     To WGS84 & getting coordinates
if (is.na(st_crs(crematoriums_sf))) {
  stop("CRS of 'crematoriums' is unknown. Set CRS first.")
}
crematoriums_sf <- st_transform(crematoriums_sf, 4326)

#     Making clean ID column for crematoriums
if (!("crematoriums_id" %in% names(crematoriums_sf))) {
  crematoriums_sf$crematoriums_id <- as.character(seq_len(nrow(crematoriums_sf)))
} else {
  crematoriums_sf$crematoriums_id <- as.character(crematoriums_sf$crematoriums_id)
}

crematoriums_df <- crematoriums_sf |>
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2],
    opportunity = 1L          # every crematoriums = 1 opportunity
  ) |>
  st_drop_geometry() |>
  select(id = crematoriums_id, lon, lat, opportunity)


#Public transport stops
public_transport_stops_path <- "Public_transport_stops.shp"

public_transport_stops_sf <- st_read(public_transport_stops_path, quiet = TRUE)

#     To WGS84 & getting coordinates
if (is.na(st_crs(public_transport_stops_sf))) {
  stop("CRS of 'public_transport_stops' is unknown. Set CRS first.")
}
public_transport_stops_sf <- st_transform(public_transport_stops_sf, 4326)

#     Making clean ID column for public_transport_stops
if (!("public_transport_stops_id" %in% names(public_transport_stops_sf))) {
  public_transport_stops_sf$public_transport_stops_id <- as.character(seq_len(nrow(public_transport_stops_sf)))
} else {
  public_transport_stops_sf$public_transport_stops_id <- as.character(public_transport_stops_sf$public_transport_stops_id)
}

public_transport_stops_df <- public_transport_stops_sf |>
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2],
    opportunity = 1L          # every public_transport_stops = 1 opportunity
  ) |>
  st_drop_geometry() |>
  select(id = public_transport_stops_id, lon, lat, opportunity)

#Nursing homes
nursing_homes_path <- "Nursing homes.shp"

nursing_homes_sf <- st_read(nursing_homes_path, quiet = TRUE)

#     To WGS84 & getting coordinates
if (is.na(st_crs(nursing_homes_sf))) {
  stop("CRS of 'nursing_homes' is unknown. Set CRS first.")
}
nursing_homes_sf <- st_transform(nursing_homes_sf, 4326)

#     Making clean ID column for nursing_homes
if (!("nursing_homes_id" %in% names(nursing_homes_sf))) {
  nursing_homes_sf$nursing_homes_id <- as.character(seq_len(nrow(nursing_homes_sf)))
} else {
  nursing_homes_sf$nursing_homes_id <- as.character(nursing_homes_sf$nursing_homes_id)
}

nursing_homes_df <- nursing_homes_sf |>
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2],
    opportunity = 1L          # every nursing_homes = 1 opportunity
  ) |>
  st_drop_geometry() |>
  select(id = nursing_homes_id, lon, lat, opportunity)


#Pharmacies
pharmacies_path <- "Pharmacies.shp"

pharmacies_sf <- st_read(pharmacies_path, quiet = TRUE)

#     To WGS84 & getting coordinates
if (is.na(st_crs(pharmacies_sf))) {
  stop("CRS of 'pharmacies' is unknown. Set CRS first.")
}
pharmacies_sf <- st_transform(pharmacies_sf, 4326)

#     Making clean ID column for pharmacies
if (!("pharmacies_id" %in% names(pharmacies_sf))) {
  pharmacies_sf$pharmacies_id <- as.character(seq_len(nrow(pharmacies_sf)))
} else {
  pharmacies_sf$pharmacies_id <- as.character(pharmacies_sf$pharmacies_id)
}

pharmacies_df <- pharmacies_sf |>
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2],
    opportunity = 1L          # every pharmacies = 1 opportunity
  ) |>
  st_drop_geometry() |>
  select(id = pharmacies_id, lon, lat, opportunity)


#Cemeteries
cemeteries_path <- "Cemeteries.shp"

cemeteries_sf <- st_read(cemeteries_path, quiet = TRUE)

#     To WGS84 & getting coordinates
if (is.na(st_crs(cemeteries_sf))) {
  stop("CRS of 'cemeteries' is unknown. Set CRS first.")
}
cemeteries_sf <- st_transform(cemeteries_sf, 4326)

#     Making clean ID column for cemeteries
if (!("cemeteries_id" %in% names(cemeteries_sf))) {
  cemeteries_sf$cemeteries_id <- as.character(seq_len(nrow(cemeteries_sf)))
} else {
  cemeteries_sf$cemeteries_id <- as.character(cemeteries_sf$cemeteries_id)
}

cemeteries_df <- cemeteries_sf |>
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2],
    opportunity = 1L          # every cemeteries = 1 opportunity
  ) |>
  st_drop_geometry() |>
  select(id = cemeteries_id, lon, lat, opportunity)

#Schools
schools_path <- "Schools.shp"

schools_sf <- st_read(schools_path, quiet = TRUE)

#     To WGS84 & getting coordinates
if (is.na(st_crs(schools_sf))) {
  stop("CRS of 'schools' is unknown. Set CRS first.")
}
schools_sf <- st_transform(schools_sf, 4326)

#     Making clean ID column for schools
if (!("school_id" %in% names(schools_sf))) {
  schools_sf$school_id <- as.character(seq_len(nrow(schools_sf)))
} else {
  schools_sf$school_id <- as.character(schools_sf$school_id)
}

schools_df <- schools_sf |>
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2],
    opportunity = 1L          # every school = 1 opportunity
  ) |>
  st_drop_geometry() |>
  select(id = school_id, lon, lat, opportunity)


#Checking for dubbel ID's --> should be 0 for all
destination_dfs <- list(
  hospitals = hospitals_df,
  adult_day_care_centres = adult_day_care_centres_df,
  socio_cultural_activities = socio_cultural_activities_df,
  groceries = groceries_df,
  speech_therapists = speech_therapists_df,
  physiotherapists = physiotherapists_df,
  dietitians = dietitians_df,
  occupational_therapists = occupational_therapists_df,
  general_practitioners = general_practitioners_df,
  hairdressers = hairdressers_df,
  restaurants = restaurants_df,
  dentists = dentists_df,
  town_halls = town_halls_df,
  local_service_centres = local_service_centres_df,
  special_needs_education = special_needs_education_df,
  crematoriums = crematoriums_df,
  public_transport_stops = public_transport_stops_df,
  nursing_homes = nursing_homes_df,
  pharmacies = pharmacies_df,
  cemeteries = cemeteries_df,
  schools = schools_df
)

duplicate_ids <- sapply(
  destination_dfs,
  function(x) anyDuplicated(as.character(x$id))
)

duplicate_ids

#Train stations
train_path <- "NMBS_stations.shp"

train_sf <- st_read(train_path, quiet = TRUE)

#     To WGS84 & getting coordinates
if (is.na(st_crs(train_sf))) {
  stop("CRS of 'train' is unknown. Set CRS first.")
}
train_sf <- st_transform(train_sf, 4326)

#     Making clean ID column for train
if (!("train_id" %in% names(train_sf))) {
  train_sf$train_id <- as.character(seq_len(nrow(train_sf)))
} else {
  train_sf$train_id <- as.character(train_sf$train_id)
}

train_df <- train_sf |>
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2],
    opportunity = 1L          # every train station = 1 opportunity
  ) |>
  st_drop_geometry() |>
  select(id = train_id, lon, lat, opportunity)





# -----------------------------------------
# 4) Basis parameters r5r
# -----------------------------------------
mode               <- c("WALK", "TRANSIT")
max_walk_time      <- 15     # minutes

cutoff_minutes     <- 30L    # 30 minutes to reach opportunities

departure_datetime <- as.POSIXct("18-01-2024 06:00:00", format="%d-%m-%Y %H:%M:%S") #typical Thursday
# -------------------------------------------------------
# 5) 30 minutes cumulative accessibility
#    via r5r::accessibility() with step-decay (cutoff=30)
# -------------------------------------------------------
# We calculate per origin the number of opportunities (destinations of goals)
# Accessible within 30 minutes PT+walking
#ids <- all_unique_origins$new_ID
#n   <- length(ids)

analysis_origins <- all_unique_origins


origins_analysis_df <- analysis_origins %>%
  transmute(
    id     = as.character(new_ID),  
    new_ID = as.integer(new_ID),    
    lon    = as.numeric(lon),
    lat    = as.numeric(lat)
  ) %>%
  distinct(new_ID, .keep_all = TRUE)

stopifnot(
  !anyDuplicated(origins_analysis_df$id),
  !anyNA(origins_analysis_df$lon),
  !anyNA(origins_analysis_df$lat)
)


#Hospitals

ttm_hospitals <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = hospitals_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Error hospitals: ", conditionMessage(e))
  }
)

time_col_hospitals <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_hospitals),
  value = TRUE
)[1]

if (is.na(time_col_hospitals)) {
  stop(
    "Geen reistijdkolom gevonden. Kolommen: ",
    paste(names(ttm_hospitals), collapse = ", ")
  )
}

acc_hospitals_30 <- ttm_hospitals %>%
  filter(
    !is.na(.data[[time_col_hospitals]]),
    .data[[time_col_hospitals]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    hospitals_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    hospitals_30min
  )

acc_hospitals_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_hospitals_30,
    by = "new_ID"
  ) %>%
  mutate(
    hospitals_30min = tidyr::replace_na(hospitals_30min, 0L)
  )

write.csv(acc_hospitals_30, "output_hospitals_30min_06_combined_NMBS_DeLijn.csv", row.names = FALSE)

rm(ttm_hospitals)
gc()

#Adult day care centres
ttm_adult_day_care_centres <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = adult_day_care_centres_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop(
      "Fout bij berekening dagverzorgingscentra: ",
      conditionMessage(e)
    )
  }
)

time_col_adult_day_care_centres <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_adult_day_care_centres),
  value = TRUE
)[1]

if (is.na(time_col_adult_day_care_centres)) {
  stop(
    "Geen reistijdkolom gevonden voor dagverzorgingscentra. Kolommen: ",
    paste(names(ttm_adult_day_care_centres), collapse = ", ")
  )
}


acc_adult_day_care_centres_30 <- ttm_adult_day_care_centres %>%
  filter(
    !is.na(.data[[time_col_adult_day_care_centres]]),
    .data[[time_col_adult_day_care_centres]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    adult_day_care_centres_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    adult_day_care_centres_30min
  )

acc_adult_day_care_centres_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_adult_day_care_centres_30,
    by = "new_ID"
  ) %>%
  mutate(
    adult_day_care_centres_30min = tidyr::replace_na(adult_day_care_centres_30min, 0L)
  )



write.csv(
  acc_adult_day_care_centres_30,
  "output_adult_day_care_centres_30min_06_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_adult_day_care_centres)
gc()


#Socio-cultural amenities
ttm_socio_cultural_activities <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = socio_cultural_activities_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening sociaal-culturele voorzieningen: ", conditionMessage(e))
  }
)

time_col_socio_cultural_activities <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_socio_cultural_activities),
  value = TRUE
)[1]

if (is.na(time_col_socio_cultural_activities)) {
  stop(
    "Geen reistijdkolom gevonden voor sociaal-culturele voorzieningen. Kolommen: ",
    paste(names(ttm_socio_cultural_activities), collapse = ", ")
  )
}

acc_socio_cultural_activities_30 <- ttm_socio_cultural_activities %>%
  filter(
    !is.na(.data[[time_col_socio_cultural_activities]]),
    .data[[time_col_socio_cultural_activities]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    socio_cultural_activities_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    socio_cultural_activities_30min
  )

acc_socio_cultural_activities_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_socio_cultural_activities_30,
    by = "new_ID"
  ) %>%
  mutate(
    socio_cultural_activities_30min = tidyr::replace_na(socio_cultural_activities_30min, 0L)
  )

write.csv(
  acc_socio_cultural_activities_30,
  "output_socio_cultural_act_30min_06_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_socio_cultural_activities)
gc()

#Groceries
ttm_groceries <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = groceries_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening voedingswinkels: ", conditionMessage(e))
  }
)

time_col_groceries <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_groceries),
  value = TRUE
)[1]

if (is.na(time_col_groceries)) {
  stop(
    "Geen reistijdkolom gevonden voor voedingswinkels. Kolommen: ",
    paste(names(ttm_groceries), collapse = ", ")
  )
}

acc_groceries_30 <- ttm_groceries %>%
  filter(
    !is.na(.data[[time_col_groceries]]),
    .data[[time_col_groceries]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    groceries_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    groceries_30min
  )

acc_groceries_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_groceries_30,
    by = "new_ID"
  ) %>%
  mutate(
    groceries_30min = tidyr::replace_na(groceries_30min, 0L)
  )


write.csv(
  acc_groceries_30,
  "output_groceries_30min_06_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_groceries)
gc()

#Speech therapists
ttm_speech_therapists <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = speech_therapists_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening logopedisten: ", conditionMessage(e))
  }
)

time_col_speech_therapists <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_speech_therapists),
  value = TRUE
)[1]

if (is.na(time_col_speech_therapists)) {
  stop(
    "Geen reistijdkolom gevonden voor logopedisten. Kolommen: ",
    paste(names(ttm_speech_therapists), collapse = ", ")
  )
}

acc_speech_therapists_30 <- ttm_speech_therapists %>%
  filter(
    !is.na(.data[[time_col_speech_therapists]]),
    .data[[time_col_speech_therapists]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    speech_therapists_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    speech_therapists_30min
  )

acc_speech_therapists_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_speech_therapists_30,
    by = "new_ID"
  ) %>%
  mutate(
    speech_therapists_30min = tidyr::replace_na(speech_therapists_30min, 0L)
  )
write.csv(
  acc_speech_therapists_30,
  "output_speech_therapists_30min_06_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_speech_therapists)
gc()


#Physiotherapists
ttm_physiotherapists <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = physiotherapists_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening kinesitherapeuten: ", conditionMessage(e))
  }
)

time_col_physiotherapists <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_physiotherapists),
  value = TRUE
)[1]

if (is.na(time_col_physiotherapists)) {
  stop(
    "Geen reistijdkolom gevonden voor kinesitherapeuten. Kolommen: ",
    paste(names(ttm_physiotherapists), collapse = ", ")
  )
}

acc_physiotherapists_30 <- ttm_physiotherapists %>%
  filter(
    !is.na(.data[[time_col_physiotherapists]]),
    .data[[time_col_physiotherapists]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    physiotherapists_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    physiotherapists_30min
  )

acc_physiotherapists_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_physiotherapists_30,
    by = "new_ID"
  ) %>%
  mutate(
    physiotherapists_30min = tidyr::replace_na(physiotherapists_30min, 0L)
  )


write.csv(
  acc_physiotherapists_30,
  "output_physiotherapists_30min_06_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_physiotherapists)
gc()

#Occupational therapists
ttm_occupational_therapists <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = occupational_therapists_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening ergotherapeuten: ", conditionMessage(e))
  }
)

time_col_occupational_therapists <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_occupational_therapists),
  value = TRUE
)[1]

if (is.na(time_col_occupational_therapists)) {
  stop(
    "Geen reistijdkolom gevonden voor ergotherapeuten. Kolommen: ",
    paste(names(ttm_occupational_therapists), collapse = ", ")
  )
}

acc_occupational_therapists_30 <- ttm_occupational_therapists %>%
  filter(
    !is.na(.data[[time_col_occupational_therapists]]),
    .data[[time_col_occupational_therapists]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    occupational_therapists_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    occupational_therapists_30min
  )

acc_occupational_therapists_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_occupational_therapists_30,
    by = "new_ID"
  ) %>%
  mutate(
    occupational_therapists_30min = tidyr::replace_na(occupational_therapists_30min, 0L)
  )


write.csv(
  acc_occupational_therapists_30,
  "output_occupational_therapists_30min_06_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_occupational_therapists)
gc()

#General practitioners 
ttm_general_practitioners <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = general_practitioners_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening huisartsen: ", conditionMessage(e))
  }
)

time_col_general_practitioners <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_general_practitioners),
  value = TRUE
)[1]

if (is.na(time_col_general_practitioners)) {
  stop(
    "Geen reistijdkolom gevonden voor huisartsen. Kolommen: ",
    paste(names(ttm_general_practitioners), collapse = ", ")
  )
}

acc_general_practitioners_30 <- ttm_general_practitioners %>%
  filter(
    !is.na(.data[[time_col_general_practitioners]]),
    .data[[time_col_general_practitioners]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    general_practitioners_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    general_practitioners_30min
  )

acc_general_practitioners_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_general_practitioners_30,
    by = "new_ID"
  ) %>%
  mutate(
    general_practitioners_30min =
      tidyr::replace_na(general_practitioners_30min, 0L)
  )

write.csv(
  acc_general_practitioners_30,
  "output_GP_30min_06_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_general_practitioners)
gc()


#Hairdressers
ttm_hairdressers <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = hairdressers_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening kappers: ", conditionMessage(e))
  }
)

time_col_hairdressers <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_hairdressers),
  value = TRUE
)[1]

if (is.na(time_col_hairdressers)) {
  stop(
    "Geen reistijdkolom gevonden voor kappers. Kolommen: ",
    paste(names(ttm_hairdressers), collapse = ", ")
  )
}

acc_hairdressers_30 <- ttm_hairdressers %>%
  filter(
    !is.na(.data[[time_col_hairdressers]]),
    .data[[time_col_hairdressers]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    hairdressers_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    hairdressers_30min
  )

acc_hairdressers_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_hairdressers_30,
    by = "new_ID"
  ) %>%
  mutate(
    hairdressers_30min = tidyr::replace_na(hairdressers_30min, 0L)
  )

write.csv(
  acc_hairdressers_30,
  "output_hairdressers_30min_06_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_hairdressers)
gc()

#Restaurants
ttm_restaurants <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = restaurants_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening restaurants: ", conditionMessage(e))
  }
)

time_col_restaurants <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_restaurants),
  value = TRUE
)[1]

if (is.na(time_col_restaurants)) {
  stop(
    "Geen reistijdkolom gevonden voor restaurants. Kolommen: ",
    paste(names(ttm_restaurants), collapse = ", ")
  )
}

acc_restaurants_30 <- ttm_restaurants %>%
  filter(
    !is.na(.data[[time_col_restaurants]]),
    .data[[time_col_restaurants]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    restaurants_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    restaurants_30min
  )

acc_restaurants_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_restaurants_30,
    by = "new_ID"
  ) %>%
  mutate(
    restaurants_30min = tidyr::replace_na(restaurants_30min, 0L)
  )


write.csv(
  acc_restaurants_30,
  "output_restaurants_30min_06_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_restaurants)
gc()


#Dentists
ttm_dentists <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = dentists_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening tandartsen: ", conditionMessage(e))
  }
)

time_col_dentists <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_dentists),
  value = TRUE
)[1]

if (is.na(time_col_dentists)) {
  stop(
    "Geen reistijdkolom gevonden voor tandartsen. Kolommen: ",
    paste(names(ttm_dentists), collapse = ", ")
  )
}

acc_dentists_30 <- ttm_dentists %>%
  filter(
    !is.na(.data[[time_col_dentists]]),
    .data[[time_col_dentists]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    dentists_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    dentists_30min
  )

acc_dentists_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_dentists_30,
    by = "new_ID"
  ) %>%
  mutate(
    dentists_30min = tidyr::replace_na(dentists_30min, 0L)
  )


write.csv(
  acc_dentists_30,
  "output_dentists_30min_06_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_dentists)
gc()


#Town halls
ttm_town_halls <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = town_halls_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening gemeentehuizen: ", conditionMessage(e))
  }
)

time_col_town_halls <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_town_halls),
  value = TRUE
)[1]

if (is.na(time_col_town_halls)) {
  stop(
    "Geen reistijdkolom gevonden voor gemeentehuizen. Kolommen: ",
    paste(names(ttm_town_halls), collapse = ", ")
  )
}

acc_town_halls_30 <- ttm_town_halls %>%
  filter(
    !is.na(.data[[time_col_town_halls]]),
    .data[[time_col_town_halls]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    town_halls_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    town_halls_30min
  )

acc_town_halls_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_town_halls_30,
    by = "new_ID"
  ) %>%
  mutate(
    town_halls_30min = tidyr::replace_na(town_halls_30min, 0L)
  )

write.csv(
  acc_town_halls_30,
  "output_town_halls_30min_06_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_town_halls)
gc()


#Local service centres
ttm_local_service_centres <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = local_service_centres_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening lokale dienstencentra: ", conditionMessage(e))
  }
)

time_col_local_service_centres <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_local_service_centres),
  value = TRUE
)[1]

if (is.na(time_col_local_service_centres)) {
  stop(
    "Geen reistijdkolom gevonden voor lokale dienstencentra. Kolommen: ",
    paste(names(ttm_local_service_centres), collapse = ", ")
  )
}

acc_local_service_centres_30 <- ttm_local_service_centres %>%
  filter(
    !is.na(.data[[time_col_local_service_centres]]),
    .data[[time_col_local_service_centres]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    local_service_centres_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    local_service_centres_30min
  )

acc_local_service_centres_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_local_service_centres_30,
    by = "new_ID"
  ) %>%
  mutate(
    local_service_centres_30min = tidyr::replace_na(local_service_centres_30min, 0L)
  )
write.csv(
  acc_local_service_centres_30,
  "output_local_service_centres_30min_06_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_local_service_centres)
gc()

#Special needs education
ttm_special_needs_education <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = special_needs_education_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening buitengewoon onderwijs: ", conditionMessage(e))
  }
)

time_col_special_needs_education <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_special_needs_education),
  value = TRUE
)[1]

if (is.na(time_col_special_needs_education)) {
  stop(
    "Geen reistijdkolom gevonden voor buitengewoon onderwijs. Kolommen: ",
    paste(names(ttm_special_needs_education), collapse = ", ")
  )
}

acc_special_needs_education_30 <- ttm_special_needs_education %>%
  filter(
    !is.na(.data[[time_col_special_needs_education]]),
    .data[[time_col_special_needs_education]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    special_needs_education_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    special_needs_education_30min
  )

acc_special_needs_education_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_special_needs_education_30,
    by = "new_ID"
  ) %>%
  mutate(
    special_needs_education_30min = tidyr::replace_na(special_needs_education_30min, 0L)
  )

write.csv(
  acc_special_needs_education_30,
  "output_special_needs_education_30min_06_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_special_needs_education)
gc()
#Crematoriums 
ttm_crematoriums <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = crematoriums_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening crematoria: ", conditionMessage(e))
  }
)

time_col_crematoriums <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_crematoriums),
  value = TRUE
)[1]

if (is.na(time_col_crematoriums)) {
  stop(
    "Geen reistijdkolom gevonden voor crematoria. Kolommen: ",
    paste(names(ttm_crematoriums), collapse = ", ")
  )
}

acc_crematoriums_30 <- ttm_crematoriums %>%
  filter(
    !is.na(.data[[time_col_crematoriums]]),
    .data[[time_col_crematoriums]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    crematoriums_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    crematoriums_30min
  )

acc_crematoriums_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_crematoriums_30,
    by = "new_ID"
  ) %>%
  mutate(
    crematoriums_30min = tidyr::replace_na(crematoriums_30min, 0L)
  )

write.csv(
  acc_crematoriums_30,
  "output_crematoriums_30min_06_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_crematoriums)
gc()


#Public transport stops
ttm_public_transport_stops <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = public_transport_stops_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening haltes openbaar vervoer: ", conditionMessage(e))
  }
)

time_col_public_transport_stops <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_public_transport_stops),
  value = TRUE
)[1]

if (is.na(time_col_public_transport_stops)) {
  stop(
    "Geen reistijdkolom gevonden voor haltes openbaar vervoer. Kolommen: ",
    paste(names(ttm_public_transport_stops), collapse = ", ")
  )
}

acc_public_transport_stops_30 <- ttm_public_transport_stops %>%
  filter(
    !is.na(.data[[time_col_public_transport_stops]]),
    .data[[time_col_public_transport_stops]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    public_transport_stops_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    public_transport_stops_30min
  )

acc_public_transport_stops_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_public_transport_stops_30,
    by = "new_ID"
  ) %>%
  mutate(
    public_transport_stops_30min = tidyr::replace_na(public_transport_stops_30min, 0L)
  )

write.csv(
  acc_public_transport_stops_30,
  "output_PT_stops_30min_06_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_public_transport_stops)
gc()


#Nursing homes
ttm_nursing_homes <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = nursing_homes_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening woonzorgcentra: ", conditionMessage(e))
  }
)

time_col_nursing_homes <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_nursing_homes),
  value = TRUE
)[1]

if (is.na(time_col_nursing_homes)) {
  stop(
    "Geen reistijdkolom gevonden voor woonzorgcentra. Kolommen: ",
    paste(names(ttm_nursing_homes), collapse = ", ")
  )
}

acc_nursing_homes_30 <- ttm_nursing_homes %>%
  filter(
    !is.na(.data[[time_col_nursing_homes]]),
    .data[[time_col_nursing_homes]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    nursing_homes_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    nursing_homes_30min
  )

acc_nursing_homes_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_nursing_homes_30,
    by = "new_ID"
  ) %>%
  mutate(
    nursing_homes_30min = tidyr::replace_na(nursing_homes_30min, 0L)
  )
write.csv(
  acc_nursing_homes_30,
  "output_nursing_homes_30min_06_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_nursing_homes)
gc()


#Pharmacies
ttm_pharmacies <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = pharmacies_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening apotheken: ", conditionMessage(e))
  }
)

time_col_pharmacies <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_pharmacies),
  value = TRUE
)[1]

if (is.na(time_col_pharmacies)) {
  stop(
    "Geen reistijdkolom gevonden voor apotheken. Kolommen: ",
    paste(names(ttm_pharmacies), collapse = ", ")
  )
}

acc_pharmacies_30 <- ttm_pharmacies %>%
  filter(
    !is.na(.data[[time_col_pharmacies]]),
    .data[[time_col_pharmacies]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    pharmacies_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    pharmacies_30min
  )

acc_pharmacies_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_pharmacies_30,
    by = "new_ID"
  ) %>%
  mutate(
    pharmacies_30min = tidyr::replace_na(pharmacies_30min, 0L)
  )

write.csv(
  acc_pharmacies_30,
  "output_pharmacies_30min_06_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_pharmacies)
gc()


#Cemeteries
ttm_cemeteries <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = cemeteries_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening begraafplaatsen: ", conditionMessage(e))
  }
)

time_col_cemeteries <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_cemeteries),
  value = TRUE
)[1]

if (is.na(time_col_cemeteries)) {
  stop(
    "Geen reistijdkolom gevonden voor begraafplaatsen. Kolommen: ",
    paste(names(ttm_cemeteries), collapse = ", ")
  )
}

acc_cemeteries_30 <- ttm_cemeteries %>%
  filter(
    !is.na(.data[[time_col_cemeteries]]),
    .data[[time_col_cemeteries]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    cemeteries_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    cemeteries_30min
  )

acc_cemeteries_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_cemeteries_30,
    by = "new_ID"
  ) %>%
  mutate(
    cemeteries_30min = tidyr::replace_na(cemeteries_30min, 0L)
  )


write.csv(
  acc_cemeteries_30,
  "output_cemeteries_30min_06_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_cemeteries)
gc()


#Schools
ttm_schools <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = schools_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening scholen: ", conditionMessage(e))
  }
)

time_col_schools <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_schools),
  value = TRUE
)[1]

if (is.na(time_col_schools)) {
  stop(
    "Geen reistijdkolom gevonden voor scholen. Kolommen: ",
    paste(names(ttm_schools), collapse = ", ")
  )
}

acc_schools_30 <- ttm_schools %>%
  filter(
    !is.na(.data[[time_col_schools]]),
    .data[[time_col_schools]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    schools_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    schools_30min
  )

acc_schools_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_schools_30,
    by = "new_ID"
  ) %>%
  mutate(
    schools_30min = tidyr::replace_na(schools_30min, 0L)
  )


write.csv(
  acc_schools_30,
  "output_schools_30min_06_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_schools)
gc()


#Dieticians
ttm_dietitians <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = dietitians_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening diëtisten: ", conditionMessage(e))
  }
)

time_col_dietitians <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_dietitians),
  value = TRUE
)[1]

if (is.na(time_col_dietitians)) {
  stop(
    "Geen reistijdkolom gevonden voor diëtisten. Kolommen: ",
    paste(names(ttm_dietitians), collapse = ", ")
  )
}

acc_dietitians_30 <- ttm_dietitians %>%
  filter(
    !is.na(.data[[time_col_dietitians]]),
    .data[[time_col_dietitians]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    dietitians_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    dietitians_30min
  )

acc_dietitians_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_dietitians_30,
    by = "new_ID"
  ) %>%
  mutate(
    dietitians_30min = tidyr::replace_na(dietitians_30min, 0L)
  )

write.csv(
  acc_dietitians_30,
  "output_dietitians_30min_06_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_dietitians)
gc()

# -----------------------------------------
# 6) Joining results and writing csv
# -----------------------------------------

# Join to origins
analysis_origins <- analysis_origins %>%
  select(-ID) %>%
  rename(ID = new_ID)

origin_summary <- analysis_origins %>%
  select(ID, Trip_ID, lon, lat) %>%
  left_join(acc_schools_30, by = "ID") %>%
  left_join(acc_hospitals_30, by = "ID") %>%
  left_join(acc_adult_day_care_centres_30, by = "ID") %>%
  left_join(acc_socio_cultural_activities_30, by = "ID") %>%
  left_join(acc_groceries_30, by = "ID") %>%
  left_join(acc_speech_therapists_30, by = "ID") %>%
  left_join(acc_physiotherapists_30, by = "ID") %>%
  left_join(acc_dietitians_30, by = "ID") %>%
  left_join(acc_occupational_therapists_30, by = "ID") %>%
  left_join(acc_general_practitioners_30, by = "ID") %>%
  left_join(acc_hairdressers_30, by = "ID") %>%
  left_join(acc_restaurants_30, by = "ID") %>%
  left_join(acc_dentists_30, by = "ID") %>%
  left_join(acc_town_halls_30, by = "ID") %>%
  left_join(acc_local_service_centres_30, by = "ID") %>%
  left_join(acc_special_needs_education_30, by = "ID") %>%
  left_join(acc_crematoriums_30, by = "ID") %>%
  left_join(acc_public_transport_stops_30, by = "ID") %>%
  left_join(acc_nursing_homes_30, by = "ID") %>%
  left_join(acc_pharmacies_30, by = "ID") %>%
  left_join(acc_cemeteries_30, by = "ID")


# Writing csv
write.csv(origin_summary, "output_cumulative_accessibility_06_combined.csv", row.names = FALSE)



# 9 am
# -----------------------------------------
# 4) Basis parameters r5r
# -----------------------------------------
mode               <- c("WALK", "TRANSIT")
max_walk_time      <- 15     # minutes

cutoff_minutes     <- 30L    # 30 minutes to reach opportunities

departure_datetime <- as.POSIXct("18-01-2024 09:00:00", format="%d-%m-%Y %H:%M:%S") #typical Thursday
# -------------------------------------------------------
# 5) 30 minutes cumulative accessibility
#    via r5r::accessibility() with step-decay (cutoff=30)
# -------------------------------------------------------
# We calculate per origin the number of opportunities (destinations of goals)
# Accessible within 30 minutes PT+walking
#ids <- all_unique_origins$new_ID
#n   <- length(ids)

analysis_origins <- all_unique_origins



origins_analysis_df <- analysis_origins %>%
  transmute(
    id     = as.character(new_ID),  # alleen voor r5r
    new_ID = as.integer(new_ID),    # sleutel voor koppeling
    lon    = as.numeric(lon),
    lat    = as.numeric(lat)
  ) %>%
  distinct(new_ID, .keep_all = TRUE)



stopifnot(
  !anyDuplicated(origins_analysis_df$id),
  !anyNA(origins_analysis_df$lon),
  !anyNA(origins_analysis_df$lat)
)


#Hospitals

ttm_hospitals <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = hospitals_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Error hospitals: ", conditionMessage(e))
  }
)

time_col_hospitals <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_hospitals),
  value = TRUE
)[1]

if (is.na(time_col_hospitals)) {
  stop(
    "Geen reistijdkolom gevonden. Kolommen: ",
    paste(names(ttm_hospitals), collapse = ", ")
  )
}

acc_hospitals_30 <- ttm_hospitals %>%
  filter(
    !is.na(.data[[time_col_hospitals]]),
    .data[[time_col_hospitals]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    hospitals_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    hospitals_30min
  )

acc_hospitals_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_hospitals_30,
    by = "new_ID"
  ) %>%
  mutate(
    hospitals_30min = tidyr::replace_na(hospitals_30min, 0L)
  )

write.csv(acc_hospitals_30, "output_hospitals_30min_09_combined_NMBS_DeLijn.csv", row.names = FALSE)
rm(ttm_hospitals)
gc()

#Adult day care centres
ttm_adult_day_care_centres <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = adult_day_care_centres_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop(
      "Fout bij berekening dagverzorgingscentra: ",
      conditionMessage(e)
    )
  }
)

time_col_adult_day_care_centres <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_adult_day_care_centres),
  value = TRUE
)[1]

if (is.na(time_col_adult_day_care_centres)) {
  stop(
    "Geen reistijdkolom gevonden voor dagverzorgingscentra. Kolommen: ",
    paste(names(ttm_adult_day_care_centres), collapse = ", ")
  )
}

acc_adult_day_care_centres_30 <- ttm_adult_day_care_centres %>%
  filter(
    !is.na(.data[[time_col_adult_day_care_centres]]),
    .data[[time_col_adult_day_care_centres]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    adult_day_care_centres_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    adult_day_care_centres_30min
  )

acc_adult_day_care_centres_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_adult_day_care_centres_30,
    by = "new_ID"
  ) %>%
  mutate(
    adult_day_care_centres_30min = tidyr::replace_na(adult_day_care_centres_30min, 0L)
  )

write.csv(
  acc_adult_day_care_centres_30,
  "output_adult_day_care_centres_30min_09_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_adult_day_care_centres)
gc()


#Socio-cultural amenities
ttm_socio_cultural_activities <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = socio_cultural_activities_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening sociaal-culturele voorzieningen: ", conditionMessage(e))
  }
)

time_col_socio_cultural_activities <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_socio_cultural_activities),
  value = TRUE
)[1]

if (is.na(time_col_socio_cultural_activities)) {
  stop(
    "Geen reistijdkolom gevonden voor sociaal-culturele voorzieningen. Kolommen: ",
    paste(names(ttm_socio_cultural_activities), collapse = ", ")
  )
}

acc_socio_cultural_activities_30 <- ttm_socio_cultural_activities %>%
  filter(
    !is.na(.data[[time_col_socio_cultural_activities]]),
    .data[[time_col_socio_cultural_activities]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    socio_cultural_activities_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    socio_cultural_activities_30min
  )

acc_socio_cultural_activities_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_socio_cultural_activities_30,
    by = "new_ID"
  ) %>%
  mutate(
    socio_cultural_activities_30min = tidyr::replace_na(socio_cultural_activities_30min, 0L)
  )

write.csv(
  acc_socio_cultural_activities_30,
  "output_socio_cultural_act_30min_09_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_socio_cultural_activities)
gc()

#Groceries
ttm_groceries <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = groceries_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening voedingswinkels: ", conditionMessage(e))
  }
)

time_col_groceries <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_groceries),
  value = TRUE
)[1]

if (is.na(time_col_groceries)) {
  stop(
    "Geen reistijdkolom gevonden voor voedingswinkels. Kolommen: ",
    paste(names(ttm_groceries), collapse = ", ")
  )
}

acc_groceries_30 <- ttm_groceries %>%
  filter(
    !is.na(.data[[time_col_groceries]]),
    .data[[time_col_groceries]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    groceries_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    groceries_30min
  )

acc_groceries_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_groceries_30,
    by = "new_ID"
  ) %>%
  mutate(
    groceries_30min = tidyr::replace_na(groceries_30min, 0L)
  )


write.csv(
  acc_groceries_30,
  "output_groceries_30min_09_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_groceries)
gc()

#Speech therapists
ttm_speech_therapists <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = speech_therapists_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening logopedisten: ", conditionMessage(e))
  }
)

time_col_speech_therapists <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_speech_therapists),
  value = TRUE
)[1]

if (is.na(time_col_speech_therapists)) {
  stop(
    "Geen reistijdkolom gevonden voor logopedisten. Kolommen: ",
    paste(names(ttm_speech_therapists), collapse = ", ")
  )
}

acc_speech_therapists_30 <- ttm_speech_therapists %>%
  filter(
    !is.na(.data[[time_col_speech_therapists]]),
    .data[[time_col_speech_therapists]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    speech_therapists_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    speech_therapists_30min
  )

acc_speech_therapists_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_speech_therapists_30,
    by = "new_ID"
  ) %>%
  mutate(
    speech_therapists_30min = tidyr::replace_na(speech_therapists_30min, 0L)
  )

write.csv(
  acc_speech_therapists_30,
  "output_speech_therapists_30min_09_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_speech_therapists)
gc()


#Physiotherapists
ttm_physiotherapists <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = physiotherapists_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening kinesitherapeuten: ", conditionMessage(e))
  }
)

time_col_physiotherapists <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_physiotherapists),
  value = TRUE
)[1]

if (is.na(time_col_physiotherapists)) {
  stop(
    "Geen reistijdkolom gevonden voor kinesitherapeuten. Kolommen: ",
    paste(names(ttm_physiotherapists), collapse = ", ")
  )
}

acc_physiotherapists_30 <- ttm_physiotherapists %>%
  filter(
    !is.na(.data[[time_col_physiotherapists]]),
    .data[[time_col_physiotherapists]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    physiotherapists_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    physiotherapists_30min
  )

acc_physiotherapists_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_physiotherapists_30,
    by = "new_ID"
  ) %>%
  mutate(
    physiotherapists_30min = tidyr::replace_na(physiotherapists_30min, 0L)
  )


write.csv(
  acc_physiotherapists_30,
  "output_physiotherapists_30min_09_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_physiotherapists)
gc()

#Occupational therapists
ttm_occupational_therapists <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = occupational_therapists_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening ergotherapeuten: ", conditionMessage(e))
  }
)

time_col_occupational_therapists <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_occupational_therapists),
  value = TRUE
)[1]

if (is.na(time_col_occupational_therapists)) {
  stop(
    "Geen reistijdkolom gevonden voor ergotherapeuten. Kolommen: ",
    paste(names(ttm_occupational_therapists), collapse = ", ")
  )
}

acc_occupational_therapists_30 <- ttm_occupational_therapists %>%
  filter(
    !is.na(.data[[time_col_occupational_therapists]]),
    .data[[time_col_occupational_therapists]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    occupational_therapists_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    occupational_therapists_30min
  )

acc_occupational_therapists_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_occupational_therapists_30,
    by = "new_ID"
  ) %>%
  mutate(
    occupational_therapists_30min = tidyr::replace_na(occupational_therapists_30min, 0L)
  )


write.csv(
  acc_occupational_therapists_30,
  "output_occupational_therapists_30min_09_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_occupational_therapists)
gc()

#General practitioners 
ttm_general_practitioners <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = general_practitioners_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening huisartsen: ", conditionMessage(e))
  }
)

time_col_general_practitioners <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_general_practitioners),
  value = TRUE
)[1]

if (is.na(time_col_general_practitioners)) {
  stop(
    "Geen reistijdkolom gevonden voor huisartsen. Kolommen: ",
    paste(names(ttm_general_practitioners), collapse = ", ")
  )
}

acc_general_practitioners_30 <- ttm_general_practitioners %>%
  filter(
    !is.na(.data[[time_col_general_practitioners]]),
    .data[[time_col_general_practitioners]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    general_practitioners_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    general_practitioners_30min
  )

acc_general_practitioners_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_general_practitioners_30,
    by = "new_ID"
  ) %>%
  mutate(
    general_practitioners_30min = tidyr::replace_na(general_practitioners_30min, 0L)
  )

write.csv(
  acc_general_practitioners_30,
  "output_GP_30min_09_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_general_practitioners)
gc()


#Hairdressers
ttm_hairdressers <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = hairdressers_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening kappers: ", conditionMessage(e))
  }
)

time_col_hairdressers <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_hairdressers),
  value = TRUE
)[1]

if (is.na(time_col_hairdressers)) {
  stop(
    "Geen reistijdkolom gevonden voor kappers. Kolommen: ",
    paste(names(ttm_hairdressers), collapse = ", ")
  )
}

acc_hairdressers_30 <- ttm_hairdressers %>%
  filter(
    !is.na(.data[[time_col_hairdressers]]),
    .data[[time_col_hairdressers]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    hairdressers_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    hairdressers_30min
  )

acc_hairdressers_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_hairdressers_30,
    by = "new_ID"
  ) %>%
  mutate(
    hairdressers_30min = tidyr::replace_na(hairdressers_30min, 0L)
  )
write.csv(
  acc_hairdressers_30,
  "output_hairdressers_30min_09_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_hairdressers)
gc()

#Restaurants
ttm_restaurants <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = restaurants_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening restaurants: ", conditionMessage(e))
  }
)

time_col_restaurants <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_restaurants),
  value = TRUE
)[1]

if (is.na(time_col_restaurants)) {
  stop(
    "Geen reistijdkolom gevonden voor restaurants. Kolommen: ",
    paste(names(ttm_restaurants), collapse = ", ")
  )
}

acc_restaurants_30 <- ttm_restaurants %>%
  filter(
    !is.na(.data[[time_col_restaurants]]),
    .data[[time_col_restaurants]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    restaurants_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    restaurants_30min
  )

acc_restaurants_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_restaurants_30,
    by = "new_ID"
  ) %>%
  mutate(
    restaurants_30min = tidyr::replace_na(restaurants_30min, 0L)
  )


write.csv(
  acc_restaurants_30,
  "output_restaurants_30min_09_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_restaurants)
gc()


#Dentists
ttm_dentists <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = dentists_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening tandartsen: ", conditionMessage(e))
  }
)

time_col_dentists <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_dentists),
  value = TRUE
)[1]

if (is.na(time_col_dentists)) {
  stop(
    "Geen reistijdkolom gevonden voor tandartsen. Kolommen: ",
    paste(names(ttm_dentists), collapse = ", ")
  )
}

acc_dentists_30 <- ttm_dentists %>%
  filter(
    !is.na(.data[[time_col_dentists]]),
    .data[[time_col_dentists]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    dentists_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    dentists_30min
  )

acc_dentists_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_dentists_30,
    by = "new_ID"
  ) %>%
  mutate(
    dentists_30min = tidyr::replace_na(dentists_30min, 0L)
  )


write.csv(
  acc_dentists_30,
  "output_dentists_30min_09_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_dentists)
gc()


#Town halls
ttm_town_halls <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = town_halls_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening gemeentehuizen: ", conditionMessage(e))
  }
)

time_col_town_halls <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_town_halls),
  value = TRUE
)[1]

if (is.na(time_col_town_halls)) {
  stop(
    "Geen reistijdkolom gevonden voor gemeentehuizen. Kolommen: ",
    paste(names(ttm_town_halls), collapse = ", ")
  )
}

acc_town_halls_30 <- ttm_town_halls %>%
  filter(
    !is.na(.data[[time_col_town_halls]]),
    .data[[time_col_town_halls]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    town_halls_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    town_halls_30min
  )

acc_town_halls_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_town_halls_30,
    by = "new_ID"
  ) %>%
  mutate(
    town_halls_30min = tidyr::replace_na(town_halls_30min, 0L)
  )

write.csv(
  acc_town_halls_30,
  "output_town_halls_30min_09_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_town_halls)
gc()


#Local service centres
ttm_local_service_centres <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = local_service_centres_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening lokale dienstencentra: ", conditionMessage(e))
  }
)

time_col_local_service_centres <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_local_service_centres),
  value = TRUE
)[1]

if (is.na(time_col_local_service_centres)) {
  stop(
    "Geen reistijdkolom gevonden voor lokale dienstencentra. Kolommen: ",
    paste(names(ttm_local_service_centres), collapse = ", ")
  )
}

acc_local_service_centres_30 <- ttm_local_service_centres %>%
  filter(
    !is.na(.data[[time_col_local_service_centres]]),
    .data[[time_col_local_service_centres]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    local_service_centres_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    local_service_centres_30min
  )

acc_local_service_centres_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_local_service_centres_30,
    by = "new_ID"
  ) %>%
  mutate(
    local_service_centres_30min = tidyr::replace_na(local_service_centres_30min, 0L)
  )

write.csv(
  acc_local_service_centres_30,
  "output_local_service_centres_30min_09_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_local_service_centres)
gc()

#Special needs education
ttm_special_needs_education <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = special_needs_education_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening buitengewoon onderwijs: ", conditionMessage(e))
  }
)

time_col_special_needs_education <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_special_needs_education),
  value = TRUE
)[1]

if (is.na(time_col_special_needs_education)) {
  stop(
    "Geen reistijdkolom gevonden voor buitengewoon onderwijs. Kolommen: ",
    paste(names(ttm_special_needs_education), collapse = ", ")
  )
}

acc_special_needs_education_30 <- ttm_special_needs_education %>%
  filter(
    !is.na(.data[[time_col_special_needs_education]]),
    .data[[time_col_special_needs_education]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    special_needs_education_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    special_needs_education_30min
  )

acc_special_needs_education_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_special_needs_education_30,
    by = "new_ID"
  ) %>%
  mutate(
    special_needs_education_30min = tidyr::replace_na(special_needs_education_30min, 0L)
  )

write.csv(
  acc_special_needs_education_30,
  "output_special_needs_education_30min_09_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_special_needs_education)
gc()
#Crematoriums 
ttm_crematoriums <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = crematoriums_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening crematoria: ", conditionMessage(e))
  }
)

time_col_crematoriums <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_crematoriums),
  value = TRUE
)[1]

if (is.na(time_col_crematoriums)) {
  stop(
    "Geen reistijdkolom gevonden voor crematoria. Kolommen: ",
    paste(names(ttm_crematoriums), collapse = ", ")
  )
}

acc_crematoriums_30 <- ttm_crematoriums %>%
  filter(
    !is.na(.data[[time_col_crematoriums]]),
    .data[[time_col_crematoriums]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    crematoriums_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    crematoriums_30min
  )

acc_crematoriums_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_crematoriums_30,
    by = "new_ID"
  ) %>%
  mutate(
    crematoriums_30min = tidyr::replace_na(crematoriums_30min, 0L)
  )

write.csv(
  acc_crematoriums_30,
  "output_crematoriums_30min_09_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_crematoriums)
gc()


#Public transport stops
ttm_public_transport_stops <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = public_transport_stops_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening haltes openbaar vervoer: ", conditionMessage(e))
  }
)

time_col_public_transport_stops <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_public_transport_stops),
  value = TRUE
)[1]

if (is.na(time_col_public_transport_stops)) {
  stop(
    "Geen reistijdkolom gevonden voor haltes openbaar vervoer. Kolommen: ",
    paste(names(ttm_public_transport_stops), collapse = ", ")
  )
}

acc_public_transport_stops_30 <- ttm_public_transport_stops %>%
  filter(
    !is.na(.data[[time_col_public_transport_stops]]),
    .data[[time_col_public_transport_stops]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    public_transport_stops_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    public_transport_stops_30min
  )

acc_public_transport_stops_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_public_transport_stops_30,
    by = "new_ID"
  ) %>%
  mutate(
    public_transport_stops_30min = tidyr::replace_na(public_transport_stops_30min, 0L)
  )

write.csv(
  acc_public_transport_stops_30,
  "output_PT_stops_30min_09_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_public_transport_stops)
gc()


#Nursing homes
ttm_nursing_homes <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = nursing_homes_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening woonzorgcentra: ", conditionMessage(e))
  }
)

time_col_nursing_homes <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_nursing_homes),
  value = TRUE
)[1]

if (is.na(time_col_nursing_homes)) {
  stop(
    "Geen reistijdkolom gevonden voor woonzorgcentra. Kolommen: ",
    paste(names(ttm_nursing_homes), collapse = ", ")
  )
}

acc_nursing_homes_30 <- ttm_nursing_homes %>%
  filter(
    !is.na(.data[[time_col_nursing_homes]]),
    .data[[time_col_nursing_homes]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    nursing_homes_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    nursing_homes_30min
  )

acc_nursing_homes_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_nursing_homes_30,
    by = "new_ID"
  ) %>%
  mutate(
    nursing_homes_30min = tidyr::replace_na(nursing_homes_30min, 0L)
  )

write.csv(
  acc_nursing_homes_30,
  "output_nursing_homes_30min_09_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_nursing_homes)
gc()


#Pharmacies
ttm_pharmacies <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = pharmacies_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening apotheken: ", conditionMessage(e))
  }
)

time_col_pharmacies <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_pharmacies),
  value = TRUE
)[1]

if (is.na(time_col_pharmacies)) {
  stop(
    "Geen reistijdkolom gevonden voor apotheken. Kolommen: ",
    paste(names(ttm_pharmacies), collapse = ", ")
  )
}

acc_pharmacies_30 <- ttm_pharmacies %>%
  filter(
    !is.na(.data[[time_col_pharmacies]]),
    .data[[time_col_pharmacies]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    pharmacies_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    pharmacies_30min
  )

acc_pharmacies_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_pharmacies_30,
    by = "new_ID"
  ) %>%
  mutate(
    pharmacies_30min = tidyr::replace_na(pharmacies_30min, 0L)
  )

write.csv(
  acc_pharmacies_30,
  "output_pharmacies_30min_09_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_pharmacies)
gc()


#Cemeteries
ttm_cemeteries <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = cemeteries_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening begraafplaatsen: ", conditionMessage(e))
  }
)

time_col_cemeteries <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_cemeteries),
  value = TRUE
)[1]

if (is.na(time_col_cemeteries)) {
  stop(
    "Geen reistijdkolom gevonden voor begraafplaatsen. Kolommen: ",
    paste(names(ttm_cemeteries), collapse = ", ")
  )
}

acc_cemeteries_30 <- ttm_cemeteries %>%
  filter(
    !is.na(.data[[time_col_cemeteries]]),
    .data[[time_col_cemeteries]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    cemeteries_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    cemeteries_30min
  )

acc_cemeteries_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_cemeteries_30,
    by = "new_ID"
  ) %>%
  mutate(
    cemeteries_30min = tidyr::replace_na(cemeteries_30min, 0L)
  )


write.csv(
  acc_cemeteries_30,
  "output_cemeteries_30min_09_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_cemeteries)
gc()


#Schools
ttm_schools <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = schools_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening scholen: ", conditionMessage(e))
  }
)

time_col_schools <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_schools),
  value = TRUE
)[1]

if (is.na(time_col_schools)) {
  stop(
    "Geen reistijdkolom gevonden voor scholen. Kolommen: ",
    paste(names(ttm_schools), collapse = ", ")
  )
}

acc_schools_30 <- ttm_schools %>%
  filter(
    !is.na(.data[[time_col_schools]]),
    .data[[time_col_schools]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    schools_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    schools_30min
  )

acc_schools_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_schools_30,
    by = "new_ID"
  ) %>%
  mutate(
    schools_30min = tidyr::replace_na(schools_30min, 0L)
  )

write.csv(
  acc_schools_30,
  "output_schools_30min_09_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_schools)
gc()


#Dieticians
ttm_dietitians <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = dietitians_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening diëtisten: ", conditionMessage(e))
  }
)

time_col_dietitians <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_dietitians),
  value = TRUE
)[1]

if (is.na(time_col_dietitians)) {
  stop(
    "Geen reistijdkolom gevonden voor diëtisten. Kolommen: ",
    paste(names(ttm_dietitians), collapse = ", ")
  )
}

acc_dietitians_30 <- ttm_dietitians %>%
  filter(
    !is.na(.data[[time_col_dietitians]]),
    .data[[time_col_dietitians]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    dietitians_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    dietitians_30min
  )

acc_dietitians_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_dietitians_30,
    by = "new_ID"
  ) %>%
  mutate(
    dietitians_30min = tidyr::replace_na(dietitians_30min, 0L)
  )

write.csv(
  acc_dietitians_30,
  "output_dietitians_30min_09_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_dietitians)
gc()

# -----------------------------------------
# 6) Joining results and writing csv
# -----------------------------------------

# Join to origins
analysis_origins <- analysis_origins %>%
  select(-ID) %>%
  rename(ID = new_ID)

origin_summary <- analysis_origins %>%
  select(ID, Trip_ID, lon, lat) %>%
  left_join(acc_schools_30, by = "ID") %>%
  left_join(acc_hospitals_30, by = "ID") %>%
  left_join(acc_adult_day_care_centres_30, by = "ID") %>%
  left_join(acc_socio_cultural_activities_30, by = "ID") %>%
  left_join(acc_groceries_30, by = "ID") %>%
  left_join(acc_speech_therapists_30, by = "ID") %>%
  left_join(acc_physiotherapists_30, by = "ID") %>%
  left_join(acc_dietitians_30, by = "ID") %>%
  left_join(acc_occupational_therapists_30, by = "ID") %>%
  left_join(acc_general_practitioners_30, by = "ID") %>%
  left_join(acc_hairdressers_30, by = "ID") %>%
  left_join(acc_restaurants_30, by = "ID") %>%
  left_join(acc_dentists_30, by = "ID") %>%
  left_join(acc_town_halls_30, by = "ID") %>%
  left_join(acc_local_service_centres_30, by = "ID") %>%
  left_join(acc_special_needs_education_30, by = "ID") %>%
  left_join(acc_crematoriums_30, by = "ID") %>%
  left_join(acc_public_transport_stops_30, by = "ID") %>%
  left_join(acc_nursing_homes_30, by = "ID") %>%
  left_join(acc_pharmacies_30, by = "ID") %>%
  left_join(acc_cemeteries_30, by = "ID")


# Writing csv
write.csv(origin_summary, "output_cumulative_accessibility_09_combined.csv", row.names = FALSE)


# 12 pm
# -----------------------------------------
# 4) Basis parameters r5r
# -----------------------------------------
mode               <- c("WALK", "TRANSIT")
max_walk_time      <- 15     # minutes

cutoff_minutes     <- 30L    # 30 minutes to reach opportunities

departure_datetime <- as.POSIXct("18-01-2024 12:00:00", format="%d-%m-%Y %H:%M:%S") #typical Thursday
# -------------------------------------------------------
# 5) 30 minutes cumulative accessibility
#    via r5r::accessibility() with step-decay (cutoff=30)
# -------------------------------------------------------
# We calculate per origin the number of opportunities (destinations of goals)
# Accessible within 30 minutes PT+walking
#ids <- all_unique_origins$new_ID
#n   <- length(ids)

analysis_origins <- all_unique_origins


origins_analysis_df <- analysis_origins %>%
  transmute(
    id     = as.character(new_ID),  # alleen voor r5r
    new_ID = as.integer(new_ID),    # sleutel voor koppeling
    lon    = as.numeric(lon),
    lat    = as.numeric(lat)
  ) %>%
  distinct(new_ID, .keep_all = TRUE)

stopifnot(
  !anyDuplicated(origins_analysis_df$id),
  !anyNA(origins_analysis_df$lon),
  !anyNA(origins_analysis_df$lat)
)


#Hospitals

ttm_hospitals <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = hospitals_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Error hospitals: ", conditionMessage(e))
  }
)

time_col_hospitals <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_hospitals),
  value = TRUE
)[1]

if (is.na(time_col_hospitals)) {
  stop(
    "Geen reistijdkolom gevonden. Kolommen: ",
    paste(names(ttm_hospitals), collapse = ", ")
  )
}

acc_hospitals_30 <- ttm_hospitals %>%
  filter(
    !is.na(.data[[time_col_hospitals]]),
    .data[[time_col_hospitals]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    hospitals_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    hospitals_30min
  )

acc_hospitals_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_hospitals_30,
    by = "new_ID"
  ) %>%
  mutate(
    hospitals_30min = tidyr::replace_na(hospitals_30min, 0L)
  )
write.csv(acc_hospitals_30, "output_hospitals_30min_12_combined_NMBS_DeLijn.csv", row.names = FALSE)
rm(ttm_hospitals)
gc()

#Adult day care centres
ttm_adult_day_care_centres <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = adult_day_care_centres_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop(
      "Fout bij berekening dagverzorgingscentra: ",
      conditionMessage(e)
    )
  }
)

time_col_adult_day_care_centres <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_adult_day_care_centres),
  value = TRUE
)[1]

if (is.na(time_col_adult_day_care_centres)) {
  stop(
    "Geen reistijdkolom gevonden voor dagverzorgingscentra. Kolommen: ",
    paste(names(ttm_adult_day_care_centres), collapse = ", ")
  )
}

acc_adult_day_care_centres_30 <- ttm_adult_day_care_centres %>%
  filter(
    !is.na(.data[[time_col_adult_day_care_centres]]),
    .data[[time_col_adult_day_care_centres]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    adult_day_care_centres_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    adult_day_care_centres_30min
  )

acc_adult_day_care_centres_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_adult_day_care_centres_30,
    by = "new_ID"
  ) %>%
  mutate(
    adult_day_care_centres_30min = tidyr::replace_na(adult_day_care_centres_30min, 0L)
  )

write.csv(
  acc_adult_day_care_centres_30,
  "output_adult_day_care_centres_30min_12_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_adult_day_care_centres)
gc()


#Socio-cultural amenities
ttm_socio_cultural_activities <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = socio_cultural_activities_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening sociaal-culturele voorzieningen: ", conditionMessage(e))
  }
)

time_col_socio_cultural_activities <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_socio_cultural_activities),
  value = TRUE
)[1]

if (is.na(time_col_socio_cultural_activities)) {
  stop(
    "Geen reistijdkolom gevonden voor sociaal-culturele voorzieningen. Kolommen: ",
    paste(names(ttm_socio_cultural_activities), collapse = ", ")
  )
}

acc_socio_cultural_activities_30 <- ttm_socio_cultural_activities %>%
  filter(
    !is.na(.data[[time_col_socio_cultural_activities]]),
    .data[[time_col_socio_cultural_activities]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    socio_cultural_activities_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    socio_cultural_activities_30min
  )

acc_socio_cultural_activities_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_socio_cultural_activities_30,
    by = "new_ID"
  ) %>%
  mutate(
    socio_cultural_activities_30min = tidyr::replace_na(socio_cultural_activities_30min, 0L)
  )

write.csv(
  acc_socio_cultural_activities_30,
  "output_socio_cultural_act_30min_12_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_socio_cultural_activities)
gc()

#Groceries
ttm_groceries <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = groceries_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening voedingswinkels: ", conditionMessage(e))
  }
)

time_col_groceries <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_groceries),
  value = TRUE
)[1]

if (is.na(time_col_groceries)) {
  stop(
    "Geen reistijdkolom gevonden voor voedingswinkels. Kolommen: ",
    paste(names(ttm_groceries), collapse = ", ")
  )
}

acc_groceries_30 <- ttm_groceries %>%
  filter(
    !is.na(.data[[time_col_groceries]]),
    .data[[time_col_groceries]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    groceries_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    groceries_30min
  )

acc_groceries_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_groceries_30,
    by = "new_ID"
  ) %>%
  mutate(
    groceries_30min = tidyr::replace_na(groceries_30min, 0L)
  )


write.csv(
  acc_groceries_30,
  "output_groceries_30min_12_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_groceries)
gc()

#Speech therapists
ttm_speech_therapists <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = speech_therapists_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening logopedisten: ", conditionMessage(e))
  }
)

time_col_speech_therapists <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_speech_therapists),
  value = TRUE
)[1]

if (is.na(time_col_speech_therapists)) {
  stop(
    "Geen reistijdkolom gevonden voor logopedisten. Kolommen: ",
    paste(names(ttm_speech_therapists), collapse = ", ")
  )
}

acc_speech_therapists_30 <- ttm_speech_therapists %>%
  filter(
    !is.na(.data[[time_col_speech_therapists]]),
    .data[[time_col_speech_therapists]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    speech_therapists_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    speech_therapists_30min
  )

acc_speech_therapists_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_speech_therapists_30,
    by = "new_ID"
  ) %>%
  mutate(
    speech_therapists_30min = tidyr::replace_na(speech_therapists_30min, 0L)
  )

write.csv(
  acc_speech_therapists_30,
  "output_speech_therapists_30min_12_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_speech_therapists)
gc()


#Physiotherapists
ttm_physiotherapists <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = physiotherapists_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening kinesitherapeuten: ", conditionMessage(e))
  }
)

time_col_physiotherapists <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_physiotherapists),
  value = TRUE
)[1]

if (is.na(time_col_physiotherapists)) {
  stop(
    "Geen reistijdkolom gevonden voor kinesitherapeuten. Kolommen: ",
    paste(names(ttm_physiotherapists), collapse = ", ")
  )
}

acc_physiotherapists_30 <- ttm_physiotherapists %>%
  filter(
    !is.na(.data[[time_col_physiotherapists]]),
    .data[[time_col_physiotherapists]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    physiotherapists_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    physiotherapists_30min
  )

acc_physiotherapists_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_physiotherapists_30,
    by = "new_ID"
  ) %>%
  mutate(
    physiotherapists_30min = tidyr::replace_na(physiotherapists_30min, 0L)
  )



write.csv(
  acc_physiotherapists_30,
  "output_physiotherapists_30min_12_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_physiotherapists)
gc()

#Occupational therapists
ttm_occupational_therapists <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = occupational_therapists_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening ergotherapeuten: ", conditionMessage(e))
  }
)

time_col_occupational_therapists <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_occupational_therapists),
  value = TRUE
)[1]

if (is.na(time_col_occupational_therapists)) {
  stop(
    "Geen reistijdkolom gevonden voor ergotherapeuten. Kolommen: ",
    paste(names(ttm_occupational_therapists), collapse = ", ")
  )
}

acc_occupational_therapists_30 <- ttm_occupational_therapists %>%
  filter(
    !is.na(.data[[time_col_occupational_therapists]]),
    .data[[time_col_occupational_therapists]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    occupational_therapists_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    occupational_therapists_30min
  )

acc_occupational_therapists_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_occupational_therapists_30,
    by = "new_ID"
  ) %>%
  mutate(
    occupational_therapists_30min = tidyr::replace_na(occupational_therapists_30min, 0L)
  )


write.csv(
  acc_occupational_therapists_30,
  "output_occupational_therapists_30min_12_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_occupational_therapists)
gc()

#General practitioners 
ttm_general_practitioners <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = general_practitioners_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening huisartsen: ", conditionMessage(e))
  }
)

time_col_general_practitioners <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_general_practitioners),
  value = TRUE
)[1]

if (is.na(time_col_general_practitioners)) {
  stop(
    "Geen reistijdkolom gevonden voor huisartsen. Kolommen: ",
    paste(names(ttm_general_practitioners), collapse = ", ")
  )
}

acc_general_practitioners_30 <- ttm_general_practitioners %>%
  filter(
    !is.na(.data[[time_col_general_practitioners]]),
    .data[[time_col_general_practitioners]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    general_practitioners_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    general_practitioners_30min
  )

acc_general_practitioners_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_general_practitioners_30,
    by = "new_ID"
  ) %>%
  mutate(
    general_practitioners_30min = tidyr::replace_na(general_practitioners_30min, 0L)
  )

write.csv(
  acc_general_practitioners_30,
  "output_GP_30min_12_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_general_practitioners)
gc()


#Hairdressers
ttm_hairdressers <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = hairdressers_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening kappers: ", conditionMessage(e))
  }
)

time_col_hairdressers <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_hairdressers),
  value = TRUE
)[1]

if (is.na(time_col_hairdressers)) {
  stop(
    "Geen reistijdkolom gevonden voor kappers. Kolommen: ",
    paste(names(ttm_hairdressers), collapse = ", ")
  )
}

acc_hairdressers_30 <- ttm_hairdressers %>%
  filter(
    !is.na(.data[[time_col_hairdressers]]),
    .data[[time_col_hairdressers]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    hairdressers_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    hairdressers_30min
  )

acc_hairdressers_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_hairdressers_30,
    by = "new_ID"
  ) %>%
  mutate(
    hairdressers_30min = tidyr::replace_na(hairdressers_30min, 0L)
  )

write.csv(
  acc_hairdressers_30,
  "output_hairdressers_30min_12_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_hairdressers)
gc()

#Restaurants
ttm_restaurants <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = restaurants_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening restaurants: ", conditionMessage(e))
  }
)

time_col_restaurants <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_restaurants),
  value = TRUE
)[1]

if (is.na(time_col_restaurants)) {
  stop(
    "Geen reistijdkolom gevonden voor restaurants. Kolommen: ",
    paste(names(ttm_restaurants), collapse = ", ")
  )
}

acc_restaurants_30 <- ttm_restaurants %>%
  filter(
    !is.na(.data[[time_col_restaurants]]),
    .data[[time_col_restaurants]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    restaurants_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    restaurants_30min
  )

acc_restaurants_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_restaurants_30,
    by = "new_ID"
  ) %>%
  mutate(
    restaurants_30min = tidyr::replace_na(restaurants_30min, 0L)
  )


write.csv(
  acc_restaurants_30,
  "output_restaurants_30min_12_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_restaurants)
gc()


#Dentists
ttm_dentists <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = dentists_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening tandartsen: ", conditionMessage(e))
  }
)

time_col_dentists <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_dentists),
  value = TRUE
)[1]

if (is.na(time_col_dentists)) {
  stop(
    "Geen reistijdkolom gevonden voor tandartsen. Kolommen: ",
    paste(names(ttm_dentists), collapse = ", ")
  )
}

acc_dentists_30 <- ttm_dentists %>%
  filter(
    !is.na(.data[[time_col_dentists]]),
    .data[[time_col_dentists]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    dentists_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    dentists_30min
  )

acc_dentists_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_dentists_30,
    by = "new_ID"
  ) %>%
  mutate(
    dentists_30min = tidyr::replace_na(dentists_30min, 0L)
  )

write.csv(
  acc_dentists_30,
  "output_dentists_30min_12_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_dentists)
gc()


#Town halls
ttm_town_halls <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = town_halls_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening gemeentehuizen: ", conditionMessage(e))
  }
)

time_col_town_halls <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_town_halls),
  value = TRUE
)[1]

if (is.na(time_col_town_halls)) {
  stop(
    "Geen reistijdkolom gevonden voor gemeentehuizen. Kolommen: ",
    paste(names(ttm_town_halls), collapse = ", ")
  )
}

acc_town_halls_30 <- ttm_town_halls %>%
  filter(
    !is.na(.data[[time_col_town_halls]]),
    .data[[time_col_town_halls]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    town_halls_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    town_halls_30min
  )

acc_town_halls_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_town_halls_30,
    by = "new_ID"
  ) %>%
  mutate(
    town_halls_30min = tidyr::replace_na(town_halls_30min, 0L)
  )

write.csv(
  acc_town_halls_30,
  "output_town_halls_30min_12_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_town_halls)
gc()


#Local service centres
ttm_local_service_centres <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = local_service_centres_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening lokale dienstencentra: ", conditionMessage(e))
  }
)

time_col_local_service_centres <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_local_service_centres),
  value = TRUE
)[1]

if (is.na(time_col_local_service_centres)) {
  stop(
    "Geen reistijdkolom gevonden voor lokale dienstencentra. Kolommen: ",
    paste(names(ttm_local_service_centres), collapse = ", ")
  )
}

acc_local_service_centres_30 <- ttm_local_service_centres %>%
  filter(
    !is.na(.data[[time_col_local_service_centres]]),
    .data[[time_col_local_service_centres]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    local_service_centres_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    local_service_centres_30min
  )

acc_local_service_centres_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_local_service_centres_30,
    by = "new_ID"
  ) %>%
  mutate(
    local_service_centres_30min = tidyr::replace_na(local_service_centres_30min, 0L)
  )

write.csv(
  acc_local_service_centres_30,
  "output_local_service_centres_30min_12_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_local_service_centres)
gc()

#Special needs education
ttm_special_needs_education <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = special_needs_education_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening buitengewoon onderwijs: ", conditionMessage(e))
  }
)

time_col_special_needs_education <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_special_needs_education),
  value = TRUE
)[1]

if (is.na(time_col_special_needs_education)) {
  stop(
    "Geen reistijdkolom gevonden voor buitengewoon onderwijs. Kolommen: ",
    paste(names(ttm_special_needs_education), collapse = ", ")
  )
}

acc_special_needs_education_30 <- ttm_special_needs_education %>%
  filter(
    !is.na(.data[[time_col_special_needs_education]]),
    .data[[time_col_special_needs_education]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    special_needs_education_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    special_needs_education_30min
  )

acc_special_needs_education_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_special_needs_education_30,
    by = "new_ID"
  ) %>%
  mutate(
    special_needs_education_30min = tidyr::replace_na(special_needs_education_30min, 0L)
  )

write.csv(
  acc_special_needs_education_30,
  "output_special_needs_education_30min_12_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_special_needs_education)
gc()
#Crematoriums 
ttm_crematoriums <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = crematoriums_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening crematoria: ", conditionMessage(e))
  }
)

time_col_crematoriums <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_crematoriums),
  value = TRUE
)[1]

if (is.na(time_col_crematoriums)) {
  stop(
    "Geen reistijdkolom gevonden voor crematoria. Kolommen: ",
    paste(names(ttm_crematoriums), collapse = ", ")
  )
}

acc_crematoriums_30 <- ttm_crematoriums %>%
  filter(
    !is.na(.data[[time_col_crematoriums]]),
    .data[[time_col_crematoriums]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    crematoriums_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    crematoriums_30min
  )

acc_crematoriums_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_crematoriums_30,
    by = "new_ID"
  ) %>%
  mutate(
    crematoriums_30min = tidyr::replace_na(crematoriums_30min, 0L)
  )

write.csv(
  acc_crematoriums_30,
  "output_crematoriums_30min_12_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_crematoriums)
gc()


#Public transport stops
ttm_public_transport_stops <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = public_transport_stops_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening haltes openbaar vervoer: ", conditionMessage(e))
  }
)

time_col_public_transport_stops <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_public_transport_stops),
  value = TRUE
)[1]

if (is.na(time_col_public_transport_stops)) {
  stop(
    "Geen reistijdkolom gevonden voor haltes openbaar vervoer. Kolommen: ",
    paste(names(ttm_public_transport_stops), collapse = ", ")
  )
}

acc_public_transport_stops_30 <- ttm_public_transport_stops %>%
  filter(
    !is.na(.data[[time_col_public_transport_stops]]),
    .data[[time_col_public_transport_stops]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    public_transport_stops_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    public_transport_stops_30min
  )

acc_public_transport_stops_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_public_transport_stops_30,
    by = "new_ID"
  ) %>%
  mutate(
    public_transport_stops_30min = tidyr::replace_na(public_transport_stops_30min, 0L)
  )

write.csv(
  acc_public_transport_stops_30,
  "output_PT_stops_30min_12_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_public_transport_stops)
gc()


#Nursing homes
ttm_nursing_homes <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = nursing_homes_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening woonzorgcentra: ", conditionMessage(e))
  }
)

time_col_nursing_homes <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_nursing_homes),
  value = TRUE
)[1]

if (is.na(time_col_nursing_homes)) {
  stop(
    "Geen reistijdkolom gevonden voor woonzorgcentra. Kolommen: ",
    paste(names(ttm_nursing_homes), collapse = ", ")
  )
}

acc_nursing_homes_30 <- ttm_nursing_homes %>%
  filter(
    !is.na(.data[[time_col_nursing_homes]]),
    .data[[time_col_nursing_homes]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    nursing_homes_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    nursing_homes_30min
  )

acc_nursing_homes_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_nursing_homes_30,
    by = "new_ID"
  ) %>%
  mutate(
    nursing_homes_30min = tidyr::replace_na(nursing_homes_30min, 0L)
  )

write.csv(
  acc_nursing_homes_30,
  "output_nursing_homes_30min_12_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_nursing_homes)
gc()


#Pharmacies
ttm_pharmacies <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = pharmacies_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening apotheken: ", conditionMessage(e))
  }
)

time_col_pharmacies <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_pharmacies),
  value = TRUE
)[1]

if (is.na(time_col_pharmacies)) {
  stop(
    "Geen reistijdkolom gevonden voor apotheken. Kolommen: ",
    paste(names(ttm_pharmacies), collapse = ", ")
  )
}

acc_pharmacies_30 <- ttm_pharmacies %>%
  filter(
    !is.na(.data[[time_col_pharmacies]]),
    .data[[time_col_pharmacies]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    pharmacies_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    pharmacies_30min
  )

acc_pharmacies_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_pharmacies_30,
    by = "new_ID"
  ) %>%
  mutate(
    pharmacies_30min = tidyr::replace_na(pharmacies_30min, 0L)
  )

write.csv(
  acc_pharmacies_30,
  "output_pharmacies_30min_12_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_pharmacies)
gc()


#Cemeteries
ttm_cemeteries <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = cemeteries_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening begraafplaatsen: ", conditionMessage(e))
  }
)

time_col_cemeteries <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_cemeteries),
  value = TRUE
)[1]

if (is.na(time_col_cemeteries)) {
  stop(
    "Geen reistijdkolom gevonden voor begraafplaatsen. Kolommen: ",
    paste(names(ttm_cemeteries), collapse = ", ")
  )
}

acc_cemeteries_30 <- ttm_cemeteries %>%
  filter(
    !is.na(.data[[time_col_cemeteries]]),
    .data[[time_col_cemeteries]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    cemeteries_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    cemeteries_30min
  )

acc_cemeteries_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_cemeteries_30,
    by = "new_ID"
  ) %>%
  mutate(
    cemeteries_30min = tidyr::replace_na(cemeteries_30min, 0L)
  )


write.csv(
  acc_cemeteries_30,
  "output_cemeteries_30min_12_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_cemeteries)
gc()


#Schools
ttm_schools <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = schools_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening scholen: ", conditionMessage(e))
  }
)

time_col_schools <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_schools),
  value = TRUE
)[1]

if (is.na(time_col_schools)) {
  stop(
    "Geen reistijdkolom gevonden voor scholen. Kolommen: ",
    paste(names(ttm_schools), collapse = ", ")
  )
}

acc_schools_30 <- ttm_schools %>%
  filter(
    !is.na(.data[[time_col_schools]]),
    .data[[time_col_schools]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    schools_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    schools_30min
  )

acc_schools_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_schools_30,
    by = "new_ID"
  ) %>%
  mutate(
    schools_30min = tidyr::replace_na(schools_30min, 0L)
  )


write.csv(
  acc_schools_30,
  "output_schools_30min_12_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_schools)
gc()


#Dieticians
ttm_dietitians <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = dietitians_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening diëtisten: ", conditionMessage(e))
  }
)

time_col_dietitians <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_dietitians),
  value = TRUE
)[1]

if (is.na(time_col_dietitians)) {
  stop(
    "Geen reistijdkolom gevonden voor diëtisten. Kolommen: ",
    paste(names(ttm_dietitians), collapse = ", ")
  )
}

acc_dietitians_30 <- ttm_dietitians %>%
  filter(
    !is.na(.data[[time_col_dietitians]]),
    .data[[time_col_dietitians]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    dietitians_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    dietitians_30min
  )

acc_dietitians_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_dietitians_30,
    by = "new_ID"
  ) %>%
  mutate(
    dietitians_30min = tidyr::replace_na(dietitians_30min, 0L)
  )
write.csv(
  acc_dietitians_30,
  "output_dietitians_30min_12_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_dietitians)
gc()

# -----------------------------------------
# 6) Joining results and writing csv
# -----------------------------------------

# Join to origins
analysis_origins <- analysis_origins %>%
  select(-ID) %>%
  rename(ID = new_ID)

origin_summary <- analysis_origins %>%
  select(ID, Trip_ID, lon, lat) %>%
  left_join(acc_schools_30, by = "ID") %>%
  left_join(acc_hospitals_30, by = "ID") %>%
  left_join(acc_adult_day_care_centres_30, by = "ID") %>%
  left_join(acc_socio_cultural_activities_30, by = "ID") %>%
  left_join(acc_groceries_30, by = "ID") %>%
  left_join(acc_speech_therapists_30, by = "ID") %>%
  left_join(acc_physiotherapists_30, by = "ID") %>%
  left_join(acc_dietitians_30, by = "ID") %>%
  left_join(acc_occupational_therapists_30, by = "ID") %>%
  left_join(acc_general_practitioners_30, by = "ID") %>%
  left_join(acc_hairdressers_30, by = "ID") %>%
  left_join(acc_restaurants_30, by = "ID") %>%
  left_join(acc_dentists_30, by = "ID") %>%
  left_join(acc_town_halls_30, by = "ID") %>%
  left_join(acc_local_service_centres_30, by = "ID") %>%
  left_join(acc_special_needs_education_30, by = "ID") %>%
  left_join(acc_crematoriums_30, by = "ID") %>%
  left_join(acc_public_transport_stops_30, by = "ID") %>%
  left_join(acc_nursing_homes_30, by = "ID") %>%
  left_join(acc_pharmacies_30, by = "ID") %>%
  left_join(acc_cemeteries_30, by = "ID")


# Writing csv
write.csv(origin_summary, "output_cumulative_accessibility_12_combined.csv", row.names = FALSE)

#3 pm
# -----------------------------------------
# 4) Basis parameters r5r
# -----------------------------------------
mode               <- c("WALK", "TRANSIT")
max_walk_time      <- 15     # minutes

cutoff_minutes     <- 30L    # 30 minutes to reach opportunities

departure_datetime <- as.POSIXct("18-01-2024 15:00:00", format="%d-%m-%Y %H:%M:%S") #typical Thursday
# -------------------------------------------------------
# 5) 30 minutes cumulative accessibility
#    via r5r::accessibility() with step-decay (cutoff=30)
# -------------------------------------------------------
# We calculate per origin the number of opportunities (destinations of goals)
# Accessible within 30 minutes PT+walking
#ids <- all_unique_origins$new_ID
#n   <- length(ids)

analysis_origins <- all_unique_origins


origins_analysis_df <- analysis_origins %>%
  transmute(
    id     = as.character(new_ID),  # alleen voor r5r
    new_ID = as.integer(new_ID),    # sleutel voor koppeling
    lon    = as.numeric(lon),
    lat    = as.numeric(lat)
  ) %>%
  distinct(new_ID, .keep_all = TRUE)

stopifnot(
  !anyDuplicated(origins_analysis_df$id),
  !anyNA(origins_analysis_df$lon),
  !anyNA(origins_analysis_df$lat)
)


#Hospitals

ttm_hospitals <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = hospitals_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Error hospitals: ", conditionMessage(e))
  }
)

time_col_hospitals <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_hospitals),
  value = TRUE
)[1]

if (is.na(time_col_hospitals)) {
  stop(
    "Geen reistijdkolom gevonden. Kolommen: ",
    paste(names(ttm_hospitals), collapse = ", ")
  )
}

acc_hospitals_30 <- ttm_hospitals %>%
  filter(
    !is.na(.data[[time_col_hospitals]]),
    .data[[time_col_hospitals]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    hospitals_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    hospitals_30min
  )

acc_hospitals_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_hospitals_30,
    by = "new_ID"
  ) %>%
  mutate(
    hospitals_30min = tidyr::replace_na(hospitals_30min, 0L)
  )

write.csv(acc_hospitals_30, "output_hospitals_30min_15_combined_NMBS_DeLijn.csv", row.names = FALSE)
rm(ttm_hospitals)
gc()

#Adult day care centres
ttm_adult_day_care_centres <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = adult_day_care_centres_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop(
      "Fout bij berekening dagverzorgingscentra: ",
      conditionMessage(e)
    )
  }
)

time_col_adult_day_care_centres <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_adult_day_care_centres),
  value = TRUE
)[1]

if (is.na(time_col_adult_day_care_centres)) {
  stop(
    "Geen reistijdkolom gevonden voor dagverzorgingscentra. Kolommen: ",
    paste(names(ttm_adult_day_care_centres), collapse = ", ")
  )
}
acc_adult_day_care_centres_30 <- ttm_adult_day_care_centres %>%
  filter(
    !is.na(.data[[time_col_adult_day_care_centres]]),
    .data[[time_col_adult_day_care_centres]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    adult_day_care_centres_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    adult_day_care_centres_30min
  )

acc_adult_day_care_centres_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_adult_day_care_centres_30,
    by = "new_ID"
  ) %>%
  mutate(
    adult_day_care_centres_30min = tidyr::replace_na(adult_day_care_centres_30min, 0L)
  )


write.csv(
  acc_adult_day_care_centres_30,
  "output_adult_day_care_centres_30min_15_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_adult_day_care_centres)
gc()


#Socio-cultural amenities
ttm_socio_cultural_activities <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = socio_cultural_activities_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening sociaal-culturele voorzieningen: ", conditionMessage(e))
  }
)

time_col_socio_cultural_activities <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_socio_cultural_activities),
  value = TRUE
)[1]

if (is.na(time_col_socio_cultural_activities)) {
  stop(
    "Geen reistijdkolom gevonden voor sociaal-culturele voorzieningen. Kolommen: ",
    paste(names(ttm_socio_cultural_activities), collapse = ", ")
  )
}

acc_socio_cultural_activities_30 <- ttm_socio_cultural_activities %>%
  filter(
    !is.na(.data[[time_col_socio_cultural_activities]]),
    .data[[time_col_socio_cultural_activities]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    socio_cultural_activities_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    socio_cultural_activities_30min
  )

acc_socio_cultural_activities_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_socio_cultural_activities_30,
    by = "new_ID"
  ) %>%
  mutate(
    socio_cultural_activities_30min = tidyr::replace_na(socio_cultural_activities_30min, 0L)
  )

write.csv(
  acc_socio_cultural_activities_30,
  "output_socio_cultural_act_30min_15_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_socio_cultural_activities)
gc()

#Groceries
ttm_groceries <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = groceries_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening voedingswinkels: ", conditionMessage(e))
  }
)

time_col_groceries <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_groceries),
  value = TRUE
)[1]

if (is.na(time_col_groceries)) {
  stop(
    "Geen reistijdkolom gevonden voor voedingswinkels. Kolommen: ",
    paste(names(ttm_groceries), collapse = ", ")
  )
}

acc_groceries_30 <- ttm_groceries %>%
  filter(
    !is.na(.data[[time_col_groceries]]),
    .data[[time_col_groceries]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    groceries_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    groceries_30min
  )

acc_groceries_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_groceries_30,
    by = "new_ID"
  ) %>%
  mutate(
    groceries_30min = tidyr::replace_na(groceries_30min, 0L)
  )


write.csv(
  acc_groceries_30,
  "output_groceries_30min_15_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_groceries)
gc()

#Speech therapists
ttm_speech_therapists <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = speech_therapists_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening logopedisten: ", conditionMessage(e))
  }
)

time_col_speech_therapists <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_speech_therapists),
  value = TRUE
)[1]

if (is.na(time_col_speech_therapists)) {
  stop(
    "Geen reistijdkolom gevonden voor logopedisten. Kolommen: ",
    paste(names(ttm_speech_therapists), collapse = ", ")
  )
}

acc_speech_therapists_30 <- ttm_speech_therapists %>%
  filter(
    !is.na(.data[[time_col_speech_therapists]]),
    .data[[time_col_speech_therapists]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    speech_therapists_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    speech_therapists_30min
  )

acc_speech_therapists_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_speech_therapists_30,
    by = "new_ID"
  ) %>%
  mutate(
    speech_therapists_30min = tidyr::replace_na(speech_therapists_30min, 0L)
  )
write.csv(
  acc_speech_therapists_30,
  "output_speech_therapists_30min_15_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_speech_therapists)
gc()


#Physiotherapists
ttm_physiotherapists <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = physiotherapists_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening kinesitherapeuten: ", conditionMessage(e))
  }
)

time_col_physiotherapists <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_physiotherapists),
  value = TRUE
)[1]

if (is.na(time_col_physiotherapists)) {
  stop(
    "Geen reistijdkolom gevonden voor kinesitherapeuten. Kolommen: ",
    paste(names(ttm_physiotherapists), collapse = ", ")
  )
}

acc_physiotherapists_30 <- ttm_physiotherapists %>%
  filter(
    !is.na(.data[[time_col_physiotherapists]]),
    .data[[time_col_physiotherapists]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    physiotherapists_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    physiotherapists_30min
  )

acc_physiotherapists_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_physiotherapists_30,
    by = "new_ID"
  ) %>%
  mutate(
    physiotherapists_30min = tidyr::replace_na(physiotherapists_30min, 0L)
  )



write.csv(
  acc_physiotherapists_30,
  "output_physiotherapists_30min_15_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_physiotherapists)
gc()

#Occupational therapists
ttm_occupational_therapists <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = occupational_therapists_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening ergotherapeuten: ", conditionMessage(e))
  }
)

time_col_occupational_therapists <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_occupational_therapists),
  value = TRUE
)[1]

if (is.na(time_col_occupational_therapists)) {
  stop(
    "Geen reistijdkolom gevonden voor ergotherapeuten. Kolommen: ",
    paste(names(ttm_occupational_therapists), collapse = ", ")
  )
}

acc_occupational_therapists_30 <- ttm_occupational_therapists %>%
  filter(
    !is.na(.data[[time_col_occupational_therapists]]),
    .data[[time_col_occupational_therapists]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    occupational_therapists_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    occupational_therapists_30min
  )

acc_occupational_therapists_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_occupational_therapists_30,
    by = "new_ID"
  ) %>%
  mutate(
    occupational_therapists_30min = tidyr::replace_na(occupational_therapists_30min, 0L)
  )


write.csv(
  acc_occupational_therapists_30,
  "output_occupational_therapists_30min_15_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_occupational_therapists)
gc()

#General practitioners 
ttm_general_practitioners <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = general_practitioners_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening huisartsen: ", conditionMessage(e))
  }
)

time_col_general_practitioners <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_general_practitioners),
  value = TRUE
)[1]

if (is.na(time_col_general_practitioners)) {
  stop(
    "Geen reistijdkolom gevonden voor huisartsen. Kolommen: ",
    paste(names(ttm_general_practitioners), collapse = ", ")
  )
}

acc_general_practitioners_30 <- ttm_general_practitioners %>%
  filter(
    !is.na(.data[[time_col_general_practitioners]]),
    .data[[time_col_general_practitioners]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    general_practitioners_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    general_practitioners_30min
  )

acc_general_practitioners_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_general_practitioners_30,
    by = "new_ID"
  ) %>%
  mutate(
    general_practitioners_30min = tidyr::replace_na(general_practitioners_30min, 0L)
  )

write.csv(
  acc_general_practitioners_30,
  "output_GP_30min_15_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_general_practitioners)
gc()


#Hairdressers
ttm_hairdressers <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = hairdressers_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening kappers: ", conditionMessage(e))
  }
)

time_col_hairdressers <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_hairdressers),
  value = TRUE
)[1]

if (is.na(time_col_hairdressers)) {
  stop(
    "Geen reistijdkolom gevonden voor kappers. Kolommen: ",
    paste(names(ttm_hairdressers), collapse = ", ")
  )
}

aacc_hairdressers_30 <- ttm_hairdressers %>%
  filter(
    !is.na(.data[[time_col_hairdressers]]),
    .data[[time_col_hairdressers]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    hairdressers_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    hairdressers_30min
  )

acc_hairdressers_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_hairdressers_30,
    by = "new_ID"
  ) %>%
  mutate(
    hairdressers_30min = tidyr::replace_na(hairdressers_30min, 0L)
  )

write.csv(
  acc_hairdressers_30,
  "output_hairdressers_30min_15_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_hairdressers)
gc()

#Restaurants
ttm_restaurants <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = restaurants_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening restaurants: ", conditionMessage(e))
  }
)

time_col_restaurants <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_restaurants),
  value = TRUE
)[1]

if (is.na(time_col_restaurants)) {
  stop(
    "Geen reistijdkolom gevonden voor restaurants. Kolommen: ",
    paste(names(ttm_restaurants), collapse = ", ")
  )
}

acc_restaurants_30 <- ttm_restaurants %>%
  filter(
    !is.na(.data[[time_col_restaurants]]),
    .data[[time_col_restaurants]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    restaurants_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    restaurants_30min
  )

acc_restaurants_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_restaurants_30,
    by = "new_ID"
  ) %>%
  mutate(
    restaurants_30min = tidyr::replace_na(restaurants_30min, 0L)
  )


write.csv(
  acc_restaurants_30,
  "output_restaurants_30min_15_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_restaurants)
gc()


#Dentists
ttm_dentists <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = dentists_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening tandartsen: ", conditionMessage(e))
  }
)

time_col_dentists <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_dentists),
  value = TRUE
)[1]

if (is.na(time_col_dentists)) {
  stop(
    "Geen reistijdkolom gevonden voor tandartsen. Kolommen: ",
    paste(names(ttm_dentists), collapse = ", ")
  )
}

acc_dentists_30 <- ttm_dentists %>%
  filter(
    !is.na(.data[[time_col_dentists]]),
    .data[[time_col_dentists]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    dentists_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    dentists_30min
  )

acc_dentists_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_dentists_30,
    by = "new_ID"
  ) %>%
  mutate(
    dentists_30min = tidyr::replace_na(dentists_30min, 0L)
  )

write.csv(
  acc_dentists_30,
  "output_dentists_30min_15_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_dentists)
gc()


#Town halls
ttm_town_halls <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = town_halls_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening gemeentehuizen: ", conditionMessage(e))
  }
)

time_col_town_halls <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_town_halls),
  value = TRUE
)[1]

if (is.na(time_col_town_halls)) {
  stop(
    "Geen reistijdkolom gevonden voor gemeentehuizen. Kolommen: ",
    paste(names(ttm_town_halls), collapse = ", ")
  )
}

acc_town_halls_30 <- ttm_town_halls %>%
  filter(
    !is.na(.data[[time_col_town_halls]]),
    .data[[time_col_town_halls]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    town_halls_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    town_halls_30min
  )

acc_town_halls_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_town_halls_30,
    by = "new_ID"
  ) %>%
  mutate(
    town_halls_30min = tidyr::replace_na(town_halls_30min, 0L)
  )

write.csv(
  acc_town_halls_30,
  "output_town_halls_30min_15_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_town_halls)
gc()


#Local service centres
ttm_local_service_centres <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = local_service_centres_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening lokale dienstencentra: ", conditionMessage(e))
  }
)

time_col_local_service_centres <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_local_service_centres),
  value = TRUE
)[1]

if (is.na(time_col_local_service_centres)) {
  stop(
    "Geen reistijdkolom gevonden voor lokale dienstencentra. Kolommen: ",
    paste(names(ttm_local_service_centres), collapse = ", ")
  )
}

acc_local_service_centres_30 <- ttm_local_service_centres %>%
  filter(
    !is.na(.data[[time_col_local_service_centres]]),
    .data[[time_col_local_service_centres]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    local_service_centres_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    local_service_centres_30min
  )

acc_local_service_centres_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_local_service_centres_30,
    by = "new_ID"
  ) %>%
  mutate(
    local_service_centres_30min = tidyr::replace_na(local_service_centres_30min, 0L)
  )

write.csv(
  acc_local_service_centres_30,
  "output_local_service_centres_30min_15_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_local_service_centres)
gc()

#Special needs education
ttm_special_needs_education <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = special_needs_education_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening buitengewoon onderwijs: ", conditionMessage(e))
  }
)

time_col_special_needs_education <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_special_needs_education),
  value = TRUE
)[1]

if (is.na(time_col_special_needs_education)) {
  stop(
    "Geen reistijdkolom gevonden voor buitengewoon onderwijs. Kolommen: ",
    paste(names(ttm_special_needs_education), collapse = ", ")
  )
}

acc_special_needs_education_30 <- ttm_special_needs_education %>%
  filter(
    !is.na(.data[[time_col_special_needs_education]]),
    .data[[time_col_special_needs_education]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    special_needs_education_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    special_needs_education_30min
  )

acc_special_needs_education_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_special_needs_education_30,
    by = "new_ID"
  ) %>%
  mutate(
    special_needs_education_30min = tidyr::replace_na(special_needs_education_30min, 0L)
  )

write.csv(
  acc_special_needs_education_30,
  "output_special_needs_education_30min_15_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_special_needs_education)
gc()
#Crematoriums 
ttm_crematoriums <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = crematoriums_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening crematoria: ", conditionMessage(e))
  }
)

time_col_crematoriums <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_crematoriums),
  value = TRUE
)[1]

if (is.na(time_col_crematoriums)) {
  stop(
    "Geen reistijdkolom gevonden voor crematoria. Kolommen: ",
    paste(names(ttm_crematoriums), collapse = ", ")
  )
}

acc_crematoriums_30 <- ttm_crematoriums %>%
  filter(
    !is.na(.data[[time_col_crematoriums]]),
    .data[[time_col_crematoriums]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    crematoriums_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    crematoriums_30min
  )

acc_crematoriums_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_crematoriums_30,
    by = "new_ID"
  ) %>%
  mutate(
    crematoriums_30min = tidyr::replace_na(crematoriums_30min, 0L)
  )

write.csv(
  acc_crematoriums_30,
  "output_crematoriums_30min_15_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_crematoriums)
gc()


#Public transport stops
ttm_public_transport_stops <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = public_transport_stops_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening haltes openbaar vervoer: ", conditionMessage(e))
  }
)

time_col_public_transport_stops <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_public_transport_stops),
  value = TRUE
)[1]

if (is.na(time_col_public_transport_stops)) {
  stop(
    "Geen reistijdkolom gevonden voor haltes openbaar vervoer. Kolommen: ",
    paste(names(ttm_public_transport_stops), collapse = ", ")
  )
}

acc_public_transport_stops_30 <- ttm_public_transport_stops %>%
  filter(
    !is.na(.data[[time_col_public_transport_stops]]),
    .data[[time_col_public_transport_stops]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    public_transport_stops_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    public_transport_stops_30min
  )

acc_public_transport_stops_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_public_transport_stops_30,
    by = "new_ID"
  ) %>%
  mutate(
    public_transport_stops_30min = tidyr::replace_na(public_transport_stops_30min, 0L)
  )

write.csv(
  acc_public_transport_stops_30,
  "output_PT_stops_30min_15_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_public_transport_stops)
gc()


#Nursing homes
ttm_nursing_homes <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = nursing_homes_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening woonzorgcentra: ", conditionMessage(e))
  }
)

time_col_nursing_homes <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_nursing_homes),
  value = TRUE
)[1]

if (is.na(time_col_nursing_homes)) {
  stop(
    "Geen reistijdkolom gevonden voor woonzorgcentra. Kolommen: ",
    paste(names(ttm_nursing_homes), collapse = ", ")
  )
}

acc_nursing_homes_30 <- ttm_nursing_homes %>%
  filter(
    !is.na(.data[[time_col_nursing_homes]]),
    .data[[time_col_nursing_homes]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    nursing_homes_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    nursing_homes_30min
  )

acc_nursing_homes_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_nursing_homes_30,
    by = "new_ID"
  ) %>%
  mutate(
    nursing_homes_30min = tidyr::replace_na(nursing_homes_30min, 0L)
  )

write.csv(
  acc_nursing_homes_30,
  "output_nursing_homes_30min_15_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_nursing_homes)
gc()


#Pharmacies
ttm_pharmacies <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = pharmacies_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening apotheken: ", conditionMessage(e))
  }
)

time_col_pharmacies <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_pharmacies),
  value = TRUE
)[1]

if (is.na(time_col_pharmacies)) {
  stop(
    "Geen reistijdkolom gevonden voor apotheken. Kolommen: ",
    paste(names(ttm_pharmacies), collapse = ", ")
  )
}

acc_pharmacies_30 <- ttm_pharmacies %>%
  filter(
    !is.na(.data[[time_col_pharmacies]]),
    .data[[time_col_pharmacies]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    pharmacies_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    pharmacies_30min
  )

acc_pharmacies_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_pharmacies_30,
    by = "new_ID"
  ) %>%
  mutate(
    pharmacies_30min = tidyr::replace_na(pharmacies_30min, 0L)
  )

write.csv(
  acc_pharmacies_30,
  "output_pharmacies_30min_15_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_pharmacies)
gc()


#Cemeteries
ttm_cemeteries <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = cemeteries_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening begraafplaatsen: ", conditionMessage(e))
  }
)

time_col_cemeteries <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_cemeteries),
  value = TRUE
)[1]

if (is.na(time_col_cemeteries)) {
  stop(
    "Geen reistijdkolom gevonden voor begraafplaatsen. Kolommen: ",
    paste(names(ttm_cemeteries), collapse = ", ")
  )
}

acc_cemeteries_30 <- ttm_cemeteries %>%
  filter(
    !is.na(.data[[time_col_cemeteries]]),
    .data[[time_col_cemeteries]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    cemeteries_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    cemeteries_30min
  )

acc_cemeteries_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_cemeteries_30,
    by = "new_ID"
  ) %>%
  mutate(
    cemeteries_30min = tidyr::replace_na(cemeteries_30min, 0L)
  )


write.csv(
  acc_cemeteries_30,
  "output_cemeteries_30min_15_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_cemeteries)
gc()


#Schools
ttm_schools <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = schools_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening scholen: ", conditionMessage(e))
  }
)

time_col_schools <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_schools),
  value = TRUE
)[1]

if (is.na(time_col_schools)) {
  stop(
    "Geen reistijdkolom gevonden voor scholen. Kolommen: ",
    paste(names(ttm_schools), collapse = ", ")
  )
}

acc_schools_30 <- ttm_schools %>%
  filter(
    !is.na(.data[[time_col_schools]]),
    .data[[time_col_schools]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    schools_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    schools_30min
  )

acc_schools_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_schools_30,
    by = "new_ID"
  ) %>%
  mutate(
    schools_30min = tidyr::replace_na(schools_30min, 0L)
  )


write.csv(
  acc_schools_30,
  "output_schools_30min_15_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_schools)
gc()


#Dieticians
ttm_dietitians <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = dietitians_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening diëtisten: ", conditionMessage(e))
  }
)

time_col_dietitians <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_dietitians),
  value = TRUE
)[1]

if (is.na(time_col_dietitians)) {
  stop(
    "Geen reistijdkolom gevonden voor diëtisten. Kolommen: ",
    paste(names(ttm_dietitians), collapse = ", ")
  )
}

acc_dietitians_30 <- ttm_dietitians %>%
  filter(
    !is.na(.data[[time_col_dietitians]]),
    .data[[time_col_dietitians]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    dietitians_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    dietitians_30min
  )

acc_dietitians_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_dietitians_30,
    by = "new_ID"
  ) %>%
  mutate(
    dietitians_30min = tidyr::replace_na(dietitians_30min, 0L)
  )

write.csv(
  acc_dietitians_30,
  "output_dietitians_30min_15_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_dietitians)
gc()

# -----------------------------------------
# 6) Joining results and writing csv
# -----------------------------------------

# Join to origins
analysis_origins <- analysis_origins %>%
  select(-ID) %>%
  rename(ID = new_ID)

origin_summary <- analysis_origins %>%
  select(ID, Trip_ID, lon, lat) %>%
  left_join(acc_schools_30, by = "ID") %>%
  left_join(acc_hospitals_30, by = "ID") %>%
  left_join(acc_adult_day_care_centres_30, by = "ID") %>%
  left_join(acc_socio_cultural_activities_30, by = "ID") %>%
  left_join(acc_groceries_30, by = "ID") %>%
  left_join(acc_speech_therapists_30, by = "ID") %>%
  left_join(acc_physiotherapists_30, by = "ID") %>%
  left_join(acc_dietitians_30, by = "ID") %>%
  left_join(acc_occupational_therapists_30, by = "ID") %>%
  left_join(acc_general_practitioners_30, by = "ID") %>%
  left_join(acc_hairdressers_30, by = "ID") %>%
  left_join(acc_restaurants_30, by = "ID") %>%
  left_join(acc_dentists_30, by = "ID") %>%
  left_join(acc_town_halls_30, by = "ID") %>%
  left_join(acc_local_service_centres_30, by = "ID") %>%
  left_join(acc_special_needs_education_30, by = "ID") %>%
  left_join(acc_crematoriums_30, by = "ID") %>%
  left_join(acc_public_transport_stops_30, by = "ID") %>%
  left_join(acc_nursing_homes_30, by = "ID") %>%
  left_join(acc_pharmacies_30, by = "ID") %>%
  left_join(acc_cemeteries_30, by = "ID")


# Writing csv
write.csv(origin_summary, "output_cumulative_accessibility_15_combined.csv", row.names = FALSE)

#6 pm
# -----------------------------------------
# 4) Basis parameters r5r
# -----------------------------------------
mode               <- c("WALK", "TRANSIT")
max_walk_time      <- 15     # minutes

cutoff_minutes     <- 30L    # 30 minutes to reach opportunities

departure_datetime <- as.POSIXct("18-01-2024 18:00:00", format="%d-%m-%Y %H:%M:%S") #typical Thursday
# -------------------------------------------------------
# 5) 30 minutes cumulative accessibility
#    via r5r::accessibility() with step-decay (cutoff=30)
# -------------------------------------------------------
# We calculate per origin the number of opportunities (destinations of goals)
# Accessible within 30 minutes PT+walking
#ids <- all_unique_origins$new_ID
#n   <- length(ids)

analysis_origins <- all_unique_origins


origins_analysis_df <- analysis_origins %>%
  transmute(
    id     = as.character(new_ID),  # alleen voor r5r
    new_ID = as.integer(new_ID),    # sleutel voor koppeling
    lon    = as.numeric(lon),
    lat    = as.numeric(lat)
  ) %>%
  distinct(new_ID, .keep_all = TRUE)

stopifnot(
  !anyDuplicated(origins_analysis_df$id),
  !anyNA(origins_analysis_df$lon),
  !anyNA(origins_analysis_df$lat)
)


#Hospitals

ttm_hospitals <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = hospitals_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Error hospitals: ", conditionMessage(e))
  }
)

time_col_hospitals <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_hospitals),
  value = TRUE
)[1]

if (is.na(time_col_hospitals)) {
  stop(
    "Geen reistijdkolom gevonden. Kolommen: ",
    paste(names(ttm_hospitals), collapse = ", ")
  )
}

acc_hospitals_30 <- ttm_hospitals %>%
  filter(
    !is.na(.data[[time_col_hospitals]]),
    .data[[time_col_hospitals]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    hospitals_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    hospitals_30min
  )

acc_hospitals_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_hospitals_30,
    by = "new_ID"
  ) %>%
  mutate(
    hospitals_30min = tidyr::replace_na(hospitals_30min, 0L)
  )

write.csv(acc_hospitals_30, "output_hospitals_30min_18_combined_NMBS_DeLijn.csv", row.names = FALSE)

rm(ttm_hospitals)
gc()
#Adult day care centres
ttm_adult_day_care_centres <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = adult_day_care_centres_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop(
      "Fout bij berekening dagverzorgingscentra: ",
      conditionMessage(e)
    )
  }
)

time_col_adult_day_care_centres <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_adult_day_care_centres),
  value = TRUE
)[1]

if (is.na(time_col_adult_day_care_centres)) {
  stop(
    "Geen reistijdkolom gevonden voor dagverzorgingscentra. Kolommen: ",
    paste(names(ttm_adult_day_care_centres), collapse = ", ")
  )
}

acc_adult_day_care_centres_30 <- ttm_adult_day_care_centres %>%
  filter(
    !is.na(.data[[time_col_adult_day_care_centres]]),
    .data[[time_col_adult_day_care_centres]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    adult_day_care_centres_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    adult_day_care_centres_30min
  )

acc_adult_day_care_centres_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_adult_day_care_centres_30,
    by = "new_ID"
  ) %>%
  mutate(
    adult_day_care_centres_30min = tidyr::replace_na(adult_day_care_centres_30min, 0L)
  )


write.csv(
  acc_adult_day_care_centres_30,
  "output_adult_day_care_centres_30min_18_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_adult_day_care_centres)
gc()


#Socio-cultural amenities
ttm_socio_cultural_activities <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = socio_cultural_activities_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening sociaal-culturele voorzieningen: ", conditionMessage(e))
  }
)

time_col_socio_cultural_activities <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_socio_cultural_activities),
  value = TRUE
)[1]

if (is.na(time_col_socio_cultural_activities)) {
  stop(
    "Geen reistijdkolom gevonden voor sociaal-culturele voorzieningen. Kolommen: ",
    paste(names(ttm_socio_cultural_activities), collapse = ", ")
  )
}

acc_socio_cultural_activities_30 <- ttm_socio_cultural_activities %>%
  filter(
    !is.na(.data[[time_col_socio_cultural_activities]]),
    .data[[time_col_socio_cultural_activities]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    socio_cultural_activities_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    socio_cultural_activities_30min
  )

acc_socio_cultural_activities_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_socio_cultural_activities_30,
    by = "new_ID"
  ) %>%
  mutate(
    socio_cultural_activities_30min = tidyr::replace_na(socio_cultural_activities_30min, 0L)
  )

write.csv(
  acc_socio_cultural_activities_30,
  "output_socio_cultural_act_30min_18_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_socio_cultural_activities)
gc()

#Groceries
ttm_groceries <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = groceries_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening voedingswinkels: ", conditionMessage(e))
  }
)

time_col_groceries <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_groceries),
  value = TRUE
)[1]

if (is.na(time_col_groceries)) {
  stop(
    "Geen reistijdkolom gevonden voor voedingswinkels. Kolommen: ",
    paste(names(ttm_groceries), collapse = ", ")
  )
}

acc_groceries_30 <- ttm_groceries %>%
  filter(
    !is.na(.data[[time_col_groceries]]),
    .data[[time_col_groceries]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    groceries_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    groceries_30min
  )

acc_groceries_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_groceries_30,
    by = "new_ID"
  ) %>%
  mutate(
    groceries_30min = tidyr::replace_na(groceries_30min, 0L)
  )


write.csv(
  acc_groceries_30,
  "output_groceries_30min_18_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_groceries)
gc()

#Speech therapists
ttm_speech_therapists <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = speech_therapists_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening logopedisten: ", conditionMessage(e))
  }
)

time_col_speech_therapists <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_speech_therapists),
  value = TRUE
)[1]

if (is.na(time_col_speech_therapists)) {
  stop(
    "Geen reistijdkolom gevonden voor logopedisten. Kolommen: ",
    paste(names(ttm_speech_therapists), collapse = ", ")
  )
}

acc_speech_therapists_30 <- ttm_speech_therapists %>%
  filter(
    !is.na(.data[[time_col_speech_therapists]]),
    .data[[time_col_speech_therapists]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    speech_therapists_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    speech_therapists_30min
  )

acc_speech_therapists_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_speech_therapists_30,
    by = "new_ID"
  ) %>%
  mutate(
    speech_therapists_30min = tidyr::replace_na(speech_therapists_30min, 0L)
  )

write.csv(
  acc_speech_therapists_30,
  "output_speech_therapists_30min_18_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_speech_therapists)
gc()


#Physiotherapists
ttm_physiotherapists <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = physiotherapists_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening kinesitherapeuten: ", conditionMessage(e))
  }
)

time_col_physiotherapists <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_physiotherapists),
  value = TRUE
)[1]

if (is.na(time_col_physiotherapists)) {
  stop(
    "Geen reistijdkolom gevonden voor kinesitherapeuten. Kolommen: ",
    paste(names(ttm_physiotherapists), collapse = ", ")
  )
}

acc_physiotherapists_30 <- ttm_physiotherapists %>%
  filter(
    !is.na(.data[[time_col_physiotherapists]]),
    .data[[time_col_physiotherapists]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    physiotherapists_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    physiotherapists_30min
  )

acc_physiotherapists_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_physiotherapists_30,
    by = "new_ID"
  ) %>%
  mutate(
    physiotherapists_30min = tidyr::replace_na(physiotherapists_30min, 0L)
  )


write.csv(
  acc_physiotherapists_30,
  "output_physiotherapists_30min_18_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_physiotherapists)
gc()

#Occupational therapists
ttm_occupational_therapists <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = occupational_therapists_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening ergotherapeuten: ", conditionMessage(e))
  }
)

time_col_occupational_therapists <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_occupational_therapists),
  value = TRUE
)[1]

if (is.na(time_col_occupational_therapists)) {
  stop(
    "Geen reistijdkolom gevonden voor ergotherapeuten. Kolommen: ",
    paste(names(ttm_occupational_therapists), collapse = ", ")
  )
}

acc_occupational_therapists_30 <- ttm_occupational_therapists %>%
  filter(
    !is.na(.data[[time_col_occupational_therapists]]),
    .data[[time_col_occupational_therapists]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    occupational_therapists_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    occupational_therapists_30min
  )

acc_occupational_therapists_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_occupational_therapists_30,
    by = "new_ID"
  ) %>%
  mutate(
    occupational_therapists_30min = tidyr::replace_na(occupational_therapists_30min, 0L)
  )


write.csv(
  acc_occupational_therapists_30,
  "output_occupational_therapists_30min_18_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_occupational_therapists)
gc()

#General practitioners 
ttm_general_practitioners <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = general_practitioners_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening huisartsen: ", conditionMessage(e))
  }
)

time_col_general_practitioners <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_general_practitioners),
  value = TRUE
)[1]

if (is.na(time_col_general_practitioners)) {
  stop(
    "Geen reistijdkolom gevonden voor huisartsen. Kolommen: ",
    paste(names(ttm_general_practitioners), collapse = ", ")
  )
}

aacc_general_practitioners_30 <- ttm_general_practitioners %>%
  filter(
    !is.na(.data[[time_col_general_practitioners]]),
    .data[[time_col_general_practitioners]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    general_practitioners_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    general_practitioners_30min
  )

acc_general_practitioners_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_general_practitioners_30,
    by = "new_ID"
  ) %>%
  mutate(
    general_practitioners_30min = tidyr::replace_na(general_practitioners_30min, 0L)
  )

write.csv(
  acc_general_practitioners_30,
  "output_GP_30min_18_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_general_practitioners)
gc()


#Hairdressers
ttm_hairdressers <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = hairdressers_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening kappers: ", conditionMessage(e))
  }
)

time_col_hairdressers <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_hairdressers),
  value = TRUE
)[1]

if (is.na(time_col_hairdressers)) {
  stop(
    "Geen reistijdkolom gevonden voor kappers. Kolommen: ",
    paste(names(ttm_hairdressers), collapse = ", ")
  )
}

acc_hairdressers_30 <- ttm_hairdressers %>%
  filter(
    !is.na(.data[[time_col_hairdressers]]),
    .data[[time_col_hairdressers]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    hairdressers_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    hairdressers_30min
  )

acc_hairdressers_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_hairdressers_30,
    by = "new_ID"
  ) %>%
  mutate(
    hairdressers_30min = tidyr::replace_na(hairdressers_30min, 0L)
  )

write.csv(
  acc_hairdressers_30,
  "output_hairdressers_30min_18_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_hairdressers)
gc()

#Restaurants
ttm_restaurants <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = restaurants_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening restaurants: ", conditionMessage(e))
  }
)

time_col_restaurants <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_restaurants),
  value = TRUE
)[1]

if (is.na(time_col_restaurants)) {
  stop(
    "Geen reistijdkolom gevonden voor restaurants. Kolommen: ",
    paste(names(ttm_restaurants), collapse = ", ")
  )
}

acc_restaurants_30 <- ttm_restaurants %>%
  filter(
    !is.na(.data[[time_col_restaurants]]),
    .data[[time_col_restaurants]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    restaurants_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    restaurants_30min
  )

acc_restaurants_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_restaurants_30,
    by = "new_ID"
  ) %>%
  mutate(
    restaurants_30min = tidyr::replace_na(restaurants_30min, 0L)
  )


write.csv(
  acc_restaurants_30,
  "output_restaurants_30min_18_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_restaurants)
gc()


#Dentists
ttm_dentists <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = dentists_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening tandartsen: ", conditionMessage(e))
  }
)

time_col_dentists <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_dentists),
  value = TRUE
)[1]

if (is.na(time_col_dentists)) {
  stop(
    "Geen reistijdkolom gevonden voor tandartsen. Kolommen: ",
    paste(names(ttm_dentists), collapse = ", ")
  )
}

acc_dentists_30 <- ttm_dentists %>%
  filter(
    !is.na(.data[[time_col_dentists]]),
    .data[[time_col_dentists]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    dentists_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    dentists_30min
  )

acc_dentists_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_dentists_30,
    by = "new_ID"
  ) %>%
  mutate(
    dentists_30min = tidyr::replace_na(dentists_30min, 0L)
  )


write.csv(
  acc_dentists_30,
  "output_dentists_30min_18_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_dentists)
gc()


#Town halls
ttm_town_halls <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = town_halls_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening gemeentehuizen: ", conditionMessage(e))
  }
)

time_col_town_halls <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_town_halls),
  value = TRUE
)[1]

if (is.na(time_col_town_halls)) {
  stop(
    "Geen reistijdkolom gevonden voor gemeentehuizen. Kolommen: ",
    paste(names(ttm_town_halls), collapse = ", ")
  )
}

acc_town_halls_30 <- ttm_town_halls %>%
  filter(
    !is.na(.data[[time_col_town_halls]]),
    .data[[time_col_town_halls]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    town_halls_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    town_halls_30min
  )

acc_town_halls_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_town_halls_30,
    by = "new_ID"
  ) %>%
  mutate(
    town_halls_30min = tidyr::replace_na(town_halls_30min, 0L)
  )

write.csv(
  acc_town_halls_30,
  "output_town_halls_30min_18_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_town_halls)
gc()


#Local service centres
ttm_local_service_centres <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = local_service_centres_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening lokale dienstencentra: ", conditionMessage(e))
  }
)

time_col_local_service_centres <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_local_service_centres),
  value = TRUE
)[1]

if (is.na(time_col_local_service_centres)) {
  stop(
    "Geen reistijdkolom gevonden voor lokale dienstencentra. Kolommen: ",
    paste(names(ttm_local_service_centres), collapse = ", ")
  )
}

acc_local_service_centres_30 <- ttm_local_service_centres %>%
  filter(
    !is.na(.data[[time_col_local_service_centres]]),
    .data[[time_col_local_service_centres]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    local_service_centres_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    local_service_centres_30min
  )

acc_local_service_centres_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_local_service_centres_30,
    by = "new_ID"
  ) %>%
  mutate(
    local_service_centres_30min = tidyr::replace_na(local_service_centres_30min, 0L)
  )

write.csv(
  acc_local_service_centres_30,
  "output_local_service_centres_30min_18_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_local_service_centres)
gc()

#Special needs education
ttm_special_needs_education <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = special_needs_education_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening buitengewoon onderwijs: ", conditionMessage(e))
  }
)

time_col_special_needs_education <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_special_needs_education),
  value = TRUE
)[1]

if (is.na(time_col_special_needs_education)) {
  stop(
    "Geen reistijdkolom gevonden voor buitengewoon onderwijs. Kolommen: ",
    paste(names(ttm_special_needs_education), collapse = ", ")
  )
}

acc_special_needs_education_30 <- ttm_special_needs_education %>%
  filter(
    !is.na(.data[[time_col_special_needs_education]]),
    .data[[time_col_special_needs_education]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    special_needs_education_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    special_needs_education_30min
  )

acc_special_needs_education_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_special_needs_education_30,
    by = "new_ID"
  ) %>%
  mutate(
    special_needs_education_30min = tidyr::replace_na(special_needs_education_30min, 0L)
  )

write.csv(
  acc_special_needs_education_30,
  "output_special_needs_education_30min_18_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_special_needs_education)
gc()
#Crematoriums 
ttm_crematoriums <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = crematoriums_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening crematoria: ", conditionMessage(e))
  }
)

time_col_crematoriums <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_crematoriums),
  value = TRUE
)[1]

if (is.na(time_col_crematoriums)) {
  stop(
    "Geen reistijdkolom gevonden voor crematoria. Kolommen: ",
    paste(names(ttm_crematoriums), collapse = ", ")
  )
}

acc_crematoriums_30 <- ttm_crematoriums %>%
  filter(
    !is.na(.data[[time_col_crematoriums]]),
    .data[[time_col_crematoriums]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    crematoriums_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    crematoriums_30min
  )

acc_crematoriums_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_crematoriums_30,
    by = "new_ID"
  ) %>%
  mutate(
    crematoriums_30min = tidyr::replace_na(crematoriums_30min, 0L)
  )

write.csv(
  acc_crematoriums_30,
  "output_crematoriums_30min_18_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_crematoriums)
gc()


#Public transport stops
ttm_public_transport_stops <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = public_transport_stops_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening haltes openbaar vervoer: ", conditionMessage(e))
  }
)

time_col_public_transport_stops <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_public_transport_stops),
  value = TRUE
)[1]

if (is.na(time_col_public_transport_stops)) {
  stop(
    "Geen reistijdkolom gevonden voor haltes openbaar vervoer. Kolommen: ",
    paste(names(ttm_public_transport_stops), collapse = ", ")
  )
}

acc_public_transport_stops_30 <- ttm_public_transport_stops %>%
  filter(
    !is.na(.data[[time_col_public_transport_stops]]),
    .data[[time_col_public_transport_stops]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    public_transport_stops_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    public_transport_stops_30min
  )

acc_public_transport_stops_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_public_transport_stops_30,
    by = "new_ID"
  ) %>%
  mutate(
    public_transport_stops_30min = tidyr::replace_na(public_transport_stops_30min, 0L)
  )

write.csv(
  acc_public_transport_stops_30,
  "output_PT_stops_30min_18_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_public_transport_stops)
gc()


#Nursing homes
ttm_nursing_homes <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = nursing_homes_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening woonzorgcentra: ", conditionMessage(e))
  }
)

time_col_nursing_homes <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_nursing_homes),
  value = TRUE
)[1]

if (is.na(time_col_nursing_homes)) {
  stop(
    "Geen reistijdkolom gevonden voor woonzorgcentra. Kolommen: ",
    paste(names(ttm_nursing_homes), collapse = ", ")
  )
}

acc_nursing_homes_30 <- ttm_nursing_homes %>%
  filter(
    !is.na(.data[[time_col_nursing_homes]]),
    .data[[time_col_nursing_homes]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    nursing_homes_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    nursing_homes_30min
  )

acc_nursing_homes_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_nursing_homes_30,
    by = "new_ID"
  ) %>%
  mutate(
    nursing_homes_30min = tidyr::replace_na(nursing_homes_30min, 0L)
  )

write.csv(
  acc_nursing_homes_30,
  "output_nursing_homes_30min_18_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_nursing_homes)
gc()


#Pharmacies
ttm_pharmacies <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = pharmacies_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening apotheken: ", conditionMessage(e))
  }
)

time_col_pharmacies <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_pharmacies),
  value = TRUE
)[1]

if (is.na(time_col_pharmacies)) {
  stop(
    "Geen reistijdkolom gevonden voor apotheken. Kolommen: ",
    paste(names(ttm_pharmacies), collapse = ", ")
  )
}

acc_pharmacies_30 <- ttm_pharmacies %>%
  filter(
    !is.na(.data[[time_col_pharmacies]]),
    .data[[time_col_pharmacies]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    pharmacies_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    pharmacies_30min
  )

acc_pharmacies_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_pharmacies_30,
    by = "new_ID"
  ) %>%
  mutate(
    pharmacies_30min = tidyr::replace_na(pharmacies_30min, 0L)
  )

write.csv(
  acc_pharmacies_30,
  "output_pharmacies_30min_18_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_pharmacies)
gc()


#Cemeteries
ttm_cemeteries <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = cemeteries_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening begraafplaatsen: ", conditionMessage(e))
  }
)

time_col_cemeteries <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_cemeteries),
  value = TRUE
)[1]

if (is.na(time_col_cemeteries)) {
  stop(
    "Geen reistijdkolom gevonden voor begraafplaatsen. Kolommen: ",
    paste(names(ttm_cemeteries), collapse = ", ")
  )
}

acc_cemeteries_30 <- ttm_cemeteries %>%
  filter(
    !is.na(.data[[time_col_cemeteries]]),
    .data[[time_col_cemeteries]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    cemeteries_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    cemeteries_30min
  )

acc_cemeteries_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_cemeteries_30,
    by = "new_ID"
  ) %>%
  mutate(
    cemeteries_30min = tidyr::replace_na(cemeteries_30min, 0L)
  )


write.csv(
  acc_cemeteries_30,
  "output_cemeteries_30min_18_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_cemeteries)
gc()


#Schools
ttm_schools <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = schools_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening scholen: ", conditionMessage(e))
  }
)

time_col_schools <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_schools),
  value = TRUE
)[1]

if (is.na(time_col_schools)) {
  stop(
    "Geen reistijdkolom gevonden voor scholen. Kolommen: ",
    paste(names(ttm_schools), collapse = ", ")
  )
}

acc_schools_30 <- ttm_schools %>%
  filter(
    !is.na(.data[[time_col_schools]]),
    .data[[time_col_schools]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    schools_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    schools_30min
  )

acc_schools_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_schools_30,
    by = "new_ID"
  ) %>%
  mutate(
    schools_30min = tidyr::replace_na(schools_30min, 0L)
  )


write.csv(
  acc_schools_30,
  "output_schools_30min_18_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_schools)
gc()


#Dieticians
ttm_dietitians <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = dietitians_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening diëtisten: ", conditionMessage(e))
  }
)

time_col_dietitians <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_dietitians),
  value = TRUE
)[1]

if (is.na(time_col_dietitians)) {
  stop(
    "Geen reistijdkolom gevonden voor diëtisten. Kolommen: ",
    paste(names(ttm_dietitians), collapse = ", ")
  )
}

acc_dietitians_30 <- ttm_dietitians %>%
  filter(
    !is.na(.data[[time_col_dietitians]]),
    .data[[time_col_dietitians]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    dietitians_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    dietitians_30min
  )

acc_dietitians_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_dietitians_30,
    by = "new_ID"
  ) %>%
  mutate(
    dietitians_30min = tidyr::replace_na(dietitians_30min, 0L)
  )

write.csv(
  acc_dietitians_30,
  "output_dietitians_30min_18_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_dietitians)
gc()

# -----------------------------------------
# 6) Joining results and writing csv
# -----------------------------------------

# Join to origins
analysis_origins <- analysis_origins %>%
  select(-ID) %>%
  rename(ID = new_ID)

origin_summary <- analysis_origins %>%
  select(ID, Trip_ID, lon, lat) %>%
  left_join(acc_schools_30, by = "ID") %>%
  left_join(acc_hospitals_30, by = "ID") %>%
  left_join(acc_adult_day_care_centres_30, by = "ID") %>%
  left_join(acc_socio_cultural_activities_30, by = "ID") %>%
  left_join(acc_groceries_30, by = "ID") %>%
  left_join(acc_speech_therapists_30, by = "ID") %>%
  left_join(acc_physiotherapists_30, by = "ID") %>%
  left_join(acc_dietitians_30, by = "ID") %>%
  left_join(acc_occupational_therapists_30, by = "ID") %>%
  left_join(acc_general_practitioners_30, by = "ID") %>%
  left_join(acc_hairdressers_30, by = "ID") %>%
  left_join(acc_restaurants_30, by = "ID") %>%
  left_join(acc_dentists_30, by = "ID") %>%
  left_join(acc_town_halls_30, by = "ID") %>%
  left_join(acc_local_service_centres_30, by = "ID") %>%
  left_join(acc_special_needs_education_30, by = "ID") %>%
  left_join(acc_crematoriums_30, by = "ID") %>%
  left_join(acc_public_transport_stops_30, by = "ID") %>%
  left_join(acc_nursing_homes_30, by = "ID") %>%
  left_join(acc_pharmacies_30, by = "ID") %>%
  left_join(acc_cemeteries_30, by = "ID")


# Writing csv
write.csv(origin_summary, "output_cumulative_accessibility_18_combined.csv", row.names = FALSE)

#9 pm
# -----------------------------------------
# 4) Basis parameters r5r
# -----------------------------------------
mode               <- c("WALK", "TRANSIT")
max_walk_time      <- 15     # minutes

cutoff_minutes     <- 30L    # 30 minutes to reach opportunities

departure_datetime <- as.POSIXct("18-01-2024 21:00:00", format="%d-%m-%Y %H:%M:%S") #typical Thursday
# -------------------------------------------------------
# 5) 30 minutes cumulative accessibility
#    via r5r::accessibility() with step-decay (cutoff=30)
# -------------------------------------------------------
# We calculate per origin the number of opportunities (destinations of goals)
# Accessible within 30 minutes PT+walking
#ids <- all_unique_origins$new_ID
#n   <- length(ids)

analysis_origins <- all_unique_origins



origins_analysis_df <- analysis_origins %>%
  transmute(
    id     = as.character(new_ID),  # alleen voor r5r
    new_ID = as.integer(new_ID),    # sleutel voor koppeling
    lon    = as.numeric(lon),
    lat    = as.numeric(lat)
  ) %>%
  distinct(new_ID, .keep_all = TRUE)

stopifnot(
  !anyDuplicated(origins_analysis_df$id),
  !anyNA(origins_analysis_df$lon),
  !anyNA(origins_analysis_df$lat)
)


#Hospitals

ttm_hospitals <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = hospitals_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Error hospitals: ", conditionMessage(e))
  }
)

time_col_hospitals <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_hospitals),
  value = TRUE
)[1]

if (is.na(time_col_hospitals)) {
  stop(
    "Geen reistijdkolom gevonden. Kolommen: ",
    paste(names(ttm_hospitals), collapse = ", ")
  )
}

acc_hospitals_30 <- ttm_hospitals %>%
  filter(
    !is.na(.data[[time_col_hospitals]]),
    .data[[time_col_hospitals]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    hospitals_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    hospitals_30min
  )

acc_hospitals_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_hospitals_30,
    by = "new_ID"
  ) %>%
  mutate(
    hospitals_30min = tidyr::replace_na(hospitals_30min, 0L)
  )

write.csv(acc_hospitals_30, "output_hospitals_30min_21_combined_NMBS_DeLijn.csv", row.names = FALSE)

rm(ttm_hospitals)
gc()
#Adult day care centres
ttm_adult_day_care_centres <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = adult_day_care_centres_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop(
      "Fout bij berekening dagverzorgingscentra: ",
      conditionMessage(e)
    )
  }
)

time_col_adult_day_care_centres <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_adult_day_care_centres),
  value = TRUE
)[1]

if (is.na(time_col_adult_day_care_centres)) {
  stop(
    "Geen reistijdkolom gevonden voor dagverzorgingscentra. Kolommen: ",
    paste(names(ttm_adult_day_care_centres), collapse = ", ")
  )
}

acc_adult_day_care_centres_30 <- ttm_adult_day_care_centres %>%
  filter(
    !is.na(.data[[time_col_adult_day_care_centres]]),
    .data[[time_col_adult_day_care_centres]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    adult_day_care_centres_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    adult_day_care_centres_30min
  )

acc_adult_day_care_centres_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_adult_day_care_centres_30,
    by = "new_ID"
  ) %>%
  mutate(
    adult_day_care_centres_30min = tidyr::replace_na(adult_day_care_centres_30min, 0L)
  )


write.csv(
  acc_adult_day_care_centres_30,
  "output_adult_day_care_centres_30min_21_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_adult_day_care_centres)
gc()


#Socio-cultural amenities
ttm_socio_cultural_activities <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = socio_cultural_activities_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening sociaal-culturele voorzieningen: ", conditionMessage(e))
  }
)

time_col_socio_cultural_activities <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_socio_cultural_activities),
  value = TRUE
)[1]

if (is.na(time_col_socio_cultural_activities)) {
  stop(
    "Geen reistijdkolom gevonden voor sociaal-culturele voorzieningen. Kolommen: ",
    paste(names(ttm_socio_cultural_activities), collapse = ", ")
  )
}

acc_socio_cultural_activities_30 <- ttm_socio_cultural_activities %>%
  filter(
    !is.na(.data[[time_col_socio_cultural_activities]]),
    .data[[time_col_socio_cultural_activities]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    socio_cultural_activities_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    socio_cultural_activities_30min
  )

acc_socio_cultural_activities_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_socio_cultural_activities_30,
    by = "new_ID"
  ) %>%
  mutate(
    socio_cultural_activities_30min = tidyr::replace_na(socio_cultural_activities_30min, 0L)
  )

write.csv(
  acc_socio_cultural_activities_30,
  "output_socio_cultural_act_30min_21_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_socio_cultural_activities)
gc()

#Groceries
ttm_groceries <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = groceries_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening voedingswinkels: ", conditionMessage(e))
  }
)

time_col_groceries <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_groceries),
  value = TRUE
)[1]

if (is.na(time_col_groceries)) {
  stop(
    "Geen reistijdkolom gevonden voor voedingswinkels. Kolommen: ",
    paste(names(ttm_groceries), collapse = ", ")
  )
}

acc_groceries_30 <- ttm_groceries %>%
  filter(
    !is.na(.data[[time_col_groceries]]),
    .data[[time_col_groceries]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    groceries_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    groceries_30min
  )

acc_groceries_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_groceries_30,
    by = "new_ID"
  ) %>%
  mutate(
    groceries_30min = tidyr::replace_na(groceries_30min, 0L)
  )

write.csv(
  acc_groceries_30,
  "output_groceries_30min_21_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_groceries)
gc()

#Speech therapists
ttm_speech_therapists <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = speech_therapists_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening logopedisten: ", conditionMessage(e))
  }
)

time_col_speech_therapists <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_speech_therapists),
  value = TRUE
)[1]

if (is.na(time_col_speech_therapists)) {
  stop(
    "Geen reistijdkolom gevonden voor logopedisten. Kolommen: ",
    paste(names(ttm_speech_therapists), collapse = ", ")
  )
}

acc_speech_therapists_30 <- ttm_speech_therapists %>%
  filter(
    !is.na(.data[[time_col_speech_therapists]]),
    .data[[time_col_speech_therapists]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    speech_therapists_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    speech_therapists_30min
  )

acc_speech_therapists_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_speech_therapists_30,
    by = "new_ID"
  ) %>%
  mutate(
    speech_therapists_30min = tidyr::replace_na(speech_therapists_30min, 0L)
  )
write.csv(
  acc_speech_therapists_30,
  "output_speech_therapists_30min_21_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_speech_therapists)
gc()


#Physiotherapists
ttm_physiotherapists <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = physiotherapists_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening kinesitherapeuten: ", conditionMessage(e))
  }
)

time_col_physiotherapists <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_physiotherapists),
  value = TRUE
)[1]

if (is.na(time_col_physiotherapists)) {
  stop(
    "Geen reistijdkolom gevonden voor kinesitherapeuten. Kolommen: ",
    paste(names(ttm_physiotherapists), collapse = ", ")
  )
}

acc_physiotherapists_30 <- ttm_physiotherapists %>%
  filter(
    !is.na(.data[[time_col_physiotherapists]]),
    .data[[time_col_physiotherapists]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    physiotherapists_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    physiotherapists_30min
  )

acc_physiotherapists_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_physiotherapists_30,
    by = "new_ID"
  ) %>%
  mutate(
    physiotherapists_30min = tidyr::replace_na(physiotherapists_30min, 0L)
  )



write.csv(
  acc_physiotherapists_30,
  "output_physiotherapists_30min_21_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_physiotherapists)
gc()

#Occupational therapists
ttm_occupational_therapists <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = occupational_therapists_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening ergotherapeuten: ", conditionMessage(e))
  }
)

time_col_occupational_therapists <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_occupational_therapists),
  value = TRUE
)[1]

if (is.na(time_col_occupational_therapists)) {
  stop(
    "Geen reistijdkolom gevonden voor ergotherapeuten. Kolommen: ",
    paste(names(ttm_occupational_therapists), collapse = ", ")
  )
}

acc_occupational_therapists_30 <- ttm_occupational_therapists %>%
  filter(
    !is.na(.data[[time_col_occupational_therapists]]),
    .data[[time_col_occupational_therapists]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    occupational_therapists_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    occupational_therapists_30min
  )

acc_occupational_therapists_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_occupational_therapists_30,
    by = "new_ID"
  ) %>%
  mutate(
    occupational_therapists_30min = tidyr::replace_na(occupational_therapists_30min, 0L)
  )

write.csv(
  acc_occupational_therapists_30,
  "output_occupational_therapists_30min_21_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_occupational_therapists)
gc()

#General practitioners 
ttm_general_practitioners <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = general_practitioners_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening huisartsen: ", conditionMessage(e))
  }
)

time_col_general_practitioners <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_general_practitioners),
  value = TRUE
)[1]

if (is.na(time_col_general_practitioners)) {
  stop(
    "Geen reistijdkolom gevonden voor huisartsen. Kolommen: ",
    paste(names(ttm_general_practitioners), collapse = ", ")
  )
}

acc_general_practitioners_30 <- ttm_general_practitioners %>%
  filter(
    !is.na(.data[[time_col_general_practitioners]]),
    .data[[time_col_general_practitioners]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    general_practitioners_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    general_practitioners_30min
  )

acc_general_practitioners_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_general_practitioners_30,
    by = "new_ID"
  ) %>%
  mutate(
    general_practitioners_30min = tidyr::replace_na(general_practitioners_30min, 0L)
  )

write.csv(
  acc_general_practitioners_30,
  "output_GP_30min_21_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_general_practitioners)
gc()


#Hairdressers
ttm_hairdressers <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = hairdressers_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening kappers: ", conditionMessage(e))
  }
)

time_col_hairdressers <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_hairdressers),
  value = TRUE
)[1]

if (is.na(time_col_hairdressers)) {
  stop(
    "Geen reistijdkolom gevonden voor kappers. Kolommen: ",
    paste(names(ttm_hairdressers), collapse = ", ")
  )
}

acc_hairdressers_30 <- ttm_hairdressers %>%
  filter(
    !is.na(.data[[time_col_hairdressers]]),
    .data[[time_col_hairdressers]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    hairdressers_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    hairdressers_30min
  )

acc_hairdressers_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_hairdressers_30,
    by = "new_ID"
  ) %>%
  mutate(
    hairdressers_30min = tidyr::replace_na(hairdressers_30min, 0L)
  )

write.csv(
  acc_hairdressers_30,
  "output_hairdressers_30min_21_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_hairdressers)
gc()

#Restaurants
ttm_restaurants <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = restaurants_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening restaurants: ", conditionMessage(e))
  }
)

time_col_restaurants <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_restaurants),
  value = TRUE
)[1]

if (is.na(time_col_restaurants)) {
  stop(
    "Geen reistijdkolom gevonden voor restaurants. Kolommen: ",
    paste(names(ttm_restaurants), collapse = ", ")
  )
}

acc_restaurants_30 <- ttm_restaurants %>%
  filter(
    !is.na(.data[[time_col_restaurants]]),
    .data[[time_col_restaurants]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    restaurants_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    restaurants_30min
  )

acc_restaurants_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_restaurants_30,
    by = "new_ID"
  ) %>%
  mutate(
    restaurants_30min = tidyr::replace_na(restaurants_30min, 0L)
  )


write.csv(
  acc_restaurants_30,
  "output_restaurants_30min_21_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_restaurants)
gc()


#Dentists
ttm_dentists <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = dentists_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening tandartsen: ", conditionMessage(e))
  }
)

time_col_dentists <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_dentists),
  value = TRUE
)[1]

if (is.na(time_col_dentists)) {
  stop(
    "Geen reistijdkolom gevonden voor tandartsen. Kolommen: ",
    paste(names(ttm_dentists), collapse = ", ")
  )
}

acc_dentists_30 <- ttm_dentists %>%
  filter(
    !is.na(.data[[time_col_dentists]]),
    .data[[time_col_dentists]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    dentists_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    dentists_30min
  )

acc_dentists_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_dentists_30,
    by = "new_ID"
  ) %>%
  mutate(
    dentists_30min = tidyr::replace_na(dentists_30min, 0L)
  )


write.csv(
  acc_dentists_30,
  "output_dentists_30min_21_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_dentists)
gc()


#Town halls
ttm_town_halls <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = town_halls_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening gemeentehuizen: ", conditionMessage(e))
  }
)

time_col_town_halls <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_town_halls),
  value = TRUE
)[1]

if (is.na(time_col_town_halls)) {
  stop(
    "Geen reistijdkolom gevonden voor gemeentehuizen. Kolommen: ",
    paste(names(ttm_town_halls), collapse = ", ")
  )
}

acc_town_halls_30 <- ttm_town_halls %>%
  filter(
    !is.na(.data[[time_col_town_halls]]),
    .data[[time_col_town_halls]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    town_halls_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    town_halls_30min
  )

acc_town_halls_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_town_halls_30,
    by = "new_ID"
  ) %>%
  mutate(
    town_halls_30min = tidyr::replace_na(town_halls_30min, 0L)
  )

write.csv(
  acc_town_halls_30,
  "output_town_halls_30min_21_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_town_halls)
gc()


#Local service centres
ttm_local_service_centres <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = local_service_centres_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening lokale dienstencentra: ", conditionMessage(e))
  }
)

time_col_local_service_centres <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_local_service_centres),
  value = TRUE
)[1]

if (is.na(time_col_local_service_centres)) {
  stop(
    "Geen reistijdkolom gevonden voor lokale dienstencentra. Kolommen: ",
    paste(names(ttm_local_service_centres), collapse = ", ")
  )
}

acc_local_service_centres_30 <- ttm_local_service_centres %>%
  filter(
    !is.na(.data[[time_col_local_service_centres]]),
    .data[[time_col_local_service_centres]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    local_service_centres_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    local_service_centres_30min
  )

acc_local_service_centres_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_local_service_centres_30,
    by = "new_ID"
  ) %>%
  mutate(
    local_service_centres_30min = tidyr::replace_na(local_service_centres_30min, 0L)
  )
write.csv(
  acc_local_service_centres_30,
  "output_local_service_centres_30min_21_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_local_service_centres)
gc()

#Special needs education
ttm_special_needs_education <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = special_needs_education_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening buitengewoon onderwijs: ", conditionMessage(e))
  }
)

time_col_special_needs_education <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_special_needs_education),
  value = TRUE
)[1]

if (is.na(time_col_special_needs_education)) {
  stop(
    "Geen reistijdkolom gevonden voor buitengewoon onderwijs. Kolommen: ",
    paste(names(ttm_special_needs_education), collapse = ", ")
  )
}

acc_special_needs_education_30 <- ttm_special_needs_education %>%
  filter(
    !is.na(.data[[time_col_special_needs_education]]),
    .data[[time_col_special_needs_education]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    special_needs_education_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    special_needs_education_30min
  )

acc_special_needs_education_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_special_needs_education_30,
    by = "new_ID"
  ) %>%
  mutate(
    special_needs_education_30min = tidyr::replace_na(special_needs_education_30min, 0L)
  )

write.csv(
  acc_special_needs_education_30,
  "output_special_needs_education_30min_21_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_special_needs_education)
gc()
#Crematoriums 
ttm_crematoriums <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = crematoriums_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening crematoria: ", conditionMessage(e))
  }
)

time_col_crematoriums <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_crematoriums),
  value = TRUE
)[1]

if (is.na(time_col_crematoriums)) {
  stop(
    "Geen reistijdkolom gevonden voor crematoria. Kolommen: ",
    paste(names(ttm_crematoriums), collapse = ", ")
  )
}

acc_crematoriums_30 <- ttm_crematoriums %>%
  filter(
    !is.na(.data[[time_col_crematoriums]]),
    .data[[time_col_crematoriums]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    crematoriums_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    crematoriums_30min
  )

acc_crematoriums_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_crematoriums_30,
    by = "new_ID"
  ) %>%
  mutate(
    crematoriums_30min = tidyr::replace_na(crematoriums_30min, 0L)
  )

write.csv(
  acc_crematoriums_30,
  "output_crematoriums_30min_21_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_crematoriums)
gc()


#Public transport stops
ttm_public_transport_stops <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = public_transport_stops_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening haltes openbaar vervoer: ", conditionMessage(e))
  }
)

time_col_public_transport_stops <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_public_transport_stops),
  value = TRUE
)[1]

if (is.na(time_col_public_transport_stops)) {
  stop(
    "Geen reistijdkolom gevonden voor haltes openbaar vervoer. Kolommen: ",
    paste(names(ttm_public_transport_stops), collapse = ", ")
  )
}

acc_public_transport_stops_30 <- ttm_public_transport_stops %>%
  filter(
    !is.na(.data[[time_col_public_transport_stops]]),
    .data[[time_col_public_transport_stops]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    public_transport_stops_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    public_transport_stops_30min
  )

acc_public_transport_stops_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_public_transport_stops_30,
    by = "new_ID"
  ) %>%
  mutate(
    public_transport_stops_30min = tidyr::replace_na(public_transport_stops_30min, 0L)
  )

write.csv(
  acc_public_transport_stops_30,
  "output_PT_stops_30min_21_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_public_transport_stops)
gc()


#Nursing homes
ttm_nursing_homes <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = nursing_homes_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening woonzorgcentra: ", conditionMessage(e))
  }
)

time_col_nursing_homes <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_nursing_homes),
  value = TRUE
)[1]

if (is.na(time_col_nursing_homes)) {
  stop(
    "Geen reistijdkolom gevonden voor woonzorgcentra. Kolommen: ",
    paste(names(ttm_nursing_homes), collapse = ", ")
  )
}

acc_nursing_homes_30 <- ttm_nursing_homes %>%
  filter(
    !is.na(.data[[time_col_nursing_homes]]),
    .data[[time_col_nursing_homes]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    nursing_homes_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    nursing_homes_30min
  )

acc_nursing_homes_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_nursing_homes_30,
    by = "new_ID"
  ) %>%
  mutate(
    nursing_homes_30min = tidyr::replace_na(nursing_homes_30min, 0L)
  )

write.csv(
  acc_nursing_homes_30,
  "output_nursing_homes_30min_21_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_nursing_homes)
gc()


#Pharmacies
ttm_pharmacies <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = pharmacies_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening apotheken: ", conditionMessage(e))
  }
)

time_col_pharmacies <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_pharmacies),
  value = TRUE
)[1]

if (is.na(time_col_pharmacies)) {
  stop(
    "Geen reistijdkolom gevonden voor apotheken. Kolommen: ",
    paste(names(ttm_pharmacies), collapse = ", ")
  )
}

acc_pharmacies_30 <- ttm_pharmacies %>%
  filter(
    !is.na(.data[[time_col_pharmacies]]),
    .data[[time_col_pharmacies]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    pharmacies_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    pharmacies_30min
  )

acc_pharmacies_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_pharmacies_30,
    by = "new_ID"
  ) %>%
  mutate(
    pharmacies_30min = tidyr::replace_na(pharmacies_30min, 0L)
  )
write.csv(
  acc_pharmacies_30,
  "output_pharmacies_30min_21_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_pharmacies)
gc()


#Cemeteries
ttm_cemeteries <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = cemeteries_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening begraafplaatsen: ", conditionMessage(e))
  }
)

time_col_cemeteries <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_cemeteries),
  value = TRUE
)[1]

if (is.na(time_col_cemeteries)) {
  stop(
    "Geen reistijdkolom gevonden voor begraafplaatsen. Kolommen: ",
    paste(names(ttm_cemeteries), collapse = ", ")
  )
}

acc_cemeteries_30 <- ttm_cemeteries %>%
  filter(
    !is.na(.data[[time_col_cemeteries]]),
    .data[[time_col_cemeteries]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    cemeteries_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    cemeteries_30min
  )

acc_cemeteries_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_cemeteries_30,
    by = "new_ID"
  ) %>%
  mutate(
    cemeteries_30min = tidyr::replace_na(cemeteries_30min, 0L)
  )


write.csv(
  acc_cemeteries_30,
  "output_cemeteries_30min_21_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_cemeteries)
gc()


#Schools
ttm_schools <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = schools_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening scholen: ", conditionMessage(e))
  }
)

time_col_schools <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_schools),
  value = TRUE
)[1]

if (is.na(time_col_schools)) {
  stop(
    "Geen reistijdkolom gevonden voor scholen. Kolommen: ",
    paste(names(ttm_schools), collapse = ", ")
  )
}

acc_schools_30 <- ttm_schools %>%
  filter(
    !is.na(.data[[time_col_schools]]),
    .data[[time_col_schools]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    schools_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    schools_30min
  )

acc_schools_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_schools_30,
    by = "new_ID"
  ) %>%
  mutate(
    schools_30min = tidyr::replace_na(schools_30min, 0L)
  )


write.csv(
  acc_schools_30,
  "output_schools_30min_21_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_schools)
gc()


#Dieticians
ttm_dietitians <- tryCatch(
  {
    r5r::travel_time_matrix(
      r5r_network        = r5r_network_2024,
      origins            = origins_analysis_df,
      destinations       = dietitians_df[, c("id", "lon", "lat")],
      mode               = mode,
      departure_datetime = departure_datetime,
      max_walk_time      = max_walk_time,
      max_trip_duration  = cutoff_minutes,
      progress           = TRUE
    )
  },
  error = function(e) {
    stop("Fout bij berekening diëtisten: ", conditionMessage(e))
  }
)

time_col_dietitians <- grep(
  "^travel_time$|^travel_time_p[0-9]+$",
  names(ttm_dietitians),
  value = TRUE
)[1]

if (is.na(time_col_dietitians)) {
  stop(
    "Geen reistijdkolom gevonden voor diëtisten. Kolommen: ",
    paste(names(ttm_dietitians), collapse = ", ")
  )
}

acc_dietitians_30 <- ttm_dietitians %>%
  filter(
    !is.na(.data[[time_col_dietitians]]),
    .data[[time_col_dietitians]] <= cutoff_minutes
  ) %>%
  group_by(from_id) %>%
  summarise(
    dietitians_30min = n_distinct(to_id),
    .groups = "drop"
  ) %>%
  transmute(
    new_ID = as.integer(from_id),
    dietitians_30min
  )

acc_dietitians_30 <- origins_analysis_df %>%
  transmute(
    new_ID = as.integer(new_ID)
  ) %>%
  distinct(new_ID) %>%
  left_join(
    acc_dietitians_30,
    by = "new_ID"
  ) %>%
  mutate(
    dietitians_30min = tidyr::replace_na(dietitians_30min, 0L)
  )

write.csv(
  acc_dietitians_30,
  "output_dietitians_30min_21_combined_NMBS_DeLijn.csv",
  row.names = FALSE
)

rm(ttm_dietitians)
gc()

# -----------------------------------------
# 6) Joining results and writing csv
# -----------------------------------------

# Join to origins
analysis_origins <- analysis_origins %>%
  select(-ID) %>%
  rename(ID = new_ID)

origin_summary <- analysis_origins %>%
  select(ID, Trip_ID, lon, lat) %>%
  left_join(acc_schools_30, by = "ID") %>%
  left_join(acc_hospitals_30, by = "ID") %>%
  left_join(acc_adult_day_care_centres_30, by = "ID") %>%
  left_join(acc_socio_cultural_activities_30, by = "ID") %>%
  left_join(acc_groceries_30, by = "ID") %>%
  left_join(acc_speech_therapists_30, by = "ID") %>%
  left_join(acc_physiotherapists_30, by = "ID") %>%
  left_join(acc_dietitians_30, by = "ID") %>%
  left_join(acc_occupational_therapists_30, by = "ID") %>%
  left_join(acc_general_practitioners_30, by = "ID") %>%
  left_join(acc_hairdressers_30, by = "ID") %>%
  left_join(acc_restaurants_30, by = "ID") %>%
  left_join(acc_dentists_30, by = "ID") %>%
  left_join(acc_town_halls_30, by = "ID") %>%
  left_join(acc_local_service_centres_30, by = "ID") %>%
  left_join(acc_special_needs_education_30, by = "ID") %>%
  left_join(acc_crematoriums_30, by = "ID") %>%
  left_join(acc_public_transport_stops_30, by = "ID") %>%
  left_join(acc_nursing_homes_30, by = "ID") %>%
  left_join(acc_pharmacies_30, by = "ID") %>%
  left_join(acc_cemeteries_30, by = "ID")


# Writing csv
write.csv(origin_summary, "output_cumulative_accessibility_21_combined.csv", row.names = FALSE)


