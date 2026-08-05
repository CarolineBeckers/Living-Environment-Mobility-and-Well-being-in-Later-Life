#Time travel matrix PT with arrival time 
#arrival_travel_time_matrix function!
# =========================================================
# PT travel times 
# =========================================================

# Packages 
pkgs <- c(
  'r5r', 'accessibility', 'rJavaEnv', 'ggplot2', 'mapview',
  'quantreg', 'dplyr', 'h3jsr', 'sf', 'data.table'
)
# install.packages(pkgs)

# Sufficient memory for Java / R5
options(java.parameters = "-Xmx64G")

# Libraries 
library(r5r)
library(h3jsr)
library(dplyr)
library(mapview)
library(ggplot2)
library(sf)
library(data.table)
library(tidyverse)

# ----------------------------
# 1) Loading networks (R5R)
# ----------------------------
path_2022a <- system.file("extdata_Flanders_2022a", package = "r5r")
path_2022b <- system.file("extdata_Flanders_2022b", package = "r5r")
path_2023  <- system.file("extdata_Flanders_2023",  package = "r5r")
path_2024  <- system.file("extdata_Flanders_2024",  package = "r5r")

r5r_network_2022a <- build_network(data_path = path_2022a, overwrite = TRUE, verbose = TRUE)
r5r_network_2022b <- build_network(data_path = path_2022b, overwrite = TRUE, verbose = TRUE)
r5r_network_2023  <- build_network(data_path = path_2023,  overwrite = TRUE, verbose = TRUE)
r5r_network_2024  <- build_network(data_path = path_2024,  overwrite = TRUE, verbose = TRUE)

# -----------------------------------------
# 2) Origins & (pairwise) destinations
# -----------------------------------------
points_origin      <- read.csv(system.file("extdata_Flanders_2022a/Points_origin.csv",      package = "r5r"))
points_destination <- read.csv(system.file("extdata_Flanders_2022a/Points_destination.csv", package = "r5r"))

points_origin$ID      <- as.character(points_origin$ID)
points_destination$ID <- as.character(points_destination$ID)

#points_origin$Departure_datetime <- paste0(points_origin$Departure_datetime, ":00")
points_origin <- points_origin %>% separate(Arrival_datetime, into = c("date", "hour"), sep = " ", remove = FALSE)
points_origin <- points_origin %>% separate(date, into = c("day", "month", "year"), sep = "/", remove = FALSE)
# points_origin$day <- as.numeric(points_origin$day)
# points_origin$day <- ifelse(points_origin$day < 10, paste0("0", points_origin$day), points_origin$day)
# points_origin <- points_origin %>% separate(hour, into = c("hours", "minutes", "seconds"), sep = ":", remove = FALSE)
# points_origin$hours <- as.numeric(points_origin$hours)
# points_origin$hours <- ifelse(points_origin$hours < 10, paste0("0", points_origin$hours), points_origin$hours)
points_origin <- points_origin %>% mutate(Arrival_datetime = paste0(day, "-", month, "-", year, " ", hour))

points_origin$Arrival_datetime <- as.POSIXct(points_origin$Arrival_datetime,
                                             format = "%d-%m-%Y %H:%M:%S", tz = "Europe/Brussels")


# # Date - time format (POSIXct)
# if (!inherits(points_origin$Departure_datetime, "POSIXct")) {
#   points_origin <- points_origin %>%
#     mutate(Departure_datetime = as.POSIXct(Departure_datetime, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Brussels"))
# }

# Destinations lookup per ID
dest_by_id <- points_destination %>%
  select(ID, lon, lat) %>%
  as.data.frame()
row.names(dest_by_id) <- dest_by_id$ID

# -----------------------------------------
# 4) Supporting functions: choosing network & snapping GTFS date
# -----------------------------------------
#New column of chosen GTFS file

#dates of GTFS
svc_2022a_start <- as.Date("2022-05-01"); svc_2022a_end <- as.Date("2022-05-21")
svc_2022b_start <- as.Date("2022-09-16"); svc_2022b_end <- as.Date("2022-11-30")
svc_2023_start  <- as.Date("2023-03-07");  svc_2023_end  <- as.Date("2023-05-21")
svc_2024_start  <- as.Date("2024-01-15");  svc_2024_end  <- as.Date("2024-02-29")

#windows of GTFS
window_2022a_start <- as.Date("2022-01-01"); window_2022a_end <- as.Date("2022-09-15")
window_2022b_start <- as.Date("2022-09-16"); window_2022b_end <- as.Date("2023-03-06")
window_2023_start  <- as.Date("2023-03-07");  window_2023_end  <- as.Date("2024-01-14")
window_2024_start  <- as.Date("2024-01-15");  window_2024_end  <- as.Date("2024-12-31")

points_origin <- points_origin%>%
  mutate(date = paste0(year,"-",month,"-", day))
points_origin <- points_origin%>%
  mutate(date = as.Date(date))

points_origin <- points_origin %>%
  mutate(GTFS_network = ifelse(
    date <= window_2022a_end, "2022a",
    ifelse(
      date >= window_2022b_start & date <= window_2022b_end, "2022b",
      ifelse(
        date >= window_2023_start & date <= window_2023_end, "2023",
        "2024"
      )
    )
  ))

#New date

points_origin <- points_origin %>%
  mutate(
    svc_start = case_when(
      GTFS_network == "2022a"~ svc_2022a_start,
      GTFS_network == "2022b" ~ svc_2022b_start,
      GTFS_network == "2023" ~ svc_2023_start,
      GTFS_network == "2024" ~ svc_2024_start,
      TRUE ~ NA_Date_
    ),
    svc_end = case_when(
      GTFS_network == "2022a"~ svc_2022a_end,
      GTFS_network == "2022b" ~ svc_2022b_end,
      GTFS_network == "2023" ~ svc_2023_end,
      GTFS_network == "2024" ~ svc_2024_end,
      TRUE ~ NA_Date_
    )
  )

points_origin <- points_origin %>%
  mutate(
    date = as.Date(date),
    svc_start = as.Date(svc_start),
    svc_end = as.Date(svc_end)
  )

#snapping function
get_snapped_date <- function(d, s, e) { 
  # if date is NA
  if (is.na(d) | is.na(s) | is.na(e)) return(NA_Date_) 
  # if date in GTFS file
  if (d >= s & d <= e) return(d) 
  # week day of d and of svc_start (1 = monday, 7 = sunday) 
  wd_d <- as.integer(format(d, "%u")) 
  wd_s <- as.integer(format(s, "%u")) 
  
  # Offset to first same week day starting from svc_start 
  offset <- (wd_d - wd_s) %% 7 
  candidate <- s + offset 
  # if outside window 
  if (candidate > e) { 
    # Take last same week day 
    wd_e <- as.integer(format(e, "%u")) 
    offset_back <- (wd_e - wd_d) %% 7 
    candidate_last <- e - offset_back 
    if (candidate_last < s) { 
      return(NA_Date_)  # no good day in window 
    } else { 
      return(candidate_last) 
    } 
  } 
  return(candidate) 
} 

points_origin <- points_origin %>%
  rowwise() %>%
  mutate(
    new_date = get_snapped_date(date, svc_start, svc_end)
  ) %>%
  ungroup()

#Adding arrival time as well to the new date column
points_origin <- points_origin %>%
  mutate(new_arrival_datetime = paste0(new_date, " ", hour))

points_origin <- points_origin %>%
  mutate(new_arrival_datetime = as.POSIXct(new_arrival_datetime))


# -----------------------------------------
# 5) Basis parameters r5r
# -----------------------------------------
mode               <- c("WALK", "TRANSIT")
max_walk_time      <- 15     # minutes
max_trip_duration  <- 120    # minutes


# -----------------------------------------
# 6) Calculating travel time by PT
# -----------------------------------------
ids <- points_origin$ID
n   <- length(ids)

results_list <- vector("list", n)
results_list_det_it <- vector("list", n)
fail_log <- data.frame(ID = character(0), reason = character(0), stringsAsFactors = FALSE)

pb <- txtProgressBar(min = 0, max = n, style = 3)

for (i in seq_len(n)) {
  setTxtProgressBar(pb, i)
  this_id <- as.character(ids[i])
  
  ori_row <- points_origin[i, c("ID", "lon", "lat", "GTFS_network","new_arrival_datetime")]
  ar_dt  <- ori_row$new_arrival_datetime
  
  dest_row <- dest_by_id[this_id, c("ID", "lon", "lat"), drop = FALSE]
  
  origins_df <- data.frame(id = ori_row$ID, lon = ori_row$lon, lat = ori_row$lat)
  dest_df    <- data.frame(id = dest_row$ID, lon = dest_row$lon, lat = dest_row$lat)
  
  
  if (ori_row$GTFS_network == "2022a") { 
    net <- r5r_network_2022a 
  } else if (ori_row$GTFS_network == "2022b") { 
    net <- r5r_network_2022b 
  } else if (ori_row$GTFS_network == "2023") { 
    net <- r5r_network_2023 
  } else { 
    net <- r5r_network_2024
  }
  ttm_i <- tryCatch(
    {
      arrival_travel_time_matrix(
        r5r_network        = net,
        origins            = origins_df,
        destinations       = dest_df,
        mode               = mode,
        arrival_datetime   = ar_dt,
        max_walk_time      = max_walk_time,
        max_trip_duration  = max_trip_duration,
        progress           = FALSE
      )
    },
    error = function(e) e
  )
  
  
  if (inherits(ttm_i, "error")) {
    fail_log <- rbind(fail_log, data.frame(
      ID = this_id, reason = paste("r5r fout:", conditionMessage(ttm_i))
    ))
    next
  }
  
  ttm_i$ID <- this_id
  # ttm_i$Arrival_new_datetime  <- dep_dt
  results_list[[i]] <- ttm_i
}
close(pb)

ttm_all <- dplyr::bind_rows(results_list)

# Writing csv
write.csv(ttm_all, "output_travel_time_matrix_all_with_arrival.csv", row.names = FALSE)

