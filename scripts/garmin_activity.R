# remotes::install_github("grimbough/FITfileR")
library(FITfileR)
library(purrr)
library(lubridate)
library(tidyverse)
library(sf)
library(zoo)

ff <- readFitFile("data/garmin/20485267039_ACTIVITY.fit")
rec_list <- records(ff)

# pick the record table with the most rows and a timestamp
cand <- vapply(rec_list, nrow, integer(1))
has_time <- vapply(rec_list, function(x) "timestamp" %in% names(x), logical(1))

rec <- rec_list[[ which.max(ifelse(has_time, cand, -Inf)) ]]


# DATA PREP ---------------------------------------------------------------

# define timezone
tz <- "Asia/Kathmandu"

tz_local <- "Asia/Kathmandu"

annapurna_circuit <- rec %>%
  arrange(timestamp) %>%
  slice(-(1:7)) %>%
  mutate(
    timestamp = with_tz(timestamp, tzone = tz),
    lat = if_else(round(position_lat,  1) %in% c(180, -180), NA_real_, position_lat),
    lon = if_else(round(position_long, 1) %in% c(180, -180), NA_real_, position_long),
    dist_m   = distance,
    spd_ms   = enhanced_speed,
    altitude = enhanced_altitude
  ) %>%
  filter(!is.na(lat), !is.na(lon)) %>%  # remove NA coords
  mutate(
    dt_s        = c(0, as.numeric(diff(timestamp), units = "secs")),
    d_m         = c(0, diff(dist_m)),
    moving      = (spd_ms > 0.5) | (coalesce(cadence + fractional_cadence, 0) > 0),
    elev_smooth = rollapply(altitude, 11, median, fill = "extend", align = "center"),
    day_local   = as_date(timestamp, tz = tz)
  )

write_csv(annapurna_circuit, "data/garmin/annapurna_circuit.csv")



  