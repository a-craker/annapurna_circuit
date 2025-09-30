library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)
library(sf)
library(readr)
library(stringr)

annapurna_circuit <- read.csv("data/garmin/annapurna_circuit.csv") %>% 
  as_tibble()

# DAY SPLITTING -----------------------------------------------------------

days_list <- split(annapurna_circuit, annapurna_circuit$day_local)

annapurna_circuit %>% 
  group_by(day_local) %>% 
  nest(data = everything()) %>% unnest()

day_keys <- sort(unique(annapurna_circuit$day_local))
day_labels <- sprintf("day%02d", seq_along(day_keys))
day_map <- tibble(day_local = day_keys, day_label = day_labels)

days_list <- map(day_keys, ~ filter(annapurna_circuit, day_local == .x))
names(days_list) <- day_labels

# saving daily files
iwalk(days_list, ~ write_csv(.x, file.path("data/garmin/", paste0(.y, ".csv"))))


# SPATIAL -----------------------------------------------------------------



# ---- 4) Daily stats table + save ----

day_stats <- rec_all %>%
  group_by(day_local) %>%
  summarise(
    dist_km       = (max(dist_m, na.rm=TRUE) - min(dist_m, na.rm=TRUE)) / 1000,
    moving_h      = sum(dt_s[moving], na.rm = TRUE) / 3600,
    elapsed_h     = as.numeric(max(timestamp) - min(timestamp), units = "hours"),
    elev_gain_m   = sum(pmax(dz_m, 0) * (abs(dz_m) > 0.5), na.rm = TRUE),
    elev_loss_m   = sum(pmax(-dz_m, 0) * (abs(dz_m) > 0.5), na.rm = TRUE),
    avg_moving_kmh= ifelse(moving_h > 0, dist_km / moving_h, NA_real_),
    med_hr        = median(heart_rate, na.rm = TRUE),
    med_cad       = median(cadence + fractional_cadence, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(day_map, by = "day_local") %>%
  relocate(day_label, .before = 1)

write_rds(day_stats, file.path(data_dir, "route_day_stats.rds"))
write_csv(day_stats, file.path(data_dir, "route_day_stats.csv"))