library(sf)
library(tidyverse)


# simplifying routes for 3D Map ------------------------------------------------------

annapurna_simple <- st_read("data/garmin/routes/annapurna_circuit.geojson", quiet = TRUE) %>%  
  st_zm(drop=TRUE) %>% 
  st_transform(crs = 3857) %>% 
  st_simplify(dTolerance = 2) %>% 
  st_transform(crs = 4326)

st_write(annapurna_simple["geometry"], "data/garmin/routes/simple/annapurna_simple.geojson",
         delete_dsn=TRUE, driver="GeoJSON",
         layer_options=c("COORDINATE_PRECISION=5"))


# adding village points -----------------------------------------------------------

circuit <- read_csv("data/garmin/annapurna_circuit.csv")

# village markers  
markers <- circuit %>% group_by(day_local) %>% 
  summarise(max = max(dist_m)/1000,
            min = min(dist_m)/1000,
            travelled = (max - min)/1000) %>% pull(max)

# create geometry points 
points <- circuit %>% 
  filter((distance/1000) %in% markers | 
           round(distance, 1) == 42.4) %>% 
  st_as_sf(
    coords= c("position_long", "position_lat"),
    crs = 4326
  ) %>% 
  mutate(distance_km = distance/1000,
         role = case_when(
           row_number() == 1 ~ "start",
           row_number() == n() ~ "end",
           TRUE ~ "mid"
           )
         ) %>% 
  select(timestamp, distance_km, enhanced_speed, enhanced_altitude,
         heart_rate, cadence, role)
  

annapurna_simple <- annapurna_simple %>%
  mutate(feature = "route", role = NA_character_) %>%
  select(feature, role, name, geometry)

# create feature collection of line string and points
annapurna_simple <- bind_rows(annapurna_simple, points)

st_write(annapurna_simple["geometry"], "data/garmin/simple_routes/annapurna_simple.geojson",
         delete_dsn=TRUE, driver="GeoJSON",
         layer_options=c("COORDINATE_PRECISION=5"))
