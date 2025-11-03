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
  nest(data = everything()) %>% 
  nnest()

day_keys <- sort(unique(annapurna_circuit$day_local))
day_labels <- sprintf("day%02d", seq_along(day_keys))
day_map <- tibble(day_local = day_keys, day_label = day_labels)

days_list <- map(day_keys, ~ filter(annapurna_circuit, day_local == .x))
names(days_list) <- day_labels

# saving daily files
iwalk(days_list, ~ write_csv(.x, file.path("data/garmin/", paste0(.y, ".csv"))))



