library(lubridate)
library(tidyverse)

circuit <- read_csv("data/garmin/annapurna_circuit.csv")

daily_stats <- circuit %>%
  filter(moving == TRUE) %>% 
  arrange(day_local, timestamp) %>%
  group_by(day_local) %>%
  # Row-wise deltas with sensible fallbacks
  mutate(
    # altitude diff from smoothed series; zero out obvious GPS spikes (>30 m between points)
    alt_diff_raw = c(0, diff(elev_smooth)),
    alt_diff = if_else(abs(alt_diff_raw) > 30, 0, alt_diff_raw),
    
    # distance step (prefer d_m; fall back to diff of dist_m)
    d_step = if_else(!is.na(d_m), d_m,
                     replace_na(c(0, diff(dist_m)), 0)),
    
    # time step (prefer dt_s; fall back to diff(timestamp))
    dt_step = if_else(!is.na(dt_s), dt_s,
                      as.numeric(replace_na(c(NA, diff(timestamp)), 0), units = "secs")),
    
    # moving flag fallback (if missing): treat >0.5 m/s as moving
    is_moving = if_else(is.na(moving), spd_ms > 0.5, moving)
  ) %>%
  summarise(
    start_time        = first(timestamp),
    finish_time       = last(timestamp),
    
    elapsed_time_s    = as.numeric(difftime(last(timestamp), first(timestamp), units = "secs")),
    moving_time_s     = sum(if_else(is_moving, dt_step, 0), na.rm = TRUE),
    
    # distance
    distance_m        = (max(dist_m, na.rm = TRUE) - min(dist_m, na.rm = TRUE)),
    distance_moving_m = sum(if_else(is_moving, d_step, 0), na.rm = TRUE),
    
    # elevation
    elev_gain_m       = sum(pmax(alt_diff, 0), na.rm = TRUE),
    elev_loss_m       = sum(pmax(-alt_diff, 0), na.rm = TRUE),
    
    # heart rate & altitude
    avg_hr_bpm        = mean(heart_rate[is_moving], na.rm = TRUE),
    max_hr_bpm        = max(heart_rate, na.rm = TRUE),
    avg_alt_m         = mean(elev_smooth, na.rm = TRUE),
    min_alt_m         = min(elev_smooth, na.rm = TRUE),
    max_alt_m         = max(elev_smooth, na.rm = TRUE),
    
    # speed/pace
    max_speed_ms      = max(spd_ms, na.rm = TRUE),
    avg_speed_ms      = distance_moving_m / moving_time_s,
    pace_min_per_km   = 16.6667 / (distance_moving_m / moving_time_s),  # = (1000/speed)/60
    
    # efficiency
    pct_moving        = moving_time_s / elapsed_time_s,
    vam_m_per_h       = if_else(moving_time_s > 0, elev_gain_m * 3600 / moving_time_s, NA_real_)
  ) %>%
  ungroup() %>% 
  select(day_local, distance_m, elev_gain_m, elev_loss_m, avg_alt_m, moving_time_s)


daily_stats %>% 
  mutate(
    moving_period = seconds_to_period(moving_time_s),
    hrs  = hour(moving_period) + 24 * day(moving_period),
    mins = minute(moving_period),
    moving_hm = sprintf("%d:%02d", hrs, mins)
  )


# speed and gradient -------------------------------------------------------------------------


circuit %>%  mutate(alt_bin = cut(altitude, breaks = seq(min(altitude), max(altitude), by=250))) %>% 
  ggplot(aes(alt_bin, 1000/spd_ms)) + geom_boxplot() +
  labs(y="Pace min/km", x="Altitude bins")

c_gain <- circuit %>% 
  group_by(day_local)  %>% 
  mutate(dh = altitude - lag(altitude, default = first(altitude)),
         gain = pmax(dh,0), loss = -pmin(dh,0)) %>% 
  summarise(gain_m = sum(gain, na.rm=TRUE), loss_m = sum(loss, na.rm=TRUE))
ggplot(c_gain, aes(day_local, gain_m)) + geom_col() + labs(y="Gain m")



# -------------------------------------------------------------------------

library(tidyverse)
library(ggridges)
library(lubridate)
library(hms)
library(scales)

circuit <- circuit %>%
  mutate(
    timestamp_fixed = timestamp + hours(5) + minutes(40)   # shift all rows by +5h
  ) 

# Prep: time-of-day and weights
ridge_df <- circuit %>%
  mutate(
    tod = as_hms(timestamp_fixed),              
    w = pmax(d_m, 0)
  ) %>%
  filter(moving, !is.na(tod), w > 0)

day_labels <- c(
  "2025-09-14" = "Day 1",
  "2025-09-15" = "Day 2",
  "2025-09-16" = "Day 3",
  "2025-09-17" = "Day 4",
  "2025-09-18" = "Day 5",
  "2025-09-19" = "Day 6",
  "2025-09-20" = "Day 7",
  "2025-09-21" = "Day 8",
  "2025-09-22" = "Day 9",
  "2025-09-23" = "Day 10",
  "2025-09-24" = "Day 11"
)

ridgeplot <- ggplot(ridge_df, aes(x = tod, y = factor(day_local), weight = w)) +
    stat_density_ridges(
      scale = 2, rel_min_height = 0.01, alpha = 0.8, fill = "#fbe16d", color = "#4a2740",
      kernel = "gaussian"
    ) +
    scale_x_time(
      breaks = hms::hms(hours = seq(4, 20, by = 2)),
      labels = time_format("%H:%M")
    ) +
    scale_y_discrete(labels = day_labels) +
    labs(
      x = NULL,
      y = NULL
      # title = "When Distance Was Covered Each Day",
      # colour = "white"
    ) +
    theme_ridges() +
    theme(
      plot.background  = element_rect(fill = "#4a2740", colour = NA),
      panel.background = element_rect(fill = "#4a2740", colour = NA),
      panel.grid.major = element_line(colour = "white", size = 0.2, linetype = "solid"),
      panel.grid.minor = element_line(colour = "white", size = 0.1, linetype = "solid"),
      axis.text.x      = element_text(colour = "white", margin = margin(t = 5), size = 12),
      axis.text.y      = element_text(colour = "white", margin = margin(r = 5), size = 12),
      plot.title       = element_text(hjust = 0.5, colour = "white", size = 16, face = "bold"),
      legend.position  = "none"
    )
  
  
ggsave("output/plots/time_ridgeplot.png", ridgeplot, width = 12, height = 6, dpi = 300)
  
  
ggsave(
  "output/plots/time_ridgeplot.png",
  ridgeplot,
  width = 6, height = 3, dpi = 300
)
  

# AVERAGE TIME PER DAY ----------------------------------------------------

circuit %>%
  mutate(day = coalesce(day_local, as_date(timestamp), as_date(timestamp))) %>%
  group_by(day) %>%
  summarise(
    start = min(coalesce(timestamp, timestamp), na.rm = TRUE),
    end   = max(coalesce(timestamp, timestamp), na.rm = TRUE),
    span_s = as.numeric(difftime(end, start, units = "secs")),
    span   = seconds_to_period(span_s),
    .groups = "drop"
  )

