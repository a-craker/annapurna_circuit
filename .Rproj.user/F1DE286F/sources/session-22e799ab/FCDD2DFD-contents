library(lubridate)
library(glue)
library(scales)
library(tidyverse)



create_elevation <- function(df, markers) {
  
  directory <- glue("output/plots/elevation_{deparse(substitute(df))}.png")
  
  df <- df %>% 
    mutate(dist_m = dist_m - first(dist_m))
  
  plot_data <- df %>%
    mutate(
      km = dist_m / 1000,
      m  = elev_smooth,
      .keep = "none"
    ) %>%
    arrange(km) %>%
    # optional light decimation to speed plotting, keep shape
    group_by(grp = floor(km * 10) / 10) %>%
    summarise(km = first(km), m = first(m), .groups = "drop")
  
  xmin <- floor(min(plot_data$km) / 10) * 10
  xmax <- ceiling(max(plot_data$km) / 2) * 2
  ymin <- floor(min(plot_data$m)  / 200) * 200
  ymax <- ceiling(max(plot_data$m) / 200) * 200
  
  xbreaks <- seq(xmin, xmax, by = 2)
  ybreaks <- seq(ymin, ymax, by = 200)
  
  lab_km <- function(x) paste0(label_number(accuracy = 1)(x), "KM")
  lab_m  <- function(x) paste0(label_number(accuracy = 1)(x), "M")
  
  elevation <- ggplot(plot_data, aes(km, m)) +
    geom_area(fill = "#fbe16d", colour = NA) +                     # visible profile
    geom_vline(xintercept = markers, linetype = "22", linewidth = 1, colour = "#4a2740") +
    scale_x_continuous(breaks = xbreaks, labels = lab_km,
                       expand = expansion(mult = c(0, 0))) +
    scale_y_continuous(breaks = ybreaks, labels = lab_m,
                       expand = expansion(mult = c(0, 0.02))) +
    coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
    labs(x = NULL, y = NULL) +
    theme_classic(base_size = 16) +
    theme(
      plot.background  = element_rect(fill = "#4a2740", colour = NA),
      panel.background = element_rect(fill = "#4a2740", colour = NA),
      axis.text.x      = element_text(colour = "white", margin = margin(t = 5), size = 12),
      axis.text.y      = element_text(colour = "white", margin = margin(r = 5), size = 12),
      axis.ticks       = element_line(colour = "white"),
      axis.line        = element_line(colour = "white"),
      plot.margin      = margin(10, 20, 10, 20)
    )
  
  ggsave(
    filename = directory,
    plot     = elevation,
    device   = ragg::agg_png,
    width    = 1800, height = 900, units = "px",
    dpi      = 120,
    bg       = "#4a2740"
  )
  
}

# DAY 1: Dharaphani to Chame
# Timang (2750m)
day1 <- read_csv("data/garmin/day01.csv")
markers <- c(4.21, 10.1)
create_elevation(day1, markers)

# DAY2: Chame to Upper Pisang
# Dhukur Pokhari (3240m) 
day2 <- read_csv("data/garmin/day02.csv")
markers <- c(11.2)
create_elevation(day2, markers)

# DAY3: Upper Pisang to Ghyaru 
# Ghyaru (3730m)
day3 <- read_csv("data/garmin/day03.csv")
markers <- c(5.42)
create_elevation(day3, markers)

# DAY4: Ghyaru to Manang
# Manang (3540m)
day4 <- read_csv("data/garmin/day04.csv")
markers <- c(29)
create_elevation(day4, markers)

# DAY5: Day Hike to Ice Lake
# Ice Lake (4600m)
day5 <- read_csv("data/garmin/day05.csv")
markers <- c(8.399)
create_elevation(day5, markers)

# DAY6: Manang to Tilicho Base Camp
# Shri Kharka (4080m)
day6 <- read_csv("data/garmin/day06.csv")
markers <- c(8.52)
create_elevation(day6, markers)

# DAY7: Day Hike to Tilicho Lake
# Tilicho Lake (5080m)
day7 <- read_csv("data/garmin/day07.csv")
markers <- c(6.1)
create_elevation(day7, markers)

# DAY8: Tilicho Base Camp to Yak Kharka 
# Churilatter Suspension Bridge altitude (3850m)
day8 <- read_csv("data/garmin/day08.csv")
markers <- c(12.72)
create_elevation(day8, markers)

# DAY9: Yak Kharka to Thorung Phedi 
# Thorong Phedi (4450m)
day9 <- read_csv("data/garmin/day09.csv")
markers <- c(29)
create_elevation(day9, markers)

# DAY10: Thorung Phedi to Muktinath
# Thorong la Pass (5416m)
# Phedi Restaurant (4380)
day10 <- read_csv("data/garmin/day10.csv")
markers <- c(5.622, 11.65)
create_elevation(day10, markers)

# DAY11: Muktinath to Jomsom
# Kagbeni (2810m) 
day11 <- read_csv("data/garmin/day11.csv")
markers <- c(12.3)
create_elevation(day11, markers)
