library(lubridate)
library(scales)
library(tidyverse)

circuit <- read_csv("data/garmin/annapurna_circuit.csv")

  # village markers  
markers <- circuit %>% group_by(day_local) %>% 
    summarise(max = max(dist_m)/1000,
              min = min(dist_m)/1000,
              travelled = (max - min)/1000) %>% pull(max)
  
  


  plot_data <- circuit %>%
    mutate(
      km = dist_m / 1000,
      m  = elev_smooth,
      .keep = "none"
    ) %>%
    arrange(km) %>%
    # optional light decimation to speed plotting, keep shape
    group_by(grp = floor(km * 10) / 10) %>%
    summarise(km = first(km), m = first(m), .groups = "drop")
  

  # axis ranges and labels
  xmin <- floor(min(plot_data$km) / 10) * 10
  xmax <- ceiling(max(plot_data$km) / 10) * 10
  ymin <- floor(min(plot_data$m)  / 200) * 200
  ymax <- ceiling(max(plot_data$m) / 200) * 200
  
  xbreaks <- seq(xmin, xmax, by = 10)
  ybreaks <- seq(ymin, ymax, by = 200)

  lab_km <- function(x) paste0(label_number(accuracy = 1)(x), "KM")
  lab_m  <- function(x) paste0(label_number(accuracy = 1)(x), "M")
  
# PLOT --------------------------------------------------------------------

  
  # ---- plot ----
  # ggplot(plot_data, aes(km, m)) +
  #   geom_area(fill = "#fbe16d", colour = NA) +
  #   geom_vline(xintercept = markers, linetype = "22", linewidth = 0.4) +
  #   scale_x_continuous(breaks = xbreaks, labels = lab_km, expand = expansion(mult = c(0, 0))) +
  #   scale_y_continuous(breaks = ybreaks, labels = lab_m,  expand = expansion(mult = c(0, 0.02))) +
  #   coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  #   labs(x = NULL, y = NULL) +
  #   theme(
  #     panel.grid = element_blank(),
  #     axis.text.x = element_text(margin = margin(t = 5), size = 16),
  #     axis.text.y = element_text(margin = margin(r = 5), size = 16),
  #     base_size = 16,
  #     plot.margin  = margin(10, 20, 10, 20)
  #   )
  
  
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


# ggsave(
#   filename = "output/plots/circuit_elevation.png",
#   plot     = elevation,
#   device   = ragg::agg_png,
#   width    = 1600, height = 600, units = "px",
#   dpi      = 120,
#   bg       = "#4a2740"
# )


ggsave(
  filename = "output/plots/circuit_elevation.png",
  plot     = elevation,
  device   = ragg::agg_png,
  width    = 1800, height = 900, units = "px",
  dpi      = 120,
  bg       = "#4a2740"
)
