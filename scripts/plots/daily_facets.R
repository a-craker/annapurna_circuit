# displaying a hybrid satellite map: 
# https://maplibre.org/maplibre-gl-js/docs/examples/display-a-hybrid-satellite-map-with-terrain-elevation/


# PROCESS DATA ------------------------------------------------------------

process_geojson <- function(file) {
  g <- sf::st_read(file, quiet = TRUE)
  
  # keep only line-like geometries and normalise
  line_ix <- sf::st_geometry_type(g, by_geometry = TRUE) %in% c("LINESTRING", "MULTILINESTRING")
  g <- g[line_ix, , drop = FALSE]
  if (nrow(g) == 0) stop("No line features in: ", file)
  
  # cast to LINESTRINGs and ensure WGS84 lon/lat
  g <- sf::st_cast(g, "LINESTRING")
  if (is.na(sf::st_crs(g))) {
    sf::st_crs(g) <- 4326
  } else {
    g <- sf::st_transform(g, 4326)
  }
  
  # build per-feature point tibble
  rows <- lapply(seq_len(nrow(g)), function(i) {
    coords <- sf::st_coordinates(g[i, , drop = FALSE])
    if (nrow(coords) < 2) return(NULL)
    
    lon <- coords[, "X"]
    lat <- coords[, "Y"]
    
    # elevation from Z, if present
    ele <- if ("Z" %in% colnames(coords)) coords[, "Z"] else rep(NA_real_, length(lon))
    
    # distances in km between successive vertices
    d_m <- geosphere::distHaversine(
      cbind(dplyr::lag(lon, default = lon[1]),
            dplyr::lag(lat, default = lat[1])),
      cbind(lon, lat)
    )
    dist_to_prev <- c(0, as.numeric(d_m[-1])) / 1000
    cumdist <- cumsum(dist_to_prev)
    
    tibble::tibble(
      id = stringr::str_extract(basename(file), "\\d+") %>% as.integer(),
      file = basename(file),
      lon = lon,
      lat = lat,
      ele = ele,
      dist_to_prev = dist_to_prev,
      cumdist = cumdist
    )
  })
  
  out <- dplyr::bind_rows(rows)
  
  if (is.null(out) || nrow(out) == 0) stop("No valid line vertices in: ", file)
  
  # optional: warn once if Z was absent
  if (all(is.na(out$ele))) warning("No Z values found in geometries for: ", file)
  
  out
}


files <- list.files("data/garmin/routes/", full.names = TRUE) %>% 
  (function(x) x[basename(x) != "annapurna_circuit.geojson"])()

facet_data <- map_dfr(files, process_geojson)


# DISTANCE FACET -------------------------------------------------------------------------

plot_facets <- function(data, labels = FALSE, scales = "free", color = FALSE) {
  
  # Constants
  possible_values_scales<- c("free", "fixed")
  
  # Check if the scales argument is correct
  if (!(scales %in% possible_values_scales)) {
    stop("This argument value for `scales` is not available! Use 'free' or 'fixed' instead!")
  }
  
  # Check if the data is joined with activities.
  if (color) {
    if (sum(colnames(data) == "Activity.Type") == 0) {
      stop("The data frame does not contain 'Activity.Type' column. Load activities with process_activities function and run join_data_activities function first!")
    }
  }
  
  # Summarise data
  summary <- data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(lon = mean(range(lon)),
                     lat = mean(range(lat)),
                     distance = sprintf("%.1f", max(cumdist)))
  
  # Decide if tracks will all be scaled to similar size ("free") or if
  # track sizes reflect absolute distance in each dimension ("fixed")
  if (scales == "fixed") {
    data <- data %>%
      dplyr::group_by(id) %>% # for each track,
      dplyr::mutate(lon = lon - mean(lon), # centre data on zero so facets can
                    lat = lat - mean(lat)) # be plotted on same distance scale
  } else {
    scales = "free" # default, in case a non-valid option was specified
  }
  
  # Decide if plot is colored by activity type or not and create a plot
  if (color) {
    p <- ggplot2::ggplot() + ggplot2::geom_path(ggplot2::aes(lon, lat, group = id, color = Activity.Type), data, linewidth = 1.5, lineend = "round", alpha = 0.5) + ggplot2::scale_color_brewer(palette = "Dark2", name = NULL) # color by activity type
  } else {
    p <- ggplot2::ggplot() + ggplot2::geom_path(ggplot2::aes(lon, lat, group = id), data, linewidth = 1.5, lineend = "round")
  }
  
  p <- p + ggplot2::facet_wrap(~id, scales = scales) + ggplot2::theme_void() +
    ggplot2::theme(panel.spacing = ggplot2::unit(0, "lines"),
                   strip.background = ggplot2::element_blank(), strip.text = ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(rep(1, 4), "cm"),
                   legend.position = "bottom") # place a legend on the bottom of a plot
  
  if (scales == "fixed") {
    p <- p + ggplot2::coord_fixed() # make aspect ratio == 1
  }
  
  # Add labels
  if(labels) {
    p <- p +
      ggplot2::geom_text(ggplot2::aes(lon, lat, label = distance), data = summary,
                         alpha = 0.25, linewidth = 3)
  }
  
  # Return plot
  p
}


# revised for labels ------------------------------------------------------

plot_facets <- function(data, labels = FALSE, scales = "free", color = FALSE) {
  
  # Constants
  possible_values_scales <- c("free", "fixed")
  
  # Check if the scales argument is correct
  if (!(scales %in% possible_values_scales)) {
    stop("This argument value for `scales` is not available! Use 'free' or 'fixed' instead!")
  }
  
  # Check if the data is joined with activities.
  if (color) {
    if (sum(colnames(data) == "Activity.Type") == 0) {
      stop("The data frame does not contain 'Activity.Type' column. Load activities with process_activities function and run join_data_activities function first!")
    }
  }
  
  # Summarise data
  summary <- data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      lon = mean(range(lon)),
      lat = mean(range(lat)),
      distance = sprintf("%.1f", max(cumdist)),
      .groups = "drop"
    )
  
  # Decide if tracks will all be scaled to similar size ("free") or if
  # track sizes reflect absolute distance in each dimension ("fixed")
  if (scales == "fixed") {
    data <- data %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(
        lon = lon - mean(lon),
        lat = lat - mean(lat)
      ) %>%
      dplyr::ungroup()
  } else {
    scales <- "free"
  }
  
  # Base plot
  if (color) {
    p <- ggplot2::ggplot() +
      ggplot2::geom_path(
        ggplot2::aes(lon, lat, group = id, color = Activity.Type),
        data,
        linewidth = 1.5, lineend = "round", alpha = 0.5
      ) +
      ggplot2::scale_color_brewer(palette = "Dark2", name = NULL)
  } else {
    p <- ggplot2::ggplot() +
      ggplot2::geom_path(
        ggplot2::aes(lon, lat, group = id),
        data,
        linewidth = 1.5, lineend = "round"
      )
  }
  
  # If a 'labels' column exists, build an id -> label mapping for facet strips
  labeller_arg <- NULL
  if ("labels" %in% names(data)) {
    lab_map <- data %>%
      dplyr::distinct(id, labels) %>%
      dplyr::arrange(id) 
    labeller_arg <- ggplot2::as_labeller(stats::setNames(as.character(lab_map$labels), lab_map$id))
  }
  
  # Facets and theme
  p <- p +
    ggplot2::facet_wrap(
      ~ id,
      scales = scales,
      labeller = labeller_arg,
      strip.position = "top"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.spacing = ggplot2::unit(0, "lines"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "plain", size = 10),
      plot.margin = ggplot2::unit(rep(1, 4), "cm"),
      legend.position = "bottom"
    )
  
  if (scales == "fixed") {
    p <- p + ggplot2::coord_fixed()
  }
  
  # Optional in-plot labels for distance (kept as original behaviour)
  if (labels) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(lon, lat, label = distance),
        data = summary,
        alpha = 0.25, linewidth = 3
      )
  }
  
  p
}




# -------------------------------------------------------------------------



facet_data_labels <- facet_data %>% 
  mutate(labels = paste0("Day ", id))

daily_facets <- plot_facets(facet_data_labels)

ggsave("output/plots/daily_facets.png", daily_facets, width = 14, height = 10, dpi = 300)


# -------------------------------------------------------------------------

plot_elevations <- function(data, scale_free_y = FALSE) {
  # Compute total distance for each activity
  dist <- data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(dist = max(cumdist))
  
  # Normalise distance
  data <- data %>%
    dplyr::left_join(dist, by = "id") %>%
    dplyr::mutate(dist_scaled = cumdist / dist) %>%
    dplyr::arrange(id, cumdist)
  
  # Create plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(dist_scaled, ele, group = id), data, alpha = 0.75, linewidth = 1.5) +
    ggplot2::facet_wrap(~id, scales = ifelse(scale_free_y, "free_y", "fixed")) +
    ggplot2::theme_void() +
    ggplot2::theme(panel.spacing = ggplot2::unit(0, "lines"),
                   strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(rep(1, 4), "cm"))
  p
}


# ELEVATION AGGREGSIVE ----------------------------------------------------

plot_elevations <- function(data,
                            scale_free_y = TRUE,
                            vertical_exaggeration = 6,
                            panel_aspect = 0.7,
                            use_relative_elevation = FALSE) {
  library(dplyr); library(ggplot2)
  
  dist <- data %>% group_by(id) %>%
    summarise(dist = max(cumdist), .groups = "drop")
  
  df <- data %>%
    left_join(dist, by = "id") %>%
    mutate(dist_scaled = cumdist / dist) %>%
    group_by(id) %>%
    mutate(ele_plot = if (use_relative_elevation) ele - min(ele, na.rm = TRUE) else ele) %>%
    ungroup() %>%
    mutate(ele_exag = ele_plot * vertical_exaggeration)
  
  ggplot(df, aes(dist_scaled, ele_exag, group = id)) +
    geom_line(alpha = 0.75, linewidth = 1.5, lineend = "round", colour = "black") +
    ggplot2::scale_color_brewer(palette = "Dark2", name = NULL) +
    facet_wrap(~ id, scales = ifelse(scale_free_y, "free_y", "fixed")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
    theme_void() +
    theme(
      panel.spacing = unit(0, "lines"),
      strip.background = element_blank(),
      strip.text = element_blank(),
      plot.margin = unit(rep(0.6, 4), "cm"),
      aspect.ratio = panel_aspect
    )
}


# -------------------------------------------------------------------------



elevation_facets <- plot_elevations(facet_data)
ggsave("output/plots/elevation_facets2.png", elevation_facets, width = 14, height = 10, dpi = 300)


