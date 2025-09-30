library(sf)
library(tools)
library(tmap)
library(tidyverse)

csv_to_geojson_line <- function(csv_path,
                                out_dir = "~/projects/annapurna_circuit/data/garmin/routes",
                                lon_col = "lon",
                                lat_col = "lat") {
  
  df <- read_csv(csv_path, show_col_types = FALSE)
  coords <- as.matrix(df[, c(lon_col, lat_col)])
  geom <- st_sfc(st_linestring(coords), crs = 4326)
  ln   <- st_sf(name = file_path_sans_ext(basename(csv_path)), geometry = geom)
  
  out_file <- file.path(path.expand(out_dir),
                        paste0(file_path_sans_ext(basename(csv_path)), ".geojson"))
  
  st_write(ln, out_file, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
  out_file
}

csv_to_geojson_line("data/garmin/day02.csv")


# Batch convert a folder of CSVs
# batch_csvs_to_geojson <- function(csv_dir,
#                                   out_dir = "~/projects/annapurna_circuit/data/garmin/routes",
#                                   pattern = "\\.csv$",
#                                   ...) {
#   csvs <- list.files(csv_dir, pattern = pattern, full.names = TRUE)
#   vapply(csvs, function(p) csv_to_geojson_line(p, out_dir = out_dir, ...),
#          FUN.VALUE = character(1))
# }


# CREATING ROUTES ---------------------------------------------------------

files <- list.files(
  "~/projects/annapurna_circuit/data/garmin/",
  pattern = "\\.csv$",
  full.names = TRUE
)


walk(
  files,
  ~ csv_to_geojson_line(.x, out_dir = "~/projects/annapurna_circuit/data/garmin/routes")
)


# test plot --------------------------------------------------------------

example <- st_read('data/garmin/routes/annapurna_circuit.geojson')


tmap_mode("view")
tm_basemap("OpenStreetMap") + tm_shape(example) + tm_lines(lwd = 4)
