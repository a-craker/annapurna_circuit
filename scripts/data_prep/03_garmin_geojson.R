library(sf)
library(tools)
library(tmap)
library(tidyverse)

# EXCLUDES ELEVATION AS Z
# csv_to_geojson_line <- function(csv_path,
#                                 out_dir = "~/projects/annapurna_circuit/data/garmin/routes",
#                                 lon_col = "lon",
#                                 lat_col = "lat") {
#   
#   df <- read_csv(csv_path, show_col_types = FALSE)
#   coords <- as.matrix(df[, c(lon_col, lat_col)])
#   geom <- st_sfc(st_linestring(coords), crs = 4326)
#   ln   <- st_sf(name = file_path_sans_ext(basename(csv_path)), geometry = geom)
#   
#   out_file <- file.path(path.expand(out_dir),
#                         paste0(file_path_sans_ext(basename(csv_path)), ".geojson"))
#   
#   st_write(ln, out_file, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
#   out_file
# }

# INCLUDES ELEVATION AS Z
csv_to_geojson_line <- function(csv_path,
                                out_dir = "~/projects/annapurna_circuit/data/garmin/routes",
                                lon_col = "lon",
                                lat_col = "lat",
                                elev_col = "elev_smooth") {
  
  df <- read_csv(csv_path, show_col_types = FALSE)
  
  coords <- as.matrix(df[, c(lon_col, lat_col, elev_col)])
  
  geom <- st_sfc(st_linestring(coords, dim = "XYZ"), crs = 4326)
  ln   <- st_sf(name = file_path_sans_ext(basename(csv_path)), geometry = geom)
  
  out_file <- file.path(path.expand(out_dir),
                        paste0(file_path_sans_ext(basename(csv_path)), ".geojson"))
  
  st_write(ln, out_file, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
  out_file
}


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

example <- st_read('data/garmin/routes/day11.geojson')


tmap_mode("view")
tm_basemap("OpenStreetMap") + tm_shape(example) + tm_lines(lwd = 4)



