library(tidyverse)
library(giscoR)
library(terra)
library(elevatr)

# Custom functions -----

# Load country geometry data in SF format
get_sf <- function(crs, ...) {
  country_sf <- giscoR::gisco_get_countries(...)
  country_transformed <- sf::st_transform(country_sf, crs = crs)
  return(country_transformed)
}

# Get elevation data for a given area
get_elevation_data <- function(...) {
  country_elevation <- elevatr::get_elev_raster(...)
  country_elevation_df <- as.data.frame(country_elevation, xy = TRUE) %>%
    terra::na.omit()
  colnames(country_elevation_df)[3] <- "elevation"
  return(country_elevation_df)
}

# Background map -----

# set CRS
wgs84 <- "+proj=longlat +datum=WGS84 +no_defs"

# Download geometry for French Polynesia
# french_polynesia_sf <- get_sf(
#   crs = wgs84, # WGS84
#   year = "2020",
#   epsg = "4326",
#   resolution = "01", # highest resolution
#   country = "PYF" # ISO3 code for French Polynesia
# )

# OR, load in from data downloaded from https://www.diva-gis.org/
# https://biogeo.ucdavis.edu/data/diva/adm/PYF_adm.zip
# much more accurate for Moorea than get_sf()
french_polynesia_sf <- sf::read_sf("data/PYF_adm/PYF_adm1.shp")

# Crop geometry to Moorea
moorea_sf <- st_crop(
  french_polynesia_sf,
  y = c(
    xmin = -149.7,
    xmax = -150.0,
    ymin = -17.6,
    ymax = -17.4
  )
)

# Get elevation data for Moorea
moorea_elevation <- get_elevation_data(
  locations = moorea_sf,
  z = 11, # maximum resolution is 14
  clip = "locations"
)

# C apii specimens -----

# Read in records of C. apiifolia exported from specimens database
c_apii_specimens <- read_csv("data/specimens_capii_moorea.csv") %>%
  select(latitude, longitude, is_gametophyte) %>%
  filter(!is.na(latitude), !is.na(longitude), !is.na(is_gametophyte)) %>%
  rename(x = longitude, y = latitude) %>%
  mutate(is_gametophyte = as.logical(is_gametophyte))

# Map ----

ggplot(mapping = aes(x = x, y = y)) +
  geom_raster(data = moorea_elevation, aes(fill = elevation)) +
  geom_point(
    data = c_apii_specimens,
    aes(shape = is_gametophyte),
    size = 4.5) +
  scale_fill_viridis_c(
    name = "標高 (m)"
  ) +
  scale_shape_manual(
    name = "ライフステージ",
    values = c(
      "TRUE" = 1,
      "FALSE" = 3
    ),
    labels = c(
      "配偶体",
      "胞子体"
    )
  ) +
  coord_sf(crs = wgs84) +
  theme_void(base_family = "HiraKakuPro-W3", base_size = 20)

ggsave("moorea_c_apii.png")
