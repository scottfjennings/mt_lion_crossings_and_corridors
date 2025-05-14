
# download and import a DEM (Digital Elevation Model) using the elevatr package, which pulls elevation data from AWS Terrain Tiles (based on USGS data), and works well for areas in the U.S. like California.This DEM is based on SRTM/USGS data hosted via AWS

# Install if needed
# install.packages(c("elevatr", "sf", "terra", "tigris"))

library(elevatr)
library(sf)
library(terra)
library(tigris)
library(tidyverse)
library(here)

options(tigris_use_cache = TRUE)

# Step 1: Get Sonoma and Napa county boundaries
counties_ca <- counties(state = "CA", cb = TRUE, class = "sf")
target_counties <- counties_ca %>%
  filter(NAME %in% c("Sonoma", "Napa"))

# Reproject to WGS84 (required by elevatr)
target_counties_wgs84 <- st_transform(target_counties, crs = 4326)

puma_hr <- st_read(here("data/shapefiles/combined_puma_homeranges_99.shp"))
puma_hr_wgs84 <- st_transform(puma_hr, crs = 4326)

# Step 2: Download DEM from AWS Terrain Tiles
dem <- get_elev_raster(
  locations = puma_hr_wgs84,
  z = 11,        # ~5m resolution; you can increase to 11–12 for finer detail
  clip = "locations"
)

# Step 3: Convert to SpatRaster (terra)
dem_terra <- rast(dem)

# Reproject DEM to UTM Zone 10N (EPSG:32610)
dem_utm <- project(dem_terra, "EPSG:32610")

# Save reprojected DEM
writeRaster(dem_utm, "sonoma_napa_dem_utm.tif", overwrite = TRUE)

# Plot to confirm
plot(dem_utm, main = "DEM Reprojected to UTM Zone 10N")
