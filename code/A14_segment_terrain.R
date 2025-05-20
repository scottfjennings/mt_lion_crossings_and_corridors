
# download and import a DEM (Digital Elevation Model) using the elevatr package, which pulls elevation data from AWS Terrain Tiles (based on USGS data), and works well for areas in the U.S. like California.This DEM is based on SRTM/USGS data hosted via AWS



# data citation info
citation("elevatr")

# Primary Data Source: AWS Terrain Tiles (from USGS NED/SRTM)
# The elevation data served through elevatr and AWS Terrain Tiles originates from the USGS National Elevation Dataset (NED) and NASA SRTM. AWS hosts a merged and tiled version of this dataset.

#  Recommended citation:
# United States Geological Survey. (2013). National Elevation Dataset (NED). U.S. Geological Survey. Retrieved from https://ned.usgs.gov/
  
# Data accessed via AWS Terrain Tiles through the elevatr R package (Hollister et al., 2023).

# methods inline example from Chat
# We obtained a 5-meter resolution digital elevation model (DEM) for the study area using the elevatr R package (Hollister et al., 2023), which retrieves data from AWS Terrain Tiles derived from the USGS National Elevation Dataset (USGS, 2013).



# Install if needed
# install.packages(c("elevatr", "sf", "terra", "tigris"))

library(elevatr)
library(sf)
library(terra)
library(tigris)
library(tidyverse)
library(here)

options(tigris_use_cache = TRUE,
        scipen = 999)

# download DEM. only needed to do this once ----
# Step 1: Get Sonoma and Napa county boundaries
counties_ca <- counties(state = "CA", cb = TRUE, class = "sf")
target_counties <- counties_ca %>%
  filter(NAME %in% c("Sonoma", "Napa"))

# Reproject to WGS84 (required by elevatr)
target_counties_wgs84 <- st_transform(target_counties, crs = 4326)

combined_puma_homeranges_95 <- st_read(here("data/shapefiles/combined_puma_homeranges_95.shp"))
combined_puma_homeranges_95_wgs84 <- st_transform(combined_puma_homeranges_95, crs = 4326)

# Step 2: Download DEM from AWS Terrain Tiles
dem <- get_elev_raster(
  locations = combined_puma_homeranges_95_wgs84,
  z = 11,        # ~5m resolution; you can increase to 11–12 for finer detail
  clip = "locations"
)

# Step 3: Convert to SpatRaster (terra). elevation units = meters
dem_terra <- rast(dem)

# Reproject DEM to UTM Zone 10N (EPSG:32610)
dem_utm <- project(dem_terra, "EPSG:32610")

# Save reprojected DEM
writeRaster(dem_utm, here("data/sonoma_napa_dem_utm.tif"), overwrite = TRUE)

# Plot to confirm
terra::plot(dem_utm, main = "DEM Reprojected to UTM Zone 10N")

# calculating terrain metrics ----
dem_utm <- rast(here("data/sonoma_napa_dem_utm.tif"))

# calculate slope
# Use 'terrain' to get slope in degrees (or use "radians")
slope_raster <- terrain(dem_utm, v = "slope", unit = "degrees")

# testing "smoothed" slope calculation
calculate_smoothed_slope <- function(dem, window_size = 3, unit = "degrees") {
  dem_smooth <- terra::focal(dem, w = window_size, fun = mean, na.policy = "omit")
  slope <- terra::terrain(dem_smooth, v = "slope", unit = unit)
  return(slope)
}

smoothed_slope_raster <- calculate_smoothed_slope(dem_utm, window_size = 3)

# not much difference between smoothed and unsmoothed, sticking with unsmoothed
terra::plot(slope_raster)
terra::plot(smoothed_slope_raster)


# calculate slope and ruggedness (SD of elevation) together
calculate_slope_and_ruggedness <- function(dem, window_size = 3, unit = "degrees") {
  # Step 1: Calculate slope from raw DEM
  slope <- terrain(dem, v = "slope", unit = unit)
  
  # Step 2: Calculate ruggedness as std dev of elevation in local window
  ruggedness <- terra::focal(dem, w = window_size, fun = sd, na.policy = "omit")
  
  # Combine into one raster stack
  result <- c(slope, ruggedness)
  names(result) <- c("slope", "ruggedness")
  
  return(result)
}

# Example usage
slope_rugged_stack <- calculate_slope_and_ruggedness(dem_utm, window_size = 3)

# Plot both layers
terra::plot(slope_rugged_stack)


# extract for segment buffers ----

# Ensure same CRS

segments_in_combined_homeranges <- readRDS(here("data/segments_in_combined_homeranges"))
segments_utm <- st_transform(segments_in_combined_homeranges, crs = crs(dem_utm))

buffer_roads <- function(zbuff) {
  buff_road <- segments_utm %>% 
    group_by(seg.label) %>% 
    st_buffer(zbuff, endCapStyle = "ROUND") %>% 
    mutate(buff = zbuff)
}

hr_road_buffers <- map_df(seq(30, 300, by = 30), buffer_roads)

# Ensure your buffers are in the same CRS as the raster
hr_road_buffers_utm <- st_transform(hr_road_buffers, crs = crs(slope_rugged_stack))

# Convert sf to SpatVector (terra format)
hr_road_buffers_vect <- vect(hr_road_buffers_utm)

# Extract mean slope and ruggedness per buffer
terrain_mean <- terra::extract(
  slope_rugged_stack,
  hr_road_buffers_vect,
  fun = mean,
  na.rm = TRUE,
  ID = TRUE,
  cells = FALSE
) %>%
  rename_with(~ paste0("mean.", .x), .cols = -ID)



# Median values
terrain_median <- terra::extract(
  slope_rugged_stack,
  hr_road_buffers_vect,
  fun = median,
  na.rm = TRUE,
  ID = TRUE
) %>%
  rename_with(~ paste0("median.", .x), .cols = -ID)

# Merge both summaries
terrain_summary <- full_join(terrain_mean, terrain_median, by = "ID")

# Add back buffer metadata
hr_road_buffers_df <- hr_road_buffers %>%
  data.frame() %>%
  select(seg.label, buff) %>%
  mutate(ID = row_number())

# Final joined data
hr_seg_terrain <- left_join(hr_road_buffers_df, terrain_summary, by = "ID")


hr_seg_terrain %>% 
  ggplot() +
  geom_point(aes(x = mean.ruggedness, y = mean.slope), alpha = 0.1) +
  geom_point(aes(x = mean.slope, y = median.slope), color = "red", alpha = 0.1) +
  geom_point(aes(x = mean.ruggedness, y = median.ruggedness), color = "blue", alpha = 0.1) +
  geom_abline()

hr_seg_terrain %>% 
  ggplot() +
  geom_point(aes(x = mean.slope, y = median.slope))

hr_seg_terrain %>% 
  ggplot() +
  geom_point(aes(x = mean.ruggedness, y = median.ruggedness))

# the relationship between mean.slope and mean.ruggedness is constant over the range of both, but mean.ruggedness increases 2x. and within each metric, the mean and median are fairly similar.
# all would be basically the same in a model so just keeping mean slope

hr_seg_terrain %>% 
  select(seg.label, buff, mean.slope) %>% 
  saveRDS(here("data/hr_seg_slope"))
