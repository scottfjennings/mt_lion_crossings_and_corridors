library(terra)
library(sf)
library(here)
library(tidyverse)

# Step 1: Load raster, shapefile (home ranges), and point data
impervious_rast <- readRDS(here("data/sonoma_napa_nlcd/sonoma_napa_impervious2019_rast"))
home_ranges <- st_read(here("data/shapefiles/combined_puma_homeranges_99.shp"))
points_df <- readRDS(here("data/analysis_table"))  # Assumes lat/lon columns

# Step 2: Convert point data to sf object in lat/lon
points_sf <- st_as_sf(points_df, coords = c("longitude", "latitude"), crs = 4326)

# Step 3: Reproject everything to match raster CRS (EPSG:5070)
home_ranges_proj <- st_transform(home_ranges, crs = crs(impervious_rast))
points_proj <- st_transform(points_sf, crs = crs(impervious_rast))

# Step 4: Crop and mask raster to home range area
imperv_crop <- crop(impervious_rast, vect(home_ranges_proj))
imperv_mask <- mask(imperv_crop, vect(home_ranges_proj))

# Step 5: Get raster cells that were "used" — intersected by any point
# Extract cell numbers for each point
cell_numbers_used <- cellFromXY(imperv_mask, st_coordinates(points_proj)) %>% unique()

# Step 6: Create a data frame of all non-NA raster cell values
imperv_vals_all <- as.data.frame(imperv_mask, xy = TRUE, cells = TRUE, na.rm = TRUE)

# Step 7: Flag whether each cell was used
imperv_vals_all <- imperv_vals_all %>%
  mutate(used = ifelse(cell %in% cell_numbers_used, "used", "unused")) %>%
  rename(lyr.1 = mrlc_download__NLCD_2019_Impervious_L48) 

# Step 8: Compare impervious scores
summary_stats <- imperv_vals_all%>% 
  group_by(used) %>%
  summarize(
    mean_impervious = mean(lyr.1, na.rm = TRUE),
    median_impervious = median(lyr.1, na.rm = TRUE),
    sd_impervious = sd(lyr.1, na.rm = TRUE),
    max_impervious = max(lyr.1, na.rm = TRUE),
    n = n()
  )

# Optional: Boxplot
library(ggplot2)
ggplot(imperv_vals_all, aes(x = used, y = lyr.1)) +
  geom_violin() +
  labs(y = "Impervious Surface (%)", x = "Cell Use", title = "Comparison of Impervious Surface by Use") +
  theme_minimal()



# 1. Threshold: impervious > 50
binary_imperv <- imperv_mask > 50

# 2. Identify patches (connected areas)
patches_rast <- patches(binary_imperv, directions = 8)


# 3. Convert to data frame (keep patch ID as named column)
patch_df <- as.data.frame(patches_rast, xy = TRUE, cells = TRUE, na.rm = TRUE)
names(patch_df)[which(names(patch_df) == names(patches_rast))] <- "patch_id"

# 4. Add raster resolution (assumes square cells)
cell_area <- res(imperv_mask)[1]^2  # in projected CRS units (e.g., m²)

# 5. Calculate patch sizes (number of cells × cell area)
patch_sizes <- patch_df %>%
  group_by(patch_id) %>%
  summarize(patch_area_m2 = n() * cell_area)

# 6. Identify used cell numbers
cell_numbers_used <- cellFromXY(imperv_mask, st_coordinates(points_proj)) %>% unique()

# 7. Find patch IDs for used cells
used_patch_ids <- patch_df %>%
  filter(cell %in% cell_numbers_used) %>%
  pull(patch_id) %>%
  unique()

# 8. Get the size of patches that were used
used_patch_sizes <- patch_sizes %>%
  filter(patch_id %in% used_patch_ids)

# 9. Maximum used patch size
max_patch_size_m2 <- max(used_patch_sizes$patch_area_m2, na.rm = TRUE)

# Report
cat("Maximum patch size (impervious > 50%) among used cells:", round(max_patch_size_m2, 2), "m²\n")



# 5. Label patch usage
patch_df <- patch_df %>%
  mutate(used = ifelse(patch_id %in% used_patch_ids, "Used", "Not Used"))

# 6. Plot
ggplot(patch_df, aes(x = x, y = y, fill = used)) +
  geom_raster() +
  coord_equal() +
  scale_fill_manual(values = c("Used" = "firebrick", "Not Used" = "grey80")) +
  labs(title = "Impervious Surface Patches > 50%",
       subtitle = "Patches used vs not used by points",
       x = "Easting", y = "Northing", fill = "Patch Status") +
  theme_minimal()





#####################

library(terra)
library(sf)
library(ggplot2)
library(dplyr)

# 1. Load data
imperv_rast <- readRDS("data/sonoma_napa_nlcd/sonoma_napa_impervious2019_rast")  # terra SpatRaster
home_ranges <- st_read("data/shapefiles/combined_puma_homeranges_99.shp")         # sf object
points_df <- readRDS("data/analysis_table")                                       # DataFrame with lat/lon

# 2. Convert points to sf and reproject to raster CRS
points_sf <- st_as_sf(points_df, coords = c("longitude", "latitude"), crs = 4326)
points_proj <- st_transform(points_sf, crs = crs(imperv_rast))

# Reproject home_ranges to match raster CRS
home_ranges_proj <- st_transform(home_ranges, crs = crs(imperv_rast))

# Convert sf to SpatVector for terra
home_ranges_vect <- vect(home_ranges_proj)

# Now crop and mask work properly
imperv_crop <- crop(imperv_rast, home_ranges_vect)
imperv_mask <- mask(imperv_crop, home_ranges_vect)
names(imperv_mask) <- "imperv" 

# 4. Threshold: only include cells > 50% impervious
binary_imperv <- ifel(imperv_mask < 50 | is.nan(imperv_mask), NA, 1)

# 5. Label contiguous patches
patches_rast <- patches(binary_imperv, directions = 8)

# 6. Get cell numbers from points (used cells)
cell_numbers_used <- cellFromXY(imperv_mask, st_coordinates(points_proj)) %>% na.omit() %>% unique()

# 7. Convert patches raster to data frame
patch_df <- as.data.frame(patches_rast, xy = TRUE, cells = TRUE, na.rm = TRUE)


# 8. Label patch_id as used/not used
used_patch_ids <- patch_df %>%
  filter(cell %in% cell_numbers_used) %>%
  pull(patches) %>%
  unique()

patch_df <- patch_df %>%
  mutate(used = ifelse(patches %in% used_patch_ids, "Used", "Not Used"))

# 9. Plot
ggplot(patch_df, aes(x = x, y = y, fill = used)) +
  geom_raster() +
  coord_equal() +
  scale_fill_manual(values = c("Used" = "firebrick", "Not Used" = "grey80")) +
  labs(title = "Impervious Surface Patches > 50%",
       subtitle = "Used vs Not Used by Points",
       x = "Easting", y = "Northing", fill = "Patch Status") +
  theme_minimal()



library(terra)

# 1. Create a classified raster: 1 = Used, 0 = Not Used, NA = No patch
# First, initialize a new raster matching patches_rast
used_rast <- patches_rast

# 2. Get vector of used patch IDs
used_patch_ids <- patch_df %>%
  filter(used == "Used") %>%
  pull(patches) %>%
  unique()

# 3. Reclassify: set all used patches to 1, others to 0 or NA
# This can be done efficiently using classify
class_matrix <- cbind(used_patch_ids, rep(1, length(used_patch_ids)))  # matrix: patch ID -> 1
# Apply classification
used_rast_vals <- classify(patches_rast, rcl = class_matrix, others = 0)

# 4. Write to GeoTIFF for ArcGIS Pro
writeRaster(used_rast_vals, here::here("data/shapefiles/predictor_variable_checking/impervious_patch_use_classification.tif"), 
            overwrite = TRUE, gdal = TRUE, datatype = "INT1U")


library(terra)
library(dplyr)
library(sf)

# 1. Convert raster patches to polygons
# Dissolve = TRUE merges contiguous cells with same patch ID into one polygon
patch_polygons <- as.polygons(patches_rast, dissolve = TRUE, na.rm = TRUE)

# 2. Convert to sf object for easier manipulation and export
patch_polygons_sf <- st_as_sf(patch_polygons)

# 3. Add patch_id column (rename the original 'patches' column)
names(patch_polygons_sf)[names(patch_polygons_sf) == "patches"] <- "patch_id"

# 4. Classify patches as used or not
patch_polygons_sf <- patch_polygons_sf %>%
  mutate(used = ifelse(patch_id %in% used_patch_ids, "Used", "Not Used"))

# 5. Export as shapefile
st_write(patch_polygons_sf, "data/shapefiles/predictor_variable_checking/impervious_patch_use_classification.shp", delete_layer = TRUE)

# Code to Calculate Distance to Nearest Patch Edge ----

library(terra)
library(sf)
library(dplyr)

# 1. Convert patch raster to polygons
patch_polys <- as.polygons(patches_rast, dissolve = TRUE, na.rm = TRUE)
names(patch_polys) <- "patch_id"

# 2. Convert points to SpatVector and transform to raster CRS
points_spat <- vect(st_transform(points_proj, crs(imperv_rast)))

# 3. Extract patch ID for each point
points_spat$patch_id <- extract(patches_rast, points_spat)[, 2]

# 4. Filter only points inside patches (i.e., not NA)
points_in_patches <- points_spat[!is.na(points_spat$patch_id), ]
# Extract attribute data
point_data <- as.data.frame(points_in_patches)

# Extract XY coordinates separately
point_coords <- crds(points_in_patches, df = TRUE)  # Returns a data.frame with x and y columns

# Combine with patch_id and distance
coords_df <- cbind(point_coords, patch_id = point_data$patch_id)

# 5. For each point, calculate distance to boundary of its own patch
# Create a data frame to hold distances
distance_df <- data.frame()

for (pid in unique(points_in_patches$patch_id)) {
  # Get the patch polygon
  patch_geom <- patch_polys[patch_polys$patch_id == pid, ]
  
  # Subset the points in this patch
  pts <- points_in_patches[points_in_patches$patch_id == pid, ]
  
  # Get patch boundary as line
  patch_boundary <- as.lines(patch_geom)
  
  # Calculate distances to the boundary
  dists <- terra::distance(pts, patch_boundary)
  
  distance_df <- rbind(distance_df, data.frame(
    point_id = seq_along(dists),
    patch_id = pid,
    distance_m = dists
  ))
}

# Combine with point coordinates if desired
coords_df <- as.data.frame(points_in_patches)[, c("x", "y", "patch_id")]
distance_df <- cbind(coords_df, distance_df["distance_m"])
distance_df <- cbind(coords_df, distance_m = distance_df$distance_m)

# Preview
head(distance_df)


####
library(terra)
library(sf)

# Assuming 'patches_rast' is your raster with patch IDs (from earlier)
patches_poly <- as.polygons(patches_rast, dissolve = TRUE)  # Convert raster to polygon and dissolve to get whole patches

# Convert to 'sf' object for easier manipulation
patches_sf <- st_as_sf(patches_poly)

# Now, get the exterior ring (outer boundary) for each patch
patches_outer <- st_cast(st_geometry(patches_sf), "POLYGON")  # Ensures we only keep the outer boundaries

# If there are multi-part patches (i.e., multiple disconnected areas), we'll keep only the outer boundary
patches_outer_sf <- st_sf(geometry = patches_outer)

# Check the geometry (make sure we only have one boundary per patch)
plot(patches_outer_sf)

points_in_patches_sf <- st_as_sf(points_in_patches)

# Assuming 'points_in_patches_sf' is your sf object with the points inside the patches
# Calculate the distance from each point to the outer boundary of the patches
distances_to_outer <- st_distance(points_in_patches_sf, patches_outer_sf)

# Get the minimum distance for each point to the nearest outer boundary (excluding holes)
min_distances <- apply(distances_to_outer, 1, min)

# Add these distances back to your points data
points_in_patches_sf$distance_to_outer <- min_distances



### raster of just high density development (impervious >80)
# Threshold cells with >80% impervious surface
imperv80_rast <- ifel(imperv_rast > 80, 1, NA)


# Write GeoTIFF
writeRaster(imperv80_rast, filename = here::here("data/shapefiles/predictor_variable_checking/impervious_over80.tif"), filetype = "GTiff", overwrite = TRUE)

