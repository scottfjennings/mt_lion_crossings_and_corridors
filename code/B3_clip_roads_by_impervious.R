


# ad hoc checking to see if there's a way to block out areas of high development from the entire analysis.

# NLCD Class Name	NLCD Class Code	Approx. % Impervious Surface
#Developed, Open Space	21	< 20%
#Developed, Low Intensity	22	20–49%
#Developed, Medium Intensity	23	50–79%
#Developed, High Intensity	24	80–100%


library(terra)
library(sf)
library(here)
library(tidyverse)


# Load raster, shapefile (home ranges), and point data
impervious_rast <- readRDS("data/sonoma_napa_nlcd/sonoma_napa_impervious2019_rast")  # terra SpatRaster
home_ranges <- st_read("data/shapefiles/combined_puma_homeranges_95.shp")         # sf object
# Reproject home_ranges to match raster CRS
home_ranges_proj <- st_transform(home_ranges, crs = crs(impervious_rast))
points_df <- readRDS("data/analysis_table_max_filt_puma")                                       # DataFrame with lat/lon
# Convert points to sf and reproject to raster CRS
points_sf <- st_as_sf(points_df, coords = c("longitude", "latitude"), crs = 4326)
points_proj <- st_transform(points_sf, crs = crs(impervious_rast))



# Crop and mask NLCD impervious raster to home range area
imperv_crop <- crop(impervious_rast, vect(home_ranges_proj))
imperv_mask <- mask(imperv_crop, vect(home_ranges_proj))
plot(imperv_mask, main = "imperv_mask")

#### summarize % impervious values between used (raw GPS point there) and unused (no raw fix) cells ----


# Get raster cells that were "used" — intersected by any point
# Extract cell numbers for each point
cell_numbers_used <- cellFromXY(imperv_mask, st_coordinates(points_proj)) %>% unique()

# Create a data frame of all non-NA raster cell values
imperv_vals_all <- as.data.frame(imperv_mask, xy = TRUE, cells = TRUE, na.rm = TRUE)

# Flag whether each cell was used
imperv_vals_all <- imperv_vals_all %>%
  mutate(used = ifelse(cell %in% cell_numbers_used, "used", "unused")) %>%
  rename(lyr.1 = mrlc_download__NLCD_2019_Impervious_L48) 

# Compare impervious scores
summary_stats <- imperv_vals_all%>% 
  group_by(used) %>%
  summarize(
    mean_impervious = mean(lyr.1, na.rm = TRUE),
    median_impervious = median(lyr.1, na.rm = TRUE),
    sd_impervious = sd(lyr.1, na.rm = TRUE),
    max_impervious = max(lyr.1, na.rm = TRUE),
    n = n()
  )

# plot to check
imperv_vals_all %>% 
  filter(lyr.1 > 5) %>% 
  ggplot(aes(x = used, y = lyr.1)) +
  geom_violin() +
  labs(y = "Impervious Surface (%)", x = "Cell Use", title = "Comparison of Impervious Surface by Use") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 100, by = 10), labels = seq(0, 100, by = 10))


imperv_vals_all %>% 
  filter(lyr.1 > 5) %>%
  ggplot() +
  geom_density(aes(x = lyr.1, color = used)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10), labels = seq(0, 100, by = 10)) +
  theme_minimal() 

# no raw fixes in cells with >88% impervious, and few over 70.
# but worry that excluding individual cells by % impervious will be too strict
# really should look at excluding big patches of continuous high % impervious
# % impervious > 50 = moderate and high density development

#### next several chunks deal with patches (groups of cells) of >50 impervious ----
# create patches

# Threshold: only include cells > 50% impervious
binary_imperv <- ifel(imperv_mask < 50 | is.nan(imperv_mask), NA, 1)
plot(binary_imperv, main = "binary_imperv")
# create and label contiguous patches
patches_rast <- patches(binary_imperv, directions = 8, zeroAsNA = TRUE)
plot(patches_rast, main = "patches_rast")


# Assuming 'patches_rast' is your raster with patch IDs (from earlier)
patches_poly <- as.polygons(patches_rast, dissolve = TRUE)  # Convert raster to polygon and dissolve to get whole patches
plot(patches_poly, main = "patches_poly")

#
#### first looking at size of used vs unused patches (groups of cells) ----

# Compute cell area (assuming projected CRS with meters)
cell_area <- res(imperv_mask)[1]^2

# Get cell numbers from points (used cells)
cell_numbers_used <- cellFromXY(imperv_mask, st_coordinates(points_proj)) %>% na.omit() %>% unique()

# Convert patches raster to data frame
patch_df <- as.data.frame(patches_rast, xy = TRUE, cells = TRUE, na.rm = TRUE)


# Summarize patch sizes
patch_sizes <- patch_df %>%
  filter(!is.na(patches)) %>%
  group_by(patches) %>%
  summarize(patch_area_m2 = n() * cell_area, .groups = "drop")


# Find patch IDs for used cells
used_patch_ids <- patch_df %>%
  filter(cell %in% cell_numbers_used) %>%
  pull(patches) %>%
  unique()

# Get size of patches used
used_patch_sizes <- patch_sizes %>%
  filter(patches %in% used_patch_ids)

# Maximum used patch size
max_patch_size_m2 <- max(used_patch_sizes$patch_area_m2, na.rm = TRUE)

cat("Maximum impervious patch size (>50%) among used cells:", round(max_patch_size_m2, 2), "m²\n")


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

# save as SHP for ArcGIS Pro
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

ggplot() +
  geom_sf(data = patch_polygons_sf, aes(color = used))
# 5. Export as shapefile
st_write(patch_polygons_sf, "data/shapefiles/predictor_variable_checking/impervious_patches_used_classification.shp", delete_layer = TRUE)


# but there are some points in the big continuous Santa Rosa patch, so excluding by patch size based on used/non used won't work



#### Next idea is to check the Distance of each raw GPS fix to the Nearest Patch Edge ----


# first get outer boundary of each patch
# Convert to 'sf' object for easier manipulation
patches_sf <- st_as_sf(patches_poly)

# Assume patches_sf is your MULTIPOLYGON sf object of impervious patches
# Step 1: Ensure geometries are valid and explicitly MULTIPOLYGON
patches_sf <- st_make_valid(patches_sf)
patches_sf <- st_cast(patches_sf, "MULTIPOLYGON")

# Step 2: Explode into individual POLYGONs (each with 1 outer ring and possible holes)
patch_polys <- st_cast(patches_sf, "POLYGON", group_or_split = TRUE)

# Step 3: Get only the outer boundary of each polygon (as LINESTRING)
# st_exterior_ring does this reliably
outer_boundaries <- st_sf(
  geometry = st_exterior_ring(patch_polys),
  crs = st_crs(patch_polys)
)

# export shp to check in ArcGIS Pro
st_write(outer_boundaries, "data/shapefiles/predictor_variable_checking/impervious_patch_outer_boundaries_50.shp", delete_layer = TRUE)

# next calculate distances

# 1. Get the true outer boundaries (lines) of each polygon
outer_lines <- st_boundary(outer_boundaries)  # This returns LINESTRING geometries
plot(outer_lines)
# 1. Select only points inside outer boundaries
points_inside <- st_join(points_proj, outer_boundaries, left = FALSE)

# 2. For each point, calculate distance to the nearest boundary polygon
# (st_distance returns a matrix of distances)
dist_matrix <- st_distance(points_inside, outer_lines)

# 3. For each point, take the minimum distance to any polygon boundary
min_dists <- apply(dist_matrix, 1, min)

# 4. Add to the points data
points_inside$dist_to_boundary_m <- as.numeric(min_dists)

points_inside %>% 
  ggplot() +
  geom_density(aes(x = dist_to_boundary_m)) +
  geom_vline(xintercept = 50)

# very few points are >150m from the developed edge

# create a new polygon with the core developed area - all patches of >50% impervious, shrunk 150m
# 1. Create 150-meter "interior buffer" (shrinking each polygon inward)
interior_polygons <- st_buffer(outer_boundaries, dist = -50)

# 2. Remove invalid geometries that may arise during negative buffering
interior_polygons <- st_make_valid(interior_polygons)
interior_polygons <- interior_polygons[!st_is_empty(interior_polygons), ]

# 3. (Optional) Filter out small or sliver polygons, if needed
interior_polygons <- interior_polygons[st_area(interior_polygons) > units::set_units(1000, "m^2"), ]

st_write(interior_polygons, "data/shapefiles/predictor_variable_checking/impervious_patch_interior_polygons_50.shp", delete_layer = TRUE)


#### export raster of just high density development (impervious >80) ----
# Threshold cells with >80% impervious surface
imperv80_rast <- ifel(impervious_rast > 80, 1, NA)


# Write GeoTIFF
writeRaster(imperv80_rast, filename = here::here("data/shapefiles/predictor_variable_checking/impervious_over80.tif"), filetype = "GTiff", overwrite = TRUE)


