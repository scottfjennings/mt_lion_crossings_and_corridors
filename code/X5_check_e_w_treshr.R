library(terra)
library(sf)
library(tidyverse)
library(here)

zyear <- 2022

# 2. Load the combined TRE+SHR raster for the specified year
zhab <- rast(paste("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/Harvey_north bay habitat/RSF_Layers_", zyear, "_2024-03-19.TIF", sep = ""))

# Load your home ranges multipolygon
home_ranges <- st_read("data/shapefiles/combined_puma_homeranges_99.shp")         # sf object
home_ranges_proj <- st_transform(home_ranges, crs = crs(zhab))

# 1. Separate the multipolygon into individual polygons
home_ranges_poly <- st_cast(home_ranges_proj, "POLYGON")

# 2. Calculate the centroids for each polygon
home_ranges_centroids <- st_centroid(home_ranges_poly)

# 3. Add a column for 'east' or 'west' based on the median longitude of the centroid
home_ranges_poly <- home_ranges_poly %>%
  mutate(
    centroid_lon = st_coordinates(home_ranges_centroids)[, 1],
    label = ifelse(centroid_lon > median(centroid_lon), "East", "West")
  )

ggplot() +
  geom_sf(data = home_ranges_poly, aes(fill = label)) +
  labs(title = "home_ranges_poly")

# Convert the polygons to SpatVector
home_ranges_vect <- vect(home_ranges_poly)
# 2. Convert the SpatVector to a data frame and add the relevant attributes (e.g., 'FID' or any other identifier)
home_ranges_df <- as.data.frame(home_ranges_vect)


zhab_crop <- crop(zhab, vect(home_ranges_proj))
zhab_mask <- mask(zhab_crop, vect(home_ranges_proj))

# Check the names of the layers in your raster
names(zhab)

# Assuming the layers are named something like "TRE" and "SHR"
# You can access them as follows:
tre_rast <- zhab_mask[["TRE"]]  # Tree cover
shr_rast <- zhab_mask[["SHR"]]  # Shrub cover
# 4. Stack rasters and create a combined layer for TRE + SHR
cover_stack <- c(tre_rast, shr_rast)
names(cover_stack) <- c("TRE", "SHR")
cover_stack$TRE_SHR <- cover_stack$TRE + cover_stack$SHR

# This will return TRUE for cells to keep (i.e., those with no 0s)
nonzero_mask <- app(cover_stack, fun = function(x) all(x != 0, na.rm = TRUE))

# 2. Mask out the cells with any 0 values in any layer
cover_stack_no_zeros <- mask(cover_stack, nonzero_mask, maskvalues = 0, updatevalue = NA)

# 3. Plot the cleaned raster stack
plot(cover_stack_no_zeros)
plot(cover_stack)


# 4. Extract the raster values for each polygon
tre_values <- terra::extract(tre_rast, home_ranges_vect, cells = TRUE)
tre_values$label <- home_ranges_df$label[rep(1:nrow(home_ranges_df), each = nrow(tre_values) / nrow(home_ranges_df))]

# 4. Extract the raster values for each polygon
shr_values <- terra::extract(shr_rast, home_ranges_vect, cells = TRUE)
shr_values$label <- home_ranges_df$label[rep(1:nrow(home_ranges_df), each = nrow(shr_values) / nrow(home_ranges_df))]
shr_values %>% 
  #filter(SHR > 0) %>% 
#  filter(SHR < 50) %>% 
  ggplot() +
  geom_density(aes(x = SHR, color = label)) 


# 4. Extract the raster values for each polygon
tre_shr_values <- terra::extract(cover_stack, home_ranges_vect, cells = TRUE)
tre_shr_values$label <- home_ranges_df$label[rep(1:nrow(home_ranges_df), each = nrow(tre_shr_values) / nrow(home_ranges_df))]

all_values <- full_join(tre_values, shr_values) %>% 
  full_join(tre_shr_values)

all_values_longer <- all_values %>% 
  select(-ID) %>% 
  pivot_longer(cols = c(TRE, SHR, TRE_SHR), names_to = "layer")


east_west_summary <- all_values_longer %>% 
  group_by(layer, label) %>% 
  summarise(mean.val = mean(value),
            med.val = median(value),
            mean.val.no0 = mean(value > 0, na.rm = TRUE),
            med.val.no0 = median(value > 0, na.rm = TRUE),
            tot.0 = sum(value == 0, na.rm = TRUE),
            tot = n(),
            prop.0 = tot.0/tot)


ggplot(all_values_longer) +
  geom_density(aes(x = value, color = label)) +
  facet_wrap(~layer)
