


library(tidyverse)
library(here)
library(sf)
library(terra)
library(lubridate)
library(flextable)
library(readxl)
library(spaMM)
library(maps)
library(cowplot)
library(ggspatial)

options(scipen = 999)

source(here("code/utilities.R"))
source(here("code/helper_data.R"))


hr_polgons <- st_read(here("data/shapefiles/puma_homeranges_95.shp"))   %>% 
  rename(animal.id = puma) %>% 
  filter(animal.id %in% analysis_pumas,
         !animal.id %in% few_crossings_pumas,
         !animal.id %in% hr_exclude_pumas)  %>% 
  mutate(region = ifelse(animal.id %in% c("P31", "P39"), 1, 2))


hr_polgons_combined <- hr_polgons %>% 
  group_by(region) %>% 
  summarise()

seg_regions <- readRDS(here("data/hr_segments_prop_in_developed")) %>% 
  select(-seg.length)   %>% 
  filter(animal.id %in% analysis_pumas,
         !animal.id %in% few_crossings_pumas,
         !animal.id %in% hr_exclude_pumas,
         !seg.label %in% p31_exclude_segments) %>% 
  mutate(region = ifelse(animal.id %in% c("P31", "P39"), 1, 2)) %>% 
  distinct(seg.label, region, geometry) %>%
  st_as_sf()


# prep county wide tree + shrub combined raster ----
# only need to run this once
hab_rast <- rast("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/Harvey_north bay habitat/RSF_Layers_2023_2024-03-19.TIF")


study_counties <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/data/study_area_counties") %>% 
  st_transform(crs = 26910) %>% 
  filter(NAME %in% c("Sonoma", "Napa"))

study_counties_tre <- hab_rast[["TRE"]] %>%
  terra::crop(study_counties) %>%
  terra::project("EPSG:26910") %>%
  terra::mask(study_counties)
  

study_counties_shr <- hab_rast[["SHR"]] %>%
  terra::crop(study_counties) %>%
  terra::project("EPSG:26910") %>%
  terra::mask(study_counties)

  
# Sum and save combined raster
study_counties_treshr <- terra::app(c(study_counties_tre, study_counties_shr), sum)

saveRDS(study_counties_treshr, here("data/processed_habitat_rasters/study_counties_treshr2023"))
writeRaster(study_counties_treshr, here("data/processed_habitat_rasters/study_counties_treshr2023.tiff"))

# Convert the SpatRaster to a data.frame
# `xy = TRUE` adds coordinates; `na.rm = TRUE` removes NA cells
study_counties_treshr_df <- as.data.frame(study_counties_treshr, xy = TRUE, na.rm = TRUE)

# If your raster has more than one layer, you'll want to name the layer
# Let's assume it's a single-layer raster for now
names(study_counties_treshr_df)[3] <- "value"  # Rename the value column if needed

saveRDS(study_counties_treshr_df, here("data/processed_habitat_rasters/study_counties_treshr_df"))

study_counties_dev <- hab_rast[["Development"]] %>%
  terra::crop(study_counties) %>%
  terra::project("EPSG:26910") %>%
  terra::mask(study_counties)

study_counties_dev[study_counties_dev == 0] <- NA
r_reclassified <- classify(study_counties_dev, matrix(c(0, 0, NA), ncol = 3, byrow = TRUE))

saveRDS(study_counties_dev, here("data/processed_habitat_rasters/study_counties_dev2023"))
writeRaster(study_counties_dev, here("data/processed_habitat_rasters/study_counties_dev2023.tiff"), overwrite = TRUE)

# Convert the SpatRaster to a data.frame
# `xy = TRUE` adds coordinates; `na.rm = TRUE` removes NA cells
study_counties_dev_df <- as.data.frame(study_counties_dev, xy = TRUE, na.rm = TRUE)

# If your raster has more than one layer, you'll want to name the layer
# Let's assume it's a single-layer raster for now
names(study_counties_dev_df)[3] <- "value"  # Rename the value column if needed

saveRDS(study_counties_dev_df, here("data/processed_habitat_rasters/study_counties_dev_df"))

# Now plot make map ----

study_counties_dev_df <- readRDS(here("data/processed_habitat_rasters/study_counties_dev_df")) %>% 
  #mutate(value = ifelse(value == 0, NA, value)) %>% 
  filter(value == 1)

study_counties_treshr_df <- readRDS(here("data/processed_habitat_rasters/study_counties_treshr_df")) 
  



ggplot() +
  geom_tile(data = study_counties_treshr_df, aes(x = x, y = y, fill = value)) +
  geom_tile(data = study_counties_dev_df, aes(x = x, y = y), fill = "orange") +
  geom_sf(data = hr_polgons_combined, fill = NA, color = "red", linewidth = 2) +
#  coord_equal() +  # ensures square cells
  scale_fill_viridis_c() +  # better color scale
  theme_minimal() +
  labs(fill = "% woody cover")





# Get California boundary (could use tigris or rnaturalearth for better quality)
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
california <- subset(states, ID == "california")

# Make a bbox for Sonoma + Napa
bbox <- st_as_sfc(st_bbox(hr_polgons_combined))  # or make your own

inset_map <- ggplot() +
  geom_sf(data = california, fill = "white") +
  geom_sf(data = bbox, fill = NA, color = "red", linewidth = 1) +
  theme_void()



# Add indicator values
study_counties_treshr_df$layer <- "woody"
study_counties_dev_df$layer <- "development"

# Use same column names
names(study_counties_treshr_df)[which(names(study_counties_treshr_df) == "value")] <- "fill_value"
study_counties_dev_df$fill_value <- NA  # placeholder

# Combine
all_tiles_df <- bind_rows(study_counties_treshr_df, study_counties_dev_df)

main_map <- ggplot() +
  geom_tile(data = all_tiles_df, aes(x = x, y = y, fill = ifelse(layer == "woody", fill_value, layer))) +
  geom_sf(data = hr_polgons_combined, fill = NA, color = "red", linewidth = 2) +
  scale_fill_gradientn(name = "% woody cover", 
                       colors = viridis::viridis(256),
                       na.value = "orange",
                       guide = guide_colorbar(order = 1)) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering()) +
  theme_minimal()

# Main map (your ggplot with raster + HRs)
main_map <- ggplot() +
  geom_tile(data = study_counties_treshr_df, aes(x = x, y = y, fill = value)) +
  geom_tile(data = study_counties_dev_df, aes(x = x, y = y), fill = "orange") +
  geom_sf(data = hr_polgons_combined, fill = NA, color = "red", linewidth = 2) +
  scale_fill_viridis_c(name = "% woody cover") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering()) +
  theme_minimal()

# Combine maps with inset
ggdraw() +
  draw_plot(main_map) +
  draw_plot(inset_map, x = 0.7, y = 0.7, width = 0.25, height = 0.25)









# some simple calculations ----


# region areas 

region_area <- hr_polgons %>% 
  group_by(region) %>% 
  summarise() %>% 
  st_area() %>% 
  data.frame() %>%
  rownames_to_column("region") %>% 
  rename("region.m2" = 2) %>% 
  mutate(region.km2 = as.numeric(region.m2)/1000000) %>% 
  select(region, region.km2)


# km of road per region

region_road_km <- seg_regions %>% 
  distinct(seg.label, region, geometry) %>% 
  group_by(region) %>% 
  summarise() %>% 
  st_length() %>% 
  data.frame() %>%
  rownames_to_column("region") %>% 
  rename("roads.m" = 2) %>% 
  mutate(roads.km = as.numeric(roads.m)/1000) %>% 
  select(region, roads.km)

region_area_roads <- full_join(region_area, region_road_km)  
