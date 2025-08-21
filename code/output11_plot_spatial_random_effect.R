


library(tidyverse)
library(here)
library(spaMM)
library(ggnewscale)
library(patchwork)
library(sf)
library(ggspatial)
library(rosm)
library(ggmap)

# load data ----
source(here("code/helper_data.R"))


segments_in_combined_homeranges <- readRDS(here("data/segments_in_combined_homeranges"))


study_counties <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/data/study_area_counties") %>% 
  st_transform(crs = 26910) %>% 
  filter(NAME %in% c("Sonoma", "Napa"))


combined_hr_polgons <- st_read(here("data/shapefiles/combined_puma_homeranges_95.shp"))


seg_midpoints <- readRDS(here("data/seg_midpoints"))

# Extract coordinates from the sf geometry
coords <- seg_midpoints %>%
  mutate(coord = sf::st_coordinates(geometry)) %>%
  # st_coordinates returns a matrix with X and Y columns
  mutate(
    x = coord[, "X"],
    y = coord[, "Y"]
  ) %>%
  select(-coord, -geometry)

seg_regions <- readRDS(here("data/hr_segments_prop_in_developed")) %>% 
  data.frame() %>% 
  select(-geometry, -seg.length)   %>% 
  filter(animal.id %in% analysis_pumas,
         !animal.id %in% few_crossings_pumas,
         !animal.id %in% hr_exclude_pumas,
         !seg.label %in% p31_exclude_segments) %>% 
  mutate(region = ifelse(animal.id %in% c("P31", "P39"), 1, 2)) %>% # region 1 is west of 101, 2 is east of 101
  distinct(seg.label, region)



coarsener = 2000

main_analysis_table <- readRDS(here("data/analysis_inputs/combined_lions_analysis_table")) %>% 
  mutate(class = ifelse(class %in% c("Highway", "Arterial"), "Major", as.character(class))
         , num.creek.bin = pmin(num.creek, 3)) %>% 
  left_join(coords) %>% 
  left_join(seg_regions) %>% 
  mutate(coarse_x = (floor(x / coarsener) + 0.5) * coarsener,
         coarse_y = (floor(y / coarsener) + 0.5) * coarsener) 

best_logreg_sp <- readRDS(here("model_objects/lions_combined/exploratory_mods_logreg_sp"))$"cohesion.200.25 + patch.touches.road + mean.tre.shr.30 + mean.dev.60 + num.creek.bin + class"




spatial_re <- ranef(best_logreg_sp)$Matern %>% 
  data.frame() %>% 
  rename(re.effect = 1) %>% 
  rownames_to_column("zcoords") %>%
  separate(zcoords, into = c("junk", "coarse_x", "coarse_y"), sep = ":", convert = TRUE) %>%
  select(coarse_x, coarse_y, re.effect)



# Assume spatial_re has columns: coarse_x, coarse_y, re.effect
rast_df <- spatial_re

# Create SpatRaster from XYZ
r <- terra::rast(spatial_re, type = "xyz", crs = "EPSG:32610")

terra::writeRaster(r, here("model_objects/lions_combined/spatial_random_effect.tif"), overwrite = TRUE)


seg_plot_df <- main_analysis_table %>% 
  distinct(seg.label, bin.crossing) %>% 
  group_by(seg.label) %>% 
  summarise(bin.crossing = sum(bin.crossing)) %>% 
  left_join(segments_in_combined_homeranges) %>% 
  st_as_sf() %>% 
  mutate(bin.crossing = ifelse(bin.crossing == 0, "Not crossed", "Crossed"))


# Make sure CRS is set (you said it's UTM Zone 10N, EPSG:32610)
st_crs(seg_plot_df) <- 32610  # only set if not already assigned

st_write(seg_plot_df, here("data/analysis_segments_crossed_not.shp"), delete_layer = TRUE)

# Define bounding box based on your data (example coordinates)
bbox <- c(left = 500000, bottom = 4200000, right = 550000, top = 4250000)


seg_union <- st_union(seg_plot_df)

# Step 2: Transform to WGS84 (lat/lon)
seg_latlon <- st_transform(seg_union, 4326)

# Step 3: Get bbox from that
bbox <- st_bbox(seg_latlon)
basemap <- get_stadiamap(
  bbox = bbox,
  zoom = 11,
  maptype = "stamen_toner_lite"
)


# Get a light basemap from Stamen
basemap <- basemap(bbox = bbox, zoom = 11, maptype = "stamen_toner_lite")

# Plot
ggmap(basemap) +
  geom_tile(data = spatial_re, aes(x = coarse_x, y = coarse_y, fill = re.effect), alpha = 0.7) +
  scale_fill_gradient2(low = "blue", mid = "gray90", high = "red", midpoint = 0)


ggplot() +
  ggspatial::annotation_map_tile(type = "osm") +
  geom_tile(data = spatial_re, aes(x = coarse_x, y = coarse_y, fill = re.effect)) +
  scale_fill_gradient2(
    low = "blue", mid = "gray90", high = "red", midpoint = 0,
    name = "Spatial random\nfield (logit scale)"
  ) +
  new_scale_color() +
  geom_sf(data = seg_plot_df, aes(color = as.factor(bin.crossing)), linewidth = 1) +
  scale_color_manual(
    values = c("Not crossed" = "gray50", "Crossed" = "black"),
    name = NULL,  # Remove title text
    guide = guide_legend(title = NULL)  # Prevent space for the title
  ) +
  geom_sf(data = combined_hr_polgons, fill = NA, color = "black") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),        # remove grid lines
    axis.text = element_blank(),         # remove axis numbers
    axis.title = element_blank(),        # remove axis titles
    axis.ticks = element_blank(),        # remove tick marks
    plot.title = element_text(size = 14, face = "bold"),  # optional: tweak title,
    legend.position.inside = c(0.05, 0.05),        # lower-left corner (x, y)
    legend.justification = c(0, 0),         # anchor point of legend box
    legend.background = element_rect(fill = "white", color = "black") 
  ) +
  guides(
    fill = guide_colorbar(position = "inside"),
    color = guide_legend(position = "inside")
  )




