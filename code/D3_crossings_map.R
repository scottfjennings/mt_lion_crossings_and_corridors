

library(tidyverse)
library(here)
library(RColorBrewer)
library(sf)
library(ggmap)
library(basemaps)
library(ggnewscale)
library(cowplot)
library(ggspatial)

source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/stadiamaps_api_key.R")
source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/google_api_key.R")

source(here("code/utilities.R"))


# data prep ----
analysis_table_complete_sp <-readRDS(here("data/analysis_table_complete_sp")) %>% 
  ungroup() %>% 
  st_as_sf()



available_segments_years <- analysis_table_complete_sp %>% 
  filter(used.available == 0) %>% 
  distinct(seg.label, year, geometry) %>% 
  st_as_sf()


available_segments <- analysis_table_complete_sp %>% 
  filter(used.available == 0) %>% 
  distinct(seg.label, geometry) %>% 
  st_as_sf() %>% 
  mutate(seg.length = st_length(.)) %>% 
  group_by(seg.label) %>% 
  filter(seg.length == max(seg.length))



crossed_segments <- analysis_table_complete_sp %>% 
  filter(used.available == 1) %>% 
  animal.id_from_crossing.step() %>% 
  st_as_sf() %>% 
  select(crossed.seg.label = seg.label, animal.id, crossing.year = year, geometry) %>% 
  distinct()

available_crossed_intersection <- st_intersection(available_segments_years, crossed_segments)

# keep only linestrings and multilinestrings (from collections too)
available_crossed_intersection_lines <- available_crossed_intersection %>%
  filter(year == crossing.year) %>% 
  st_collection_extract("LINESTRING") # st_intersection above creates some points where roads intersect. extract just the linestrings

available_crossed_tally <- available_crossed_intersection_lines %>% 
  data.frame() %>% 
  select(-geometry) %>% 
  distinct(seg.label, year, animal.id, crossed.seg.label) %>% 
  count(seg.label, year, animal.id) %>% 
  ungroup() 


available_segments_mean_annual_crossings <- available_crossed_tally %>% 
  group_by(seg.label, year) %>% 
  summarise(mean.crossings = mean(n)) %>% 
  ungroup() %>%  
  group_by(seg.label) %>% 
  summarise(mean.crossings = mean(mean.crossings)) %>% 
  ungroup() %>% 
  full_join(available_segments %>% distinct(seg.label, geometry)) %>% 
  st_as_sf() %>% 
  mutate(mean.crossings = replace_na(mean.crossings, 0))

saveRDS(available_segments_mean_annual_crossings, here("data/available_segments_mean_annual_crossings"))



# plotting ----

available_segments_mean_annual_crossings <- readRDS(here("data/available_segments_mean_annual_crossings"))

aLoCoH_hrs_100_combined_sonoma_napa <- readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs_100_combined_sonoma_napa"))

aLoCoH_hrs_100_combined_sonoma_napa <- st_transform(aLoCoH_hrs_100_combined_sonoma_napa, 4326)

# Transform your spatial data to EPSG:4326
available_segments_mean_annual_crossings <- available_segments_mean_annual_crossings %>% 
  st_as_sf() %>% 
  st_transform(4326)

cities <- tibble(
  name = c("Santa Rosa", "Napa", "Sonoma"),
  lon = c(-122.73, -122.28, -122.46),
  lat = c(38.45, 38.29, 38.28)
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%  # WGS84
  st_transform(crs = 3857)    

oranges <- brewer.pal(9, "Oranges")   # returns light -> dark
trimmed <- oranges[3:9]               # drop the two lightest (adjust indexes if you want different cut)


ext <- st_transform(aLoCoH_hrs_100_combined_sonoma_napa, crs = st_crs(3857))
ext_bb <- st_bbox(ext)


# legend doesn't show combined home ranges or uncrossed segments
ggplot() +
  basemap_gglayer(ext,
                  map_service = "esri",
                  map_type = "world_imagery") +
  basemap_gglayer(ext,
                  map_service = "esri",
                  map_type = "world_hillshade",
                  alpha = 0.5) +
  geom_sf(data = ext, color = NA, fill = "beige", alpha = 0.5) +
  geom_sf(data = available_segments_mean_annual_crossings, color = "black", inherit.aes = FALSE, linewidth = .8) +
  geom_sf(data = filter(available_segments_mean_annual_crossings, mean.crossings == 0),
          color = "gray45", fill = NA, linewidth = .5, inherit.aes = FALSE) +
  geom_sf(data = filter(available_segments_mean_annual_crossings, mean.crossings > 0),
          aes(color = mean.crossings), linewidth = .5, inherit.aes = FALSE) +
  geom_sf_text(data = cities, aes(label = name),
               size = 3, color = "black", fontface = "bold") +
  scale_color_gradientn(colors = trimmed, name = "Mean annual\ncrossings/lion") +
  guides(fill = "none", alpha = "none") +
#  coord_sf() +
  coord_sf(xlim = c(ext_bb["xmin"], ext_bb["xmax"]), ylim = c(ext_bb["ymin"], ext_bb["ymax"]), expand = FALSE) +
  theme_minimal() +
  scale_fill_identity()+
  labs(x = NULL, y = NULL) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.75),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),  # white fill + black border
    legend.key = element_rect(fill = "white", color = "black", linewidth = 0.5),     # white behind each key
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) +
  # 🧭 North arrow
  annotation_north_arrow(
    location = "br",   # "tl" = top-left, "br" = bottom-right, etc
    which_north = "true",
    style = north_arrow_fancy_orienteering
  ) +
  # 📏 Scale bar
  annotation_scale(
    location = "bl",   # bottom-left
    width_hint = 0.3   # relative width of bar
  )


ggsave(here("figures/mean_annual_crossings_per_lion.png"), dpi = 300, width = 8)


# zoomed in on sonoma valley zone


# Create a bounding box around the three cities + small buffer (meters)
city_bbox <- cities %>% 
  filter(name != "Napa") %>% 
  st_bbox()
buffer_m <- 10000  # 10 km buffer around cities
xlim_zoom <- c(city_bbox["xmin"] - 0.3*buffer_m, city_bbox["xmax"] + 1.3*buffer_m)
ylim_zoom <- c(city_bbox["ymin"] - 0.9*buffer_m, city_bbox["ymax"] + 0.9*buffer_m)

# Transform your other layers if not already in 3857

# Plot
ggplot() +
  basemap_gglayer(ext,
                  map_service = "esri",
                  map_type = "world_imagery") +
  basemap_gglayer(ext,
                  map_service = "esri",
                  map_type = "world_hillshade",
                  alpha = 0.5) +
  geom_sf(data = ext, color = NA, fill = "beige", alpha = 0.5) +
  geom_sf(data = available_segments_mean_annual_crossings, color = "black", inherit.aes = FALSE, linewidth = .8) +
  geom_sf(data = filter(available_segments_mean_annual_crossings, mean.crossings == 0),
          color = "gray45", fill = NA, linewidth = .5, inherit.aes = FALSE) +
  geom_sf(data = filter(available_segments_mean_annual_crossings, mean.crossings > 0),
          aes(color = mean.crossings), linewidth = .5, inherit.aes = FALSE) +
  geom_sf_text(data = cities, aes(label = name),
               size = 3, color = "black", fontface = "bold") +
  scale_color_gradientn(colors = trimmed,
                        name = "Mean annual\ncrossings/lion") +
  
  # City labels
  geom_sf_text(data = cities, aes(label = name),
               size = 3, color = "black", fontface = "bold") +
  
  # Zoom to city extent
  coord_sf(xlim = xlim_zoom,
           ylim = ylim_zoom,
           expand = FALSE) +
  
  # Theme and legend styling
  theme_minimal() +
  scale_fill_identity() +
  labs(x = NULL, y = NULL) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.85),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.key = element_rect(fill = "white", color = "black", linewidth = 0.5),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) +
  
  # North arrow & scale bar
  annotation_north_arrow(location = "br",
                         which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.3)

ggsave(here("figures/mean_annual_crossings_per_lion_core_study_area.png"), dpi = 300, width = 8)

