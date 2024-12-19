


library(tidyverse)
library(here)
library(sf)
library(ctmm)
library(ggmap)

source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/google_api_key.R")
source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/stadiamaps_api_key.R")


# crossings ----
# crossings maps data prep ----
summed_crossing_analysis_table <- readRDS(here("data/habitat_varbs_scales"))


HR_UDS <- readRDS(here("model_objects/puma_hr_uds"))


all_bbmm_ud <- readRDS(here("model_objects/all_bbmm_ud_1step"))

napa_sonoma_rds_equal_segs <- readRDS(here("data/napa_sonoma_rds_equal_segs")) %>% 
  bind_rows() %>% 
  rename("road.seg.length" = seg.length)


equal_segs_crossings <- full_join(summed_crossing_analysis_table, napa_sonoma_rds_equal_segs) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326)

clean_bridges <- st_read("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_sonoma_bridges/clean_bridges.shp") %>% 
  st_transform(crs = 4326)


# top 10th percentile crossings
# P1 2018 ----
p1_2018 <- filter(equal_segs_crossings, animal.id == "P1", year == 2018)
p1_hr <- SpatialPolygonsDataFrame.UD(HR_UDS[["P1"]],level.UD=0.999,level=0.95) %>% 
  st_as_sf() %>% 
  filter(str_detect(name, "high")) %>% 
  st_transform(crs = 4326)

p1_bbob <- st_bbox(p1_hr)


p1_bridges <- st_intersection(p1_hr, clean_bridges)

p1_map <- get_stadiamap(c(left = -122.75, bottom = 38.25, right = -122.4, top = 38.5), maptype = "stamen_terrain")
p1_map <- get_googlemap(center = c(lon = -122.533, lat = 38.365), zoom = 14, maptype = "satellite")

ggmap(p1_map)+
#ggplot()+
  geom_sf(data = p1_hr, inherit.aes = FALSE, fill = NA, linewidth = 1) +
  geom_sf(data = p1_2018 %>% filter(str_detect(label.city, "Hwy 12 |Hwy 12_|Fremont Dr_Sonoma|Broadway_Sonoma"), !str_detect(seg.label, "Fremont Dr_Sonoma_1")), linewidth = 2, inherit.aes = FALSE) +
  geom_sf(data = p1_2018, aes(color = tot.wt.cross), linewidth = 1, inherit.aes = FALSE) +
  geom_sf(data = p1_bridges, inherit.aes = FALSE, color = "#08519C", size = 2) +
  scale_color_gradient(low = "#FC9272", high = "#67000D") +
  labs(x = "",
       y = "",
       color = "crossings/\nsegment",
       title = "Mt Lion P1 in 2018")+
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.title=element_text(size=18),
        legend.text=element_text(size=14),
        plot.title = element_text(size=22))

ggsave(here("figures/p1_2018_map.png"), dpi = 300, width = 8, height = 7)

p1_map_zoom <- get_googlemap(center = c(lon = -122.533, lat = 38.365), zoom = 14, maptype = "satellite")

ggmap(p1_map_zoom)+
  #ggplot()+
  geom_sf(data = p1_hr, inherit.aes = FALSE, fill = NA, linewidth = 1) +
  geom_sf(data = p1_2018, aes(color = tot.wt.cross), linewidth = 3, inherit.aes = FALSE) +
  geom_sf(data = p1_bridges, inherit.aes = FALSE, color = "#08519C", size = 5) +
  scale_color_gradient(low = "#FC9272", high = "#67000D", guide="none") +
  labs(x = "",
       y = "",
       color = "",
       title = "")+
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(size=22))

ggsave(here("figures/p1_2018_map_zoom.png"), dpi = 300)

# P5 2018 ----
p5_2018 <- filter(equal_segs_crossings, animal.id == "P5", year == 2018)
p5_hr <- ctmm::SpatialPolygonsDataFrame.UD(HR_UDS[["P5"]],level.UD=0.999,level=0.95) %>% 
  st_as_sf() %>% 
  filter(str_detect(name, "high")) %>% 
  st_transform(crs = 4326)

p5_bbob <- st_bbox(p5_hr)


p5_bridges <- st_intersection(p5_hr, clean_bridges)

p5_map <- get_stadiamap(c(left = -122.8, bottom = 38.2, right = -122.3, top = 38.6), maptype = "stamen_terrain")

ggmap(p5_map)+
  #ggplot()+
  geom_sf(data = p5_hr, inherit.aes = FALSE, fill = NA, linewidth = 1) +
  geom_sf(data = p5_2018 %>% filter(str_detect(label.city, "Hwy 12 |Hwy 12_|Fremont Dr_Sonoma|Broadway_Sonoma"), !str_detect(seg.label, "Fremont Dr_Sonoma_1")), linewidth = 2, inherit.aes = FALSE) +
  geom_sf(data = p5_2018, aes(color = tot.wt.cross), linewidth = 1, inherit.aes = FALSE) +
  #geom_sf(data = p5_bridges, inherit.aes = FALSE, color = "#08519C", size = 3) +
  scale_color_gradient(low = "#FC9272", high = "#67000D") +
  labs(x = "",
       y = "",
       color = "crossings/\nsegment",
       title = "Mt Lion P5 in 2018")+
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.title=element_text(size=18),
        legend.text=element_text(size=14),
        plot.title = element_text(size=22))

ggsave(here("figures/p5_2018_map.png"), dpi = 300)


# P13 2023 ----
p13_2023 <- filter(equal_segs_crossings, animal.id == "P13", year == 2023)
p13_hr <- ctmm::SpatialPolygonsDataFrame.UD(HR_UDS[["P13"]],level.UD=0.999,level=0.95) %>% 
  st_as_sf() %>% 
  filter(str_detect(name, "high")) %>% 
  st_transform(crs = 4326)

p13_bbob <- st_bbox(p13_hr)


p13_bridges <- st_intersection(p13_hr, clean_bridges)

p13_map <- get_stadiamap(c(left = -122.8, bottom = 38.1, right = -122.3, top = 38.5), maptype = "stamen_terrain")

ggmap(p13_map)+
  #ggplot()+
  geom_sf(data = p13_hr, inherit.aes = FALSE, fill = NA, linewidth = 1) +
  geom_sf(data = p13_2023 %>% filter(str_detect(label.city, "Hwy 12 |Hwy 12_|Fremont Dr_Sonoma|Broadway_Sonoma"), !str_detect(seg.label, "Fremont Dr_Sonoma_1")), linewidth = 2, inherit.aes = FALSE) +
  geom_sf(data = p13_2023, aes(color = tot.wt.cross), linewidth = 1, inherit.aes = FALSE) +
  geom_sf(data = p13_bridges, inherit.aes = FALSE, color = "#08519C", size = 3) +
  scale_color_gradient(low = "#FC9272", high = "#67000D") +
  labs(x = "",
       y = "",
       color = "crossings/\nsegment",
       title = "Mt Lion P13 in 2023")+
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.title=element_text(size=18),
        legend.text=element_text(size=14),
        plot.title = element_text(size=22))

ggsave(here("figures/p5_2018_map.png"), dpi = 300)



# study area map ----

study_area_counties <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/data/study_area_counties") %>% 
  st_transform(crs = 4326) %>% 
  filter(NAME %in% c("Sonoma", "Napa"))

study_area_map <- get_googlemap(center = c(lon = -122.8, lat = 38.365), zoom = 9, maptype = "terrain")

ggmap(study_area_map)+
#ggplot()+
  geom_sf(data = study_area_counties, aes(color = NAME), inherit.aes = FALSE, fill = NA, linewidth = 2)+
  scale_color_brewer(palette = "Set1") +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.title=element_text(size=18),
        legend.text=element_text(size=14),
        plot.title = element_text(size=22)) +
  labs(color = "")
ggsave(here("figures/study_area.png"), dpi = 300)



# mortality locations ----

study_area_counties <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/data/study_area_counties") %>% 
  st_transform(crs = 4326) %>% 
  filter(NAME %in% c("Sonoma", "Napa"))

mort_map <- get_googlemap(center = c(lon = -122.8, lat = 38.365), zoom = 9, maptype = "roadmap")

mortalities <- read.csv("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/data/North Bay Puma Roadkill Data.csv") %>% 
  st_as_sf(x = .,
           coords = c("long", "lat"),
           crs = 4326) %>% 
  filter(county %in% c("Sonoma", "Napa"))


napa_sonoma_rds_equal_segs <- readRDS(here("data/napa_sonoma_rds_equal_segs")) %>% 
  bind_rows() %>% 
  rename("road.seg.length" = seg.length)

# ggmap(mort_map) +
ggplot() +
  geom_sf(data = study_area_counties, inherit.aes = FALSE, fill = NA) +
  geom_sf(data = napa_sonoma_rds_equal_segs, inherit.aes = FALSE, alpha = 0.5) +
  geom_sf(data = mortalities, aes(color = county), inherit.aes = FALSE, size = 3) +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position = c(.15,.25)) +
  labs(color = "Mt lion vehicle\ncollision mortality\n2015-2022") +
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))

ggsave(here("figures/roadkill_locations_map.png"), dpi = 300, width = 6, height = 5)

