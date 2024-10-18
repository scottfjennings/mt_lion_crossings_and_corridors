


library(tidyverse)
library(here)
library(sf)
library(ggmap)

source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/google_api_key.R")

summed_crossing_analysis_table <- readRDS(here("data/habitat_varbs_scales"))


napa_sonoma_rds_equal_segs <- readRDS(here("data/napa_sonoma_rds_equal_segs")) %>% 
  bind_rows() %>% 
  rename("road.seg.length" = seg.length)


equal_segs_crossings <- full_join(summed_crossing_analysis_table, napa_sonoma_rds_equal_segs) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326)


p1_2018 <- filter(equal_segs_crossings, animal.id == "P1", year == 2018)


map <- get_googlemap("Santa Rosa, USA", zoom = 8, maptype = "terrain")

ggmap(map) +
  geom_sf(data = p1_2018, aes(color = tot.wt.cross), linewidth = 3)
