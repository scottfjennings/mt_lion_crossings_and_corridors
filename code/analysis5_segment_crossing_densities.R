
library(tidyverse)
library(here)
library(sf)

options(scipen = 999)

source(here("code/utilities.R"))


all_bbmm_road_slices <- readRDS(here("model_objects/crossed_bbmm_roads_1step")) %>% 
  bind_rows(., .id = "crossing.step") 


bbmm_crossed_equal_seg <- readRDS(here("data/bbmm_crossed_equal_seg")) %>% 
  bind_rows(., .id = "crossing.step") %>% 
  select(-X, -seg.length) %>% 
  mutate(crossed.seg.length = st_length(geometry))


napa_sonoma_rds_equal_segs <- readRDS(here("data/napa_sonoma_rds_equal_segs")) %>% 
  bind_rows() %>% 
  rename("road.seg.length" = seg.length)

crossing_years <- readRDS(here("data/crossing_clusters_gps_1step")) %>% 
  mutate(year = year(datetime.local)) %>% 
  distinct(crossing.step, year)

# create a weighted crossing value based on the proportion of the equal length segment that was included in the BBMM UD
wt_road_crossed_segs <- bbmm_crossed_equal_seg %>% 
  left_join(napa_sonoma_rds_equal_segs %>% 
                                 data.frame() %>% 
                                 select(-geometry)) %>% 
  mutate(raw.crossing = 1,
         weighted.crossing = as.numeric(crossed.seg.length/road.seg.length)) %>% 
  full_join(crossing_years) %>% 
  arrange(label.city, seg.label)  %>% 
  mutate(animal.id = str_extract(crossing.step, "^[^_]+(?=_)"))
  
saveRDS(wt_road_crossed_segs, here("data/wt_road_crossed_segs"))


ggplot(road_crossed_segs) +
  geom_density(aes(x = weighted.crossing))

# zsegment = "Buhman Ave_1"


crossings_per_segment_crossed <- road_crossed_segs %>% 
  data.frame() %>% 
  select(-geometry) %>% 
  group_by(seg.label) %>% 
  summarise(raw.crossings.per.segment = sum(raw.crossing),
            weighted.crossings.per.segment = sum(weighted.crossing)) %>% 
  ungroup() 

crossings_per_segment_all <- crossings_per_segment_crossed %>% 
  full_join(napa_sonoma_rds_equal_segs) %>% 
  mutate(across(contains("crossings.per.segment"), ~replace_na(., 0))) %>% 
  st_as_sf()


ggplot() +
  geom_sf(data = filter(crossings_per_segment_all, weighted.crossings.per.segment == 0), color = "gray") +
  geom_sf(data = filter(crossings_per_segment_all, weighted.crossings.per.segment > 0), aes(color = weighted.crossings.per.segment))


  
