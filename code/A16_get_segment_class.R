


library(tidyverse)
library(sf)
library(here)

# get the midpoint of each segment ----
# just need one segment per year, don't need one segment per lion per year

all_analysis_segments <- readRDS(here("data/all_analysis_segments"))


all_analysis_segments %>%
  st_geometry_type() %>%
  table()



all_analysis_segments_midpoints <- all_analysis_segments %>%
  group_by(seg.label, year) %>%
  summarise() %>%
  st_cast("LINESTRING") %>%
  mutate(midpoint = st_line_sample(geometry, sample = 0.5, type = "regular")) %>%
  st_as_sf()


all_analysis_segments_midpoints_clean <- all_analysis_segments_midpoints %>% 
  data.frame() %>% 
  select(seg.label, year, midpoint) %>% 
  rename(geometry = midpoint) %>% 
  st_as_sf() %>% 
  st_transform(crs = 26910)


zseg = "Pythian Ct_Santa Rosa_1_P1_11082_432"

ggplot() +
  geom_sf(data = all_analysis_segments %>% filter(seg.label == zseg)) +
  geom_sf(data = all_analysis_segments_midpoints_clean %>% filter(seg.label == zseg))


# read table that has the road class ----
napa_sonoma_rds_filtered <-readRDS(here("data/napa_sonoma_rds_filtered")) %>% 
  st_transform(crs = 26910)



# make a small buffer around the midpoints in case they don't fall exactly on the roads ----
all_analysis_segments_midpoints_clean_buff1 <- all_analysis_segments_midpoints_clean %>% 
  st_buffer(., 1) 


all_analysis_segments_class <- st_join(all_analysis_segments_midpoints_clean_buff1, napa_sonoma_rds_filtered) %>% 
  rowwise() %>% # grepl in the next step doesn't work right without rowwise() but doesn't throw error
  mutate(correct.road = str_detect(seg.label, label))


all_analysis_segments_class_clean <- all_analysis_segments_class %>% 
  filter(correct.road == TRUE) %>% 
  data.frame() %>% 
  distinct(seg.label, year, class) %>% 
  full_join(all_analysis_segments)


saveRDS(all_analysis_segments_class_clean, here("data/all_analysis_segments_class_clean"))


# Sep 16, 2025 quick fix
# today I realized I extracted habitat covariates for all BBMM roads, not just the naive roads, but I only want to analyze the naive roads.
# I remade here("data/all_analysis_segments") to just be the naive roads and just crossings that crossed the naive roads once
# but I don't want to re-extract habitat values because that takes many hours of computing
# so just doing a filtering join witht he new all_analysis_segments as a quick fix

all_analysis_segments_class_clean <- readRDS(here("data/all_analysis_segments_class_clean"))


all_analysis_segments_class_clean <- all_analysis_segments_class_clean %>% 
  right_join(readRDS(here("data/all_analysis_segments")))

saveRDS(all_analysis_segments_class_clean, here("data/all_analysis_segments_class_clean"))



