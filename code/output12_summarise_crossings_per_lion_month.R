



library(tidyverse)
library(here)
library(sf)
library(raster)
library(terra)
library(tidyterra)
#library(igraph)

options(scipen = 999)

source(here("code/utilities.R"))
source(here("code/helper_data.R"))
#

segments_in_combined_homeranges <- readRDS(here("data/segments_in_combined_homeranges")) %>% 
  select(seg.label, geometry)



monthly_seg_crossings_naive_roads_only_0s <- readRDS(here("data/analysis_inputs/monthly_seg_crossings_naive_roads_only_0s"))



average_crossings_per_lion_month <- monthly_seg_crossings_naive_roads_only_0s %>% 
  group_by(seg.label) %>% 
  summarise(mean.raw.crossing.per.lion.month = mean(monthly.seg.raw.crossing),
            max.raw.crossing.per.lion.month = max(monthly.seg.raw.crossing),
            total.raw.crossing = sum(monthly.seg.raw.crossing),
            total.lion.months = n(),
            num.unique.lions = n_distinct(animal.id),
            num.year = n_distinct(year)) %>% 
  ungroup() %>% 
  left_join(segments_in_combined_homeranges)


average_crossings_per_lion_month %>% 
  filter(mean.raw.crossing.per.lion.month > 0) %>%
  summarise(xx = quantile(mean.raw.crossing.per.lion.month, 0.9))
  


average_crossings_per_lion_month %>% 
  filter(mean.raw.crossing.per.lion.month > 0.8) %>% 
  view()


average_crossings_per_lion_month %>% 
  filter(mean.raw.crossing.per.lion.month > 0) %>% 
st_write(here("data/shapefiles/average_crossings_per_lion_month.shp"), append=FALSE)

average_crossings_per_lion_month %>% 
  filter(mean.raw.crossing.per.lion.month == 0) %>% 
  st_write(here("data/shapefiles/uncrossed_segments.shp"))
