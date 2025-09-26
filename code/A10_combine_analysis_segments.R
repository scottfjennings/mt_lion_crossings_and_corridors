

# this script builds up a table of all used and available road segments each year
# also builds up the polygons for all the buffer distances around each segment. do this once and save to disk for less computing time


library(terra)
library(sf)
library(landscapemetrics)
library(here)
library(tidyverse)

source(here("code/utilities.R"))

# first the available road segments
# this is the equal length segments in the 100% a-LoCoH homerange polygon for the analysis lions
# from A9_make_road_segments.R
# want a record for each segment each year, but to save computing time extracting habita variables  want to exclude dupicate segments where home ranges overlap


segments_in_lion_year_homeranges_100 <- readRDS(here("data/segments_in_lion_year_homeranges_100"))


# for extracting habitat variables where lion home ranges overlap, only need one record per year for each segment, 
# so add in lions_years to ensure have all required years,
# then remove animal.id and run distinct()
# because this starts out with only the valid lion-year combos, this effectively makes a different "available" combined polygon for each year, which is good
available_segments_years <- segments_in_lion_year_homeranges_100  %>% 
  select(-animal.id) %>%
  group_by(seg.label, year) %>% 
  # some segments for different lions have slightly different geometries so the distinct below doesn't fully work. this summarise fixes that
  summarise(geometry = st_union(geometry)) %>% 
  ungroup() %>% 
  distinct() %>% 
  st_transform(crs = 26910) %>% 
  st_as_sf() %>% 
  mutate(used.available = 0,
         seg.length = st_length(.))
# seg.length now may be shorter than the original equal length if the year specific home ranges only capture a portion of it
any(st_is_empty(available_segments_years))


# for actual analysis want all segment-lion-year combos

available_segments_lions_years <- segments_in_lion_year_homeranges_100 %>%
  distinct() %>% 
  st_transform(crs = 26910) %>% 
  st_as_sf() %>% 
  mutate(used.available = 0,
         seg.length = st_length(.))


# crossing all segments in the combined home range polygon with all years in the study makes about 2x segmentXyear combos that need habitat values extracted at each buffer distance
#segments_in_combined_homeranges <- readRDS(here("data/segments_in_combined_homeranges")) %>% 
#  st_transform(crs = 26910) %>% 
#  crossing(year = seq(2016, 2023)) %>% 
#  st_as_sf() %>% 
#  mutate(used.available = 0)

# next the used road segments
# this is the segments of road inside the BBMM UD for each crossing step
# from A8_all_roads_in_BBMM_UD.R
# need to create a label ID equivalent to seg.label for equal length segments
# need to also add year for each crossing step
all_bbmm_roads_df_naive <- readRDS(here("data/all_bbmm_roads_df_naive")) %>% 
  filter(!animal.id %in% c("P21", "P30", "P34")) %>%  
  filter(naive.rd == TRUE) %>% 
  filter(crossings.per.naive.rd == 1) %>% 
  mutate(seg.label = paste(road.label, crossing.step, sep = "_")) %>% 
  st_transform(crs = 26910) %>% 
  mutate(used.available = 1)

# get the year that each crossing step happened
steps_years <- readRDS(here("data/naive_crossings_napa_sonoma_2hr")) %>% 
  left_join(readRDS(here("data/puma_steps"))) %>% 
  mutate(year = year(datetime.local)) %>% 
  data.frame() %>% 
  select(crossing.step = step.id, year) %>% 
  distinct()

used_segments_years <- all_bbmm_roads_df_naive  %>% 
  select(crossing.step, seg.label, seg.length = crossed.seg.length, used.available, geometry, naive.rd, crossings.per.naive.rd) %>% 
  left_join(steps_years) # need left_join because naive_crossings has steps that BBMM couldn't be calculated for
  

all_analysis_segments <- bind_rows(used_segments_years, available_segments_years)

all_analysis_segments_by_lion_year <- used_segments_years %>% 
  animal.id_from_crossing.step() %>% 
  bind_rows(available_segments_lions_years)


# checking
# any empty geometries? should be FALSE
# pretty sure I fixed these by usign left join above to exclude segments with no BBMM calculated, but still double checking
any(st_is_empty(all_analysis_segments))

# should only be 1 seg.label per year
all_analysis_segments %>% 
  data.frame() %>% 
  count(seg.label, year) %>% 
  filter(n > 1)

# should only be 1 seg.label per year per lion
all_analysis_segments_by_lion_year %>% 
  data.frame() %>% 
  count(seg.label, animal.id, year) %>% 
  filter(n > 1)


# and a plot to see if anything looks funky
ggplot() +
  geom_sf(data = all_analysis_segments %>% filter(year == 2022), aes(color = as.factor(used.available))) +
  facet_wrap(~used.available)


saveRDS(all_analysis_segments, here("data/all_analysis_segments"))


saveRDS(all_analysis_segments_by_lion_year, here("data/all_analysis_segments_by_lion_year"))



all_analysis_segments %>% 
#readRDS(here("data/all_analysis_segments")) %>% 
  st_write(here("data/shapefiles/all_analysis_segments.shp"), append = FALSE)



