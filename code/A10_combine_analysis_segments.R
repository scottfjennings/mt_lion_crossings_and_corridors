

# this script builds up a table of all used and available road segments each year
# also builds up the polygons for all the buffer distances around each segment. do this once and save to disk for less computing time


library(terra)
library(sf)
library(landscapemetrics)
library(here)
library(tidyverse)


# first the available road segments
# this is the equal length (888m) segments in the combined homerange polygon for the analysis lions
# from A9_make_road_segments.R
# expand out to have a record for each segment each year, but to save computing time don't want to include segments in home ranges in years that lion wasn't tracked
lions_years <- naive_crossings <- readRDS(here("data/puma_steps")) %>% 
  mutate(year = year(datetime.local)) %>% 
  data.frame() %>% 
  filter(!animal.id %in% c("P21", "P30", "P34")) %>% 
  distinct(animal.id, year)


segments_in_homeranges <- readRDS(here("data/segments_in_puma_homeranges"))

# where lion home ranges overlap, only need one record per year for each segment, so remove animal.id and run distinct()
available_segments_years <- segments_in_homeranges %>% 
  left_join(lions_years, relationship = "many-to-many") %>% 
  select(-animal.id) %>% 
  distinct()%>% 
  st_transform(crs = 26910) %>% 
  st_as_sf() %>% 
  mutate(used.available = 0)

any(st_is_empty(available_segments_years))

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
  mutate(seg.label = paste(road.label, crossing.step, sep = "_")) %>% 
  st_transform(crs = 26910) %>% 
  mutate(used.available = 1)

# get the year that each crossing step happened
steps_years <- naive_crossings <- readRDS(here("data/naive_crossings_napa_sonoma_2hr")) %>% 
  left_join(readRDS(here("data/puma_steps"))) %>% 
  mutate(year = year(datetime.local)) %>% 
  data.frame() %>% 
  select(crossing.step = step.id, year) %>% 
  distinct()

used_segments_years <- all_bbmm_roads_df_naive  %>% 
  select(crossing.step, seg.label, seg.length = crossed.seg.length, used.available, geometry) %>% 
  left_join(steps_years) # need left_join because naive_crossings has steps that BBMM couldn't be calculated for 
  

all_analysis_segments <- bind_rows(used_segments_years, available_segments_years)

# checking
# any empty geometries? should be FALSE
# pretty sure I fixed these by usign left join above to exclude segments with no BBMM calculated, but still double checking
any(st_is_empty(all_analysis_segments))

# and a plot to see if anything looks funky
ggplot() +
  geom_sf(data = all_analysis_segments %>% filter(year == 2022), aes(color = as.factor(used.available))) +
  facet_wrap(~used.available)


saveRDS(all_analysis_segments, here("data/all_analysis_segments"))


# finally bu
# Set buffer distances
buffers <- c(100, 200, seq(30, 300, by = 30))

# Vectorize road segments (assuming segments_in_combined_homeranges is an sf object)
road_vect <- terra::vect(all_analysis_segments)

# Create a named list of buffers
road_segment_buffers <- purrr::map(buffers, function(b) {
  buf <- sf::st_buffer(all_analysis_segments, dist = b)
  buf_sf <- sf::st_as_sf(buf)
  buf_sf$seg.label <- all_analysis_segments$seg.label
  buf_sf$buffer <- b
  buf_sf
})
names(road_segment_buffers) <- as.character(buffers)

# Optionally save to disk
saveRDS(road_segment_buffers, here("data/precomputed_road_buffers.rds"))
