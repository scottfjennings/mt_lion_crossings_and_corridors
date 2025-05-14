



### DO NOT RUN THIS WHOLE SCRIPT, THERE ARE OPTIONAL "NO RUN" CHUNKS THAT WILL OVERWRITE A MANUALLY EDITED FILE


library(tidyverse)
library(here)
library(sf)
library(ctmm)
library(units)

options(scipen = 999)

source(here("code/utilities.R"))
source(here("code/helper_data.R"))

# county boundaries for filtering statewide layers
study_area_counties <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/data/study_area_counties") %>% 
  st_transform(crs = 26910) %>% 
  filter(NAME %in% c("Sonoma", "Napa"))


# roads

# need final_cleaned_road_layer objects >500m to filter naive_crossings
final_cleaned_road_layer <- readRDS(here("data/final_cleaned_road_layer")) %>% 
  st_transform(crs = 26910)  %>%  # to UTM 10N
  mutate(road.label = paste(label, leftcity, city.road.num, sep = "_"),
         road.length = st_length(.)) 


napa_sonoma_rds_equal_segs <- readRDS(here("data/napa_sonoma_rds_equal_segs"))

napa_sonoma_rds_equal_segs <- napa_sonoma_rds_equal_segs_shp %>% 
  select(label.city, seg.length, seg.label, leftcity, speedlimit, lanes, surface, class, pubpriv, county, geometry) %>% 
#napa_sonoma_rds_equal_segs <- readRDS(here("data/napa_sonoma_rds_equal_segs")) %>% 
#  bind_rows() %>% 
  rename("road.seg.length" = seg.length) %>% 
  mutate(label.city = abbreviate_roads(label.city),
         seg.label = abbreviate_roads(seg.label))




#
# want some variable to represent traffic conditions ----
# number of lanes and speed limit data mostly incomplete so not using
# road class may be a crude proxy for traffic conditions, but is currently the best I have

equal_length_chars_summarized <- equal_length_chars %>% 
  data.frame() %>% 
  group_by(seg.label) %>% 
  summarise(min.sl = min(speedlimit, na.rm = TRUE),
            mean.sl = mean(speedlimit, na.rm = TRUE),
            max.sl = max(speedlimit, na.rm = TRUE),
            min.lanes = min(lanes, na.rm = TRUE),
            mean.lanes = mean(lanes, na.rm = TRUE),
            max.lanes = max(lanes, na.rm = TRUE),
            num.dup = n()) %>% 
  ungroup()

saveRDS(equal_length_chars_summarized, here("data/equal_length_chars_summarized"))


equal_length_classes <- equal_length_chars %>% 
  data.frame() %>% 
  distinct(seg.label, road.seg.length, class) %>% 
  mutate(class = factor(class, levels = c("Collector", "Arterial", "Highway", "Freeway"))) %>% 
  group_by(seg.label, road.seg.length) %>% 
  summarise(classes = paste(unique(class), collapse = ", "),
            max.class = max(as.numeric(class))) %>% 
  ungroup() 


saveRDS(equal_length_classes, here("data/equal_length_classes"))




ggplot() +
  geom_sf(data = filter(napa_sonoma_rds_equal_segs, label == "Dutton Meadow"), aes(color = seg.label), linewidth = 3) +
  geom_sf(data = filter(napa_sonoma_rds_utm, label == "Dutton Meadow"))
