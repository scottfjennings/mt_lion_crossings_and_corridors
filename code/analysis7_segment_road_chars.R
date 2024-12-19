



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
# I used Add Spatial Join tool in ArcGIS Pro to add road class and other attributes to the equal length segment object.
# then exported it as a shapefile
napa_sonoma_rds_equal_segs_shp <- st_read(here("data/napa_sonoma_rds_equal_segs.shp"))
# for some reason the shp doesn't read into R with the column names
names(napa_sonoma_rds_equal_segs_shp) <- c("label", "label.city", "seg.length", "seg.label", "OBJECTID.1", "join.count", "target.fid", "objectid", "label.2", "leftcity", "speedlimit", "lanes", "surface", "class", "pubpriv", "county", "shape.length", "geometry")

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
