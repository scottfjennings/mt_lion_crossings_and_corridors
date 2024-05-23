

library(tidyverse)
library(ggnewscale)
library(here)
library(sf)
#library(amt)

source(here("code/utilities.R"))
# need to make a spatial line object of each pair of points
# then overlay that on the roads

# read data ----
# steps
puma_steps <- readRDS(here("data/puma_steps"))%>% 
  #  filter(between(as.numeric(step.dur), 2700, 43200)) %>%  # 45 min and 12 hr
  filter(between(as.numeric(step.dur), 6600, 7800)) # 110 min and 130 min 
# limiting data to just steps 120 +- 20 min to just consider movement behavior near the time of the road crossing

# road layer in UTM
napa_sonoma_rds_utm <- readRDS(here("data/napa_sonoma_rds_utm")) %>% 
  filter(class %in% keep_road_classes)

# find which lion steps crossed roads ----


get_naive_rd_cross <- function(zstep) {
step <- filter(puma_steps, step.id == zstep)

sp_step <- bind_rows(step %>% 
  dplyr::select(step.id, datetime.local, easting, northing),
  step %>% 
    select(step.id, datetime.local = datetime.local.end, easting = easting.end, northing = northing.end)) %>% 
  data.frame()

step_line <- sp_step %>%
  st_as_sf(coords = c("easting", "northing"), crs = 26910) %>%
  group_by(step.id) %>%
  dplyr::summarize(do_union=FALSE) %>%  # do_union=FALSE doesn't work as well
  st_cast("LINESTRING") 

road_slicer <- st_bbox(step_line)  %>% st_as_sfc()

road_slice <- st_intersection(napa_sonoma_rds_utm, road_slicer)

#rd_cross <- st_intersection(step_line, road_slice) 

rd_cross <- st_intersects(step_line, road_slice) 
out_rd_cross <- road_slice[unlist(rd_cross),] %>% 
  mutate(step.id = zstep)

}

xx <- filter(puma_steps, animal.id == "P21", collar.id == 37474)

xx <- puma_steps[1:4,]
system.time(
road_crossing_steps <- map_df(puma_steps$step.id, get_naive_rd_cross), gcFirst = TRUE
)

# road_crossing_steps$geometry is the geometry of the road segment that is along the direct line of the step, not the step
saveRDS(road_crossing_steps, here("data/road_crossing_steps_napa_sonoma_2hr"))

road_crossing_steps <- readRDS(here("data/road_crossing_steps_napa_sonoma_2hr"))

