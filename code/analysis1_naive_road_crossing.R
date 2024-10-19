

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
puma_steps <- readRDS(here("data/puma_steps")) %>% 
  #  filter(between(as.numeric(step.dur), 2700, 43200)) %>%  # 45 min and 12 hr
  filter(between(as.numeric(step.dur), 6600, 7800)) # 110 min and 130 min 
# limiting data to just steps 120 +- 10 min to just consider movement behavior near the time of the road crossing

# road layer in UTM
#napa_sonoma_rds_utm <- readRDS(here("data/napa_sonoma_rds_utm"))

# as of Oct 2024 using the equal length segment road layer from prep_data3_equal_length_road_segments.R
napa_sonoma_rds_equal_segs <- readRDS(here("data/napa_sonoma_rds_equal_segs")) %>% 
  bind_rows(.id = "label") %>% 
  st_as_sf() %>% 
  st_transform(crs = 26910)




# functions ----

# find which road segments were crossed by lion steps ----


# zstep = puma_steps$step.id[1]

get_naive_crossed_rds <- function(zstep) {
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

road_slice <- st_intersection(napa_sonoma_rds_equal_segs, road_slicer)

#rd_cross <- st_intersection(step_line, road_slice) 

rd_cross <- st_intersects(step_line, road_slice) 
out_rd_cross <- road_slice[unlist(rd_cross),] 

out_crossed_roads <- napa_sonoma_rds_equal_segs %>% 
  filter(seg.label %in% out_rd_cross$seg.label) %>% 
  mutate(step.id = zstep)
  
  }

# xx <- filter(puma_steps, animal.id == "P21", collar.id == 37474)

yy <- get_naive_crossed_rds("P13_90388_40336")

# xx <- puma_steps[1:4,]
system.time(
  naive_crossings <- map_df(puma_steps$step.id, get_naive_crossed_rds), gcFirst = TRUE
)
# 7018 seconds for 86064 steps on 5/30/24

# road_crossing_steps$geometry is the geometry of the road segment that is along the direct line of the step, not the step
saveRDS(naive_crossings, here("data/naive_crossings_napa_sonoma_2hr"))

naive_crossings <- readRDS(here("data/naive_crossings_napa_sonoma_2hr"))


