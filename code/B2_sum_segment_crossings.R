


# this script is intended to check the crossings to see if any should be filtered out of the analysis
# the result is to add columns which may be used to filter crossing steps for various analysis iterations

# currently adding:
# number of roads each step naively crossed: num_naive_crossings
#     allows filtering for just 1-2 overall crossed roads per step
#     this does not indicate how many times each road was crossed, just how many roads were crossed
# number of times each segment was crossed: num_naive_seg_crossings
#     allows filtering to exclude even number of crossings which may not be true crossings
# whether a segment in the BBMM UD is part of the original naively crossed road 
#     allows further filtering to just the crossed road segment, rather than all segments in the BBMM UD


library(tidyverse)
library(here)
library(sf)
library(raster)
library(terra)
library(tidyterra)
library(igraph)

options(scipen = 999)

source(here("code/utilities.R"))
#

# "P11_32188_16425"

# need final_cleaned_road_layer objects >500m to filter naive_crossings
final_cleaned_road_layer <- readRDS(here("data/final_cleaned_road_layer")) %>% 
  st_transform(crs = 26910)  %>%  # to UTM 10N
  mutate(road.label = paste(label, leftcity, city.road.num, sep = "_"),
         road.length = st_length(.)) 


naive_crossings <- readRDS(here("data/naive_crossings_napa_sonoma_2hr")) %>% 
  rename(crossing.step = step.id) %>% 
  st_transform(crs = 26910) %>% 
  inner_join(final_cleaned_road_layer %>% 
              data.frame() %>% 
              select(road.label, road.length)) %>% 
  filter(as.numeric(road.length) > 500)

# all roads inside the BBMM UD for each crossing step
all_bbmm_roads <- readRDS(here("model_objects/all_bbmm_roads_1step"))

# crossing step GPS fixes in wide (step) format
puma_steps <- readRDS(here("data/puma_steps"))

# the road segments inside each BBMM UD
bbmm_equal_seg_weights <- readRDS(here("data/bbmm_equal_seg_weights")) %>% 
  st_transform(crs = 26910) %>%  # to UTM 10N
  mutate(road.label = sub("_(?:[^_]*)$", "", seg.label))



# for mapping through and filtering
crossing_steps <- bbmm_equal_seg_weights %>% 
  data.frame() %>% 
  distinct(crossing.step)




########## crossing step filter column 1. how many roads does each crossing step cross? ----

# steps crossing many roads require more complicated assumptions about which road's features are impacting the crossing.
# analysis will be cleaner using just steps that cross 1 or 2 roads.
# but this will bias the analysis away from areas with very dense roads so will need to report this.

num_naive_roads <- naive_crossings %>%
  data.frame() %>% 
  select(-geometry) %>% 
  right_join(crossing_steps) %>% # just to filter to the steps with valid BBMM UDs
  group_by(crossing.step) %>% 
  summarise(num.naive.roads = n())

count(num_naive_roads, num.naive.roads)

num_naive_roads %>% 
  filter(num.naive.roads > 2) %>% 
  nrow()
# only 175 steps have >2 crossings so that is a reasonable number to check manually at some point.
# most steps only have 1 or 2 crossings. probably fine to just use these in the analysis to avoid complicated steps with more crossings.

##########  THIS POSSIBLY NOT NEEDED, USE ROAD CROSSINGS BELOW INSTEAD crossing step filtering 2. how many times was each segment (not entire roads) naively crossed ----

# segments crossed an even number of times likely were not true crossings; the cat instead likely e.g. went around the outside of a curve.

# first need to redo the intersection of the step and the roads like in A5_naive_crossing.R, but now using the equal length segments instead of final_clean_road_layer
#' get_naive_segment_crossings
#' 
#' check whhich road segments in each BBMM UD were actually crossed by the straight line of the step
#
#' @param zcrossing.step 
#'
#' @returns list with and element for each crossing step, and for each crossing step returns the geometry for the actual naive crossing location (POINT) for each segment. This contrasts with utilities::get_all_bbmm_roads which returns the geometry for the crossed road (LINESTRING)
#'
#' @examples
get_naive_segment_crossings <- function(zcrossing.step) {
  
  # get the crossing step GPS points. use the original puma_steps for this for easiest filtering of the start and end GPS point for each step
  crossing_step_wide <- filter(puma_steps, step.id == zcrossing.step)
  
  crossing_step <- bind_rows(crossing_step_wide %>% 
                         dplyr::select(step.id, datetime.local, easting, northing),
                         crossing_step_wide %>% 
                         select(step.id, datetime.local = datetime.local.end, easting = easting.end, northing = northing.end)) %>% 
    data.frame() %>% 
    st_as_sf(coords = c("easting", "northing"), crs = 26910)
    
  # make into a line for intersection
  crossing_step_line <- crossing_step %>%
    st_as_sf(coords = c("easting", "northing"), crs = 26910) %>%
    group_by(step.id) %>%
    dplyr::summarize(do_union=FALSE) %>% 
    st_cast("LINESTRING")   
  
  # get all segments that are in this step's BBMM UD
  bbmm_ud_segs <-   bbmm_equal_seg_weights %>% 
    filter(crossing.step == zcrossing.step)
  
 # ggplot() +  geom_sf(data = bbmm_ud_segs, aes(color = seg.label)) + geom_sf(data = crossing_step_line, color = "red")   
  # intersect the crossing step with the road segments
  naive_crossed_segs <- st_intersection(crossing_step_line, bbmm_ud_segs) %>%
    rowwise() %>% 
    mutate(geometry = list(st_cast(geometry, "POINT"))) %>% 
    unnest(geometry) %>% 
    st_as_sf()
    
  return(naive_crossed_segs)
}



system.time(
naive_crossed_segs_safe <- map(crossing_steps$crossing.step, safely(get_naive_segment_crossings))
) # 124, 168
names(naive_crossed_segs_safe) <- crossing_steps$crossing.step

# naive_crossings was made on the full final_cleaned_road_layer, so we have some crossings of roads <500m. these errors seem to be from those crossings 
naive_crossed_segs_error <- map(naive_crossed_segs_safe, "error")[!sapply(map(naive_crossed_segs_safe, "error"), is.null)]

# filter out errors from the results
naive_crossed_segs <- map(naive_crossed_segs_safe, "result")[sapply(map(naive_crossed_segs_safe, "error"), is.null)]

naive_crossed_segs_df <- naive_crossed_segs %>% 
  bind_rows()


# now calculate how many crossings of each naive crossed segment there are. 
# odd # crossings per rd likely true crossings
# even number likely went around

num_naive_seg_crossings <- naive_crossed_segs_df %>%
  data.frame() %>% 
  select(-geometry) %>% 
  right_join(crossing_steps) %>% # just to filter to the steps with valid BBMM UDs
  group_by(crossing.step, seg.label) %>% 
  summarise(num.naive.seg.crossings = n()) %>% 
  ungroup()

count(num_naive_seg_crossings, num.naive.seg.crossings)


########## crossing step filtering 3. how many times was each road naively crossed ----

# roads crossed an even number of times likely were not true crossings; the cat instead likely e.g. went around the outside of a curve.
# and, roads crossed >2 time actually have an even # of crossings embedded within the total # crossings, so possibly these should be excluded too

# first need to redo the intersection of the step and the roads like in A5_naive_crossing.R, but now using the equal length segments instead of final_clean_road_layer
#' get_naive_road_crossings
#' 
#' check which road in each BBMM UD were actually crossed by the straight line of the step
#
#' @param zcrossing.step 
#'
#' @returns list with and element for each crossing step, and for each crossing step returns the geometry for the actual naive crossing location (POINT) for each road. This contrasts with utilities::get_all_bbmm_roads which returns the geometry for the crossed road (LINESTRING)
#'
#' @examples
get_naive_road_crossings <- function(zcrossing.step) {
  
  # get the crossing step GPS points. use the original puma_steps for this for easiest filtering of the start and end GPS point for each step
  crossing_step_wide <- filter(puma_steps, step.id == zcrossing.step)
  
  crossing_step <- bind_rows(crossing_step_wide %>% 
                               dplyr::select(step.id, datetime.local, easting, northing),
                             crossing_step_wide %>% 
                               select(step.id, datetime.local = datetime.local.end, easting = easting.end, northing = northing.end)) %>% 
    data.frame() %>% 
    st_as_sf(coords = c("easting", "northing"), crs = 26910)
  
  # make into a line for intersection
  crossing_step_line <- crossing_step %>%
    st_as_sf(coords = c("easting", "northing"), crs = 26910) %>%
    group_by(step.id) %>%
    dplyr::summarize(do_union=FALSE) %>% 
    st_cast("LINESTRING")   
  
  # get all roads that are in this step's BBMM UD
  bbmm_ud_roads <-   bbmm_equal_seg_weights %>% 
    filter(crossing.step == zcrossing.step) %>% 
    group_by(crossing.step, road.label) %>% 
    summarise() %>% 
    ungroup()
     
  # intersect the crossing step with the road segments
  naive_road_crossings <- st_intersection(crossing_step_line, bbmm_ud_roads) %>%
    rowwise() %>% 
    mutate(geometry = list(st_cast(geometry, "POINT"))) %>% 
    unnest(geometry) %>% 
    st_as_sf()
  
  
  # ggplot() +  geom_sf(data = bbmm_ud_roads, aes(color = road.label), linewidth = 3) + geom_sf(data = crossing_step_line, color = "red") + geom_sf(data = naive_road_crossings)
  
  
  return(naive_road_crossings)
}


system.time(
  naive_road_crossings_safe <- map(crossing_steps$crossing.step, safely(get_naive_road_crossings))
) # 478
names(naive_road_crossings_safe) <- crossing_steps$crossing.step

# naive_crossings was made on the full final_cleaned_road_layer, so we have some crossings of roads <500m. these errors seem to be from those crossings 
naive_road_crossings_error <- map(naive_road_crossings_safe, "error")[!sapply(map(naive_road_crossings_safe, "error"), is.null)]

# filter out errors from the results
naive_road_crossings <- map(naive_road_crossings_safe, "result")[sapply(map(naive_road_crossings_safe, "error"), is.null)]

naive_road_crossings_df <- naive_road_crossings %>% 
  bind_rows()


# now calculate how many crossings of each naive crossed segment there are. 
# odd # crossings per rd likely true crossings
# even number likely went around

num_naive_road_crossings <- naive_road_crossings_df %>%
  data.frame() %>% 
  select(-geometry) %>% 
#  right_join(crossing_steps) %>% # just to filter to the steps with valid BBMM UDs
  group_by(crossing.step, road.label) %>% 
  summarise(num.naive.road.crossings = n()) %>% 
  ungroup()

count(num_naive_road_crossings, num.naive.road.crossings)


##########  crossing step filtering 4.  how many segments inside the BBMM UD are part of the original naive crossed road ----
# will run the analysis with all roads int he BBMM UD and just with the naively crossed road in the BBMM UD
seg_in_naive_road <- naive_crossings %>% 
  data.frame() %>% 
  distinct(road.label, crossing.step) %>%
  mutate(seg.in.naive.road = TRUE) %>% # so far this is just the roads from naive_crossings
  right_join(bbmm_equal_seg_weights %>% 
               select(crossing.step, road.label, seg.label, sub.seg.label)) %>% # now add in all the segments in each BBMM UD
  arrange(crossing.step)

count(seg_in_naive_road, seg.in.naive.road)


# combine the filtering columns
analysis_steps_roads <- num_naive_roads %>% 
  full_join(seg_in_naive_road %>% 
              data.frame() %>% 
              select(-geometry)) %>% 
  full_join(num_naive_road_crossings) %>% 
  full_join(num_naive_seg_crossings)
  
  


  left_join(naive_crossings %>% select(crossing.step, road.label)) %>% # add back in all the naive crossed roads 
  left_join(seg_in_naive_road %>% select(crossing.step, road.label, seg.label, sub.seg.label, naive.crossed.rd))


count(analysis_steps_roads, num.naive.crossings, naive.crossed.rd)


# plotting function to check how individual crossings look compared to the roads in the BBMM UD ----


# this plots the GPS fixes, underlying roads, and BBMM UD outline raster
final_cleaned_road_layer <- readRDS(here("data/final_cleaned_road_layer")) %>% 
  st_transform(crs = 26910)  %>%  # to UTM 10N
  mutate(road.label = paste(label, leftcity, city.road.num, sep = "_"),
         road.length = st_length(.)) %>% 
  filter(as.numeric(road.length) > 500)


all_ud_rast <- readRDS(here("model_objects/all_ud_rast_1step")) %>% 
  lapply(unwrap) 
crossing_clusters_gps <- readRDS(here("data/crossing_clusters_gps_1step"))


count(bbmm_equal_seg_weights, crossing.step, seg.label) %>% filter(n > 1) %>% view()




# comparing the number of road objects from get_bbmm_crossing and confirm_bbmm_rd_cross

num_objects <- full_join(map_df(all_bbmm_roads, nrow) %>% 
                           pivot_longer(cols = everything(), names_to = "crossing.step", values_to = "all_bbmm_roads"),
                         map_df(crossed_bbmm_roads, nrow) %>% 
                           pivot_longer(cols = everything(), names_to = "crossing.step", values_to = "crossed_bbmm_roads")) %>% 
  mutate(diff.obj = all_bbmm_roads - crossed_bbmm_roads)


#' prob_road_crossing_plotter
#'
#' test plot of probabilistic road crossings
#' @param zcrossing.step 
#'
#' @return ggplot object
#' @details
#' requires all_ud_rast, napa_sonoma_rds_utm, all_ud_trim_to_step, all_bbmm_road_slices, and crossing_clusters_gps to be in the environment
#' currently plots all points in the crossing step cluster, connected by a line, and the crossing step points are colored; the UD; the trimmed UD (green); all roads within the bounding box of the UD (gray); roads within the trimmed UD (blue); and the road sections that the straight line step crossed (red)
#' 
#'
#' @examples
prob_road_crossing_plotter <- function(zcrossing.step) {
  
  # read UD raster  
  rp <- all_ud_rast[[zcrossing.step]]
  
  # clip the road layer to cut down computing time for the main st_intersection below
  road_slicer <- st_bbox(rp)  %>% 
    st_as_sfc() %>% 
    st_transform(crs = 26910) # to UTM 10N
  #road_slice <- st_intersection(napa_sonoma_rds_equal_segs, road_slicer)
  road_slice <- st_intersection(final_cleaned_road_layer, road_slicer)

  naive_rd_names <- naive_crossings %>% 
    filter(crossing.step == zcrossing.step) %>% 
    data.frame() %>% 
    distinct(road.label) %>% 
    summarise(road.label = paste(road.label, collapse = "|"))
  
  bbmm_equal_seg_weights <- filter(bbmm_equal_seg_weights, crossing.step == zcrossing.step, str_detect(seg.label, naive_rd_names$road.label))
  
  
  ggplot2::ggplot() +
    geom_sf(data = rp %>% 
              st_as_sf() %>% 
              st_transform(crs = 26910), fill = "gray", color = NA) +
    geom_sf(data = road_slice, color = "gray20") +
    #geom_sf(data = bbmm_crossed_intersection_seg[[zcrossing.step]] %>% st_as_sf(), aes(color = label.city.2), linewidth = 4, alpha = 0.5) +
    geom_sf(data = filter(bbmm_equal_seg_weights, crossing.step == zcrossing.step) %>% st_as_sf(), color = "gray20", linewidth = 2, alpha = 0.25) +
    geom_sf(data = bbmm_equal_seg_weights, aes(color = seg.label), linewidth = 3) +
    #geom_sf(data = all_bbmm_roads[[zcrossing.step]] %>% st_as_sf(), color = "red", linewidth = 2) +
    geom_path(data = filter(crossing_clusters_gps, crossing.step == zcrossing.step), aes(x = easting, y = northing), color = "#377EB8", linewidth = 1) +
    geom_point(data = filter(crossing_clusters_gps, crossing.step == zcrossing.step), aes(x = easting, y = northing), color = "#377EB8", size = 2) +
    #    geom_point(data = filter(crossing_clusters_gps, crossing.step == zcrossing.step, (step.id == zcrossing.step | lag(step.id) == zcrossing.step)), aes(x = easting, y = northing, color = step.id), size = 4) +
    scale_color_brewer(palette = "Dark2") +
    labs(title = zcrossing.step,
         x = "",
         y = "",
         color = "",
         fill = "90% UD") +
    coord_sf(datum = st_crs(26910))
}

zcrossing.step = "P13_90388_40608"
  prob_road_crossing_plotter("P1_11082_274")

prob_road_crossing_plotter("P13_90388_40336") + 
  guides(colour=FALSE) +
  labs(title = "") +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())
ggsave(here("figures/bbmm_example2a.png"))

# these have split UD with crossed roads going through the gaps
bad_ud_steps <- c("P13_29892_26815", "P16_37473_47485", "P4_23163_122801")

prob_road_crossing_plotter(bad_ud_steps[1])

prob_road_crossing_plotter("P13_37473_38760")
prob_road_crossing_plotter("P1_11082_9")

prob_road_crossing_plotter(crossing_steps[[5]])

# example tricky crossings
# prob_road_crossing_plotter("P1_37472_11128")
# prob_road_crossing_plotter("P1_37472_11324")
# prob_road_crossing_plotter("P13_37473_35018")
# prob_road_crossing_plotter("P13_37473_35901")
# prob_road_crossing_plotter("P16_37473_50936") # wide UD

# works well
# prob_road_crossing_plotter("P16_37473_51563")
# prob_road_crossing_plotter("P36_37570_96042")
prob_road_crossing_plotter("P13_90388_40608") # good example of cutting out road outside the step square
ggsave(here("figures/test_ud_crossings/P13_90388_40608_1step.png"))
prob_road_crossing_plotter("P13_90388_40336") # good example of cutting non-continuous segments that are within the step square
ggsave(here("figures/test_ud_crossings/P13_90388_40336_1step.png"))

# still possible problem
prob_road_crossing_plotter("P1_37472_12518")
ggsave(here("figures/test_ud_crossings/P1_37472_12518_1step.png"))
# prob_road_crossing_plotter("P13_29892_22992")
# prob_road_crossing_plotter("P13_37473_36228")
prob_road_crossing_plotter("P16_37473_47255") # blob UD catches a lot of road
ggsave(here("figures/test_ud_crossings/P16_37473_47255_1step.png"))

# as of 10/11/24 this new method generally seems to do a good job of getting the right looking road
# but naive steps that cross multiple roads are still a problem
# will probably filter those out for now, for TWS and CADFW mt lion working group meeting

# e.g. of probably no actual crossing:
# P1_23163_2264


num_crossings <- naive_crossings %>% 
  data.frame() %>% 
  select(-geometry) %>% 
  distinct(step.id, label.city) %>% 
  group_by(step.id) %>% 
  count()

bbmm_crossed_equal_seg <- readRDS(here("data/bbmm_crossed_equal_seg")) %>% 
  bind_rows()


# funky movements
# prob_road_crossing_plotter("P16_37473_50642")

