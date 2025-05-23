


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
#library(igraph)

options(scipen = 999)

source(here("code/utilities.R"))
source(here("code/helper_data.R"))
#

# "P11_32188_16425"

# need final_cleaned_road_layer objects >500m to filter naive_crossings
final_cleaned_road_layer <- readRDS(here("data/final_cleaned_road_layer")) %>% 
  st_transform(crs = 26910)  %>%  # to UTM 10N
  mutate(road.label = paste(label, leftcity, city.road.num, sep = "_"),
         road.length = st_length(.)) 

# use this to check if a segment is part of the naive crossed road
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

##########  crossing step filtering 2. how many times was each segment (not entire roads) naively crossed ----

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
  ungroup() %>% 
  filter(!is.na(seg.label))

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
) # 478; 591
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


##########  crossing step filtering 4.  is each segment inside the BBMM UD part of the original naive crossed road? ----
# will run the analysis with all roads in the BBMM UD and just with the naively crossed road in the BBMM UD
seg_in_naive_road <- naive_crossings %>% 
  data.frame() %>% 
  distinct(road.label, crossing.step) %>%
  mutate(seg.in.naive.road = TRUE) %>% # so far this is just the roads from naive_crossings
  right_join(bbmm_equal_seg_weights %>% 
               select(crossing.step, road.label, seg.label, sub.seg.label, raw.crossing, bbmm.segment.weight, class)) %>% # now add in all the segments in each BBMM UD
  arrange(crossing.step)

count(seg_in_naive_road, seg.in.naive.road)


# combine the filtering columns and add year and puma ID ----




# get year and animal ID , and months (added 3/17/25)
pumaid_steps_years_months <- puma_steps %>% 
  mutate(year = year(datetime.local),
         month = month(datetime.local)) %>% 
  distinct(animal.id, year, month, step.id) %>% 
  rename(crossing.step = step.id)


tallied_steps_crossings <- num_naive_roads %>% 
  full_join(seg_in_naive_road %>% 
              data.frame() %>% 
              select(-geometry)) %>% 
  full_join(num_naive_road_crossings) %>% 
  full_join(num_naive_seg_crossings) %>% 
  left_join(pumaid_steps_years_months)

saveRDS(tallied_steps_crossings, here("data/tallied_steps_crossings"))

# build up an analysis input data frame with a record for each road segment in each mt lion's home range, and the number of crossings per mt lion per month ----



# this has each segment ONCE for each mountain lion, but has many 0-crossed segments and also has segments repeated if they are in multiple home ranges
homerange_segments <- readRDS(here("data/analysis_inputs/homerange_segments"))

# this has all the segment classes, not just for home ranges, should have only 1 record per segment
seg_midpoints_road_class <- readRDS(here("data/seg_midpoints_road_class"))
# join those 2
hr_seg_midpoints_road_class <- full_join(homerange_segments, seg_midpoints_road_class) %>% 
  filter(!animal.id %in% hr_exclude_pumas)

# this has multiple segments per lion but no 0-crossed segments
seg_crossing_sums_naive_roads_only <- readRDS(here("data/analysis_inputs/seg_crossing_sums_naive_roads_only")) %>% 
  filter(!animal.id %in% hr_exclude_pumas)


puma_month_years <- seg_crossing_sums_naive_roads_only %>% 
  distinct(animal.id, year, month)

full_seg_puma_month_year <- hr_seg_midpoints_road_class %>% 
  full_join(puma_month_years)



# then finally filter to just the crossings that will be considered in the analysis
# and tally the raw and weighted crossings for each segment for each puma in each year

seg_crossing_sums_naive_segs_only <- tallied_steps_crossings %>% 
  filter(num.naive.roads < 3, num.naive.seg.crossings == 1) %>% 
  group_by(animal.id, year, month, seg.label, class) %>% 
  summarise(seg.raw.crossing = sum(raw.crossing),
            seg.wt.crossing = sum(bbmm.segment.weight)) %>% 
  ungroup() %>% 
  full_join(full_seg_puma_month_year) %>% # joinging them results in more records than either has
  mutate(seg.raw.crossing = replace_na(seg.raw.crossing, 0),
         seg.wt.crossing = replace_na(seg.wt.crossing, 0))


saveRDS(seg_crossing_sums_naive_segs_only, here("data/analysis_inputs/seg_crossing_sums_naive_segs_only"))

  
seg_crossing_sums_naive_roads_only <- tallied_steps_crossings %>% 
  filter(num.naive.roads < 3, num.naive.road.crossings == 1) %>% 
  group_by(animal.id, year, month, seg.label, class) %>% 
  summarise(seg.raw.crossing = sum(raw.crossing),
            seg.wt.crossing = sum(bbmm.segment.weight)) %>% 
  ungroup()%>% 
  full_join(full_seg_puma_month_year) %>% # joinging them results in more records than either has
  mutate(seg.raw.crossing = replace_na(seg.raw.crossing, 0),
         seg.wt.crossing = replace_na(seg.wt.crossing, 0))


saveRDS(seg_crossing_sums_naive_roads_only, here("data/analysis_inputs/seg_crossing_sums_naive_roads_only"))
seg_crossing_sums_naive_roads_only <- readRDS(here("data/analysis_inputs/seg_crossing_sums_naive_roads_only"))


seg_crossing_sums_all_bbmm <- tallied_steps_crossings %>% 
  filter(num.naive.roads < 3) %>% 
  group_by(animal.id, year, month, seg.label, class) %>% 
  summarise(seg.raw.crossing = sum(raw.crossing),
            seg.wt.crossing = sum(bbmm.segment.weight)) %>% 
  ungroup()%>% 
  full_join(full_seg_puma_month_year) %>% # joinging them results in more records than either has
  mutate(seg.raw.crossing = replace_na(seg.raw.crossing, 0),
         seg.wt.crossing = replace_na(seg.wt.crossing, 0))


count(seg_crossing_sums_all_bbmm, animal.id, year) %>% 
  pivot_wider(id_cols = animal.id, names_from = year, values_from = n)
# very few segments for P12, P24, P25

saveRDS(seg_crossing_sums_all_bbmm, here("data/analysis_inputs/seg_crossing_sums_all_bbmm"))


# also tally all crossings across all lions and save as SHP 


tallied_steps_crossings <- readRDS(here("data/tallied_steps_crossings"))


seg_geometries <- readRDS(here("data/napa_sonoma_rds_equal_segs")) %>% 
  bind_rows() %>% 
  distinct(seg.label, geometry)
  
# this is the raw sum of all crossings, not scaled for how long each lion was collared
all_lions_years_seg_crossing_sums_naive_roads_only <- tallied_steps_crossings %>% 
  filter(num.naive.roads < 3, num.naive.road.crossings == 1) %>% 
  group_by(seg.label, class) %>% 
  summarise(seg.raw.crossing = sum(raw.crossing),
            seg.wt.crossing = sum(bbmm.segment.weight)) %>% 
  ungroup() %>% 
  full_join(full_seg_puma_month_year) %>% # joinging them results in more records than either has
  mutate(seg.raw.crossing = replace_na(seg.raw.crossing, 0),
         seg.wt.crossing = replace_na(seg.wt.crossing, 0)) %>% 
  left_join(seg_geometries)


st_write(all_lions_years_seg_crossing_sums_naive_roads_only, here("data/shapefiles/all_lions_years_seg_crossing_sums_naive_roads_only.shp"), append = FALSE)


# this is scaled to the average crossings per month across all lions, and also has the number of different lions using each segment


seg_crossing_sums_naive_roads_only <- readRDS(here("data/analysis_inputs/seg_crossing_sums_naive_roads_only"))

monthly_mean <- seg_crossing_sums_naive_roads_only %>% 
  group_by(seg.label) %>% 
  summarise(mean.seg.raw.crossing = mean(seg.raw.crossing),
            mean.seg.wt.crossing = mean(seg.wt.crossing)) %>% 
  ungroup()

tot_lions <- seg_crossing_sums_naive_roads_only %>% 
  filter(seg.raw.crossing > 0) %>% 
  distinct(animal.id, seg.label) %>% 
  group_by(seg.label) %>% 
  summarise(num.lions = n(),
            which.lions = paste(animal.id, collapse = ", "))

monthly_mean_tot_lions <- full_join(monthly_mean, tot_lions) %>% 
  full_join(seg_geometries) %>% 
  mutate(num.lions = replace_na(num.lions, 0),
         which.lions = replace_na(which.lions, "No crossings"))


monthly_mean_tot_lions %>% 
  st_write(here("data/shapefiles/monthly_mean_tot_lions.shp"), append = FALSE)

monthly_mean_tot_lions %>% 
  filter(mean.seg.raw.crossing > 0) %>% 
  st_write(here("data/shapefiles/monthly_mean_tot_lions.shp"), append = FALSE)


monthly_mean_tot_lions %>% 
  filter(mean.seg.raw.crossing == 0) %>% 
  st_write(here("data/shapefiles/not_crossed_segs.shp"), append = FALSE)


# some checking for number of records:
# should now be just 1 record for each segment, mt lion, year, month
count(seg_crossing_sums, seg.label, animal.id, year, month) %>% filter(n > 1) %>% nrow()
# if nrow = 0, good
count(seg_crossing_sums, animal.id, year, month) %>% nrow()
# should still be = nrow(puma_month_years)

saveRDS(seg_crossing_sums, here("data/analysis_inputs/seg_crossing_sums"))

seg_crossing_monthly_means <- seg_crossing_sums %>% 
  group_by(animal.id, seg.label) %>% 
  summarise(mean.seg.raw.crossing = mean(seg.raw.crossing),
            mean.seg.wt.crossing = mean(seg.wt.crossing)) %>% 
  ungroup()

saveRDS(seg_crossing_monthly_means, here("data/analysis_inputs/seg_crossing_monthly_means"))


