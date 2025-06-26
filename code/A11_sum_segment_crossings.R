


# this script sums the number of raw and weighted crossings per lion per segment per month
# it calculates these values for all segments in each crossing BBMM UD, and it also adds columns which may be used to filter crossing steps for various analysis iterations

# currently adding:
# number of roads each step naively crossed: num_naive_crossings
#     allows filtering for just 1-2 overall crossed roads per step
#     this does not indicate how many times each road was crossed, just how many roads were crossed
# number of times each segment was crossed: num_naive_seg_crossings
#     allows filtering to exclude even number of crossings which may not be true crossings
# whether a segment in the BBMM UD is part of the original naively crossed road 
#     allows further filtering to just the crossed road segment, rather than all segments for the naively crossed road in the BBMM UD


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

# load data ----

# crossing step GPS fixes in wide (step) format. from A1_gps_data.R
# need this to get the naive crossed segments
# and to add back the year and month of each crossing step
puma_steps <- readRDS(here("data/puma_steps"))

# the road segments inside each BBMM UD. from A10_BBMM_segment.R
bbmm_equal_seg_weights <- readRDS(here("data/bbmm_equal_seg_weights")) %>% 
  st_transform(crs = 26910) %>%  # to UTM 10N
  mutate(road.label = sub("_(?:[^_]*)$", "", seg.label)) 

# raw.crossing = 1 in this df means the segment was inside the BBMM UD for that crossing step

# this is all crossing steps with a valid BBMM UD from A10_BBMM_segments.R
# use this for mapping through and filtering
bbmm_steps <- bbmm_equal_seg_weights %>% 
  data.frame() %>% 
  distinct(crossing.step)


# get naive segment crossings. ----
# until now only have whole road naive crossings, need to redo the spatial intersection to find the naive segment crossings
# this allows calculating how many times each segment (not entire roads) was naively crossed 

# segments crossed an even number of times likely were not true crossings; the cat instead likely e.g. went around the outside of a curve.

# first need to redo the intersection of the step and the roads like in A5_naive_road_crossing.R, but now using the equal length segments instead of final_clean_road_layer
#' get_naive_segment_crossings
#' 
#' check which road segments in each BBMM UD were actually crossed by the straight line of the step. Use the BBMM segments (not larger area) for quicker processing time
#' 
#
#' @param zcrossing.step 
#'
#' @returns list with an element for each crossing step, and for each crossing step returns the geometry for the actual naive crossing location (POINT) for each segment. This contrasts with utilities::get_all_bbmm_roads which returns the geometry for the crossed road (LINESTRING)
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
    st_cast("LINESTRING") %>% 
    rename(crossing.step = step.id)
  
  # get all segments that are in this step's BBMM UD
  bbmm_ud_segs <-   bbmm_equal_seg_weights %>% 
    filter(crossing.step == zcrossing.step) %>% 
    select(sub.seg.label, geometry)
  
  naive_crossed_segs <- st_intersection(crossing_step_line, bbmm_ud_segs) %>%
    rowwise() %>% 
    mutate(geometry = list(st_cast(geometry, "POINT"))) %>% 
    unnest(geometry) %>% 
    st_as_sf() %>% 
    mutate(sub.seg.naive.crossed = 1)
  
    
  return(naive_crossed_segs)
}



system.time(
naive_crossed_segs_safe <- map(bbmm_steps$crossing.step, safely(get_naive_segment_crossings))
) # 124, 168
names(naive_crossed_segs_safe) <- bbmm_steps$crossing.step

# the errors are for crossing steps that only crossed roads <500m 
naive_crossed_segs_error <- map(naive_crossed_segs_safe, "error")[!sapply(map(naive_crossed_segs_safe, "error"), is.null)]

# filter out errors from the results
naive_crossed_segs <- map(naive_crossed_segs_safe, "result")[sapply(map(naive_crossed_segs_safe, "error"), is.null)]

naive_crossed_segs_df <- naive_crossed_segs %>% 
  bind_rows()



# now calculate how many naive crossings of each naive crossed segment there are. ----
# some segments may have been split into sub-segments if the segment crossed in and out and back in the BBMM UD
# the calculated bbmm.segment.weight is for these sub-segments
# need to sum bbmm.segment.weight for each segment
# but if the same sub segment was crossed >1 time, then simply grouping by (crossing.step, road.label, seg.label) will lead to bbmm.segment.weight.sum values > 1.
# first need to group_by(crossing.step, road.label, seg.label, sub.seg.label, bbmm.segment.weight) and count raw crossings with a summarize so that each segment has just a single bbmm.segment.weight. including bbmm.segment.weight in the grouping is the way to carry the single weght value for that sub segment forward, while collapsing to a single row for each sub segment
# then can group_by(crossing.step, road.label, seg.label) and sum the sub segment crossings and sum bbmm.seg.weight

# first add in non-naively crossed sub segments so that num crossings and segment weights can be calculated for all segments in one step (adding up 0s for non-naively crossed sub.seg.naive.crossed isn't all that necessary, but mostly need to add up weights) 
naive_crossings_all_sub_segs <- naive_crossed_segs_df %>% 
  data.frame() %>% 
  select(-geometry) %>% 
  full_join(bbmm_equal_seg_weights %>% 
              data.frame() %>% 
              select(-geometry), by = c("crossing.step", "sub.seg.label")) %>% 
  mutate(sub.seg.naive.crossed = ifelse(is.na(sub.seg.naive.crossed), 0, sub.seg.naive.crossed)) %>% 
  arrange(crossing.step)

# first count crossings for each sub segment. 
num_naive_sub_seg_crossings <- naive_crossings_all_sub_segs %>%
  group_by(crossing.step, road.label, seg.label, sub.seg.label, bbmm.segment.weight, raw.crossing) %>% 
  summarise(num.naive.sub.seg.crossings = sum(sub.seg.naive.crossed)) %>%  
  ungroup()

# then combine sub segments and count crossings for each segment. here we need to sum the sub segment weights (which are confusingly called bbmm.segment.weight even though they are for sub segments)
num_naive_seg_crossings <- num_naive_sub_seg_crossings %>% 
  group_by(crossing.step, road.label, seg.label, raw.crossing) %>% 
  summarise(num.naive.seg.crossings = sum(num.naive.sub.seg.crossings),
            bbmm.segment.weight = sum(bbmm.segment.weight),
            num.sub.segs = n()) %>%  
  ungroup()


# finally, count crossings for each full road. want to maintain a record for each segment, so do mutate for calculation instead of summarise 
num_naive_seg_road_crossings <- num_naive_seg_crossings %>% 
  group_by(crossing.step, road.label) %>% 
  mutate(num.naive.road.crossings = sum(num.naive.seg.crossings),
            num.segs = n()) %>%  
  ungroup()

# calculate # of naive roads per crossing step
num_naive_roads <- num_naive_seg_road_crossings %>% 
  filter(num.naive.road.crossings > 0) %>% 
  distinct(crossing.step, road.label) %>% 
  group_by(crossing.step) %>% 
  summarise(num.naive.roads = n()) %>% 
  ungroup()

num_naive_seg_road_crossings_num_roads <- num_naive_seg_road_crossings %>% 
  full_join(num_naive_roads)



# some checking, both of these should be 0 rows
# this should have 1 row per crossing step and segment.
count(num_naive_seg_road_crossings_num_roads, crossing.step, seg.label) %>% filter(n>1) %>% nrow()
# and there can be records with num.naive.seg.crossings > 1, but there should be none with bbmm.segment.weight.sum > 1
filter(num_naive_seg_road_crossings_num_roads, bbmm.segment.weight > 1) %>% nrow()

filter(num_naive_seg_road_crossings_num_roads, num.naive.road.crossings > 0 & num.naive.roads < num.naive.road.crossings) %>% view()

# and now just look at the number of steps with 1, 2, ...  naive seg crossings
count(num_naive_seg_road_crossings, num.naive.seg.crossings)

num_naive_seg_road_crossings_num_roads %>% 
  distinct(crossing.step, road.label, num.naive.road.crossings) %>% 
  count(num.naive.road.crossings)



# NOTE: num_naive_seg_road_crossings_num_roads HAS A RECORD FOR EACH ROAD SEGMENT IN EACH CROSSING STEP BBMM UD
# THIS DOES NOT HAVE ANY SEGMENTS THAT WERE OUTSIDE BBMM UDs BUT STILL INSIDE LION HOME RANGES (i.e. the non-crossed segments)

# next need to add month and year for each step, then calculate the monthly number of crossings for each segment

# need the month and year for each step to assign crossings to the correct month/year
steps_month_years <- readRDS(here("data/puma_steps")) %>%
  mutate(year = year(datetime.local),
         month = month(datetime.local)) %>% 
  distinct(step.id, year, month) %>% 
  rename(crossing.step = step.id)

# and now add month and year to the summed naive segment and road crossings
# this still has all the few_crossings_pumas individuals
num_naive_seg_road_crossings_num_roads_months_years <- num_naive_seg_road_crossings_num_roads %>%
  left_join(steps_month_years) %>% 
  animal.id_from_crossing.step()

# the join above shouldn't add any rows beyond what num_naive_seg_road_crossings_num_roads already had
nrow(num_naive_seg_road_crossings_num_roads_months_years) == nrow(num_naive_seg_road_crossings_num_roads)


# in num_naive_seg_road_crossings_num_roads_months_years:
# a segment can have raw.crossing = 1 and have num.naive.seg.crossings == 0, but cannot have num.naive.seg.crossings == 0 and have raw.crossing not = 1.
# raw.crossing = 1 if a segment was inside the BBMM for that crossing step. this should be 1 for all records
# IMPORTANT raw.crossing should be used to sum monthly crossings below
# num.naive.seg.crossings and num.naive.road.crossings should not be used for summing monthly crossings, these fields should just be used for filtering out crossing steps with multiple crossings
# since raw.crossing is only = 1 or 0 (never >1), bbmm.segment.weight.sum is already the weighted  crossing value for that segment (bbmm.segment.weight.sum * raw.crossing will always be = bbmm.segment.weight.sum)
# so can simply add up bbmm.segment.weight.sum to sum monthly weighted crossings

# in num_naive_seg_road_crossings_num_roads_months_years:
# num.naive.roads - the number of naively crossed full roads in the BBMM UD
# seg.in.naive.road - is this segment part of one of the naively crossed roads?
# raw.crossing - could be named "seg.in.bbmm.ud" but not changing now. should be = 1 for every record in tallied_steps_crossings
# num.naive.road.crossings - number of naive full road crossings for this full road for this step
# num.naive.seg.crossings - number of naive segment crossings for this segment for this step
# bbmm.segment.weight.sum - the proportion of the segment inside this step's BBMM UD



# next, calculate crossings by segment-lion-month, segment-lion-year, and segment-year ----
# num_naive_seg_road_crossings_num_roads_months_years has fields to filter just naively crossed segments prior to counting monthly crossings
# but for the current analysis plan I'm counting monthly crossings for the entire naively crossed road
# could also 


# first by segment-lion-month level ----
# now calculate monthly crossings of just the naively crossed roads. this is the filtering level I'm using in the analysis
monthly_seg_crossings_naive_roads_only <- num_naive_seg_road_crossings_num_roads_months_years %>% 
  filter(num.naive.roads < 3, # just want steps that crossed 1 or 2 roads 
         num.naive.road.crossings == 1) %>%  # and only want roads that were crossed once per step
  group_by(animal.id, year, month, seg.label) %>% 
  summarise(monthly.seg.raw.crossing = sum(raw.crossing), # not sum(num.naive.seg.crossings)
            monthly.seg.wt.crossing = sum(bbmm.segment.weight), # not sum(wt.naive.seg.crossings)
            num.crossing.steps = n(), # including num.crossing.steps and which.steps for checking, these aren't actually needed for the analysis
            which.steps = paste(crossing.step, collapse = "; ")
  ) %>% 
  ungroup() 


# next need to add all the non-crossed segments for each lion each month
# build up df with a record for each segment in each lion's home range for each month that there were crossings by that lion
# this will be a many-to-many join because each lion X year X month will have many segments, and each segment may be in multiple lion home ranges
# segments_in_homeranges has only the 12 main analysis lions
seg_hr <- readRDS(here("data/segments_in_homeranges")) %>% 
  mutate(animal.id = puma) %>% 
  data.frame() %>% 
  distinct(animal.id, seg.label)


puma_month_year <- steps_month_years %>% 
  animal.id_from_crossing.step() %>% 
  distinct(animal.id, month, year)

full_seg_puma_month_year <- puma_month_year %>% 
  full_join(seg_hr) %>% 
  filter(animal.id %in% analysis_pumas, 
         !animal.id %in% few_crossings_pumas) %>% 
  mutate(expected.seg = 1)


# seg_crossing_sums_naive_roads_only has some segments for lions outside that lion's 95% home range because bbmm_equal_seg_weights is created from segments inside the combined home range polygon
# segments_in_homeranges has just the segments inside each lion's 95% home range, so use right join below to filter out segments between the 95 and 99% contours for each lion
monthly_seg_crossings_naive_roads_only_0s <- monthly_seg_crossings_naive_roads_only %>% 
  filter(animal.id %in% analysis_pumas, 
         !animal.id %in% few_crossings_pumas) %>% 
  right_join(full_seg_puma_month_year) %>% # now adding all segments for each lion for each month to get 0-crossed segments
  mutate(monthly.seg.raw.crossing = replace_na(monthly.seg.raw.crossing, 0),
         monthly.seg.wt.crossing = replace_na(monthly.seg.wt.crossing, 0),
         num.crossing.steps = replace_na(num.crossing.steps, 0)) %>% 
  arrange(animal.id, year, month, seg.label) %>% 
  select(animal.id, year, month, seg.label, which.steps, monthly.seg.wt.crossing, monthly.seg.raw.crossing, num.crossing.steps, expected.seg)


# check numbers
month_years_per_lion <- distinct(puma_month_year, animal.id, month, year) %>% count(animal.id) %>% rename(num.month.years = n)
segs_per_lion <- seg_hr %>% count(animal.id) %>% rename(num.segs = n)

# in this df, all expected/calc and actual pairs should match
full_join(month_years_per_lion, segs_per_lion) %>% 
  filter(animal.id %in% analysis_pumas, 
         !animal.id %in% few_crossings_pumas) %>% 
  mutate(lion.expected.month.segs = num.month.years * num.segs,
         total.expected.month.segs = sum(lion.expected.month.segs)) %>% 
  full_join(count(full_seg_puma_month_year, animal.id) %>% rename(calc.month.segs = n)) %>% 
  full_join(full_seg_puma_month_year %>% distinct(animal.id, month, year) %>% count(animal.id) %>% rename(calc.month.years = n)) %>% 
  full_join(full_seg_puma_month_year %>% distinct(animal.id, seg.label) %>% count(animal.id) %>% rename(calc.segs = n)) %>% 
  full_join(count(monthly_seg_crossings_naive_roads_only_0s, animal.id) %>% rename(actual.lion.month.segs = n)) %>% 
  full_join(monthly_seg_crossings_naive_roads_only_0s %>% distinct(animal.id, month, year) %>% count(animal.id) %>% rename(actual.month.years = n)) %>% 
  full_join(monthly_seg_crossings_naive_roads_only_0s %>% distinct(animal.id, seg.label) %>% count(animal.id) %>% rename(actual.segs = n)) %>% 
  select(animal.id, contains("month.years"), num.segs, calc.segs, actual.segs, lion.expected.month.segs, actual.lion.month.segs, calc.month.segs, total.expected.month.segs) %>% 
  mutate(total.actual.month.segs = nrow(seg_crossing_sums_naive_roads_only_0s))


# if they do, save 

saveRDS(monthly_seg_crossings_naive_roads_only_0s, here("data/analysis_inputs/monthly_seg_crossings_naive_roads_only_0s"))

# segment-lion-year level ----

# now calculate annual crossings of just the naively crossed roads. this is the filtering level I'm using in the analysis
annual_seg_crossings_naive_roads_only <- num_naive_seg_road_crossings_num_roads_months_years %>% 
  filter(num.naive.roads < 3, # just want steps that crossed 1 or 2 roads 
         num.naive.road.crossings == 1) %>%  # and only want roads that were crossed once per step
  group_by(animal.id, year, seg.label) %>% 
  summarise(annual.seg.raw.crossing = sum(raw.crossing), # not sum(num.naive.seg.crossings)
            annual.seg.wt.crossing = sum(bbmm.segment.weight), # not sum(wt.naive.seg.crossings)
            num.crossing.steps = n(), # including num.crossing.steps and which.steps for checking, these aren't actually needed for the analysis
            which.steps = paste(crossing.step, collapse = "; ")
  ) %>% 
  ungroup() 

# adding 0 crossed segments for each year and lion

full_seg_puma_year <- full_seg_puma_month_year %>% 
  group_by(seg.label, animal.id, year, expected.seg) %>% 
  summarise(num.months = n()) %>% 
  ungroup()

annual_seg_crossings_naive_roads_only_0s <- annual_seg_crossings_naive_roads_only %>% 
  filter(animal.id %in% analysis_pumas, 
         !animal.id %in% few_crossings_pumas) %>% 
  right_join(full_seg_puma_year) %>% # now adding all segments for each lion for each year to get 0-crossed segments
  mutate(annual.seg.raw.crossing = replace_na(annual.seg.raw.crossing, 0),
         annual.seg.wt.crossing = replace_na(annual.seg.wt.crossing, 0),
         num.crossing.steps = replace_na(num.crossing.steps, 0),
         num.months = replace_na(num.months, 0)) %>% 
  arrange(animal.id, year, seg.label) %>% 
  select(animal.id, year, seg.label, which.steps, num.months, annual.seg.wt.crossing, annual.seg.raw.crossing, num.crossing.steps, expected.seg)

saveRDS(annual_seg_crossings_naive_roads_only_0s, here("data/analysis_inputs/annual_seg_crossings_naive_roads_only_0s"))


# segment-year level ----
# now calculate annual crossings of just the naively crossed roads with all lions combined. this is the filtering level I'm using in the analysis
annual_seg_crossings_naive_roads_only_lions_combined <- num_naive_seg_road_crossings_num_roads_months_years %>% 
  filter(animal.id %in% analysis_pumas, 
         !animal.id %in% few_crossings_pumas) %>% 
  filter(num.naive.roads < 3, # just want steps that crossed 1 or 2 roads 
         num.naive.road.crossings == 1) %>%  # and only want roads that were crossed once per step
  group_by(year, seg.label) %>% 
  summarise(annual.seg.raw.crossing = sum(raw.crossing), # not sum(num.naive.seg.crossings)
            annual.seg.wt.crossing = sum(bbmm.segment.weight), # not sum(wt.naive.seg.crossings)
            num.crossing.steps = n(), # including num.crossing.steps and which.steps for checking, these aren't actually needed for the analysis
            which.steps = paste(crossing.step, collapse = "; "),
            num.lions.crossing = n_distinct(animal.id)
  ) %>% 
  ungroup() 

full_seg_year <- full_seg_puma_month_year %>% 
  group_by(seg.label, year, expected.seg) %>% 
  summarise(which.lions = paste(unique(animal.id), collapse = "; "),
            num.lions = n_distinct(animal.id),
            num.lion.months = n()) %>% 
  ungroup()


annual_seg_crossings_naive_roads_only_0s_lions_combined <- annual_seg_crossings_naive_roads_only_lions_combined %>% 
  right_join(full_seg_year) %>% # now adding all segments for each lion for each year to get 0-crossed segments
  mutate(annual.seg.raw.crossing = replace_na(annual.seg.raw.crossing, 0),
         annual.seg.wt.crossing = replace_na(annual.seg.wt.crossing, 0),
         num.crossing.steps = replace_na(num.crossing.steps, 0),
         num.lion.months = replace_na(num.lion.months, 0),
         num.lions = replace_na(num.lions, 0),
         num.lions.crossing = replace_na(num.lions.crossing, 0)) %>% 
  arrange(year, seg.label) %>% 
  select(year, seg.label, which.steps, num.lion.months, num.lions, num.lions.crossing, annual.seg.wt.crossing, annual.seg.raw.crossing, num.crossing.steps, expected.seg)


saveRDS(annual_seg_crossings_naive_roads_only_0s_lions_combined, here("data/analysis_inputs/annual_seg_crossings_naive_roads_only_0s_lions_combined"))


# this is scaled to the average crossings per month across all lions, and also has the number of different lions using each segment


monthly_seg_crossings_naive_roads_only_0s <- readRDS(here("data/analysis_inputs/monthly_seg_crossings_naive_roads_only_0s"))

monthly_mean <- monthly_seg_crossings_naive_roads_only_0s %>% 
  group_by(seg.label) %>% 
  summarise(mean.monthly.seg.raw.crossing = mean(monthly.seg.raw.crossing),
            mean.monthly.seg.wt.crossing = mean(monthly.seg.wt.crossing)) %>% 
  ungroup()

tot_lions <- monthly_seg_crossings_naive_roads_only_0s %>% 
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
  filter(mean.seg.raw.crossing > 0) %>% 
  st_write(here("data/shapefiles/monthly_mean_tot_lions.shp"), append = FALSE)


monthly_mean_tot_lions %>% 
  filter(mean.seg.raw.crossing == 0) %>% 
  st_write(here("data/shapefiles/not_crossed_segs.shp"), append = FALSE)
