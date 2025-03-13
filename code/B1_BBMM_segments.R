


# this script finds all the road segments that are in each BBMM UD and calculates how much of the segment was inside the UD



library(tidyverse)
library(here)
library(sf)
library(terra)

options(scipen = 999)

source(here("code/utilities.R"))
source(here("code/helper_data.R")) # need for hr_exclude_pumas


# the UD rasters need to be "wrapped" in A8_all_roads_in_BBMM_UD.R in order tobe saved as RDS. need to read then unwrap 
wrapped_all_ud_rast <- readRDS(here("model_objects/all_ud_rast_1step"))
all_ud_rast <- lapply(wrapped_all_ud_rast, unwrap)
rm(wrapped_all_ud_rast)
# get the crossing step IDs to loop through
crossing_steps <- names(all_ud_rast)

# find which equal length segments were in each BBMM UD

# equal length segments are stored in a list with each element being the dataframe of segments for each named road
napa_sonoma_rds_equal_segs <- readRDS(here("data/napa_sonoma_rds_equal_segs"))
# need this in a single dataframe for operations below
napa_sonoma_rds_equal_segs_df <- napa_sonoma_rds_equal_segs %>% 
  bind_rows() 


# get all equal length road segments that are within the 90% UD for a step ----
#' get_all_bbmm_equal_segs
#' 
#' get all roads that are within the 90% UD for a step 
#'
#' @param zcrossing.step character string indicating the ID for the cluster of points to evaluate

#' @return
#' 
#' 
#'
#'
#' @examples
get_all_bbmm_equal_segs <- function(zcrossing.step) {
  
  # read UD raster  
  rp <- all_ud_rast[[zcrossing.step]]
  
  # clip the road layer to cut down computing time for the main st_intersection below
  road_slicer <- st_bbox(rp)  %>% st_as_sfc()
  road_slice <- st_intersection(napa_sonoma_rds_equal_segs_df, road_slicer)
  
  bbmm_road_slice <- road_slice %>% 
    select(seg.label, geometry) %>% 
    st_as_sf() %>% 
    st_intersection(st_as_sf(rp)) %>%
    # if a segment is split and there are multiple new segments in the crossing.step ud (e.g. "P13_37473_34190" and "Alta Monte Dr_Santa Rosa_1_1"), these end up as MULTILINESTRINGs and need to be broken back up into LINESTRINGs
    rowwise() %>% 
    mutate(geometry = list(st_cast(geometry, "LINESTRING"))) %>% 
    unnest(geometry) %>% 
    st_as_sf() %>% 
    mutate(crossed.seg.length = st_length(.)) %>% 
    group_by(seg.label) %>% 
    mutate(sub.seg.label = paste(seg.label, row_number(), sep = "_"))
  
  return(bbmm_road_slice)
  
}




system.time(
  bbmm_equal_seg_safe <- map(crossing_steps, safely(get_all_bbmm_equal_segs)), gcFirst = TRUE
) # ~420 sec, or 179, 387
names(bbmm_equal_seg_safe) <- crossing_steps

# these ud can't be rasterized by terra::rast because they only have a single x or y coordinate 
bbmm_equal_seg_error <- map(bbmm_equal_seg_safe, "error")[!sapply(map(bbmm_equal_seg_safe, "error"), is.null)]

# filter out errors from the results
bbmm_equal_seg <- map(bbmm_equal_seg_safe, "result")[sapply(map(bbmm_equal_seg_safe, "error"), is.null)]

bbmm_equal_seg_df <- bbmm_equal_seg %>% 
  bind_rows(., .id = "crossing.step") %>% 
  select(-X) %>% 
  mutate(bbmm.seg.length = st_length(geometry))  %>% 
  mutate(animal.id = str_extract(crossing.step, "^[^_]+(?=_)"))


equal_segs_lengths <- napa_sonoma_rds_equal_segs_df %>% 
  rename("road.seg.length" = seg.length) %>% 
  data.frame() %>% 
  select(seg.label, road.seg.length, -geometry)

crossing_years <- readRDS(here("data/crossing_clusters_gps_1step")) %>% 
  mutate(year = year(datetime.local)) %>% 
  distinct(crossing.step, year)

# create a weighted crossing value based on the proportion of the equal length segment that was included in the BBMM UD
bbmm_equal_seg_weights <- bbmm_equal_seg_df %>% 
  left_join(equal_segs_lengths) %>% 
  mutate(raw.crossing = 1,
         bbmm.segment.weight = as.numeric(bbmm.seg.length/road.seg.length)) %>% 
#  left_join(crossing_years) %>% 
  filter(!animal.id %in% hr_exclude_pumas,
         !seg.label %in% p31_exclude_segments,
         !st_is(., c("POINT", "MULTIPOINT"))) %>%  # end up with some point objects, want only lines 
  distinct() # and still somehow ending up with duplicates - still don't know where/how this is happening

  
saveRDS(bbmm_equal_seg_weights, here("data/bbmm_equal_seg_weights"))





# zsegment = "Buhman Ave_1"


crossings_per_segment_crossed <- wt_road_crossed_segs %>% 
  data.frame() %>% 
  select(-geometry) %>% 
  group_by(seg.label) %>% 
  summarise(raw.crossings.per.segment = sum(raw.crossing),
            weighted.crossings.per.segment = sum(weighted.crossing)) %>% 
  ungroup() 

crossings_per_segment_all <- crossings_per_segment_crossed %>% 
  full_join(napa_sonoma_rds_equal_segs_df) %>% 
  mutate(across(contains("crossings.per.segment"), ~replace_na(., 0))) %>% 
  st_as_sf()


ggplot() +
  geom_sf(data = filter(crossings_per_segment_all, weighted.crossings.per.segment == 0), color = "gray") +
  geom_sf(data = filter(crossings_per_segment_all, weighted.crossings.per.segment > 0), aes(color = weighted.crossings.per.segment))


  
