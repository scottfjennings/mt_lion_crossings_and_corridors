


# this script finds all the road segments that are in each BBMM UD and calculates how much of the segment was inside the UD



library(tidyverse)
library(here)
library(sf)
library(terra)

options(scipen = 999)

source(here("code/utilities.R"))
source(here("code/helper_data.R")) # need for hr_exclude_pumas


# the UD rasters need to be "wrapped" in A8_all_roads_in_BBMM_UD.R in order to be saved as RDS. need to read then unwrap 
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
  bind_rows() %>% 
  full_join(readRDS(here("data/seg_midpoints_road_class")))


segments_in_combined_homeranges <- readRDS(here("data/segments_in_combined_homeranges"))

ggplot() +
  geom_sf(data = napa_sonoma_rds_equal_segs_df, linewidth = 3) +
  geom_sf(data = segments_in_combined_homeranges, color = "red")

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
  road_slice <- st_intersection(segments_in_combined_homeranges, road_slicer)
  
  bbmm_road_slice <- road_slice %>% 
    st_as_sf() %>% 
    st_intersection(st_as_sf(rp)) %>%
    # if a segment is split and there are multiple new segments in the crossing.step ud (e.g. "P13_37473_34190" and "Alta Monte Dr_Santa Rosa_1_1"), these end up as MULTILINESTRINGs and need to be broken back up into LINESTRINGs
    rowwise() %>% 
    mutate(geometry = list(st_cast(geometry, "LINESTRING"))) %>% 
    unnest(geometry) %>% 
    st_as_sf() %>% 
    mutate(bbmm.seg.length = st_length(.)) %>% 
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
  mutate(animal.id = str_extract(crossing.step, "^[^_]+(?=_)"))


# create a weighted crossing value based on the proportion of the equal length segment that was included in the BBMM UD
bbmm_equal_seg_weights <- bbmm_equal_seg_df %>% 
  mutate(raw.crossing = 1,
         bbmm.segment.weight = as.numeric(bbmm.seg.length/seg.length)) %>% 
  filter(!animal.id %in% hr_exclude_pumas,
         !seg.label %in% p31_exclude_segments,
         !st_is(., c("POINT", "MULTIPOINT"))) %>%  # end up with some point objects, want only lines 
  select(crossing.step, seg.length, seg.label, geometry, bbmm.seg.length, sub.seg.label, animal.id, raw.crossing, bbmm.segment.weight)

  
saveRDS(bbmm_equal_seg_weights, here("data/bbmm_equal_seg_weights"))

bbmm_equal_seg_weights <- readRDS(here("data/bbmm_equal_seg_weights"))



  
