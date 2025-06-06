



library(tidyverse)
library(here)
library(sf)
library(raster)
library(terra)
library(tidyterra)
#library(igraph)

options(scipen = 999)

source(here("code/helper_data.R"))
#


wrapped_all_ud_rast <- readRDS(here("model_objects/all_ud_rast_1step"))
all_ud_rast <- lapply(wrapped_all_ud_rast, unwrap)
crossing_steps <- names(all_ud_rast)
# napa_sonoma_rds_equal_segs <- readRDS(here("data/napa_sonoma_rds_equal_segs")) %>% bind_rows()

crossing_clusters_gps_1step <- readRDS(here("data/crossing_clusters_gps_1step"))

# the road segments inside each BBMM UD. from A10_BBMM_segment.R

bbmm_equal_seg_weights <- readRDS(here("data/bbmm_equal_seg_weights")) %>% 
  st_transform(crs = 26910) %>%  # to UTM 10N
  mutate(road.label = sub("_(?:[^_]*)$", "", seg.label))




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
step_plotter <- function(zcrossing.step) {

  ggplot2::ggplot() +
    geom_sf(data = all_ud_rast[[zcrossing.step]]) +
    geom_sf(data = filter(bbmm_equal_seg_weights, crossing.step == zcrossing.step), aes(color = road.label)) +
    geom_point(data = filter(crossing_clusters_gps_1step, crossing.step == zcrossing.step), aes(x = easting, y = northing)) +
    geom_path(data = filter(crossing_clusters_gps_1step, crossing.step == zcrossing.step), aes(x = easting, y = northing)) +
    labs(title = zcrossing.step,
         x = "",
         y = "",
         color = "",
         fill = "90% UD") +
    coord_sf(datum = st_crs(26910))
}




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
step_plotter_seg <- function(zcrossing.step) {
  
  # Filter lines for this step
  seg_data <- filter(bbmm_equal_seg_weights, crossing.step == zcrossing.step)
  
  # Extract start points of each segment
  start_pts <- st_line_sample(seg_data, sample = 0, type = "regular")

  ggplot2::ggplot() +
    geom_sf(data = all_ud_rast[[zcrossing.step]]) +
    geom_sf(data = seg_data, aes(color = seg.label), linewidth = 2) +
    geom_sf(data = start_pts, shape = 21, fill = "black", color = "white", size = 2) +
    geom_point(data = filter(crossing_clusters_gps_1step, crossing.step == zcrossing.step),
               aes(x = easting, y = northing)) +
    geom_path(data = filter(crossing_clusters_gps_1step, crossing.step == zcrossing.step),
              aes(x = easting, y = northing)) +
    labs(title = zcrossing.step,
         x = "", y = "", color = "", fill = "90% UD") +
    coord_sf(datum = st_crs(26910))
}
