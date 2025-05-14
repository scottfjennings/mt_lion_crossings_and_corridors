

library(tidyverse)
library(here)
library(sf)
library(terra)


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

naive_crossings <- readRDS(here("data/naive_crossings_napa_sonoma_2hr"))

bbmm_equal_seg_weights <- readRDS(here("data/bbmm_equal_seg_weights"))

crossing_steps = distinct(crossing_clusters_gps, crossing.step)$crossing.step

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
    filter(step.id == zcrossing.step) %>% 
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
    geom_point(data = filter(crossing_clusters_gps, crossing.step == zcrossing.step), aes(x = easting, y = northing), color = "#377EB8", size = 3) +
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


prob_road_crossing_plotter("P1_23163_2921") + 
  #guides(colour=FALSE) +
  labs(title = "") +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())
ggsave(here("figures/bbmm_example_P1_23163_2921.png"))


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

