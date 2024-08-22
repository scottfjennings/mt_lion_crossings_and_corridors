

# use utilization distributions from BBMM (from analysis3_fit_BBMM.R) to ID probabilistic road crossing areas

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
# read/prep data ----
crossing_clusters_gps <- readRDS(here("data/crossing_clusters_gps_1step")) %>% 
  arrange(crossing.step, datetime.local)

all_clusters_bbmm <- readRDS(here("model_objects/all_clusters_bbmm_1step"))
#all_clusters_bbmm <- readRDS(here("model_objects/all_clusters3_bbmm"))

all_clusters_bbmm["P13_29892_23626"] <- NULL # this one is only 1 raster cell tall and causes problems

# for the 1 step uds these steps also cause trouble
all_clusters_bbmm["P1_37472_11247"] <- NULL
all_clusters_bbmm["P1_37472_11248"] <- NULL
all_clusters_bbmm["P1_37472_14839"] <- NULL
all_clusters_bbmm["P1_37472_15955"] <- NULL
all_clusters_bbmm["P2_9323_60474"] <- NULL
all_clusters_bbmm["P33_40855_86744"] <- NULL
all_clusters_bbmm["P41_44132_132816"] <- NULL
all_clusters_bbmm["P41_44132_132817"] <- NULL

# need this to extract the road segments that were crossed
# road_crossing_steps$geometry is the geometry of the road segment that is along the direct line of the step, not the step
naive_crossings <- readRDS(here("data/naive_crossings_napa_sonoma_2hr")) %>% 
  separate(label, "label", sep = "_")

# list of crossing steps to loop through below 
#crossing_steps <- prob_check %>% 
#  filter(crossing.step != "P13_29892_23626") # this one is only 1 raster cell tall and causes problems

crossing_steps <- names(all_clusters_bbmm)

# crossing_steps = crossing_steps[crossing_steps != "P13_29892_23626"] # this one is only 1 raster cell tall and causes problems

# road layer in UTM
#napa_sonoma_rds_utm <- readRDS(here("data/napa_sonoma_rds_utm"))

# this is a single object (LINESTRING) for each named road
#road_layer <- readRDS(here("data/napa_sonoma_rds_utm_merged")) %>% bind_rows()
# this is the roads split into equal length segments (1300m as of July 29, 2024)
road_layer <- readRDS(here("data/napa_sonoma_rds_equal_segs")) %>% bind_rows()
#
# NO RUN ---- 
# check how big the BBMM probability layer is. The probabilities seem to get funky with <50 cells. prob_checker() is in utilities.R
prob_check <- map_df(names(all_clusters_bbmm), prob_checker)
filter(prob_check, prob.size > 500) %>% nrow()

# NO RUN checking the relationship between step length of each cluster and the size of the resulting BBMM probability raster 
zz <- prob_check %>% 
  left_join(crossing_clusters_gps) %>% 
  select(prob.size, crossing.step, step.id) %>% 
  left_join(readRDS(here("data/puma_steps"))) %>% 
  mutate(step.dist = as.numeric(step.dist)) %>% 
  group_by(crossing.step, prob.size) %>% 
  summarise(mean.step.len = mean(step.dist),
            min.step.len = min(step.dist),
            max.step.len = max(step.dist)) %>% 
  ungroup() 

zz %>% 
  pivot_longer(cols = contains("step.len")) %>% 
  filter(prob.size > 500) %>% 
  ggplot() +
  geom_point(aes(x = prob.size, y = value, color = name)) +
  stat_smooth(aes(x = prob.size, y = value, color = name))

# 

# now the data processing steps ----
# 1. make BBMM Utilization Distribution. creates all_bbmm_ud ----
#' bbmm_to_UD 
#'
#' create a Utilization Distribution from a Brownian Bridge Movement Model object
#'
#' @param zbbmm a BBMM object from BBMM::brownian.bridge()
#' @param zlevel UD level in proportion (0-1) format 
#'
#' @return
#'
#' @examples bbmm_to_UD(all_clusters_bbmm[[1]])
bbmm_to_UD <- function(zbbmm, zlevel = .9) {

    
  # convert to data frame, sort on probability, and calculate cumulative probability to allow filtering to level UD level
  bbmm_ud <- do.call(cbind.data.frame, zbbmm) %>% 
    mutate(zindex = row_number()) %>% 
    arrange(-probability) %>% 
    mutate(prob.sum = cumsum(probability),
           level.lab = paste(zlevel * 100, "% UD", sep = "")) %>% 
    arrange(zindex) %>% 
    filter(prob.sum <= zlevel) %>% 
    select(-zindex, -prob.sum)
  return(bbmm_ud)
}


system.time(
  all_bbmm_ud <- map(all_clusters_bbmm, bbmm_to_UD), gcFirst = TRUE
)
# ~ 20 sec
names(all_bbmm_ud) <- names(all_clusters_bbmm)
saveRDS(all_bbmm_ud, here("model_objects/all_bbmm_ud_1step"))

# 2. create a raster for each UD. creates all_ud_rast ---- 
#' UD_to_raster
#' 
#' create a raster of a Utilization Distribution from the BBMM model objects created in analysis3_fit_BBMM. should work on any output from BBMM::brownian.bridge()
#'
#' @param zud a data frame representing a Utilization Distribution. generally the result of bbmm_to_UD(). must have at least columns x, y, and probability
#'
#' @return a terra raster object projected as UTM zone 10N and with all cell values set to 1
#'
#' @examples ud_rast <- UD_to_raster(bbmm_ud)
UD_to_raster <- function(zud) {

  # make it raster, UTM  
  r <- terra::rast(cbind(zud$x, zud$y, ceiling(zud$probability)), type = "xyz")
  #r <- terra::rast(cbind(bbmm_df$x, bbmm_df$y, bbmm_df$probability), type = "xyz")
  crs(r) <- "EPSG:26910"
  # raster to polygon b/c for some reason can't to st_as_sf on a SpatRaster but can on a SpatVector
  rp <- terra::as.polygons(r, crs = 26910, values = TRUE, digits = 0, aggregate = TRUE)
  
return(rp)    
}


system.time(
  all_ud_rast <- map(all_bbmm_ud, UD_to_raster), gcFirst = TRUE
)
# ~ 27 sec
names(all_ud_rast) <- names(all_clusters_bbmm)
saveRDS(all_ud_rast, here("model_objects/all_ud_rast_1step"))

#xx <- map(all_ud_rast, "error")[!sapply(map(all_ud_rast, "error"), is.null)]

# 3. id the section of road that is within the 90% UD. creates all_bbmm_roads ----

# zcrossing.step = crossing_steps$crossing.step[272]

# zcrossing.step = "P1_23163_2781"

#' get_bbmm_crossing
#' 
#' id the section of road that is within the 90% UD 
#'
#' @param zcrossing.step character string indicating the ID for the cluster of points to evaluate
#' @details depending on what was loaded as road.layer above, output can serve different purposes. If napa_sonoma_rds_equal_segs, then allows calculating number of crossings per equal length segment for the full analysis. If napa_sonoma_rds_merged allows calculating the length of BBMM crossed segments to determine what size to cut the road layer into to create napa_sonoma_rds_equal_segs. 
#'
#' @return

#' 
#'
#' @examples
get_bbmm_crossing <- function(zcrossing.step) {
  
  # read UD raster  
  rp <- all_ud_rast[[zcrossing.step]]
  
  # clip the road layer to cut down computing time for the main st_intersection below
  road_slicer <- st_bbox(rp)  %>% st_as_sfc()
  road_slice <- st_intersection(road_layer, road_slicer)
  
  
  #crossed_rd <- naive_crossings %>% 
   # filter(step.id == zcrossing.step)
  
  bbmm_road_slice <- road_slice %>% 
    select(-seg.length) %>% 
#    filter(label %in% crossed_rd$label) %>% 
  #  filter(str_detect(label, paste(crossed_rd$label, collapse = "|"))) %>% 
    st_as_sf() %>% 
    st_intersection(st_as_sf(rp)) %>% 
    mutate(crossed.seg.length = st_length(.))
  
  return(bbmm_road_slice)
  
}


system.time(
  all_bbmm_roads <- map(crossing_steps, get_bbmm_crossing), gcFirst = TRUE
) # ~420 sec, or 179
names(all_bbmm_roads) <- crossing_steps
saveRDS(all_bbmm_roads, here("model_objects/all_bbmm_roads_1step"))

# 4. filter out any continuous road objects that aren't crossed by the straight line step. creates crossed_bbmm_roads ----
#' confirm_bbmm_rd_cross
#' 
#' confirm that the combined segments from combine_continuous() are along the straight line between the 2 crossing step end points
#'
#' @param zstep 
#'
#' @return
#' @details
#' this is used to filter out remaining roads that were not part of the crossed road
#' requires crossing_clusters_gps and all_bbmm_road_slices_continuous to be in the environment
#' 
#'
#' @examples
confirm_bbmm_rd_cross <- function(zcrossing.step) {
  sp_step <- filter(crossing_clusters_gps, crossing.step == zcrossing.step, step.id == zcrossing.step | lag(step.id) == zcrossing.step)
  
  
  step_line <- sp_step %>%
    st_as_sf(coords = c("easting", "northing"), crs = 26910) %>%
    group_by(crossing.step) %>%
    dplyr::summarize(do_union=FALSE) %>%  
    st_cast("LINESTRING") 
  
  prob_road <- all_bbmm_roads[[zcrossing.step]]
  
  rd_cross <- st_intersection(prob_road, step_line) 
  
  
  out_rd_cross <- prob_road %>% 
    filter(label %in% rd_cross$label) %>% 
    st_as_sf()
  
  
  return(out_rd_cross)
}

#xx <- confirm_bbmm_rd_cross(crossing_steps[2])
#xx <- crossing_steps[1:4]

system.time(
  crossed_bbmm_roads <- map(crossing_steps, confirm_bbmm_rd_cross), gcFirst = TRUE
)
# 217 sec

names(crossed_bbmm_roads) <- crossing_steps
saveRDS(crossed_bbmm_roads, here("model_objects/crossed_bbmm_roads_1step"))


#
# check how these crossings look ----

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
  road_slicer <- st_bbox(rp)  %>% st_as_sfc()
  road_slice <- st_intersection(road_layer, road_slicer)
  
  ggplot2::ggplot() +
    geom_sf(data = all_ud_rast[[zcrossing.step]]) +
    geom_sf(data = road_slice, aes(color = seg.label)) +
    geom_sf(data = all_bbmm_roads[[zcrossing.step]], aes(color = seg.label), alpha = 0.5, linewidth = 2)  +
    geom_sf(data = crossed_bbmm_roads[[zcrossing.step]] %>% st_as_sf(), aes(color = seg.label), linewidth = 3)  +
    geom_path(data = filter(crossing_clusters_gps, crossing.step == zcrossing.step), aes(x = easting, y = northing)) +
    geom_point(data = filter(crossing_clusters_gps, crossing.step == zcrossing.step), aes(x = easting, y = northing)) +
    geom_point(data = filter(crossing_clusters_gps, crossing.step == zcrossing.step, (step.id == zcrossing.step | lag(step.id) == zcrossing.step)), aes(x = easting, y = northing, color = step.id), size = 4) +
    labs(title = zcrossing.step,
         x = "",
         y = "",
         color = "",
         fill = "90% UD") +
    coord_sf(datum = st_crs(26910))
}



# these have split UD with crossed roads going through the gaps
bad_ud_steps <- c("P13_29892_26815", "P16_37473_47485", "P4_23163_122801")

prob_road_crossing_plotter(bad_ud_steps[1])



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



# funky movements
# prob_road_crossing_plotter("P16_37473_50642")

# 6. combine adjacent road segments into continuous objects ---- 

# combine_continuous() is in utilities
# for the whole UD
system.time(
  all_bbmm_road_slices_continuous <- map(all_bbmm_road_slices, combine_continuous) 
)
# 171 sec
names(all_bbmm_road_slices_continuous) <- names(all_bbmm_road_slices)
saveRDS(all_bbmm_road_slices_continuous, here("model_objects/all_bbmm_road_slices_continuous_1step"))

# for the trimmed UD
system.time(
  all_trimmed_bbmm_road_slices_continuous <- map(all_trimmed_bbmm_road_slices, combine_continuous) 
)
# 171 sec
names(all_trimmed_bbmm_road_slices_continuous) <- names(all_trimmed_bbmm_road_slices)
saveRDS(all_trimmed_bbmm_road_slices_continuous, here("model_objects/all_trimmed_bbmm_road_slices_continuous_1step"))


# 7. check how many continuous road objects each step has ----
#' num_segment_checker
#' 
#' check how many segments remain in the clipped road layer after touching and close segments were combined with combine_continuous()
#'
#' @param zcrossing an sf object representing the combined road segments from combine_continuous for each crossing step
#'
#' @return data frame with num.segments indicating the number of remaining segments
#'
#' @examples
num_segment_checker <- function(zcrossing) {
  num_segments = data.frame(num.segments = length(all_bbmm_road_slices_continuous[[zcrossing]][[1]]),
                            crossing = zcrossing)
}

num_segments <- map_df(names(all_bbmm_road_slices_continuous), num_segment_checker) 

filter(num_segments, num.segments != 1) %>% 
  mutate(keep.segment = 1) %>% 
  write.csv(here("data/manual_road_filter.csv"), row.names = FALSE)

zcrossing.step = "P1_32189_6078"

# check again how crossings look ----

napa_sonoma_rds_utm <- readRDS(here("data/napa_sonoma_rds_utm"))
crossing_clusters_gps <- readRDS(here("data/crossing_clusters_gps"))
all_ud_rast <- readRDS(here("model_objects/all_ud_rast_1step"))
all_ud_trim_to_step <- readRDS(here("model_objects/all_ud_trim_to_step_1step"))
all_bbmm_road_slices <- readRDS(here("model_objects/all_bbmm_road_slices_1step"))
bbmm_crossing_steps <- readRDS(here("model_objects/bbmm_crossing_steps_1step"))

zcrossing.step = "P1_37472_12612"

prob_road_crossing_plotter(zcrossing.step) 

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




all_bbmm_road_slices_continuous_df <- bind_rows(all_bbmm_road_slices_continuous, .id = "crossing.step") %>% 
  mutate(seg.length = st_length(.))



# for further clipping the BBMM 90% UD

# OPTIONAL 2.1. create a box around each crossing step ----
#' get_step_box
#' 
#' create a box centered at the crossing step, with the side parallel to the step as long as the step and the other side = 3x the step length. This is used to segment the BBMM UD to just the area around the crossing step
#'
#' @param zcrossing.step 
#'
#' @details
#' requires crossing_clusters_gps to be in the global environment.
#' 
#'
#' @return
#'
#' @examples
get_step_box <- function(zcrossing.step) {
  # from https://stackoverflow.com/questions/74844804/finding-a-set-of-equally-spaced-perpendicular-lines-along-boundaries-in-r
  
  # this is the first point for the crossing step
  p1 <- crossing_clusters_gps %>% 
    filter(crossing.step == zcrossing.step & step.id == zcrossing.step) %>% sf::st_as_sf(coords = c("easting", "northing"), crs = 26910)
  # and this is the second point for the crossing step
  p2 <- crossing_clusters_gps %>% 
    filter(crossing.step == zcrossing.step & lag(step.id) == zcrossing.step) %>% sf::st_as_sf(coords = c("easting", "northing"), crs = 26910)
  # the angle of the line between p1 and p2
  alpha <- (atan((sf::st_coordinates(p1)[2] - sf::st_coordinates(p2)[2]) / (sf::st_coordinates(p1)[1] - sf::st_coordinates(p2)[1])))+pi/2 
  # setting the width of the box perpendicular to the step direction - need something sufficiently bigger than 1/2 the width of the 90% UD
  step_distance <- as.numeric(sf::st_distance(p1, p2))
  
  # this is the rise and run of the perpendicular line end points
  x1 <- (step_distance * 1.5) * cos(alpha) 
  y1 <- (step_distance * 1.5) * sin(alpha) 
  
  # this point defines a perpendicular lines crossing p1 and p2 with length = biggest_dim
  # need to define 2 points for each line, on opposite sides of the step end point
  # this is for p1
  p3 <- bind_rows(data.frame(northing = c(sf::st_coordinates(p1)[2] + y1, sf::st_coordinates(p1)[2] - y1),
                             easting = c(sf::st_coordinates(p1)[1] + x1, sf::st_coordinates(p1)[1] - x1)))
  # this is for p2
  p4 <- bind_rows(data.frame(northing = c(sf::st_coordinates(p2)[2] - y1, sf::st_coordinates(p2)[2] + y1),
                             easting = c(sf::st_coordinates(p2)[1] - x1, sf::st_coordinates(p2)[1] + x1)))
  # need to join those 2 lines into to a rectangle, requires the first point be duplicated at the end to close the rectangle
  p_box <- bind_rows(p3, p4, p3[1,]) 
  # rectangle into a spatial polygon
  p_poly <- sf::st_polygon(list(matrix(c(p_box$easting, p_box$northing), ncol = 2))) %>% 
    st_sfc(crs=26910)
}


yy <- get_step_box(zcrossing.step)

system.time(
  all_step_boxes <- map(crossing_steps, get_step_box), gcFirst = TRUE
)
# ~ 240 sec, or 150

names(all_step_boxes) <- crossing_steps
saveRDS(all_step_boxes, here("model_objects/all_step_boxes_1step"))



# OPTIONAL 2.2. trim the BBMM UD to the step box ----
#' trim_ud_to_step
#'
#' overlay the rectangle on the 90% UD to get just the UD that is between the 2 step end points
#' 
#' @param zcrossing.step 
#'
#' @return
#' @details
#' requires all_step_boxes and all_ud_rast exist in global environment
#' 
#'
#' @examples trim_ud_to_step(crossing_steps[[40]])
trim_ud_to_step <- function(zcrossing.step) {
  p_poly <- all_step_boxes[[zcrossing.step]]
  ud_rast <- all_ud_rast[[zcrossing.step]]
  ud_slice = st_intersection(st_as_sf(ud_rast), p_poly) 
}

system.time(
  all_ud_trim_to_step <- map(crossing_steps, trim_ud_to_step), gcFirst = TRUE
)
# 21 sec

names(all_ud_trim_to_step) <- crossing_steps
saveRDS(all_ud_trim_to_step, here("model_objects/all_ud_trim_to_step_1step"))


xx <- map(all_ud_trim_to_step, "error")[!sapply(map(all_ud_trim_to_step, "error"), is.null)] 



# 5b. id the section of road that is within the trimmed 90% UD ----

# zcrossing.step = crossing_steps$crossing.step[272]

# zcrossing.step = "P1_23163_2781"

#' get_bbmm_crossing
#' 
#' id the section of road that is within the 90% UD 
#'
#' @param zcrossing.step character string indicating the ID for the cluster of points to evaluate
#' @param plot logical, return a plot for checking or return an sf object with the id'ed road segments
#'
#' @return
#' @details
#' full UD fitted to 6 steps ends up covering too much road, so get_bbmm_crossing() segments the UD to a block defined by just the crossing step and selects the roads just in that segment.
#' 
#'
#' @examples
get_trimmed_bbmm_crossing <- function(zcrossing.step) {
  
  # read UD raster  
  rp <- all_ud_rast[[zcrossing.step]]
  
  # clip the road layer to cut down computing time for the main st_intersection below
  road_slicer <- st_bbox(rp)  %>% st_as_sfc()
  road_slice <- st_intersection(napa_sonoma_rds_utm, road_slicer)
  
  
  crossed_rd <- naive_crossings %>% 
    filter(step.id == zcrossing.step)
  
  ud_slice <- all_ud_trim_to_step[[zcrossing.step]]
  
  bbmm_road_slice <- road_slice %>% 
    filter(label %in% crossed_rd$label) %>% 
    st_as_sf() %>% 
    st_intersection(st_as_sf(ud_slice)) 
  
  return(bbmm_road_slice)
  
}

system.time(
  all_trimmed_bbmm_road_slices <- map(crossing_steps, get_trimmed_bbmm_crossing), gcFirst = TRUE
) # ~420 sec, or 179
names(all_trimmed_bbmm_road_slices) <- crossing_steps
saveRDS(all_trimmed_bbmm_road_slices, here("model_objects/all_trimmed_bbmm_road_slices_1step"))

