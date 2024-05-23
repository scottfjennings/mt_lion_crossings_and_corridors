

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

# read/prep data ----
crossing_clusters_gps <- readRDS(here("data/crossing_clusters2_gps"))

all_clusters_bbmm <- readRDS(here("model_objects/all_clusters2_bbmm"))
#all_clusters_bbmm <- readRDS(here("model_objects/all_clusters3_bbmm"))

# need this to extract the 
road_crossing_steps <- readRDS(here("data/road_crossing_steps_napa_sonoma_2hr"))

# list of crossing steps to loop through below 
#crossing_steps <- prob_check %>% 
#  filter(crossing.step != "P13_29892_23626") # this one is only 1 raster cell tall and causes problems

crossing_steps <- names(all_clusters_bbmm)

# road layer in UTM
napa_sonoma_rds_utm <- readRDS(here("data/napa_sonoma_rds_utm")) %>% 
  filter(class %in% keep_road_classes)

# NO RUN ---- 
# check how big the BBMM probability layer is. The probabilities seem to get funky with <50 cells. prob_checker() is in utilities.R
prob_check <- map_df(names(all_clusters_bbmm), prob_checker)
filter(prob_check, prob.size > 500) %>% nrow()

# checking the relationship between step length of each cluster and the size of the resulting BBMM probability raster 
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


# now id the section of road that is within the 90% UD ----

# zcrossing.step = crossing_steps$crossing.step[272]

# zcrossing.step = "P1_23163_2781"


#' get_bbmm_crossing
#' 
#' id the section of road that is within the 90% UD 
#'
#' @param zcrossing.step character string indicating the ID for the cluster of points to evaluate
#' @param level UD level in proportion (0-1) format
#' @param plot logical, return a plot for checking or return an sf object with the id'ed road segments
#'
#' @return
#' @details
#' full UD fitted to 6 steps ends up covering too much road, so get_bbmm_crossing() segments the UD to a block defined by just the crossing step and selects the roads just in that segment.
#' 
#'
#' @examples
get_bbmm_crossing <- function(zcrossing.step, level = .90, plot = FALSE) {

  crossing_cluster <- filter(crossing_clusters_gps, crossing.step == zcrossing.step) %>% 
    left_join(readRDS(here("data/puma_steps")) %>% 
                dplyr::select(step.id, step.dist, step.dur)) %>% 
    mutate(step.dist.2hr = (step.dist/as.numeric(step.dur)) * 7200,
           mean.step.length = mean(step.dist.2hr))
  
  # get the BBMM model object for the zcrossing.step crossing cluster
  bbmm <- all_clusters_bbmm[[zcrossing.step]]
  # convert to data frame, sort on probability, and calculate cumulative probability to allow filtering to level UD level
  bbmm_df <- do.call(cbind.data.frame, bbmm) %>% 
    mutate(zindex = row_number()) %>% 
    arrange(-probability) %>% 
    mutate(prob.sum = cumsum(probability),
           crossing.step = zcrossing.step,
           level.lab = paste(level * 100, "% UD", sep = "")) %>% 
    arrange(zindex) %>% 
    filter(prob.sum <= level)
  
  # make it raster, UTM  
  r <- terra::rast(cbind(bbmm_df$x, bbmm_df$y, ceiling(bbmm_df$probability)), type = "xyz")
  #r <- terra::rast(cbind(bbmm_df$x, bbmm_df$y, bbmm_df$probability), type = "xyz")
  crs(r) <- "EPSG:26910"
  
  # raster to polygon b/c for some reason can't to st_as_sf on a SpatRaster but can on a SpatVector
  rp <- terra::as.polygons(r, crs = 26910, values = TRUE, digits = 0, aggregate = TRUE)

  # clip the road layer to cut down computing time for the main st_intersetcion below
  road_slicer <- st_bbox(r)  %>% st_as_sfc()
  road_slice <- st_intersection(napa_sonoma_rds_utm, road_slicer)
  
# and finally get the overlap between the UD and road layer to get the estimate of where the crossing actually happened --

# -- mask road based on either the distance between the 2 crossing step points or the perpendicular width of the 90% UD --
# from https://stackoverflow.com/questions/74844804/finding-a-set-of-equally-spaced-perpendicular-lines-along-boundaries-in-r
# this is the first point for the crossing step
p1 <- crossing_cluster[3,] %>% st_as_sf(coords = c("easting", "northing"), crs = 26910)
# and this is the second point for the crossing step
p2 <- crossing_cluster[4,] %>% st_as_sf(coords = c("easting", "northing"), crs = 26910)
# the angle of the line between p1 and p2
alpha <- (atan((sf::st_coordinates(p1)[2] - sf::st_coordinates(p2)[2]) / (sf::st_coordinates(p1)[1] - sf::st_coordinates(p2)[1])))+pi/2 
# the length of the largest easting or northing dimension of the bbmm probability layer - just needed something sufficiently bigger than 1/2 the width of the 90% UD
biggest_dim <- max(c(st_bbox(rp)$xmax - st_bbox(rp)$xmin, st_bbox(rp)$ymax - st_bbox(rp)$ymin))

# this is the rise and run of the perpendicular line end points
x1 <- (biggest_dim/2) * cos(alpha) 
y1 <- (biggest_dim/2) * sin(alpha) 

# this point defines a perpendicular lines crossing p1 and p2 with length = biggest_dim
# need to define 2 points for each line, on oposite sides of the step end point
# this is for p1
p3 <- bind_rows(data.frame(northing = c(sf::st_coordinates(p1)[2] + y1, sf::st_coordinates(p1)[2] - y1),
                       easting = c(sf::st_coordinates(p1)[1] + x1, sf::st_coordinates(p1)[1] - x1)))
# this is for p2
p4 <- bind_rows(data.frame(northing = c(sf::st_coordinates(p2)[2] - y1, sf::st_coordinates(p2)[2] + y1),
                       easting = c(sf::st_coordinates(p2)[1] - x1, sf::st_coordinates(p2)[1] + x1)))
# need to join those 2 lines into to a rectangle, requires the first point be duplicated at the end to close the rectangle
p_box <- bind_rows(p3, p4, p3[1,]) 
# rectangle into a spatial polygon
p_poly <- st_polygon(list(matrix(c(p_box$easting, p_box$northing), ncol = 2))) %>% 
  st_sfc(crs=26910)
# overlay the rectangle on the 90% UD to get just the UD that is between the 2 step end points
ud_slice = st_intersection(st_as_sf(rp), p_poly) 


bbmm_road_slice <- napa_sonoma_rds_utm %>% 
  st_as_sf() %>% 
  st_intersection(st_as_sf(ud_slice)) 

# ----  
  if(plot) {
    ggplot2::ggplot() +
      tidyterra::geom_spatraster(data = r) +
      geom_sf(data = p_poly, fill = NA) +
      geom_sf(data = ud_slice, color = "red", fill = NA, linewidth = 2)  +
      geom_sf(data = road_slice, color = "gray") +
      geom_sf(data = bbmm_road_slice, color = "green", linewidth = 2)  +
      geom_path(data = crossing_cluster, aes(x = easting, y = northing)) +
      geom_point(data = crossing_cluster, aes(x = easting, y = northing)) +
      geom_point(data = crossing_cluster[3:4,], aes(x = easting, y = northing, color = step.id), size = 4) +
      labs(title = zcrossing.step,
           x = "",
           y = "",
           color = "",
           fill = "90% UD") +
      coord_sf(datum = st_crs(26910))
    
  } else {
  return(bbmm_road_slice)
  }
}
  
  
  
get_bbmm_crossing("P39_37843_107361", level = .9, plot = TRUE) # good

 
get_bbmm_crossing(crossing_steps[375], level = .9, plot = TRUE) 

# P1_37472_15804
# P1_37472_11250 has multiple segments

xx <- get_bbmm_crossing(crossing_steps$crossing.step[272], level = .9, plot = FALSE) 


system.time(
  prob_crossings <- map(crossing_steps$crossing.step, safely(get_bbmm_crossing)), gcFirst = TRUE
)

names(prob_crossings) <- crossing_steps$crossing.step

map(prob_crossings, "error") %>% 
  compact()


prob_crossings <- map(prob_crossings, "result") %>% 
  compact() 

saveRDS(prob_crossings, here("model_objects/prob_crossings"))

# convert to df if needed, not currently needed
# prob_crossings_df <- prob_crossings %>% bind_rows(.id = "crossing.step")

# next the crossed road segments need some tidying ----

prob_crossings <- readRDS(here("model_objects/prob_crossings"))

#' combine_continuous
#' combines continuous segments of road into single 
#'
#' @param zcrossing an sf object representing the road segments clipped by get_bbmm_crossing for each crossing step
#' @param z.buffer distance below which nearby but not touching segments are still combined. some intersecting roads aren't exactly touching (generally <10m??), and some roads just nick the clipped 90% UD (max diagonal distance across a 30m raster cell is 42.42641. z.buffer ensures these segments are considered continuous
#'
#' @return
#'
#' @examples
combine_continuous <- function(zcrossing, z.buffer = 42.42641) {
  
zz <- zcrossing %>% 
    arrange(geometry) %>% 
    st_touches(snap_radius = -1) 
  
  my_igraph <- graph_from_adj_list(zz)
  
  my_components <- components(my_igraph)$membership  
  
  zz2 <- zcrossing %>% 
    group_by(section = as.character({{my_components}})) %>% 
    summarise() %>% 
    ungroup()
  
  zdist = max(as.numeric(st_distance(zz2)))
  
  if(zdist > 0 & zdist <= z.buffer) {
    zz2 <- zz2 %>% 
      summarise()
  } else {
    zz2 <- zz2
  }
  
  }


crossings_continuous <- map(prob_crossings, combine_continuous) 

plot(crossings_continuous[[4]])

#' num_segment_checker
#' 
#' check how many segments remain in the clipped road layer afer touching and close segments were combined
#'
#' @param zcrossing an sf object representing the combined road segments from combine_continuous for each crossing step
#'
#' @return data frame with num.segments indicating the number of remaining segments
#'
#' @examples
num_segment_checker <- function(zcrossing) {
  num_segments = data.frame(num.segments = length(crossings_continuous[[zcrossing]][[1]]),
                            crossing = zcrossing)
}

num_segments <- map_df(names(crossings_continuous), num_segment_checker) 

filter(num_segments, num.segments != 1) %>% 
  mutate(keep.segment = 1) %>% 
  write.csv(here("data/manual_road_filter.csv"), row.names = FALSE)

zstep = "P1_32189_6078"

confirm_bbmm_rd_cross <- function(zstep) {
  sp_step <- filter(crossing_clusters_gps, step.id == zstep | lag(step.id) == zstep)
  
  
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





