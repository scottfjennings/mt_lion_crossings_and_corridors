
# failed attempts at slicing road layer
# -- mask road layer by a circle centered on the road crossing step with circumfrence = 1.5x the length of the longest shortest line between one of the 2 crossing step points and the edge of the 90% UD ----

z.point <- crossing_cluster[3:4,] %>% st_as_sf(coords = c("easting", "northing"), crs = 26910)



nearest.point = st_nearest_points(z.point, rp_line)

step_buff <- st_buffer(z.point, dist = 1.5 * max(st_length(nearest.point)))

ud_slice = st_intersection(step_buff, rp_line)

ggplot() +
  geom_sf(data = rp_line) +
  geom_sf(data = nearest.point) +
  geom_path(data = crossing_cluster, aes(x = easting, y = northing)) +
  geom_sf(data = ud_slice, color = "red")  +
  coord_sf(datum = st_crs(26910))


# -- mask road layer by the extent of the bbmm probability layer which is 1 st dev bigger than the extent of the GPS points
#  bbmm_road_slice <- st_intersection(napa_sonoma_rds_utm, st_as_sf(rp)) %>% 
#    filter(X >= prob_cut$probability) %>% 
#    mutate(crossing.step = zcrossing.step,
#           UDlevel = level)




# -- mask road layer by a bounding box around just the crossing step
#step_slice  <- crossing_clusters_gps %>% 
#    filter(crossing.step == zcrossing.step) %>% 
#    data.frame()

#   step_slicer <- step_slice[3:4,] %>%
#    st_as_sf(coords = c("easting", "northing"), crs = 26910) %>%
#    group_by(crossing.step) %>%
#    dplyr::summarize(do_union=FALSE) %>%  
#    st_cast("LINESTRING") %>% 
#     st_bbox() %>% 
#     st_as_sfc()

#   bbmm_road_slice <- napa_sonoma_rds_utm %>% 
#      st_intersection(step_slicer) %>% 
#      st_intersection(st_as_sf(rp)) %>% 
#       filter(X >= prob_cut$probability) %>% 
#       mutate(crossing.step = zcrossing.step,
#              UDlevel = level)
# --  mask road layer by the name of the road that was crossed ----
bbmm_road_slice <- road_crossing_steps %>% 
  filter(step.id == zcrossing.step) %>% 
  data.frame() %>% 
  dplyr::select(objectid) %>% 
  left_join(napa_sonoma_rds_utm%>% 
              data.frame() %>% 
              dplyr::select(objectid, label)) %>% 
  dplyr::select(-objectid) %>% 
  left_join(napa_sonoma_rds_utm) %>% 
  st_as_sf() %>% 
  st_intersection(st_as_sf(rp)) %>% 
  filter(X >= prob_cut$probability) %>% 
  mutate(crossing.step = zcrossing.step,
         UDlevel = level)  
# -- mask road layer by a circle centered on the crossed segment with radius = Brownian motion variance    
#   road_segment <- road_crossing_steps %>% 
#     filter(step.id == zcrossing.step)

# Buffer circles by Brownian motion variance
#   dat_circles <- st_buffer(road_segment, dist = bbmm$`Brownian motion variance`)

#   bbmm_road_slice <- napa_sonoma_rds_utm %>% 
#     st_intersection(dat_circles) 



# original big clunky get_bbmm_crossing ----------------------------------------
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



get_bbmm_crossing(zstep, level = .9, plot = TRUE) # good


get_bbmm_crossing(crossing_steps[385], level = .9, plot = TRUE) 

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

