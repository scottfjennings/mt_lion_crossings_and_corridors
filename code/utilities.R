
# extract animal.id from the crossing step id ----
#' animal.id_from_crossing.step
#' 
#' extract animal.id from the crossing step id. extracts all characters before the first underscore
#'
#' @param df data frame with a column called crossing.step, with the format animal.id_collar.id_step.id. 
#'
#' @return
#' @export
#'
#' @examples
animal.id_from_crossing.step <- function(df) {
  df <- df %>% 
    mutate(animal.id = str_extract(crossing.step, "^[^_]+(?=_)"))
}



#' abbreviate_roads
#' 
#' change road full names (e.g., Road, Street) to abbreviations (e.g. Rd, St)
#'
#' @param road.col the data frame column with road names
#'
#' @return
#' @export
#'
#' @examples
abbreviate_roads <- function(road.col) {
  road.col.out = case_when(str_detect(road.col, "Road") ~ str_replace(road.col, "Road", "Rd"),
                           str_detect(road.col, "Street") ~ str_replace(road.col, "Street", "St"),
                           str_detect(road.col, "Drive") ~ str_replace(road.col, "Drive", "Dr"),
                           str_detect(road.col, "Avenue") ~ str_replace(road.col, "Avenue", "Ave"),
                           str_detect(road.col, "Connector") ~ str_replace(road.col, "Connector", "Connctr"),
                           str_detect(road.col, " Lane") ~ str_replace(road.col, "Lane", "Ln"),
                           str_detect(road.col, "Terrace") ~ str_replace(road.col, "Terrace", "Ter"),
                           str_detect(road.col, "U.s. Highway") ~ str_replace(road.col, "U.s. Highway", "Hwy"), # this needs to go before Highway to Hwy
                           str_detect(road.col, "Us Highway") ~ str_replace(road.col, "Us Highway", "Hwy"), # this needs to go before Highway to Hwy
                           str_detect(road.col, "State Highway") ~ str_replace(road.col, "State Highway", "Hwy"), # this needs to go before Highway to Hwy
                           str_detect(road.col, "^\\Sr ") ~ str_replace(road.col, "^\\Sr ", "Hwy "), # road name starts with Sr . needs to have the followng space ^\\ is starts with in regex
                           str_detect(road.col, "State Route") ~ str_replace(road.col, "State Route", "Hwy"),
                           str_detect(road.col, "St Rte") ~ str_replace(road.col, "St Rte", "Hwy"),
                           str_detect(road.col, "State Rte") ~ str_replace(road.col, "State Rte", "Hwy"),
                           str_detect(road.col, "Highway") ~ str_replace(road.col, "Highway", "Hwy"),
                           str_detect(road.col, " Circle") ~ str_replace(road.col, "Circle", "Cir"),
                           TRUE ~ road.col)
}




# combines continuous segments of road into single object ----
#' combine_continuous
#' combines continuous segments of road into single object
#'
#' @param zlines an sf object representing the LINESTRING segments to be joined. must have at least 2 fields: label with road name and geometry. should be projected in UTM 10N
#' @param z.buffer distance below which nearby but not touching segments are still combined. some intersecting roads aren't exactly touching (generally <10m??), and some roads just nick the clipped 90% UD max diagonal distance across a 30m raster cell is 42.42641. z.buffer ensures these segments are considered continuous
#'
#' @return
#' 
#' @details
#' even with the UD subsetting in get_bbmm_crossing() there are still some road segments included that aren't part of the crossed road. these are generally isolated segments, so combining the segments that touch into single segments will facilitate filtering out these isolated segments that aren't part of the crossed road 
#' 
#'
#' @examples
combine_continuous <- function(zlines, z.buffer = 42.42641) {
  
  zz <- zlines %>% 
    arrange(label, geometry) %>% 
    group_by(label) %>% 
    sf::st_touches(snap_radius = -1) 
  
  my_igraph <- igraph::graph_from_adj_list(zz)
  
  my_components <- igraph::components(my_igraph)$membership  
  
  zz2 <- zlines %>% 
    group_by(label, section = as.character({{my_components}})) %>% 
    summarise() %>% 
    ungroup()
  
  zdist = max(as.numeric(sf::st_distance(zz2)))
  
  if(zdist > 0 & zdist <= z.buffer) {
    zz2 <- zz2 %>% 
      group_by(label) %>% 
      summarise() %>% 
      mutate(section = "1")
  } else {
    zz2 <- zz2
  }
  
}



# data visualization/exploration ----

# naive crossings plot ----
#' crossings_test_plot
#' 
#' test plot showing a subset of mountain lion steps classified as road crossing or non-crossing steps
#'
#' @param zout_crossings full_out_crossings filtered to the data you want to show in the test plot
#' @param bbox_coords optional. if left blank defaults to the extent of zout_crossings. otherwise supply a 4 character string of bounding box coordinates in this sequence: xmin, xmax, ymax, ymin
#'
#' @return a ggplot object
#'
#' @examples
crossings_test_plot <- function(zout_crossings, bbox_coords = NA) {
  
  if(any(is.na(bbox_coords)))  {
    bb.xmin = min(zout_crossings$easting)
    bb.xmax = max(zout_crossings$easting)
    bb.ymax = max(zout_crossings$northing)
    bb.ymin = min(zout_crossings$northing)
  } else {
    bb.xmin = bbox_coords[1]
    bb.xmax = bbox_coords[2]
    bb.ymax = bbox_coords[3]
    bb.ymin = bbox_coords[4]  
  }
  
  out_road_slicer <- st_bbox(c(xmin = bb.xmin, xmax = bb.xmax,
                               ymax = bb.ymax, ymin = bb.ymin), crs = st_crs(26910)) %>% 
    st_as_sfc()
  
  out_roads <- full_out_roads %>% 
    st_intersection(., out_road_slicer)%>% 
    mutate(road.was.crossed = ifelse(road.was.crossed == "crossed", "Road was\ncrossed", "Road not\ncrossed"))
  
  out_road_labels <- out_roads %>% 
    group_by(label) %>% 
    filter(shape_leng == max(shape_leng)) %>% 
    ungroup() %>% 
    distinct(label, geometry)
  
  out_roads_dropped <- napa_sonoma_rds_utm %>% 
    filter(!class %in% keep_road_classes) %>% 
    st_as_sf() %>% 
    st_intersection(., out_road_slicer)
  
  my_colors <- RColorBrewer::brewer.pal(8, "Dark2")[7:8]
  
  zout_crossings <- zout_crossings %>% 
    mutate(puma.crossed.road = ifelse(puma.crossed.road == "crossed", "Step crossed\nroad", "Step didn't\ncross road"))
  
  plot_out_crossings <- zout_crossings %>% 
    filter((between(easting, bb.xmin, bb.xmax) & between(easting.end, bb.xmin, bb.xmax)) &
             (between(northing, bb.ymin, bb.ymax) & between(northing.end, bb.ymin, bb.ymax)))
  
  ztitle = paste(distinct(plot_out_crossings, animal.id), ". ", as.Date(min(plot_out_crossings$datetime.local)), " to ", as.Date(max(plot_out_crossings$datetime.local)), sep = "")
  
  ggplot(out_roads)  +
    geom_sf(aes(color = road.was.crossed), linewidth = 1.5) + 
    scale_color_brewer(palette = 'Dark2') +
    geom_sf(data = out_roads_dropped, color = "gray")+
    geom_segment(data = plot_out_crossings, aes(x = easting, y = northing, xend = easting.end, yend = northing.end, color = puma.crossed.road)) +
    geom_sf_label(data = out_road_labels, aes(label = label), size = 2) +
    coord_sf(datum = st_crs(26910)) +
    theme_bw() +
    labs(x = "",
         y = "",
         title = ztitle,
         color = NULL) +
    theme(legend.position="top",
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
}


# check for NAN in the BBMM probability ----
#' prob_checker
#' 
#' check the probability raster from a bbmm
#'
#' @param zbbmm 
#'
#' @return a data frame. nan.prob is logical indicating whether any cells have NAN probability. prob.size is the number of raster cells.
#' @details
#' 
#' 
#'
#' @examples
prob_checker <- function(zbbmm) {
  bbmm <- all_clusters_bbmm[[zbbmm]]
  prob_check <- data.frame(nan.prob = any(is.nan(bbmm$probability)),
                           prob.size = length((bbmm$probability)),
                           crossing.step = zbbmm)
}

# get all roads that are within the 90% UD for a step ----
#' get_all_bbmm_roads
#' 
#' get all roads that are within the 90% UD for a step 
#'
#' @param zcrossing.step character string indicating the ID for the cluster of points to evaluate
#' @param road_layer one of c("final_cleaned_road_layer", "napa_sonoma_rds_equal_segs", "napa_sonoma_rds_intersection_splits"). needs fields road.label and geometry.
#' @details depending on which df you set as road_layer, output can serve different purposes. If napa_sonoma_rds_equal_segs, then allows calculating number of crossings per equal length segment for the full analysis. If napa_sonoma_rds_merged allows calculating the length of BBMM crossed segments to determine what size to cut the road layer into to create napa_sonoma_rds_equal_segs. if napa_sonoma_rds_intersection_splits allows IDing the mast parsimonious overall crossed segment of road (THIS NEEDS TESTING/CONFIRMATION)
#'
#' @return

#' 
#'
#'
#' @examples
get_all_bbmm_roads <- function(road_layer = final_cleaned_road_layer, zcrossing.step) {
  
  # read UD raster  
  rp <- all_ud_rast[[zcrossing.step]]
  
  
  # clip the road layer to cut down computing time for the main st_intersection below
  road_slicer <- st_bbox(rp)  %>% st_as_sfc()
  road_slice <- st_intersection(road_layer, road_slicer)
  
  
  #crossed_rd <- naive_crossings %>% 
  # filter(step.id == zcrossing.step)
  
  bbmm_road_slice <- road_slice %>% 
    #select(road.label, geometry) %>% 
    st_as_sf() %>% 
    st_intersection(st_as_sf(rp)) %>% 
    mutate(crossed.seg.length = st_length(.))
  
  return(bbmm_road_slice)
  
}



# probabilistic crossings plotter ----

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
  road_slice <- st_intersection(napa_sonoma_rds_utm, road_slicer)
  
  ggplot2::ggplot() +
    geom_sf(data = all_ud_rast[[zcrossing.step]]) +
    #tidyterra::geom_spatraster(data = all_ud_rast[[zcrossing.step]]) +
    #geom_sf(data = all_step_boxes[[zcrossing.step]], fill = NA) +
    #geom_sf(data = all_ud_trim_to_step[[zcrossing.step]], color = "green", fill = NA, linewidth = 2)  +
    geom_sf(data = road_slice, color = "gray") +
    geom_sf(data = all_bbmm_road_slices[[zcrossing.step]], aes(color = label), linewidth = 3)  +
    #geom_sf(data = bbmm_crossing_steps[[zcrossing.step]] %>% st_as_sf(), color = "red", linewidth = 2) +
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




# probabilistic crossings plotter ----

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
prob_road_crossing_leaflet <- function(zcrossing.step) {
  
leaflet(map_data, width = "100%") %>% 
  #addTiles() %>% 
  #addProviderTiles(providers$CartoDB.Positron) %>% 
  addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>% 
  addProviderTiles("Esri.WorldTopoMap", group = "Esri.WorldTopoMap") %>%
  addProviderTiles("USGS.USImagery", group = "USGS.USImagery") %>% 
  # add a layers control
  addLayersControl(
    baseGroups = c(
      "CartoDB.Positron", "Esri.WorldTopoMap", "USGS.USImagery"
    ),
    overlayGroups = ~puma,
    # position it on the topleft
    position = "topleft" #,
    #options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  addPolylines(opacity = 0.05, color = ~pal(puma), group = ~puma) %>% 
  setView(lng = -122.54, lat = 38.36, zoom = 11) %>% 
  addLegend("bottomright", pal = pal, values = ~puma, title = "Puma ID", opacity = 1)
}
