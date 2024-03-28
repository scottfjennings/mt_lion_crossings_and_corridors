

keep_road_classes = c("Collector", 
                      "Freeway", 
                      "Highway", 
                      "Arterial",
                      "Ramp/Interchange")



# data visualization/exploration ----



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