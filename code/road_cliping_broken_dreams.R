
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
