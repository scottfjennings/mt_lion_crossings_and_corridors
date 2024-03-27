

```{r}


ggplot() +
  #geom_sf(data = study_area_counties %>% filter(NAME %in% c("Napa", "Sonoma"))) +
  geom_sf(data = out_roads, aes(color = road.was.crossed), linewidth = 1.5) + 
  scale_color_brewer(palette = 'Dark2') +
  geom_sf(data = out_roads_dropped, color = "gray")+
  #new_scale_color() +
  geom_segment(data = filter(out_crossings, (between(easting, bb.xmin, bb.xmax) & between(easting.end, bb.xmin, bb.xmax)) & 
                               (between(northing, bb.ymin, bb.ymax) & between(northing.end, bb.ymin, bb.ymax))), aes(x = easting, y = northing, xend = easting.end, yend = northing.end, color = puma.crossed.road)) +
  #scale_fill_manual(values = my_colors, labels = c(FALSE, TRUE)) +
  coord_sf(datum = st_crs(26910)) +
  theme_bw()

```





```{r}

out_crossings <- full_join(road_crossing_steps, puma_steps) %>% 
  mutate(puma.crossed.road = !is.na(objectid)) %>% 
  filter(str_detect(step.id, "P1_23163")) %>% 
  arrange(datetime.local)

bb.xmin = min(out_crossings$easting)
bb.xmax = max(out_crossings$easting)
bb.ymax = max(out_crossings$northing)
bb.ymin = min(out_crossings$northing)

bb.xmin = 534000
bb.xmax = 538000
bb.ymax = 4250000
bb.ymin = 4248000

out_road_slicer <- st_bbox(c(xmin = bb.xmin, xmax = bb.xmax,
                             ymax = bb.ymax, ymin = bb.ymin), crs = st_crs(26910)) %>% 
  st_as_sfc()


out_roads <- road_crossing_steps %>% 
  #filter(str_detect(step.id, "P1_23163")) %>% 
  data.frame() %>% 
  select(-geometry) %>% 
  full_join(napa_sonoma_rds_utm %>% filter(class %in% keep_road_classes)) %>% 
  st_as_sf() %>% 
  st_intersection(., out_road_slicer) %>% 
  mutate(road.was.crossed = !is.na(step.id))

out_roads_dropped <- napa_sonoma_rds_utm %>% 
  filter(!class %in% keep_road_classes) %>% 
  st_as_sf() %>% 
  st_intersection(., out_road_slicer)

my_colors <- RColorBrewer::brewer.pal(8, "Dark2")[7:8]

ggplot(out_roads)  +
  geom_sf(aes(color = road.was.crossed), linewidth = 1.5) + 
  scale_color_brewer(palette = 'Dark2') +
  geom_sf(data = out_roads_dropped, color = "gray")+
  #new_scale_color() +
  geom_segment(data = filter(out_crossings, (between(easting, bb.xmin, bb.xmax) & between(easting.end, bb.xmin, bb.xmax)) & 
                               (between(northing, bb.ymin, bb.ymax) & between(northing.end, bb.ymin, bb.ymax))), aes(x = easting, y = northing, xend = easting.end, yend = northing.end, color = puma.crossed.road)) +
  #scale_fill_manual(values = my_colors, labels = c(FALSE, TRUE)) +
  coord_sf(datum = st_crs(26910)) +
  theme_bw()

ggsave(here("figures/p1_test_crossings.png"), width = 9, height = 7)


ggplot(out_crossings[55:60,]) +
  geom_segment(aes(x = easting, y = northing, xend = easting.end, yend = northing.end, color = puma.crossed.road)) +
  geom_path(aes(x = easting, y = northing))




out_crossings %>% 
  select(step.id, contains("easting"), contains("northing"), contains("datetime"), step.dur) %>% 
  mutate(tester = easting.end != lead(easting)) %>% 
  view()




ggsave(here("figures/P21_37474_test_crossings.png"), width = 7, height = 7)

```
