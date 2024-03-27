# load and prep sonoma county road layer ----

napa_rds <- read_sf(dsn= "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_county/Road_Centerlines/road_centerlines_roadcenterlines_calc.shp") %>% 
  select(objectid, "label" = fullname, speedlimit, lanes, surface, "class" = roadclass, shape_Leng, geometry) %>% 
  rename_all(., ~tolower(.)) %>% 
  mutate(county = "Napa")


sonoma_rds <- read_sf(dsn= "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/sonoma_county/Streets/TRA_STREET_PUB.shp") %>% 
  select(OBJECTID, Label, Class, PubPriv, "surface" = SurfaceTyp, SHAPE_Leng, geometry) %>% 
  rename_all(., ~tolower(.)) %>% 
  mutate(county = "Sonoma")

napa_sonoma_rds <- bind_rows(napa_rds, sonoma_rds) %>% 
  mutate(class = ifelse(str_detect(class, "Arterial"), "Arterial", class),
         class = ifelse(str_detect(class, "Collector"), "Collector", class),
         class = ifelse(str_detect(class, "Ramp|Interchange"), "Ramp/Interchange", class))

napa_sonoma_rds %>% 
  ggplot() +
  geom_bar(aes(y = class)) 


napa_sonoma_rds %>%
  data.frame() %>% 
  count(county, class) %>% 
  pivot_wider(id_cols = class, names_from = county, values_from = n) %>% 
  view()


napa_sonoma_rds %>% 
  ggplot() +
  geom_density(aes(x = SHAPE_Leng))


# road class info here: https://www.fhwa.dot.gov/planning/processes/statewide/related/highway_functional_classifications/section01.cfm#Toc329359418
# and here: https://safety.fhwa.dot.gov/speedmgt/data_facts/docs/rd_func_class_1_42.pdf



napa_sonoma_rds_filtered <- napa_sonoma_rds %>% 
  filter(Class %in% keep_road_classes)

sonoma_rds_filtered %>% 
  ggplot() +
  geom_bar(aes(x = Class))

#Conversion of data frame to sf object
napa_sonoma_rds_ll <- st_as_sf(x = napa_sonoma_rds,                         
                          coords = c("Long", "Lat"),
                          crs = "+proj=longlat +datum=WGS84")

#Projection transformation
napa_sonoma_rds_utm = st_transform(napa_sonoma_rds_ll, crs = 26910)


napa_sonoma_rds_utm %>%
  data.frame() %>% 
  count(county, class) %>% 
  pivot_wider(id_cols = class, names_from = county, values_from = n) %>% 
  view()



saveRDS(napa_sonoma_rds_utm, here("data/napa_sonoma_rds_utm"))
# ggplot(sonoma_rds_utm) + geom_sf(aes(color = Class))

dropped_roads <- sonoma_rds %>% 
  filter(!Class %in% keep_road_classes) %>% 
  st_as_sf(x = .,
           coords = c("Long", "Lat"),
           crs = "+proj=longlat +datum=WGS84")

#Projection transformation
dropped_rds_utm = st_transform(dropped_roads, crs = 26910) %>% 
  select(OBJECTID, StreetID, geometry, Class, Label)



ggplot() +
  geom_sf(data = filter(study_area_counties, NAME %in% c("Sonoma", "Napa"))) +
  geom_sf(data = filter(napa_sonoma_rds_utm, !class %in% keep_road_classes), color = "gray") +
  geom_sf(data = filter(napa_sonoma_rds_utm, class %in% keep_road_classes), aes(color = class), linewidth = 1) +
  coord_sf(datum = st_crs(26910)) +
  theme_bw()

