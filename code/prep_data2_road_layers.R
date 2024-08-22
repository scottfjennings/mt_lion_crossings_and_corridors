

library(tidyverse)
library(here)
library(sf)

options(scipen = 999)
source(here("code/utilities.R"))

# load and prep sonoma county road layer ----

napa_rds <- read_sf(dsn= "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_county/Road_Centerlines/road_centerlines_roadcenterlines_calc.shp") %>% 
  dplyr::select(objectid, fullname, munileft, speedlimit, lanes, surface, roadclass, shape_Leng, geometry) %>% 
  rename("label" = fullname,
         "class" = roadclass,
         "leftcity" = munileft) %>% 
  rename_all(., ~tolower(.)) %>% 
  mutate(county = "Napa",
         leftcity = str_to_title(leftcity))


sonoma_rds <- read_sf(dsn= "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/sonoma_county/Streets/TRA_STREET_PUB.shp") %>% 
  select(OBJECTID, Label, Class, PubPriv, LeftCity, SurfaceTyp, SHAPE_Leng, geometry) %>%
  rename("surface" = SurfaceTyp) %>% 
  rename_all(., ~tolower(.)) %>% 
  mutate(county = "Sonoma")

napa_sonoma_rds <- bind_rows(napa_rds, sonoma_rds) %>% 
  mutate(class = ifelse(str_detect(class, "Arterial"), "Arterial", class),
         class = ifelse(str_detect(class, "Collector"), "Collector", class),
         class = ifelse(str_detect(class, "Ramp|Interchange"), "Ramp/Interchange", class))




# road class info here: https://www.fhwa.dot.gov/planning/processes/statewide/related/highway_functional_classifications/section01.cfm#Toc329359418
# and here: https://safety.fhwa.dot.gov/speedmgt/data_facts/docs/rd_func_class_1_42.pdf


# filtering ----
exclude_roads <- read.csv(here("data/exclude_roads.csv"))

napa_sonoma_rds_filtered <- napa_sonoma_rds %>%
  mutate(label = ifelse(objectid %in% c(41622, 17501), "Todd Rd", label),
         #label = paste(label, leftcity, sep = "_")
         ) %>% 
  filter(class %in% keep_road_classes, # just the main roads
         !label %in% exclude_labels, # exclude duplicate highway centerlines with direction indicated in label 
         !objectid %in% exclude_roads$objectid # exclude manually IDed duplicate centerlines
         ) 

napa_sonoma_rds_filtered %>% 
  ggplot() +
  geom_bar(aes(x = class))



#Projection transformation
# the data come projected in NAD83 / California zone 2 (ftUS) https://epsg.io/2226
st_crs(napa_sonoma_rds_filtered)
# convert to utm zone 10N
napa_sonoma_rds_utm = st_transform(napa_sonoma_rds_filtered, crs = 26910)


napa_sonoma_rds_utm %>%
  data.frame() %>% 
  count(county, class) %>% 
  pivot_wider(id_cols = class, names_from = county, values_from = n) %>% 
  view()


saveRDS(napa_sonoma_rds_utm, here("data/napa_sonoma_rds_utm"))

# create a shapefile

  st_write(napa_sonoma_rds_utm, "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_sonoma_main_roads/napa_sonoma_rds_utm_filtered.shp", append = FALSE)

road %>% 
  filter(is.na(label)) %>% 
  st_write("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_sonoma_main_roads/na_roads.shp", append = FALSE)
  
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

