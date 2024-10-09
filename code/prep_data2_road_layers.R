

library(tidyverse)
library(here)
library(readxl)
library(sf)

options(scipen = 999)
source(here("code/utilities.R"))
source(here("code/helper_data.R"))

# load and prep sonoma county road layer ----

napa_rds <- read_sf(dsn= "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_county/Road_Centerlines/road_centerlines_roadcenterlines_calc.shp") %>% 
  dplyr::select(objectid, fullname, munileft, speedlimit, lanes, surface, roadclass, geometry, owned_by) %>% 
  rename("label" = fullname,
         "class" = roadclass,
         "leftcity" = munileft,
         "pubpriv" = owned_by) %>% 
#  rename_all(., ~tolower(.)) %>% 
  mutate(county = "Napa",
         leftcity = str_to_title(leftcity))


sonoma_rds <- read_sf(dsn= "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/sonoma_county/Streets/TRA_STREET_PUB.shp") %>% 
  dplyr::select(OBJECTID, Label, Class, PubPriv, LeftCity, SurfaceTyp, geometry) %>%
  rename("surface" = SurfaceTyp) %>% 
  rename_all(., ~tolower(.)) %>% 
  mutate(county = "Sonoma")

napa_sonoma_rds <- bind_rows(napa_rds, sonoma_rds) %>% 
  mutate(class = ifelse(str_detect(class, "Arterial"), "Arterial", class),
         class = ifelse(str_detect(class, "Collector"), "Collector", class),
         class = ifelse(str_detect(class, "Ramp|Interchange"), "Ramp/Interchange", class))

napa_sonoma_rds %>%  
  st_write(., "data/napa_sonoma_rds.shp", append = FALSE)



# road class info here: https://www.fhwa.dot.gov/planning/processes/statewide/related/highway_functional_classifications/section01.cfm#Toc329359418
# and here: https://safety.fhwa.dot.gov/speedmgt/data_facts/docs/rd_func_class_1_42.pdf


# filtering ----
# exclude_roads.csv is a list of objects to filter out that was created by manually vieing the road layers in ArcGIS
# 
# trimmed roundabouts to be only a single path connecting the roads
# exclude all roads with 
# " Ct" in the name (courts)
# " Cir" in the name (circles)
# " Way" in the name
# data/exclude_roads.csv has all the Ct, Cir, and Way objects
# data/exclude_roads2.csv has only the manually selected objects - as of 10/8/24 this is the one to use

exclude_roads <- read.csv(here("data/exclude_roads2.csv"))

#exclude_roads <- exclude_roads %>% 
#  filter(!str_detect(label, " Way| Ct| Cir"))

#write.csv(exclude_roads, here("data/exclude_roads2.csv"))

napa_sonoma_rds_filtered <- napa_sonoma_rds %>%
  mutate(label = ifelse(objectid %in% c(41622, 17501), "Todd Rd", label),
         label = str_to_title(label),
         pubpriv = ifelse(objectid == "3654" & label == "Pleasant Hill Rd", "Public", pubpriv),
         pubpriv = ifelse(objectid %in% c("25768", "25762", "25761", "39798") & label == "Montecito Ave", "Public", pubpriv),
         surface = ifelse(objectid %in% c("1335217", "1339630") & label == "Mount Veeder Rd", "Public", surface),
         label = ifelse(objectid %in% c("1335392", "1341767", "1341768") & label == "State Highway 37", "Hwy 37", label)
         ) %>% 
  filter(class %in% keep_road_classes, # just the main roads
         !label %in% exclude_labels, # exclude duplicate highway centerlines with direction indicated in label 
         !objectid %in% exclude_roads$objectid # exclude manually IDed duplicate centerlines, loops, and other problem segments
         ) %>% 
  filter(surface %in% c("Paved", "PAVED"), pubpriv != "Private") # excluding unpaved and private roads; these mostly show up in the Local class

napa_sonoma_rds_filtered %>% 
  ggplot() +
  geom_bar(aes(x = class))

napa_sonoma_rds_filtered %>%  
  st_write(., "data/napa_sonoma_rds_filtered2.shp", append = FALSE)


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

napa_sonoma_rds_utm %>%  
  dplyr::select(-objectid) %>% 
  st_write(., "data/napa_sonoma_rds_utm.shp", append = FALSE)


# which roads did mortality happen on?

mort <- read.csv("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/data/North Bay Puma Roadkill Data.csv")

mort_sf <- mort %>%
  dplyr::select(Source, FID, Lion.ID, lat, long, date_death) %>% 
  st_as_sf(x = .,
           coords = c("long", "lat"),
           crs = "+proj=longlat +datum=WGS84") %>% 
  st_transform(., crs = 26910)

st_write(mort_sf, "data/puma_mortalities.shp")

buff_roads <- napa_sonoma_rds_utm %>% 
  st_buffer(., 30)

mort_road_points <- st_intersection(mort_sf, buff_roads)

mort_roads <- mort_road_points %>% 
  data.frame() %>% 
  dplyr::select(label, leftcity, class, FID) %>% 
  left_join(napa_sonoma_rds_utm) %>% 
  st_as_sf()

ggplot() + 
  geom_sf(data = napa_sonoma_rds_utm, color = "gray") + 
  geom_sf(data = mort_roads, aes(color = as.character(FID))) +
  geom_sf(data = mort_sf, aes(color = as.character(FID)))




napa_sonoma_rds_utm <- readRDS(here("data/napa_sonoma_rds_utm"))





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

