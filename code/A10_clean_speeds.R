


library(osmdata)
library(sf)
library(tidyverse)
library(here)


source(here("code/utilities.R"))

# get Open street map speed data ----

# Create an OSM query for highways with speed limit tags
sonoma_speed_roads <- opq("Sonoma County, California, USA") %>%
  add_osm_feature(key = "highway") %>%
  add_osm_feature(key = "maxspeed") %>%  # Filter roads that have speed limits
  osmdata_sf()# Get data as an `sf` object (spatial format)

# Create an OSM query for highways with speed limit tags
napa_speed_roads <- opq("Napa County, California, USA") %>%
  add_osm_feature(key = "highway") %>%
  add_osm_feature(key = "maxspeed") %>%  # Filter roads that have speed limits
  osmdata_sf()# Get data as an `sf` object (spatial format)


# osm_lines is the list element with the road network with speed limits
roads_with_speed_limits <- bind_rows(sonoma_speed_roads$osm_lines,
                                     napa_speed_roads$osm_lines)


# Select relevant columns and clean
osm_speed_limits <- roads_with_speed_limits %>%
  distinct(name, maxspeed, lanes, geometry) %>% 
  arrange(name) %>% 
  mutate(maxspeed = str_replace_all(maxspeed, "mph", ""),
         maxspeed = ifelse(maxspeed == "40 ;35 ", 40, maxspeed),
         maxspeed = ifelse(maxspeed == "16 ", 15, maxspeed),
         maxspeed = trimws(maxspeed),
         speed = as.numeric(maxspeed),
         name = abbreviate_roads(name),
         speed.id = paste("osm_", row_number(), sep = "")) %>% 
  select(-maxspeed) %>% 
  st_transform(crs = 26910)


#st_write(osm_speed_limits, here("data/shapefiles/osm_speed_limits.shp"), append = FALSE)


#osm_speed_limits <- st_read(here("data/shapefiles/osm_speed_limits.shp"), append = FALSE)


# get sonoma speed data ----

soco_speed <- st_read("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/sonoma_county_speed_limits/Speed_Limits_(County-Maintained_Roads).shp") %>% 
  select(name = RoadName, speed = SpeedLmt, geometry) %>% 
  distinct() %>% 
  arrange(name) %>% 
  mutate(speed.id = paste("soco_", row_number(), sep = "")) %>% 
  st_transform(crs = 26910)


# get napa speed data ----

naco_speed <- read_sf(dsn= "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_county/Road_Centerlines/road_centerlines_roadcenterlines_calc.shp") %>% 
  dplyr::select(name = fullname, speed = speedlimit, lanes, geometry) %>% 
  distinct() %>% 
  arrange(name) %>% 
  mutate(speed.id = paste("naco_", row_number(), sep = "")) %>% 
  st_transform(crs = 26910)


# combine ----
speed_limits = bind_rows(osm_speed_limits, soco_speed, naco_speed) %>% 
  filter(!(is.na(speed) & is.na(lanes))) %>% 
  mutate(name = str_to_title(name),
         name = str_replace(name, "State Highway", "Hwy"),
         name = str_replace(name, " West$", " W"),
         name = str_replace(name, " East$", " E"),
         name = str_replace(name, " South$", " S"),
         name = str_replace(name, " North$", " N"),
         #
         name = str_replace(name, "^West ", "W "),
         name = str_replace(name, "^East ", "E "),
         name = str_replace(name, "^South ", "S "),
         name = str_replace(name, "^North ", "N "),
         name = str_replace(name, "Parkway", "Pky"),
         name = str_replace(name, "Fifth", "5th"),
         name = str_replace(name, "Seventh", "7th"),
         name = str_replace(name, "Eigth|Eighth", "8th"),
         name = str_replace(name, "Boulevard", "Blvd"),
         name = ifelse(name %in% c("Lakeville Hwy", "Gravenstein Hwy South", "Gravenstein Hwy S", "Gravenstein Hwy", "CA-116 (River Rd)", "Ca-116 (River Rd)", "Lakeville Rd"), "Hwy 116", name),
         name = ifelse(name %in% c("Coast Hwy", "Bay Hwy", "Valley Ford Cutoff"), "Hwy 1", name),
         name = ifelse(name %in% c("Carneros Hwy", "Sonoma Hwy"), "Hwy 12", name),
         name = ifelse(name %in% c("Redwood Hwy"), "Hwy 101", name),
         name = ifelse(name %in% c("Saint Helena Hwy"), "Hwy 29", name),
         name = ifelse(name %in% c("Sears Point Rd"), "Hwy 37", name),
         name = ifelse(name == "D St Extension", "D St", name),
         name = ifelse(name == "I St Extension", "I St", name),
         name = ifelse(name == "Old Adobe Rd", "Adobe Rd", name),
         name = ifelse(name %in% c("Silverado Trail", "Silverado Trail South", "Silverado Trail S"), "Silverado Trl", name),
         name = ifelse(name %in% c("Stewarts Point-Skaggs Springs Rd", "Stewarts Pt Skaggs Springs"), "Stewarts Point Skaggs Springs Rd", name),
         name = ifelse(name %in% c("White Cottage Rd South"), "S White Cottage Rd", name),
         name = ifelse(name %in% c("Mark W Springs Rd"), "Mark West Springs Rd", name),
         name = ifelse(name %in% c("Mark W Station Rd"), "Mark West Station Rd", name),
         name = ifelse(name %in% c("McCray Rd"), "Mccray Rd", name),
         name = ifelse(name %in% c("McFarlane Rd", "Mcarlane Rd"), "Mcarlane Rd", name),
         name = ifelse(name %in% c("North Fitch Mtn Rd", "N Fitch Mtn Rd"), "N Fitch Mountain Rd", name),
         name = ifelse(name %in% c("South Fitch Mtn Rd", "S Fitch Mtn Rd"), "S Fitch Mountain Rd", name),
         name = ifelse(name %in% c("Scotts Rt Of Way"), "Scotts Right Of Way", name),
         name = ifelse(name %in% c("Oyster Catcher Lp"), "Oyster Catcher Loop", name),
         name = ifelse(name %in% c("Old Redwood Hwy N"), "Old Redwood Hwy", name),
         name = ifelse(name %in% c("Washington Schl Rd"), "Washington School Rd", name),
         name = ifelse(name %in% c("E Ave"), "East Ave", name),
         name = ifelse(name %in% c("W Ave"), "West Ave", name),
         name = ifelse(name %in% c("Kaitlyn Place"), "Kaitlyn Pl", name),
         name = ifelse(name %in% c("McBrown Rd"), "Mcbrown Rd", name),
         name = ifelse(name %in% c("McMinn Ave"), "Mcminn Ave", name),
         name = ifelse(name %in% c("Bailache Ave"), "Bailhache Ave", name),
         name = ifelse(name %in% c("Ely Rd N"), "Ely Rd", name))



rownames(speed_limits) <- NULL

# join with cleaned road layer ----


seg_midpoints <- readRDS(here("data/seg_midpoints")) %>% 
  st_as_sf() %>% 
  st_transform(crs = 26910)

seg_midpoints_buff <- seg_midpoints %>% 
  st_buffer(., 10)


road_speeds <- st_join(seg_midpoints_buff, speed_limits) %>% 
  rowwise() %>% # grepl in the next step doesn't work right without rowwise() but doesn't throw error
  mutate(correct.road = str_detect(seg.label, name))

filter(road_speeds, correct.road == FALSE) %>% 
  arrange(seg.label) %>% 
  view()



seg_midpoints_speed_lanes <- road_speeds %>%
  data.frame() %>% 
  filter(correct.road != FALSE) %>% 
  select(seg.label, lanes, speed) %>% 
  full_join(seg_midpoints)
  
st_write(seg_midpoints_speed_lanes, here("data/shapefiles/seg_midpoints_speed_lanes.shp"), append = FALSE)



seg_midpoints_speed_lanes <- seg_midpoints_speed_lanes %>% 
  separate(seg.label, c("label", "city", "city.num", "seg.num"), sep = "_", remove = FALSE)
