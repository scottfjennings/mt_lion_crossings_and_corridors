


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
  distinct(osm_id, name, maxspeed, lanes, geometry) %>% 
  arrange(name) %>% 
  mutate(maxspeed = str_replace_all(maxspeed, "mph", ""),
         maxspeed = ifelse(maxspeed == "40 ;35 ", 40, maxspeed),
         maxspeed = ifelse(maxspeed == "16 ", 15, maxspeed),
         maxspeed = trimws(maxspeed),
         speed = as.numeric(maxspeed),
         name = abbreviate_roads(name),
         speed.id = paste("osm_", osm_id, sep = "")) %>% 
  select(-maxspeed, -osm_id) %>% 
  st_transform(crs = 26910)

rownames(osm_speed_limits) <- NULL


#st_write(osm_speed_limits, here("data/shapefiles/osm_speed_limits.shp"), append = FALSE)


#osm_speed_limits <- st_read(here("data/shapefiles/osm_speed_limits.shp"), append = FALSE)


# get sonoma speed data ----

soco_speed <- st_read("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/sonoma_county_speed_limits/Speed_Limits_(County-Maintained_Roads).shp") %>% 
  mutate(PMBeg = round(PMBeg, 2),
         speed.id = paste("soco", OBJECTID, PMBeg, sep = "_")) %>%
  select(speed.id, name = RoadName, speed = SpeedLmt, geometry) %>% 
  distinct() %>% 
  arrange(name) %>%   
  st_transform(crs = 26910)


# get napa speed data ----

naco_speed <- read_sf(dsn= "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_county/Road_Centerlines/road_centerlines_roadcenterlines_calc.shp") %>% 
  mutate(speed.id = paste("naco_", objectid, sep = "")) %>%
  dplyr::select(speed.id, name = fullname, speed = speedlimit, lanes, geometry) %>% 
  distinct() %>% 
  arrange(name) %>%  
  st_transform(crs = 26910)


# combine ----
speed_limits = bind_rows(osm_speed_limits, soco_speed, naco_speed) %>% 
  filter(!(is.na(speed) & is.na(lanes))) %>% 
  mutate(name = str_to_title(name)) %>% 
  mutate(name = str_replace(name, "State Highway", "Hwy"),
         # ends in West, East, etc
         name = str_replace(name, " West$", " W"),
         name = str_replace(name, " East$", " E"),
         name = str_replace(name, " South$", " S"),
         name = str_replace(name, " North$", " N"),
         # starts with West, East, etc
         name = str_replace(name, "^West ", "W "),
         name = str_replace(name, "^East ", "E "),
         name = str_replace(name, "^South ", "S "),
         name = str_replace(name, "^North ", "N "),
         #
         name = str_replace(name, "Parkway", "Pky"),
         name = str_replace(name, "Fifth", "5th"),
         name = str_replace(name, "Seventh", "7th"),
         name = str_replace(name, "Eigth|Eighth", "8th"),
         name = str_replace(name, "Boulevard", "Blvd")) %>% 
  mutate(name = case_when(name %in% c("Lakeville Hwy", "Gravenstein Hwy South", "Gravenstein Hwy S", "Gravenstein Hwy", "CA-116 (River Rd)", "Ca-116 (River Rd)", "Lakeville Rd") ~ "Hwy 116",
                          name %in% c("Coast Hwy", "Bay Hwy", "Valley Ford Cutoff") ~ "Hwy 1",
                          name %in% c("Carneros Hwy", "Sonoma Hwy") ~ "Hwy 12",
                          name %in% c("Redwood Hwy") ~ "Hwy 101",
                          name %in% c("Saint Helena Hwy") ~ "Hwy 29",
                          name %in% c("Sears Point Rd") ~ "Hwy 37",
                          name == "D St Extension" ~ "D St",
                          name == "I St Extension" ~ "I St",
                          name == "Old Adobe Rd" ~ "Adobe Rd",
                          name %in% c("Silverado Trail", "Silverado Trail South", "Silverado Trail S") ~ "Silverado Trl",
                          name %in% c("Stewarts Point-Skaggs Springs Rd", "Stewarts Pt Skaggs Springs") ~ "Stewarts Point Skaggs Springs Rd",
                          name %in% c("White Cottage Rd South") ~ "S White Cottage Rd",
                          name %in% c("Mark W Springs Rd") ~ "Mark West Springs Rd",
                          name %in% c("Mark W Station Rd") ~ "Mark West Station Rd",
                          name %in% c("McCray Rd") ~ "Mccray Rd",
                          name %in% c("McFarlane Rd", "Mcarlane Rd") ~ "Mcarlane Rd",
                          name %in% c("North Fitch Mtn Rd", "N Fitch Mtn Rd") ~ "N Fitch Mountain Rd",
                          name %in% c("South Fitch Mtn Rd", "S Fitch Mtn Rd") ~ "S Fitch Mountain Rd",
                          name %in% c("Scotts Rt Of Way") ~ "Scotts Right Of Way",
                          name %in% c("Oyster Catcher Lp") ~ "Oyster Catcher Loop",
                          name %in% c("Old Redwood Hwy N") ~ "Old Redwood Hwy",
                          name %in% c("Washington Schl Rd") ~ "Washington School Rd",
                          name %in% c("E Ave") ~ "East Ave",
                          name %in% c("W Ave") ~ "West Ave",
                          name %in% c("Kaitlyn Place") ~ "Kaitlyn Pl",
                          name %in% c("McBrown Rd") ~ "Mcbrown Rd",
                          name %in% c("McMinn Ave") ~ "Mcminn Ave",
                          name %in% c("Bailache Ave") ~ "Bailhache Ave",
                          name %in% c("Ely Rd N") ~ "Ely Rd",
                          #
                          name == 'Dechene Ave' ~ 'De Chene Ave',
                          name == 'Mac Arthur St' ~ 'E Macarthur St',
                          name == 'E Washington Blvd' ~ 'E Washington St',
                          name == 'El Mercado Pkwy' ~ 'El Mercado Pky',
                          name == 'Regional Pkwy' ~ 'Regional Pky',
                          name == 'Rohnert Park Expressway' ~ 'Rohnert Park Expy',
                          name == 'White Cottage Rd S' ~ 'S White Cottage Rd',
                          name == 'Sea Eagle Loop' ~ 'Seaeagle Loop',
                          name == 'S Fork Diamond Mountain Rd' ~ 'South Fork Diamond Mountain Rd',
                          TRUE ~ name)) %>% 
  mutate(name == case_when(name == 'Copperhill Pkwy' & speed.id %in% c('soco_743_10') ~ 'Copperhill Pky 2',
                           name == 'Copperhill Pkwy' & speed.id %in% c('soco_743_10') ~ 'Copperhill Pky 2',
                           
                           name == 'Gasser Ln' & speed.id %in% c('osm_87477420') ~ 'Gasser Dr',
                           name == 'Geysers Rd' & speed.id %in% c('soco_194_36.96', 'soco_194_36.96', 'soco_194_36.96') ~ 'Old Redwood Hwy 2',
                           name == 'Lincoln Ave' & speed.id %in% c('naco_1333232', 'naco_1330637', 'naco_1336939') ~ 'Hwy 29',
                           name == 'Main St' & speed.id %in% c('naco_1336319', 'naco_1334139', 'naco_1334128', 'naco_1337338', 'naco_1332838') ~ 'Hwy 29',
                           name == 'N Ave' & speed.id %in% c('naco_1338867', 'naco_1334229') ~ 'North Ave',
                           name == 'Petaluma Blvd North E' & speed.id %in% c('osm_346964040') ~ 'Petaluma Blvd N',
                           name == 'Prince Ave' & speed.id %in% c('osm_7715307') ~ 'Prince St 2',
                           name == 'Redwood Hwy N' & speed.id %in% c('osm_736361991', 'osm_871118973', 'osm_871118975', 'osm_35261440') ~ 'Old Redwood Hwy',
                           name == 'Redwood Hwy N' & speed.id %in% c('osm_447930588', 'osm_736362000') ~ 'Petaluma Blvd N',
                           name == 'Sebastopol Ave' & speed.id %in% c('osm_1361641349', 'osm_389131196', 'osm_7716293') ~ 'Hwy 12',
                           name == 'Sebastopol Rd' & speed.id %in% c('osm_1279207271', 'osm_1279207271', 'osm_1077956657') ~ 'Hwy 12',
                           name == 'Valley Ford Rd' & speed.id %in% c('osm_26805031', 'osm_26805031', 'osm_26805031', 'osm_37516331') ~ 'Hwy 1',
                          TRUE ~ name))



rownames(speed_limits) <- NULL

st_write(speed_limits, here("data/shapefiles/speed_limits.shp"), append = FALSE)


# join with cleaned road layer ----
# joining point with line works better than line with line so using segment midpoints

# name is the road name in the speed limit data
# label is the road name in seg_midpoints

seg_midpoints <- readRDS(here("data/seg_midpoints")) %>% 
  st_as_sf() %>% 
  st_transform(crs = 26910)


seg_midpoints_buff <- seg_midpoints %>% 
  st_buffer(., 10)


seg_speeds_lanes_join <- st_join(seg_midpoints_buff, speed_limits) %>% 
  separate(seg.label, c("label", "leftcity", "road.num", "seg.num"), sep = "_", remove = FALSE) %>% 
  rowwise() %>% # grepl in the next step doesn't work right without rowwise() but doesn't throw error
  mutate(correct.road = str_detect(seg.label, name)) %>% 
  # finally need to strip the buffered geometry and add back the point geometry
  data.frame() %>% 
  select(-geometry) %>% 
  full_join(seg_midpoints) %>% 
  st_as_sf() %>% 
  st_transform(crs = 26910)

st_write(seg_speeds_lanes_join, here("data/shapefiles/seg_speeds_lanes_join.shp"), append = FALSE)


filter(seg_speeds_lanes_join, correct.road == FALSE) %>% 
  group_by(name, label) %>% 
  mutate(speed.id.paste = paste(speed.id, collapse = "', '")) %>% 
  ungroup() %>% 
  mutate(fixer =  paste0("name == '", name, "' & speed.id %in% c('", speed.id.paste, "')", " ~ '", label, "',"),
         sql = paste0("name = '", name, "' And label = '", label, "' And (speed_d = '", str_replace_all(speed.id.paste, ", ", " Or speed_d = "), "')")) %>% 
  data.frame() %>% 
  arrange(name, label) %>% 
  anti_join(read.csv(here("data/manual_edit_data/ok_speed_name_label_mismatch_20250310.csv")) %>% filter(!is.na(ok.mismatch))) %>% 
  mutate(ok.mismatch = 1) %>% 
  select(ok.mismatch, name, label, speed.id, fixer, sql) %>%
  #view()
  write.csv(here("data/manual_edit_data/ok_speed_name_label_mismatch.csv"), row.names = FALSE)
  
# if this returns 0 records, then correct.road != FALSE below will only correctly matched speed limits plus roads with no speed limit match (i.e. no incorrectly matched speed limits)
  

seg_midpoints_speed_lanes

seg_speeds_lanes_correct <- seg_speeds_lanes_join %>%
  data.frame() %>% 
  filter(correct.road != FALSE) %>% 
  distinct(seg.label, lanes, speed)

max_seg_speed <- seg_speeds_lanes_correct %>% 
  select(seg.label, speed) %>% 
  group_by(seg.label) %>% 
  filter(speed == max(speed)) %>% 
  ungroup() %>% 
  distinct()
  

max_seg_lanes <- seg_speeds_lanes_correct %>% 
  select(seg.label, lanes) %>% 
  group_by(seg.label) %>% 
  filter(lanes == max(lanes)) %>% 
  ungroup() %>% 
  distinct()

seg_speeds_lanes <- full_join(max_seg_speed, max_seg_lanes)%>% 
  full_join(seg_midpoints)


# join with seg_midpoints to get spatial info and export shp for checking in Arc

seg_speeds_lanes %>% 
  st_write(here("data/shapefiles/seg_midpoints_speed_lanes.shp"), append = FALSE)

saveRDS(seg_speeds_lanes, here("data/data/seg_midpoints_speed_lanes"))


seg_midpoints_speed_lanes <- seg_midpoints_speed_lanes %>% 
  separate(seg.label, c("label", "city", "city.num", "seg.num"), sep = "_", remove = FALSE)


# also get the road classification at each segment midpoint for interpolating road characteristics

napa_sonoma_rds_filtered <- readRDS(here("data/napa_sonoma_rds_filtered")) %>% 
  st_as_sf() %>% 
  st_transform(crs = 26910)

# can use a much smaller buffer since seg_midpoints was derived from napa_sonoma_rds_filtered
seg_midpoints_buff1 <- seg_midpoints %>% 
  st_buffer(., 1)

seg_midpoint_road_type <- st_join(seg_midpoints_buff1, napa_sonoma_rds_filtered) %>% 
  separate(seg.label, c("midpoint.label", "leftcity", "road.num", "seg.num"), sep = "_", remove = FALSE) %>% 
  rowwise() %>% # grepl in the next step doesn't work right without rowwise() but doesn't throw error
  mutate(correct.road = str_detect(midpoint.label, label))


filter(seg_midpoint_road_type, correct.road == FALSE) %>% 
  data.frame() %>% 
  distinct(label, midpoint.label) %>%
  arrange(label, midpoint.label)
# looks like only mismatches are valid ones, no name changes needed, which is good because the segment midpoint names came from napa_sonoma_rds_filtered

seg_midpoints_speed_lanes_road_type <- seg_midpoint_road_type %>% 
  data.frame() %>% 
  filter(correct.road == TRUE) %>% 
  select(seg.label, class) %>% 
  full_join(seg_midpoints_speed_lanes) %>% 
  distinct()


