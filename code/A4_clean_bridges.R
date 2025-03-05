


library(tidyverse)
library(here)
library(sf)
library(ctmm)
library(units)

options(scipen = 999)

source(here("code/utilities.R"))
source(here("code/helper_data.R"))

# county boundaries for filtering statewide layers
study_area_counties <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/data/study_area_counties") %>% 
  st_transform(crs = 26910) %>% 
  filter(NAME %in% c("Sonoma", "Napa"))


# getting the bridge data cleaned was an iterative process with multiple steps in R and ArcGIS Pro. I'm writing the methods for both platforms here.

# bridges step 1. combining multiple data sources ----
# result of this step is saved to general_data_sources/roads/napa_sonoma_bridges/all_bridges to save time
# Sonoma county maintained bridges
# from https://acr-cgrc.maps.arcgis.com/home/item.html?id=e095e2cc5d494ba6a3eeaf84ac528806#overview
# accessed 8/2/2024
# open in ArcGIS Pro and then choosing “export features”. download as a shapefile

sonoma_county_bridges <- readxl::read_xlsx("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/sonoma_culverts.xlsx") %>% 
  st_as_sf(x = .,
           coords = c("Lon", "Lat"),
           crs = 4326) %>% 
  st_transform(crs = 26910) %>% 
  select("OBJECTID" = "OBJECTID *", "FAC" = RoadName, "PM" = Postmile, "NAME" = CrossFeature, "LENG" = LengthStruc, geometry) %>% 
  mutate(across(c(OBJECTID, FAC, PM, NAME, LENG), ~as.character(.)),
         bridge.source = "sonoma",
         OBJECTID = paste(bridge.source, OBJECTID, sep = "_")) # need to create a unique identifier that is consistent across all three data sources

# state maintained bridges
# from here https://gisdata-caltrans.opendata.arcgis.com/datasets/ea685fd702f840a7a751b12373d6249c_0/explore
# accessed 7/23/2024
state_bridges <- st_read("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/State_Highway_Bridges/State_Highway_Bridges.shp") %>% 
  st_transform(crs = 26910) %>% 
  st_intersection(study_area_counties) %>% 
  select(OBJECTID, PM, NAME, FAC, LENG, geometry) %>% 
  mutate(across(c(OBJECTID, PM, NAME, FAC, LENG), ~as.character(.)),
         bridge.source = "state",
         OBJECTID = paste(bridge.source, OBJECTID, sep = "_")) # need to create a unique identifier that is consistent across all three data sources


# statewide local bridges
# from https://hgl.harvard.edu/catalog/stanford-cx668kz9577
# accessed 8/2/2024
# mostly need this for Napa county
state_bridges_harvard <- st_read("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/state_bridges_harvard/loc_bridges2015.shp") %>% 
  st_transform(crs = 26910) %>% 
  st_intersection(study_area_counties) %>% 
  select(PM, NAME, FAC, LENG, geometry) %>% 
  mutate(across(c(PM, NAME, FAC, LENG), ~as.character(.)),
         bridge.source = "harvard",
         OBJECTID = paste(bridge.source, row_number(), sep = "_")) %>%  # need to create a unique identifier that is consistent across all three data sources
  filter(!(NAME =="SONOMA CREEK" & FAC == "ARNOLD DR")) # just keeps causing problems

all_bridges <- bind_rows(sonoma_county_bridges, state_bridges, state_bridges_harvard) %>% 
  mutate(NAME = str_to_title(NAME),
         FAC = str_to_title(FAC)) %>% 
  arrange(FAC, NAME)

saveRDS(all_bridges, "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_sonoma_bridges/all_bridges")
st_write(all_bridges, "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_sonoma_bridges/napa_sonoma_bridges.shp", append = FALSE)

##################
# bridges step 2. initial object name cleaning ----
# lots of road name cleaning. all this is to make the road name in the bridges layer match road names in the roads layer.
# this is the start of the iterative workflow. these name edits and bridges to remove were IDed mostly in the steps below, 
# but I need to exclude them here so I can repeat the steps below until there are no more problem bridges left

all_bridges <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_sonoma_bridges/all_bridges")

# initial broad cleaning, often multiple records
all_bridges_clean <- all_bridges %>% 
  mutate(across(c(FAC, NAME), ~str_to_title(.)),
         NAME = case_when(NAME == "Warm Srings Creek" ~ "Warm Springs Creek", 
                          TRUE ~ NAME),
         NAME = str_replace(NAME, "\\(", ""),
         NAME = str_replace(NAME, "\\)", ""),
         FAC = abbreviate_roads(FAC),
         FAC = str_replace(FAC, "Sr ", "Hwy"),
         FAC = str_replace(FAC, "\\)", ""),
         FAC = str_replace(FAC, "\\(", ""),
         FAC = case_when(FAC %in% c("Eb Hwy 12", "Wb Hwy 12", "Hwy 12 Eb", "Hwy 12 Wb", "Hwy 12 E", "Hwy 12 W") ~ "Hwy 12",
                         FAC %in% c("Eb Hwy 29", "Wb Hwy 29", "Hwy 29 Eb", "Hwy 29 Wb", "Hwy 29 Nb", "Hwy 29 Sb") ~ "Hwy 29",
                         FAC %in% c("U.s. Highway 101", "U.s. Route 101", "U.s. Route 101 Nb", "U.s. Route 101 Sb", "Us Highway 101", "Us Highway 101 Nb", "Us Highway 101 Sb", "Hwy 101 Sb", "Hwy 101 Nb") ~ "Hwy 101",
                         TRUE ~ FAC))

# 2/12/25 don't think this is actually needed
# st_write(all_bridges_clean1, "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_sonoma_bridges/napa_sonoma_bridges_clean1.shp", append = FALSE)

# fixing misspellings and abbreviations of specific records 
all_bridges_clean <- all_bridges_clean %>% 
         mutate(FAC = case_when(FAC %in% c("Chiles-Pope Vly Rd", "Chiles Pope Vly Rd") ~ "Chiles Pope Valley Rd",
                         FAC %in% c("South Ely Rd") ~ "S Ely Rd",
                         FAC %in% c("Stwrt Pt-Skgg Sprg", "Stewarts Pt Skaggs Springs") ~ "Stewarts Point Skaggs Springs",
                         FAC %in% c("Sweetwater Sprg Rd", "Sweetwater Springs Rd", "Sweetwtr Sprngs Rd", "Swtwater Spring Rd") ~ "Sweetwater Springs Rd",
                         FAC %in% c("Trenton Hldsbg Rd", "Trenton Healdsbrg") ~ "Trenton Healdsburg Rd",
                         FAC == "<Null>" ~ "Wohler Rd",
                         FAC == "4th St East" ~ "4th St E",
                         FAC == "8th St East" ~ "8th St E",
                         FAC == "Alexander Valley R" ~ "Alexander Valley Rd",
                         FAC == "Anapolis Rd" ~ "Annapolis Rd",
                         FAC == "Austin Cr Rd" ~ "Austin Creek Rd",
                         FAC == "Bailache Ave" ~ "Bailhache Ave",
                         FAC == "Bailhache Rd" ~ "Bailhache Ave",
                         FAC == "Berryessa-Knoxvlle" ~ "Berryessa Knoxville Rd",
                         FAC == "Casa Grande Ave" ~ "Casa Grande Rd",
                         FAC == "Dry Cr Rd" ~ "Dry Creek Rd",
                         FAC == "Dry Crk Canyon Rd" ~ "Dry Creek Rd",
                         FAC == "East Railroad Ave" ~ "E Railroad Ave",
                         FAC == "East Shiloh Rd" ~ "E Shiloh Rd",
                         FAC == "East Watmaugh Rd" ~ "E Watmaugh Rd",
                         FAC == "Eighth St East" ~ "8th St E",
                         FAC == "First St" ~ "1st St",
                         FAC == "Fort Ross Rd." ~ "Fort Ross Rd",
                         FAC == "Fountain Grove Pkw" ~ "Fountaingrove Pky",
                         FAC == "Fourth Ave" ~ "4th Ave",
                         FAC == "Franz Valey Sch Rd" ~ "Franz Valley School Rd",
                         FAC == "Golf Course Rd" ~ "Golf Course Dr",
                         FAC == "Grove Rd" ~ "Grove St",
                         FAC == "Hwy 121 Wb Imola Av" ~ "W Imola Ave",
                         FAC == "Hauser Br Rd" ~ "Hauser Bridge Rd",
                         FAC == "Highland Terr" ~ "Highland Ter",
                         FAC == "Kennilworth Ave" ~ "Kenilworth Ave", #1 n is correct per google maps
                         FAC == "Knoxv-Berryessa Rd" ~ "Berryessa Knoxville Rd",
                         FAC == "Mac Arthur St" ~ "E Macarthur St",
                         FAC == "Mark W Spring Rd" ~ "Mark West Springs Rd",
                         FAC == "Mark West Sta Rd" ~ "Mark West Station Rd",
                         FAC == "Middle Terr" ~ "Middle Ter",
                         FAC %in% c("Monte Vista Terr", "Monte Vista Terrac") ~ "Monte Vista Ter",
                         FAC == "Mountain Hawk Way" ~ "Mountain Hawk",
                         FAC == "Mt Veeder Rd" ~ "Mount Veeder Rd",
                         FAC == "N Mcdowell Blvd" ~ "N Mcdowell Boulevard Ext",
                         FAC %in% c("North Fitch Mountain Rd", "North Fitch Mtn Rd") ~ "N Fitch Mountain Rd",
                         FAC == "O'donnell Ln" ~ "Odonnell Ln",
                         FAC == "Odd Fellows Park R" ~ "Odd Fellows Park Rd",
                         FAC == "Old Duncans Grd Rd" ~ "Old Duncans Grade Rd",
                         FAC == "Old Gravenstein Hy" ~ "Old Gravenstein Hwy",
                         FAC == "Old Redwood Hwy N" ~ "Old Redwood Hwy",
                         FAC == "Petrified Forst Rd" ~ "Petrified Forest Rd",
                         FAC == "Rohnert Park Expwy" ~ "Rohnert Park Expy",
                         FAC %in% c("S Fitch Mtn Rd", "South Fitch Mountain Rd") ~ "S Fitch Mountain Rd",
                         FAC == "Silverado Trail" ~ "Silverado Trl",
                         FAC == "Soda Cyn Rd" ~ "Soda Canyon Rd",
                         FAC == "Sonoma Mtn Rd" ~ "Sonoma Mountain Rd",
                         FAC == "South E St" ~ "S E St",
                         FAC == "Stage Coach Cyn Rd" ~ "Stage Coach Canyon Rd",
                         FAC == "Thomas Lk Haris Dr" ~ "Thomas Lake Harris Dr",
                         FAC == "W Third St" ~ "W 3rd St",
                         FAC == "Washington Schl Rd" ~ "Washington School Rd",
                         FAC == "West Dry Creek Rd" ~ "W Dry Creek Rd",
                         FAC == "West Olive St" ~ "Olive St",
                         FAC == "West Pueblo Ave" ~ "W Pueblo Ave",
                         FAC == "West Soda Rock Ln" ~ "W Soda Rock Ln",
                         FAC == "Westside Road" ~ "Westside Rd",
                         FAC == "Wooden Vly Cros Rd" ~ "Wooden Valley Cross Rd",
                         TRUE ~ FAC)) %>% 
  arrange(FAC, NAME, bridge.source) 

# fixing bridges that seem to just have FAC for the wrong road, or for a different road/hwy name than I used in my cleaned road file (e.g. Hwy 121 vs Arnold Dr)
# these are often along roads with many bridges so they need to be IDed with FAC and the data source-specific OBJECTID to make sure only tese ones get a new FAC 
all_bridges_clean <- all_bridges_clean %>% 
  mutate(FAC = case_when(FAC == '2nd St' & OBJECTID %in% c('harvard_492') ~ '2nd St E',
                         FAC == 'A St' & OBJECTID %in% c('harvard_213') ~ 'S A St',
                         FAC == 'Agua Caliente Rd' & OBJECTID %in% c('sonoma_149', 'harvard_112') ~ 'W Agua Caliente Rd',
                         #FAC == 'Broadway' & OBJECTID %in% c('state_1849') ~ 'Broadway 2',
                         FAC == 'Caulfield Ln' & OBJECTID %in% c('state_1782') ~ 'Caulfield Ln 2',
                         FAC == 'Cloverdale Blvd' & OBJECTID %in% c('harvard_485') ~ 'N Cloverdale Blvd',
                         FAC == 'Crocker Rd' & OBJECTID %in% c('harvard_105') ~ 'No Name Rd 28',
                         FAC == 'E St' & OBJECTID %in% c('harvard_211') ~ 'S E St',
                         FAC == 'E Railroad Ave' & OBJECTID %in% c('sonoma_84', 'harvard_330') ~ 'E Railroad Ave 2',
                         FAC == 'Fitch Mountain Rd' & OBJECTID %in% c('harvard_415') ~ 'N Fitch Mountain Rd',
                         FAC == 'Fitch Mountain Rd' & OBJECTID %in% c('harvard_416', 'harvard_490') ~ 'S Fitch Mountain Rd',
                         FAC == 'Fitzpatrick Ln' & OBJECTID %in% c('sonoma_72') ~ 'Fitzpatrick Ln 2',
                         FAC == 'Franz Valley School Rd' & OBJECTID %in% c('sonoma_289', 'harvard_345') ~ 'Franz Valley School Rd 2',
                         FAC == "Geysers Rd" & OBJECTID %in% c("harvard_104") ~ "Old Redwood Hwy 2",
                         FAC == 'Highland Ter' & OBJECTID %in% c('harvard_457', 'sonoma_179') ~ 'Highland Ter 3',
                         FAC == 'Hwy 116' & OBJECTID %in% c('state_1696', 'state_1832') ~ 'Hwy 116 2',
                         FAC == 'Hwy 12' & OBJECTID %in% c('state_1705', 'state_1810') ~ 'Hwy 12 2',
                         FAC == "Hwy 12" & OBJECTID %in% c("state_1773", "state_1774") ~ "Farmers Ln",
                         FAC == "Hwy 121" & OBJECTID %in% c("state_1658", "state_1659", "state_1704") ~ "Arnold Dr",
                         FAC == "Hwy 121" & OBJECTID %in% c("state_1660", "state_1661", "state_1700", "state_1713") ~ "Fremont Dr",
                         FAC == "Hwy 121" & OBJECTID %in% c("state_1837") ~ "Soscol Ave",
                         FAC == "Hwy 121" & OBJECTID %in% c("state_1859", "state_1884") ~ "Monticello Rd",
                         FAC == "Hwy 121" & OBJECTID %in% c("state_1889") ~ "Silverado Trl",
                         FAC == "Hwy 121" & OBJECTID %in% c("state_1890", "state_1836")  ~ "Hwy 12",
                         FAC == "Hwy 128" & OBJECTID %in% c("state_1668") ~ "Old Redwood Hwy",
                         FAC == "Hwy 128" & OBJECTID %in% c("state_1701") ~ "Geyserville Ave 2",
                         FAC == "Hwy 128" & OBJECTID %in% c("state_1842", "state_1843") ~ "Rutherford Road",
                         FAC == "Hwy 128" & OBJECTID %in% c("state_1846") ~ "Foothill Blvd",
                         FAC == "Hwy 128" & OBJECTID %in% c("state_1863", "state_1858") ~ "Sage Canyon Rd",
                         FAC == "Hwy 128" & OBJECTID %in% c("state_1887") ~ "Silverado Trl",
                         FAC == "Hwy 128" & OBJECTID %in% c("state_1888") ~ "Capell Valley Rd",
                         FAC == "Hwy 29" & OBJECTID %in% c("state_1849") ~ "Broadway 2",
                         #FAC == 'Broadway' & OBJECTID %in% c('state_1849') ~ 'Broadway 2',
                         FAC == 'Hoen Ave' & OBJECTID %in% c('harvard_146') ~ 'Hoen Ave Frontage Rd',
                         FAC == 'Labath Ave' & OBJECTID %in% c('harvard_448') ~ 'Labath Ave 2',
                         FAC == 'Lincoln Ave' & OBJECTID %in% c('state_1870') ~ 'W Lincoln Ave',
                         FAC == 'Lovall Valley Rd' & OBJECTID %in% c('sonoma_50') ~ 'Lovall Valley Rd 3',
                         FAC == 'Mark West Station Rd' & OBJECTID %in% c('sonoma_256', 'harvard_278') ~ 'Mark West Station Rd 2',
                         FAC == "North Redwood Hwy" & OBJECTID %in% c("state_1667") ~ "Old Redwood Hwy",
                         FAC == 'Oak Knoll Ave' & OBJECTID %in% c('harvard_55', 'harvard_56', 'harvard_82') ~ 'Oak Knoll Ave 2',
                         FAC == 'Old Duncans Grade' & OBJECTID %in% c('sonoma_205') ~ 'Old Duncans Grade Rd',
                         FAC == 'Old Redwood Hwy' & OBJECTID %in% c('harvard_156', 'sonoma_239') ~ 'Old Redwood Hwy 2',
                         FAC == "Old Redwood Hwy" & OBJECTID %in% c("harvard_141") ~ "Commerce Blvd",
                         FAC == "Old Redwood Hwy" & OBJECTID %in% c("state_1820") ~ "Petaluma Blvd N",
                         FAC == 'Railroad Ave' & OBJECTID %in% c('sonoma_95', 'sonoma_195', 'harvard_290', 'harvard_377') ~ 'Railroad Ave 2',
                         FAC == 'Redwood' & OBJECTID %in% c('harvard_142') ~ 'Redwood Dr',
                         FAC == "Redwood Rd" & OBJECTID %in% c("harvard_449") ~ "Redwood Dr",
                         FAC == "River Rd" & OBJECTID %in% c("harvard_476") ~ "Washington School Rd",
                         FAC == 'Riverside Dr' & OBJECTID %in% c('harvard_511', 'sonoma_58') ~ 'Riverside Dr 2',
                         FAC == 'S A St' & OBJECTID %in% c('harvard_213') ~ 'S A St 2',
                         FAC == "S Ely Rd" & OBJECTID %in% c("harvard_460") ~ "Ely Blvd S",
                         FAC == "Santa Rosa Ave" & OBJECTID %in% c('harvard_137', "sonoma_249") ~ "Roberts Lake Rd",
                         FAC == 'Silverado Trl' & OBJECTID %in% c('harvard_11', 'harvard_12', 'harvard_21', 'harvard_14', 'harvard_5') ~ 'Silverado Trl 2',
                         FAC == 'Starrett Hill Dr' & OBJECTID %in% c('harvard_437') ~ 'Moscow Rd',
                         FAC == 'Steele Ln' & OBJECTID %in% c('harvard_227') ~ 'W Steele Ln',
                         FAC == 'Stewarts Point Skaggs Springs' & OBJECTID %in% c('sonoma_21', 'harvard_196', 'sonoma_8', 'sonoma_39', 'sonoma_44', 'sonoma_54', 'sonoma_24', 'sonoma_31', 'sonoma_51', 'harvard_264', 'harvard_193', 'harvard_195', 'harvard_194') ~ 'Stewarts Point Skaggs Springs Rd',
                         FAC == 'St Helena Rd' & OBJECTID %in% c('sonoma_288', 'sonoma_290', 'harvard_297', 'harvard_296') ~ 'St Helena Rd 2',
                         FAC == 'Stony Point Rd' & OBJECTID %in% c('harvard_172') ~ 'Stony Point Rd 2',
                         FAC == "Temelec Cir" & OBJECTID %in% c("harvard_536") ~ "Via Colombard",
                         FAC == 'Tomales Rd' & OBJECTID %in% c('sonoma_64', 'harvard_171') ~ 'Tomales Rd 2',
                         FAC == "Trancas St" & OBJECTID %in% c("harvard_20") ~ "Monticello Rd",
                         FAC == 'Via Columbard' & OBJECTID %in% c('harvard_539') ~ 'Via Colombard',
                         FAC == "Vineyard Rd" & OBJECTID == "sonoma_30" ~ "River Rd",
                         FAC == "Washington St" & OBJECTID %in% c("harvard_127") ~ "E Washington St",
                         FAC == "Wild Oak Dr" & OBJECTID %in% c("harvard_528") ~ "Oakmont Dr",
                         FAC == 'Willow Creek Rd' & OBJECTID %in% c('harvard_391', 'sonoma_197', 'sonoma_198', 'sonoma_200', 'sonoma_201', 'harvard_159', 'harvard_392', 'harvard_538') ~ 'Willow Creek Rd 2',
                         FAC == 'Yountville Cross R' & OBJECTID %in% c('harvard_92') ~ 'Yountville Cross Rd',
                         TRUE ~ FAC)) %>% 
  arrange(FAC, NAME, bridge.source) 



# 2/12/25 don't think this is actually needed
#st_write(all_bridges_clean3, "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_sonoma_bridges/napa_sonoma_bridges_clean3.shp", append = FALSE)





##################
# bridges step 3. checking for duplicate objects with the same FAC (road name) and NAME (name of crossed object) ----
# first just ID any records with the same FAC and NAME
all_bridges_clean <- all_bridges_clean %>%     
  group_by(FAC, NAME) %>%
  mutate(num.dups = n(),
         fac.name = paste(FAC, NAME, sep = "_"),
         dup.fac.name = paste(FAC, NAME, row_number(), sep = "_")) %>% 
  ungroup() %>% 
  st_as_sf()

# need to isolate the duplicate bridges for the next step
all_bridges_dups <- all_bridges_clean  %>% 
  filter(num.dups > 1)


# some of these objects with the same FAC and NAME could be legitimate, i.e. if a road crosses the same creek multiple times.
# checking the distance between objects with the same FAC and NAME allows IDing possible/likely objects that represent the same actual bridge.
#' get_close_dup_bridges
#' 
#' ID pairs of bridge objects that have the same FAC and NAME and are within some distance of each other, default 25 m. this uses all_bridges_dups to save computing time by only considering the cases where different points have the same FAC and NAME. use this to ID pairs of points that may represent the same bridge.
#'
#' @param zfac.name concatenation of FAC and NAME 
#' @param too.close.dist the threshold distance that is used to fuzzy ID the same actual bridge
#'
#' @return data frame with new fields:
#' too.close for whether the pair of points is less than too.close.dist, and 
#' dup.distance for the actual distance between points
#'
#' @examples
get_close_dup_bridges = function(zfac.name, too.close.dist = 30) {
  
  dup <- all_bridges_dups %>% 
    filter(fac.name == zfac.name) %>% 
    select(fac.name, dup.fac.name) 
  
  dup_dist <- st_distance(dup, dup) 
  
  colnames(dup_dist) <- str_replace_all(c(dup$dup.fac.name), " ", "zfooz")
  
  dup_dist_longer <- dup %>% data.frame() %>% select(-geometry) %>%  
    bind_cols(data.frame(dup_dist)) %>% 
    pivot_longer(contains("zfooz"), names_to = "dup.fac.name.to", values_to = "dup.distance") %>% 
    distinct() %>% 
    mutate(dup.fac.name.to = str_replace_all(dup.fac.name.to, "zfooz", " "),
           dup.distance = as.numeric(dup.distance),
           too.close = dup.distance > 0 & dup.distance < too.close.dist) %>% 
    filter(dup.fac.name != dup.fac.name.to)
  
  true_dups <- dup_dist_longer %>% 
    filter(too.close == TRUE) %>% 
    pivot_longer(cols = contains("dup.fac.name"), values_to = "dup.fac.name") %>% 
    distinct(dup.fac.name, fac.name, too.close, dup.distance)
  
}

system.time(
  close_dup_bridges <- map_df(distinct(all_bridges_dups, fac.name)$fac.name, get_close_dup_bridges)
) # 5 sec

all_bridges_close_dups <- full_join(all_bridges_clean, close_dup_bridges) %>% 
  group_by(fac.name) %>%
  mutate(drop.dup = ifelse(any(bridge.source != "harvard") & too.close == TRUE & bridge.source == "harvard", TRUE, FALSE),
         drop.dup = replace_na(drop.dup, FALSE)) %>% 
  ungroup() %>% 
  select(drop.dup, everything()) %>% 
  arrange(FAC, NAME)

# using the output from get_close_dup_bridges
# remove duplicate bridges that are from the Harvard db and within 25 m of a non-Harvard db bridge with the same FAC and NAME
all_bridges_no_close_dup <- all_bridges_close_dups %>% 
  filter(drop.dup != TRUE) %>% 
  mutate(manual.check = "no")



##################
# bridges step 4. join bridges with roads ----
# the bridge points often don't fall right on the road lines, so need to buffer each road segment by 10m then find all the bridge objects that fall within that buffer zone.
# But because I used the 10m buffer in the previous step, some bridges near intersections or close to other roads might have been assigned to the wrong road, 
# and need to add correct.road as a check for whether the road name from the bridge layer matches the road name in the equal length segment layer

rds_buff10 <- readRDS(here("data/final_cleaned_road_layer")) %>% 
  st_transform(crs = 26910) %>% 
  mutate(road.length = st_length(.)) %>% 
  filter(as.numeric(road.length) > 500) %>% 
  st_buffer(., 10, endCapStyle = "FLAT")

road_bridges <- st_intersection(rds_buff10, all_bridges_no_close_dup) %>% 
  rowwise() %>% # grepl in the next step doesn't work right without rowwise() but doesn't throw error
  mutate(correct.road = FAC == label)

road_bridges <- road_bridges %>% 
  mutate(manual.check = "n") %>% 
  select(FAC, label, correct.road, manual.check, everything())

# it is ok to overwrite this as long as the manual filter layer from step 6 below has been saved with a different name
st_write(road_bridges, here("data/shapefiles/road_bridges.shp"), append = FALSE)


# bridges step 5. manual check each bridge to make sure they seem appropriate for mt lion crossing. ----
# read this into GIS and delete all bridges that seem like a mt lion could physically pass under the roadway using this bridge
# I did not evaluate whether a mt lion *would* use each bridge (a behavioral evaluation), just whether they could (a physical evaluation)

# I also did a manual check for any additional duplicate bridge objects that weren't flagged by get_close_dup_bridges() above. mostly this was long bridges where the 2 objects were spaced >30m. There were also some remaining name mismatches.  generally this was dups between the sonoma and harvard datasets, and I kept the sonoma points because their attribute table is more complete and the coords generally seemed to match the actual bridge better

# I deleted any bridge that was mt lion crossable, so that the remaining manually edited ArcGIS layer could then be saved as a record of the bridges that were excluded, and used in R to filter the final bridge layer

# going in order of the attribute table is more efficient than panning around the map.
# the specific workflow for this was to open the attribute table
# select the top record
# click Zoom to
# if the bridge was mt lion appropriate, delete. if it was not mt lion appropriate, change mnl_chc (ArcGIS changed name for manual.check created in step 5) from "n" to "y"
# for some bridges where the ArcGIS imagry wasn't clear enough, I would use google maps and street view to determin bridge characteristics.





# bridges step 5. removing manually excluded bridges ----
# skip this step if there is no reliable manually excluded bridges file
# the manually excluded bridge fil
# these 5 exclude bridge files are no good because they don't uniquely ID bridges("exclude_bridges.csv", "exclude_bridges2.csv", "exclude_bridges3.csv", "exclude_bridges4.csv", "exclude_bridges_hillside_shv.csv")
# exclude_bridges.csv and exclude_bridges2.csv were created with arcgis pro by selecting bridge objects that met the criteria listed above, then copying the attribute table rows for those objects to a .csv. 
# exclude_bridges.csv is the set of manually excluded bridges selected when local roads were not included. this also was created before the full bridge layer cleaning workflow above was developed so there are likely objects in this file that are also filtered out or taken care of in the steps above.
# exclude_bridges2.csv is the set of manually excluded bridges selected once local roads were included
# I'm not totally sure where exclude_bridges_hillside_shv.csv came from. doesn't seem to be code-generated so maybe I made it in AcrGIS, probably by doing a select by attribute then copying all those attribute table records to the csv. its only 34 objects
# exclude_bridges3.csv are some more bridges I found while investigating name mismatches


# the following file(s) were created from Feb 2025 onward and contain sufficient info to filter out bridge objects. these files are the true record of the bridges that were excluded from the analysis.
exclude_bridges_files <- c("exclude_bridges_20250219.csv", "exclude_bridges_20250220.csv", "exclude_bridges_20250220B.csv", "exclude_bridges_20250220C.csv", "exclude_bridges_20250304.csv", "exclude_bridges_20250305.csv")


#' exclude_bridge_reader
#'
#' @param zfile a exclude_bridges csv file in here("data/manual_edit_data/")
#'
#' @returns
#' @details
#' the exclude_bridges files have funky names after ArcGIS has changed them, this brings names back to match the original all_bridges
#' 
#'
#' @examples
exclude_bridge_reader <- function(zfile) {
  exclude_bridges <- read.csv(here(paste("data/manual_edit_data/", zfile, sep = ""))) %>%
    mutate(across(everything() ,~as.character(.)),
           file = zfile) %>% 
    filter(mnl_chc == "y")
}


all_exclude_bridges <- map_df(exclude_bridges_files, exclude_bridge_reader) %>% 
  distinct() %>% 
  select(FAC, label, NAME, OBJECTID = OBJECTI, file) %>% 
  mutate(NAME = str_replace(NAME, "\\(", ""),
         NAME = str_replace(NAME, "\\)", ""),
         exclude.bridge = TRUE) %>% 
  arrange(FAC, NAME) %>% 
  filter(!str_detect(label, "_"))


all_bridges_filtered <- road_bridges %>% 
  full_join(all_exclude_bridges) %>% 
  arrange(FAC, NAME) %>%
  filter(exclude.bridge != TRUE | is.na(exclude.bridge)) %>% 
  select(-exclude.bridge)





##################
# bridges step 5. last check for mismatch FAC (bridge layer) and label (road layer) road names

# Create the fac.fixer conditions
# this just creates code to copy into step 2 above to manually fix FACs
fac_label_mismatch <- filter(all_bridges_filtered, correct.road == FALSE) %>% 
  data.frame() %>% 
  select(FAC, label, OBJECTID) %>%
  group_by(FAC, label) %>% 
  summarise(OBJECTID = paste0("'", OBJECTID, "'", collapse = ", ")) %>% 
  ungroup()



####---- START OPTIONAL ----####
fac_fixer_filter <- fac_label_mismatch %>% 
  mutate(fac.filter = paste0("FAC == '", FAC, "' & OBJECTID %in% c(", OBJECTID, ")"),
         fac.fixer =  paste0("FAC == '", FAC, "' & OBJECTID %in% c(", OBJECTID, ")", " ~ '", label, "',")) %>% 
  arrange(FAC, label) %>% 
  select(FAC, label, fac.filter, fac.fixer)

fac_fixer_filter %>% 
  pull(fac.fixer)  # Extracts fac.fixer as a character vector


####---- END OPTIONAL ----####



all_bridges_filtered %>% 
  select(FAC, label, leftcity, city.road.num, OBJECTID, NAME, LENG, geometry) %>% 
  st_write(here("data/shapefiles/final_cleaned_bridge_layer.shp"), append = FALSE)




