


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


# roads
napa_sonoma_rds_equal_segs <- readRDS(here("data/napa_sonoma_rds_equal_segs")) %>% 
  bind_rows() %>% 
  rename("road.seg.length" = seg.length) %>% 
  mutate(label.city = abbreviate_roads(label.city),
         seg.label = abbreviate_roads(seg.label))


#
# getting number of bridges per segment ----
# Sonoma county maintained bridges
# from https://acr-cgrc.maps.arcgis.com/home/item.html?id=e095e2cc5d494ba6a3eeaf84ac528806#overview
# accessed 8/2/2024
# open in ArcGIS Pro and then choosing “export features”. download as a shapefile

sonoma_county_bridges <- readxl::read_xlsx("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/sonoma_culverts.xlsx") %>% 
  st_as_sf(x = .,
           coords = c("Lon", "Lat"),
           crs = 4326) %>% 
  st_transform(crs = 26910) %>% 
  select("OBJECTID" = "OBJECTID *", BridgeID, "FAC" = RoadName, "PM" = Postmile, "NAME" = CrossFeature, CrossFeatureType, "LENG" = LengthSpan, LengthStruc, last_edited_date, geometry) %>% 
  mutate(across(c(OBJECTID, BridgeID, FAC, PM, NAME, CrossFeatureType, LENG, LengthStruc), ~as.character(.)),
         bridge.source = "sonoma")

# state maintained bridges
# from here https://gisdata-caltrans.opendata.arcgis.com/datasets/ea685fd702f840a7a751b12373d6249c_0/explore
# accessed 7/23/2024
state_bridges <- st_read("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/State_Highway_Bridges/State_Highway_Bridges.shp") %>% 
  st_transform(crs = 26910) %>% 
  st_intersection(study_area_counties) %>% 
  select(OBJECTID, PM, NAME, FAC, LENG, MAINSPANS, geometry) %>% 
  mutate(across(c(OBJECTID, PM, NAME, FAC, LENG, MAINSPANS), ~as.character(.)),
         bridge.source = "state")


# statewide local bridges
# from https://hgl.harvard.edu/catalog/stanford-cx668kz9577
# accessed 8/2/2024
# mostly need this for Napa county
state_bridges_harvard <- st_read("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/state_bridges_harvard/loc_bridges2015.shp") %>% 
  st_transform(crs = 26910) %>% 
  st_intersection(study_area_counties) %>% 
  select(PM, NAME, FAC, LENG, MAINSPANS, geometry) %>% 
  mutate(across(c(PM, NAME, FAC, LENG, MAINSPANS), ~as.character(.)),
         bridge.source = "harvard") %>% 
  filter(!(NAME =="SONOMA CREEK" & FAC == "ARNOLD DR")) # just keeps causing problems


all_bridges <- bind_rows(sonoma_county_bridges, state_bridges, state_bridges_harvard) %>% 
  mutate(across(c(FAC, NAME), ~str_to_title(.)),
         NAME = case_when(NAME == "Warm Srings Creek" ~ "Warm Springs Creek", 
                          TRUE ~ NAME),
         FAC = abbreviate_roads(FAC),
         FAC = case_when(FAC == "Odd Fellows Park R" ~ "Odd Fellows Park Rd",
                         FAC == "Mark West Sta Rd" ~ "Mark West Station Rd",
                         FAC == "North Fitch Mtn Rd" ~ "North Fitch Mountain Rd",
                         FAC == "S Fitch Mtn Rd" ~ "South Fitch Mountain Rd",
                         FAC == "Alexander Valley R" ~ "Alexander Valley Rd",
                         FAC == "Sonoma Mtn Rd" ~ "Sonoma Mountain Rd",
                         FAC %in% c("Stwrt Pt-Skgg Sprg", "Stewarts Pt Skaggs Springs") ~ "Stewarts Point Skaggs Springs",
                         FAC %in% c("Sweetwater Sprg Rd", "Sweetwater Springs Rd", "Sweetwtr Sprngs Rd", "Swtwater Spring Rd") ~ "Sweetwater Springs Rd",
                         FAC %in% c("U.s. Highway 101", "U.s. Route 101", "U.s. Route 101 Nb", "U.s. Route 101 Sb", "Us Highway 101", "Us Highway 101 Nb", "Us Highway 101 Sb", "Hwy 101 Sb", "Hwy 101 Nb") ~ "Hwy 101",
                         FAC == "Mark W Spring Rd" ~ "Mark West Springs Rd",
                         FAC == "Rohnert Park Expy" ~ "Rohnert Park Expwy",
                         FAC == "Middle Terr" ~ "Middle Ter",
                         FAC %in% c("Monte Vista Terrac", "Monte Vista Terr") ~ "Monte Vista Ter",
                         FAC %in% c("Trenton Hldsbg Rd", "Trenton Healdsbrg") ~ "Trenton Healdsburg Rd",
                         FAC == "Washington Schl Rd" ~ "Washington School Rd",
                         FAC == "Kennilworth Ave" ~ "Kenilworth Ave", #1 n is correct per google maps
                         FAC %in% c("Chiles-Pope Vly Rd", "Chiles Pope Vly Rd") ~ "Chiles Pope Valley Rd",
                         FAC == "Dry Cr Rd" ~ "Dry Creek Rd",
                         FAC == "E Railroad Ave" ~ "East Railroad Ave",
                         FAC %in% c("Eb Hwy 12", "Wb Hwy 12", "Hwy 12 Eb", "Hwy 12 Wb") ~ "Hwy 12",
                         FAC %in% c("Eb Hwy 29", "Wb Hwy 29", "Hwy 29 Eb", "Hwy 29 Wb", "Hwy 29 Nb", "Hwy 29 Sb") ~ "Hwy 29",
                         FAC == "Fort Ross Rd." ~ "Fort Ross Rd",
                         FAC == "Franz Valey Sch Rd" ~ "Franz Valley School Rd",
                         FAC == "Old Redwood Hwy N" ~ "Old Redwood Hwy",
                         FAC == "Old Gravenstein Hy" ~ "Old Gravenstein Hwy",
                         FAC == "Via Columbard" ~ "Via Colombard",
                         TRUE ~ FAC)) %>% 
  arrange(FAC, NAME, bridge.source) %>%
  group_by(FAC, NAME) %>%
  mutate(num.dups = n(),
         fac.name = paste(FAC, NAME, sep = "_"),
         dup.label = paste(FAC, NAME, row_number(), sep = "_")) %>% 
  ungroup() %>% 
  st_as_sf()

distinct(all_bridges, FAC) %>% arrange(FAC) %>% view()

saveRDS(all_bridges, "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_sonoma_bridges/all_bridges")

# filtering out duplicate bridges. currently considering any objects with the same creek and road names that are < 20 from each other and come from different data sources to be duplicates
all_bridges_dups <- all_bridges  %>% 
  filter(num.dups > 1)
  

#' get_close_dup_bridges
#' 
#' ID pairs of bridge objects that have the same FAC and NAME and are within some distance of each other, default 25 m. this uses all_bridges_dups to save computing time by only considering the cases where different points have the same FAC and NAME
#'
#' @param zfac.name concatenation of FAC and NAME 
#' @param too.close.dist the threshold distance that is used to fuzzy ID the same actual bridge
#'
#' @return data frame with new fields:
#' too.close for whether the pair of points is less than too.close.dist, and 
#' dup.distance for the actual distance between points
#'
#' @examples
get_close_dup_bridges = function(zfac.name, too.close.dist = 25) {

  dup <- all_bridges_dups %>% 
    filter(fac.name == zfac.name) %>% 
    select(fac.name, dup.label) 
  
  dup_dist <- st_distance(dup, dup) 
  
  colnames(dup_dist) <- str_replace_all(c(dup$dup.label), " ", "zfooz")
  
  dup_dist_longer <- dup %>% data.frame() %>% select(-geometry) %>%  
    bind_cols(data.frame(dup_dist)) %>% 
    pivot_longer(contains("zfooz"), names_to = "dup.label.to", values_to = "dup.distance") %>% 
    distinct() %>% 
    mutate(dup.label.to = str_replace_all(dup.label.to, "zfooz", " "),
           dup.distance = as.numeric(dup.distance),
           too.close = dup.distance > 0 & dup.distance < too.close.dist) %>% 
    filter(dup.label != dup.label.to)
  
  true_dups <- dup_dist_longer %>% 
    filter(too.close == TRUE) %>% 
    pivot_longer(cols = contains("dup.label"), values_to = "dup.label") %>% 
    distinct(dup.label, fac.name, too.close, dup.distance)

}

system.time(
close_dup_bridges <- map_df(distinct(all_bridges_dups, fac.name)$fac.name, get_close_dup_bridges)
) # 5 sec




# remove duplicate bridges that are from the Harvard db and within 25 m of a non-Harvard db bridge with the same FAC and NAME
all_bridges_no_dup <- full_join(all_bridges, close_dup_bridges) %>% 
  group_by(fac.name) %>%
  mutate(drop.dup = ifelse(any(bridge.source != "harvard") & too.close == TRUE & bridge.source == "harvard", TRUE, FALSE),
         drop.dup = replace_na(drop.dup, FALSE)) %>% 
  ungroup() %>% 
  filter(drop.dup != TRUE)



# save shapefiles of intermediate steps for checking work in ArcGIS
st_write(all_bridges, "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_sonoma_bridges/napa_sonoma_bridges.shp", append = FALSE)

st_write(all_bridges_no_dup, "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_sonoma_bridges/napa_sonoma_bridges_no_dup.shp", append = FALSE)

# filter to just the bridges in mt lion home ranges

pumas <- readRDS(here("data/analysis_table")) %>% 
  mutate(animal.id = ifelse(animal.id == "P5*", "P5", animal.id)) %>% 
  ungroup() %>% 
  distinct(animal.id) %>% 
  filter(!animal.id %in% exclude_pumas, !animal.id %in% hr_exclude_pumas)

#' get_hr_bridges
#' 
#' mask the bridge layer to the puma home ranges. also links bridges to the equal length road segments, which should be pulled out into a separate function
#'
#' @param zpuma 
#'
#' @return
#' @export
#'
#' @examples
get_hr_bridges <- function(zpuma) {
  hr_uds <- readRDS(here("model_objects/puma_hr_uds"))[[zpuma]] %>% 
    as.sf(., DF = "PDF", level.UD = 0.999) %>% 
    st_transform(crs = 26910) %>% 
    filter(str_detect(name, "est"))
  
  rds_buff30 <- readRDS(here("data/napa_sonoma_rds_equal_segs")) %>% 
    bind_rows() %>% 
    rename("road.seg.length" = seg.length)%>% 
    st_buffer(., 10, endCapStyle = "FLAT")
  
  rd_bridges <- st_intersection(rds_buff30, all_bridges_no_dup)
  
  hr_bridges <- st_intersection(hr_uds, rd_bridges)
}

system.time(
  hr_bridges <- map_df(pumas$animal.id, get_hr_bridges)
) # 117, 169, 304, 230, 162 on 10/16/24


row.names(hr_bridges) <- NULL

hr_bridges_unique <- hr_bridges %>% 
  select(-name, -road.seg.length, -label.city, -last_edited_date) %>% 
  distinct() %>% 
  rowwise() %>% 
  mutate(correct.road = grepl(FAC, seg.label))

# even with the road name cleaning above there are still mismatched names.
# not enough time to code solutions for all, so for now exporting this and manually IDing the name mismatches to keep 
filter(hr_bridges_unique, correct.road == FALSE) %>% 
  data.frame() %>% 
  select(-geometry) %>% 
  mutate(correct.road.manual = correct.road) %>% 
  select(seg.label, FAC, correct.road.manual, everything()) %>% 
  write.csv(here("data/check_correct_road.csv"), row.names = FALSE)

manual_good_bridges <- read.csv(here("data/check_correct_road.csv")) %>% 
  filter(correct.road.manual == TRUE) %>% 
  mutate(correct.road = correct.road.manual) %>% 
  select(-correct.road.manual) %>% 
  mutate(across(c(OBJECTID, MAINSPANS), ~as.character(.)))

clean_hr_bridges_unique <- hr_bridges_unique %>% 
  filter(correct.road != FALSE) %>% 
  bind_rows(manual_good_bridges)


st_write(clean_bridges, "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_sonoma_bridges/clean_bridges.shp", append = FALSE)

# read this into GIS and check each bridge to make sure they seem appropriate for mt lion crossing.
# I excluded 
#     duplicates from the different data sources. generally this was dups between the sonoma and harvard datasets, and I kept the sonoma points because their attribute table is more complete and the coords generally seemed to match the actual bridge better
#     viaducts that were too small or one-sided for crossing
#     bridges that were over crossings for other roads that didn't also have a corridor of habitat
# this was an iterative process
# exclude_bridges.csv is the set of manually excluded bridges selected when local roads were not included
# exclude_bridges2.csv is the set of manually excluded bridges selected once local roads were included

exclude_bridges <- bind_rows(read.csv(here("data/exclude_bridges.csv")) %>% dplyr::select(dup_lbl),  
                             read.csv(here("data/exclude_bridges2.csv")) %>% dplyr::select(dup_lbl),  
                             read.csv(here("data/exclude_bridges_hillside_shv.csv")) %>% dplyr::select(dup_lbl)) %>% 
  distinct()


clean_hr_bridges_unique %>% 
  filter(dup.label %in% exclude_bridges$dup_lbl) %>% 
  st_write("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_sonoma_bridges/exclude_bridges.shp", append = FALSE)



clean_bridges <- clean_hr_bridges_unique %>% 
  filter(!dup.label %in% exclude_bridges$dup_lbl)

st_write(clean_bridges, "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_sonoma_bridges/clean_bridges.shp", append = FALSE)


check_bridge_dist_again <- st_distance(clean_bridges, clean_bridges) %>% 
  data.frame() 

colnames(check_bridge_dist_again) <- str_replace_all(c(clean_bridges$dup.label), " ", "zfooz")

dup_dist_longer <- check_bridge_dist_again %>% 
  mutate(dup.label = clean_bridges$dup.label) %>% 
  pivot_longer(-dup.label, names_to = "dup.label.to", values_to = "dup.distance") %>% 
  distinct() %>% 
  mutate(dup.label.to = str_replace_all(dup.label.to, "zfooz", " "),
         dup.distance = as.numeric(dup.distance)) %>% 
  filter(dup.label != dup.label.to) %>% 
  arrange(dup.distance)

  
puma_ok_bridges_per_seg <- clean_bridges %>% 
  data.frame() %>% 
  select(-geometry) %>% 
  group_by(seg.label) %>% 
  summarise(num.bridge = n()) %>% 
  ungroup() 

saveRDS(puma_ok_bridges_per_seg, here("data/puma_ok_bridges_per_seg"))


# want some variable to represent traffic conditions ----
# number of lanes and speed limit data mostly incomplete so not using


napa_sonoma_rds_utm <- readRDS(here("data/napa_sonoma_rds_utm")) %>% 
  select(-pubpriv)


equal_length_chars <- st_intersection(napa_sonoma_rds_equal_segs, napa_sonoma_rds_utm) %>% 
  filter(st_is(., c("MULTILINESTRING", "LINESTRING")) ) 



equal_length_chars_summarized <- equal_length_chars %>% 
  data.frame() %>% 
  group_by(seg.label) %>% 
  summarise(min.sl = min(speedlimit, na.rm = TRUE),
            mean.sl = mean(speedlimit, na.rm = TRUE),
            max.sl = max(speedlimit, na.rm = TRUE),
            min.lanes = min(lanes, na.rm = TRUE),
            mean.lanes = mean(lanes, na.rm = TRUE),
            max.lanes = max(lanes, na.rm = TRUE),
            num.dup = n()) %>% 
  ungroup()

saveRDS(equal_length_chars_summarized, here("data/equal_length_chars_summarized"))


equal_length_classes <- equal_length_chars %>% 
  data.frame() %>% 
  distinct(seg.label, road.seg.length, class) %>% 
  mutate(class = factor(class, levels = c("Collector", "Arterial", "Highway", "Freeway"))) %>% 
  group_by(seg.label, road.seg.length) %>% 
  summarise(classes = paste(unique(class), collapse = ", "),
            max.class = max(as.numeric(class))) %>% 
  ungroup() 


saveRDS(equal_length_classes, here("data/equal_length_classes"))




ggplot() +
  geom_sf(data = filter(napa_sonoma_rds_equal_segs, label == "Dutton Meadow"), aes(color = seg.label), linewidth = 3) +
  geom_sf(data = filter(napa_sonoma_rds_utm, label == "Dutton Meadow"))
