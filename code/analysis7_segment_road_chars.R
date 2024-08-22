


library(tidyverse)
library(here)
library(sf)
library(units)

options(scipen = 999)

source(here("code/utilities.R"))

# county bondaries for filtering statewide layers
study_area_counties <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/data/study_area_counties") %>% 
  st_transform(crs = 26910) %>% 
  filter(NAME %in% c("Sonoma", "Napa"))


# roads
napa_sonoma_rds_equal_segs <- readRDS(here("data/napa_sonoma_rds_equal_segs")) %>% 
  bind_rows() %>% 
  rename("road.seg.length" = seg.length)

# Sonoma county maintained bridges
# from https://acr-cgrc.maps.arcgis.com/home/item.html?id=e095e2cc5d494ba6a3eeaf84ac528806#overview
# accessed 8/2/2024
# open in ArcGIS Pro and then choosing “export features”. download as a shapefile

sonoma_county_bridges <- readxl::read_xlsx("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/sonoma_culverts.xlsx") %>% 
  st_as_sf(x = .,
           coords = c("Lon", "Lat"),
           crs = 4326) %>% 
  st_transform(crs = 26910) %>% 
  select("OBJECTID" = "OBJECTID *", BridgeID, "FAC" = RoadName, Postmile, PMBeg, PMEnd, "NAME" = CrossFeature, CrossFeatureType, "LENG" = LengthSpan, LengthStruc, last_edited_date, geometry) %>% 
  mutate(across(c(OBJECTID, BridgeID, FAC, Postmile, PMBeg, PMEnd, NAME, CrossFeatureType, LENG, LengthStruc, last_edited_date), ~as.character(.)))


# state maintained bridges
# from here https://gisdata-caltrans.opendata.arcgis.com/datasets/ea685fd702f840a7a751b12373d6249c_0/explore
# accessed 7/23/2024
state_bridges <- st_read("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/State_Highway_Bridges/State_Highway_Bridges.shp") %>% 
  st_transform(crs = 26910) %>% 
  st_intersection(study_area_counties) %>% 
  select(OBJECTID, PM, NAME, FAC, LENG, MAINSPANS, APPSPANS, DECKWIDTH, geometry) %>% 
  mutate(across(c(OBJECTID, PM, NAME, FAC, LENG, MAINSPANS, APPSPANS, DECKWIDTH), ~as.character(.)))


# statewide local bridges
# from https://hgl.harvard.edu/catalog/stanford-cx668kz9577
# accessed 8/2/2024
# mostly need this for Napa county
state_bridges_harvard <- st_read("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/state_bridges_harvard/loc_bridges2015.shp") %>% 
  st_transform(crs = 26910) %>% 
  st_intersection(study_area_counties) %>% 
  select(PM, NAME, FAC, LENG, MAINSPANS, geometry) %>% 
  mutate(across(c(PM, NAME, FAC, LENG, MAINSPANS), ~as.character(.)))


ggplot() +
  geom_sf(data = study_area_counties) +
  geom_sf(data = napa_sonoma_rds_equal_segs) +
  geom_sf(data = state_bridges, color = "red", alpha = 0.5) +
  geom_sf(data = state_bridges_harvard, color = "green", alpha = 0.5) +
  geom_sf(data = sonoma_county_bridges, color = "blue", alpha = 0.5)



all_bridges <- bind_rows(sonoma_county_bridges, state_bridges, state_bridges_harvard) %>% 
  mutate(across(c(FAC, NAME), ~str_to_title(.))) %>% 
  arrange(FAC, NAME)
