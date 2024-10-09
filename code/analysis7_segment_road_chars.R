


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
  rename("road.seg.length" = seg.length)


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
         bridge.source = "harvard")


ggplot() +
  geom_sf(data = study_area_counties) +
  geom_sf(data = napa_sonoma_rds_equal_segs) +
  geom_sf(data = state_bridges, color = "red", alpha = 0.5) +
  geom_sf(data = state_bridges_harvard, color = "green", alpha = 0.5) +
  geom_sf(data = sonoma_county_bridges, color = "blue", alpha = 0.5)



all_bridges <- bind_rows(sonoma_county_bridges, state_bridges, state_bridges_harvard) %>% 
  mutate(across(c(FAC, NAME), ~str_to_title(.))) %>% 
  arrange(FAC, NAME, bridge.source) %>%
  group_by(FAC, NAME) %>%
  mutate(num.dups = n(),
         dup.label = paste(FAC, NAME, row_number(), sep = "_")) %>% 
  ungroup() %>% 
  st_as_sf()


all_bridges_dups <- all_bridges  %>% 
  filter(num.dups > 1)


st_write(all_bridges, "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_sonoma_bridges/napa_sonoma_bridges.shp", append = FALSE)

# filter to just the bridges in mt lion home ranges

pumas <- readRDS(here("data/analysis_table")) %>% 
  mutate(animal.id = ifelse(animal.id == "P5*", "P5", animal.id)) %>% 
  ungroup() %>% 
  distinct(animal.id) %>% 
  filter(!animal.id %in% exclude_pumas, !animal.id %in% hr_exclude_pumas)

get_hr_bridges <- function(zpuma) {
  hr_uds <- readRDS(here("model_objects/puma_hr_uds"))[[zpuma]] %>% 
    as.sf(., DF = "PDF", level.UD = 0.999) %>% 
    st_transform(crs = 26910) %>% 
    filter(str_detect(name, "est"))
  
  rds_buff30 <- readRDS(here("data/napa_sonoma_rds_equal_segs")) %>% 
    bind_rows() %>% 
    rename("road.seg.length" = seg.length)%>% 
    st_buffer(., 30, endCapStyle = "FLAT")
  
  rd_bridges <- st_intersection(rds_buff30, all_bridges)
  
  hr_bridges <- st_intersection(hr_uds, rd_bridges)
}

system.time(
  hr_bridges <- map_df(pumas$animal.id, get_hr_bridges)
)


hr_bridges_unique <- hr_bridges %>% 
  select(-name) %>% 
  distinct()
  
st_write(hr_bridges_unique, "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_sonoma_bridges/hr_bridges_unique2.shp", append = FALSE)

# read this into GIS and check each bridge to make sure they seem appropriate for mt lion crossing.
# I excluded 
#     duplicates from the different data sources. generally this was dups between the sonoma and harvard datasets, and I kept the sonoma points because their attribute table is more complete and the coords generally seemed to match the actual bridge better
#     viaducts that were too small or one-sided for crossing
#     bridges that were over crossings for other roads that didn't also have a corridor of habitat

saveRDS(ud_rd_seg_habitats, here("data/ud_rd_seg_habitats"))


exclude_bridges <- read.csv(here("data/exclude_bridges.csv"))

hr_bridges_puma_ok <- hr_bridges_unique %>% 
  filter(!dup.label %in% exclude_bridges$dup_lbl)

st_write(hr_bridges_puma_ok, "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_sonoma_bridges/hr_bridges_puma_ok.shp", append = FALSE)


puma_ok_bridges_per_seg <- hr_bridges_puma_ok %>% 
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
