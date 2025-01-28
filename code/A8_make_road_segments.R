


library(tidyverse)
library(here)
library(sf)

options(scipen = 999)
source(here("code/utilities.R"))



# 10/1/24 asked Emi for GIS help on combining the road segments then breaking into equal length segments
# 10/7/24 got shapefile of equal length segments back from her: https://acr-cgrc.maps.arcgis.com/home/item.html?id=c7b4e7d1ab2240dcbf874b1a38bae804
# downloaded to data/
#equal_length_segs <- st_read(here("data/napa_sonoma_rd_dissolve_bylabel_unsplit_spltbypoints.shp"))

# it wasn't quite right but I was able to iterate through her method a few times to get a good merged road layer. 
# see A3_road_layers.R
final_cleaned_road_layer <- readRDS(here("data/final_cleaned_road_layer")) %>% 
  st_transform(crs = 26910) %>%  # to UTM 10N
  mutate(road.label = paste(label, leftcity, city.road.num, sep = "_"),
         road.length = st_length(.))

final_cleaned_road_layer <- final_cleaned_road_layer %>% 
  filter(as.numeric(road.length) > 500)

readRDS(here("model_objects/all_bbmm_roads_1step")) %>% 
  bind_rows(.id = "crossing.step") %>% 
  summary()

#
# 2 splitting roads into segments
# slightly different method for splitting into equal segment vs splitting by road intersections

# 2a. split the merged road objects into equal length segments ----
# mean bbmm crossing segment is 888 m so splitting into equal segments by that

make_equal_length_road_pts <- function(zroad) {     
  road_in <- filter(final_cleaned_road_layer, road.label == zroad)
  zdist = seq(0, as.numeric(road_in$road.length), by = 888)
  road_pts <- road_in %>%
    st_as_sfc() %>%   
    st_line_interpolate(dist = zdist) 
}

# test on one road
# zroad = "13th St_Santa Rosa_1"
# zz <- make_road_pts(zroad)


system.time(
  rd_pts <- map(final_cleaned_road_layer$road.label, make_road_pts)
)
names(rd_pts) <- final_cleaned_road_layer$road.label
# 211 sec 9/30/24
# 7.67 1/28/25

split_roads <- function(zroad) {
  road_in <- filter(final_cleaned_road_layer, road.label == zroad) 
  
  road_splitter <- rd_pts[[zroad]] %>% 
    st_buffer(., 0.01) %>% 
    st_combine() 
  
  rd_split <- lwgeom::st_split(road_in, road_splitter) %>% 
    st_collection_extract(., "LINESTRING") %>% 
    mutate(seg.length = st_length(.)) %>% 
    filter(as.numeric(seg.length) > 1) %>% 
    mutate(seg.label = paste(road.label, row_number(), sep = "_"))
  
}



system.time(
  napa_sonoma_rds_equal_segs <- map(final_cleaned_road_layer$road.label, split_roads)
)
names(napa_sonoma_rds_equal_segs) <- final_cleaned_road_layer$road.label
# 317 sec 9/30/24
# 17 sec 10/10/24 with Arc created merged roads
napa_sonoma_rds_equal_segs %>% 
#readRDS(here("data/napa_sonoma_rds_equal_segs")) %>% 
  bind_rows(.id = "label") %>% 
  st_as_sf() %>% 
  st_transform(crs = 26910) %>% 
  st_write("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_sonoma_main_roads/napa_sonoma_rds_equal_segs.shp", append = FALSE)

saveRDS(napa_sonoma_rds_equal_segs, here("data/napa_sonoma_rds_equal_segs"))
napa_sonoma_rds_equal_segs <- readRDS(here("data/napa_sonoma_rds_equal_segs"))


ggplot() +
  geom_sf(data = napa_sonoma_rds_equal_segs[["Adobe Rd_Penngrove_1"]], aes(color = seg.label)) +
  geom_sf(data = rd_pts[["Adobe Rd_Penngrove_1"]])



# 2b. split the merged road objects anywhere there is a road intersection ----
# get the points for each intersection along each road
napa_sonoma_rds_intersections <- st_collection_extract(st_intersection(final_cleaned_road_layer), "POINT")

# split roads at those intersection points
napa_sonoma_rds_intersection_splits <- lwgeom::st_split(final_cleaned_road_layer, napa_sonoma_rds_intersections) %>% 
  st_collection_extract("LINESTRING") %>% 
  group_by(road.label) %>% 
  mutate(seg.label = paste(road.label, row_number(), sep = "_")) %>% 
  ungroup()

ggplot() +
  geom_sf(data = napa_sonoma_rds_intersection_splits %>% filter(str_detect(road.label, "Adobe Rd_Penngrove")), aes(color = seg.label)) +
  geom_sf(data = napa_sonoma_rds_intersections %>% filter(str_detect(road.label, "Adobe Rd_Penngrove")))


saveRDS(napa_sonoma_rds_intersection_splits, here("data/napa_sonoma_rds_intersection_splits"))


