


library(tidyverse)
library(here)
library(sf)

options(scipen = 999)
source(here("code/utilities.R"))



# 10/1/24 asked Emi for GIS help on combining the road segments then breaking into equal length segments
# 10/7/24 got shapefile of equal length segments back from her: https://acr-cgrc.maps.arcgis.com/home/item.html?id=c7b4e7d1ab2240dcbf874b1a38bae804
# downloaded to data/
#equal_length_segs <- st_read(here("data/napa_sonoma_rd_dissolve_bylabel_unsplit_spltbypoints.shp"))

# it wasn't quite right but I was able to iterate through her method a few times to get a good merged road layer
# 


napa_sonoma_rds_arc_merged <- st_read(here("data/napa_sonoma_rds_arc_merged.shp")) %>% 
  select(-Shape_Leng) %>% # Shape_Leng from ArcGIS is in survey feet, remove... 
  st_transform(crs = 26910) %>% # convert to UTM
  mutate(seg.length = st_length(.)) %>% # then recalculate segment length in meters
  rename(label.city = label_city)



# various data checking
napa_sonoma_rds_arc_merged %>% 
  data.frame() %>% 
  filter(!is.na(label)) %>% 
#  filter(as.numeric(seg.length) >= 1300) %>% 
 filter(as.numeric(seg.length) < 1000) %>% 
  summary()
  nrow()
  summarise(tot.length = sum(as.numeric(seg.length)))

napa_sonoma_rds_utm <- readRDS(here("data/napa_sonoma_rds_utm"))


napa_sonoma_rds_utm_buff <- napa_sonoma_rds_utm %>% 
  st_buffer(10, endCapStyle = "FLAT")

zz <- st_intersection(napa_sonoma_rds_arc_merged, napa_sonoma_rds_utm_buff)

xx <- zz %>% 
  filter(label == label.1)

zlab = "Hillcrest Dr"

ggplot() +
  geom_sf(data = filter(napa_sonoma_rds_utm_buff, label == zlab)) +
  geom_sf(data = filter(napa_sonoma_rds_arc_merged, label == zlab)) +
  geom_sf(data = filter(xx, label == zlab), color = "red")


ggplot() +
  #geom_sf(data = readRDS("data/napa_sonoma_rds"), color = "blue") +
  geom_sf(data = napa_sonoma_rds_arc_merged, color = "red") +
  geom_sf(data = napa_sonoma_rds_arc_merged %>% 
            filter(!is.na(label)) %>% 
            filter(as.numeric(seg.length) >= 1000))



napa_sonoma_rds_arc_merged_clean <- napa_sonoma_rds_arc_merged %>% 
  select(-CONCATENAT) %>% 
  filter(as.numeric(seg.length) >= 1000) %>% # roads <1 km probably not very risky for mt lions
  filter(!str_starts(label.city, "NA_")) %>%  # there are only 2 roads with label == NA and seg.length >= 1000
  group_by(label.city) %>% 
  mutate(label.city = ifelse(row_number() > 1, paste(label.city, row_number(), sep = "_"), label.city)) %>% 
  ungroup() 

saveRDS(napa_sonoma_rds_arc_merged_clean, here("data/napa_sonoma_rds_arc_merged_clean"))


#
# 1. combine segments into a single object for each named road ############  this now deprecated, see ArcGIS step above  ############----
napa_sonoma_rds_utm <- readRDS(here("data/napa_sonoma_rds_utm"))  %>% 
  mutate(label = paste(label, leftcity, sep = "_"))





labels_classes <- napa_sonoma_rds_utm %>% 
  data.frame() %>% 
#  filter(!is.na(label)) %>% 
  distinct(label, class) %>% 
  group_by(label) %>% 
  count(class) %>% 
  ungroup()

# distinct(labels_classes, class)


# need to get only LINESTRINGS in order to break each road into equal length segments

# first need to join each named road together by label
# for some roads the segments line up well enough to create a LINESTRING

summarised_roads <- napa_sonoma_rds_utm %>% 
  mutate(label = replace_na(label, "noname")) %>% 
  group_by(label, leftcity) %>% 
  summarise() %>% 
  ungroup()
# but this still leaves many MULTILINESTRINGs
summarised_roads %>% 
  filter(st_geometry_type(.) == "MULTILINESTRING") %>% 
  nrow()

# peel off the LINESTRING roads that are ready to split
good_roads <- summarised_roads %>% 
  filter(st_geometry_type(.) == "LINESTRING")

# st_line_merge only works on MULTILINESTRINGs so need to filter those out, do st_line_merge, then add back to the LINESTRINGs.
# I still don't know how summarise() and st_line_merge() work differently
merged_roads <- summarised_roads %>% 
  filter(st_geometry_type(.) == "MULTILINESTRING")  %>% 
  group_by(label) %>% 
  st_line_merge() %>% 
  ungroup()

merged_roads %>% 
  filter(st_geometry_type(.) == "MULTILINESTRING") %>% 
  nrow()


# peel off the new LINESTRING roads and add to the good roads object
good_roads <- merged_roads %>% 
  filter(st_geometry_type(.) == "LINESTRING") %>% 
  bind_rows(good_roads)

# double check no non-LINESTRINGs have snuck in
# good_roads %>% filter(st_geometry_type(.) != "LINESTRING") %>% nrow()


# for those remaining MULTILINESTRING objects, there seems to be gaps between segments so st_line_merge doesn't work
# need to create another segment to bridge the gap
# this is just the remaining MULTILINESTRINGs that apparently have gaps
gap_roads <- merged_roads %>% 
  filter(st_geometry_type(.) == "MULTILINESTRING") %>%
  st_cast("LINESTRING") %>% 
  mutate(segment.length = st_length(.),
         segment.length = as.numeric(segment.length))

get_nearest_segment <- function(zroad) {
  
  zgap = filter(gap_roads, str_detect(label, zroad))
  znearest = st_nearest_feature(zgap, zgap)
  zdist = st_distance(zgap)
  
}



# create the gap bridges  
road_bridges <- gap_roads %>%  
  group_by(label) %>% 
  mutate(
    lead = geometry[row_number() + 1],
    dist = st_distance(geometry, lead, by_element = T),
    dist = as.numeric(dist),
    bridge = st_nearest_points(geometry, lead, pairwise = TRUE),
    is.bridge = TRUE
  ) %>% 
  data.frame() %>% 
  select(label, leftcity, segment.length = dist, is.bridge, geometry = bridge) %>% 
  filter(!is.na(segment.length)) %>% 
  st_as_sf()


# here we only bridge gaps that are 65m or less
# picked 65m to be no more than 5% of the equal segment length
bridged_roads <- road_bridges %>% 
  filter(segment.length <= 65) %>% 
  bind_rows(gap_roads) %>% 
  group_by(label, leftcity) %>% 
  summarise() %>% 
  st_line_merge() %>% 
  ungroup()

bridged_roads %>% 
  filter(st_geometry_type(.) == "MULTILINESTRING") %>% 
  nrow()


# peel off the new LINESTRING roads and add to the good roads object
good_roads <- bridged_roads %>% 
  filter(st_geometry_type(.) == "LINESTRING") %>% 
  bind_rows(good_roads) %>% 
  distinct()

# the remaining roads that are still MULTILINESTRING should just be treated as separate roads for the purpose of extracting habitat values
multi_seg_roads <- bridged_roads %>% 
  filter(st_geometry_type(.) == "MULTILINESTRING") %>%
  st_cast("LINESTRING") %>% 
  left_join(labels_classes) %>% 
  group_by(label) %>% 
  mutate(label2 = paste(label, row_number(), sep = "_")) %>% 
  ungroup() %>% 
  mutate(road.length = st_length(.))

zz <- multi_seg_roads %>% 
  filter(label == "10th St_Santa Rosa") %>% 
  #group_by(label) %>% 
  st_distance()



ggplot() +
  geom_sf(data = road_bridges %>%  filter(str_detect(label, "Langtry Rd")), linewidth = 3, alpha = 0.3)  +
  geom_sf(data = multi_seg_roads %>%  filter(str_detect(label, "Langtry Rd")), aes(color = label)) 


multi_seg_roads %>% 
  filter(class %in% c("Local", "Other")) %>% 
  st_write(., "data/multi_seg_roads_local_other.shp", append = FALSE)

# any MULTILINESTRING left?
multi_seg_roads %>% 
  filter(st_geometry_type(.) == "MULTILINESTRING") %>% nrow()

multi_seg_roads_filt <- multi_seg_roads %>% 
  filter(as.numeric(road.length) > 650)

multi_seg_roads %>% 
  data.frame() %>% 
  group_by(class) %>% 
  summarize(min.ln = min(as.numeric(road.length)),
            mean.ln = mean(as.numeric(road.length)),
            max.ln = max(as.numeric(road.length))) %>% view()

napa_sonoma_rds_utm_merged <- multi_seg_roads_filt %>% 
  dplyr::select(label, geometry) %>% 
  bind_rows(good_roads) %>% 
  distinct() %>% 
  mutate(road.length = st_length(.))


napa_sonoma_rds_utm_merged %>% 
  filter(st_geometry_type(.) == "MULTILINESTRING") %>% nrow()

saveRDS(napa_sonoma_rds_utm_merged, here("data/napa_sonoma_rds_utm_merged"))

st_write(napa_sonoma_rds_utm_merged, "data/napa_sonoma_rds_utm_merged.shp", append = FALSE)

napa_sonoma_rds_utm_merged <- napa_sonoma_rds_utm_merged %>% 
  mutate(road.length = st_length(.))


# 2a. now split the merged road objects into equal length segments ----
# mean bbmm crossing segment is 1300 m so starting with that


#napa_sonoma_rds_utm_merged <- readRDS(here("data/archive_roads/napa_sonoma_rds_utm_merged"))
 
napa_sonoma_rds_arc_merged_clean <- readRDS(here("data/napa_sonoma_rds_arc_merged_clean"))

make_road_pts <- function(zroad) {     
  road_in <- filter(napa_sonoma_rds_arc_merged_clean, label.city == zroad)
  zdist = seq(0, as.numeric(road_in$seg.length), by = 1300)
  road_pts <- road_in %>%
    st_as_sfc() %>%   
    st_line_interpolate(dist = zdist) 
}


zroad = napa_sonoma_rds_arc_merged_clean$label.city[1]
zz <- make_road_pts(zroad)


system.time(
  rd_pts <- map(napa_sonoma_rds_arc_merged_clean$label.city, make_road_pts)
)
names(rd_pts) <- napa_sonoma_rds_arc_merged_clean$label.city
# 211 sec 9/30/24


split_roads <- function(zroad) {
  road_in <- filter(napa_sonoma_rds_arc_merged_clean, label.city == zroad) 
  
  road_splitter <- rd_pts[[zroad]] %>% 
    st_buffer(., 0.01) %>% 
    st_combine() 
  
  
  rd_split <- lwgeom::st_split(road_in, road_splitter) %>% 
    st_collection_extract(., "LINESTRING") %>% 
    mutate(seg.length = st_length(.)) %>% 
    filter(as.numeric(seg.length) > 1) %>% 
    mutate(seg.label = paste(label.city, row_number(), sep = "_"))
  
}



system.time(
  napa_sonoma_rds_equal_segs <- map(napa_sonoma_rds_arc_merged_clean$label.city, split_roads)
)
names(napa_sonoma_rds_equal_segs) <- napa_sonoma_rds_arc_merged_clean$label.city
# 317 sec 9/30/24
# 17 sec 10/10/24 with Arc created merged roads


saveRDS(napa_sonoma_rds_equal_segs, here("data/napa_sonoma_rds_equal_segs"))
napa_sonoma_rds_equal_segs <- readRDS(here("data/napa_sonoma_rds_equal_segs"))


ggplot() +
  geom_sf(data = road_in, linewidth = 3, color = "gray70") +
  geom_sf(data = rd_split, aes(color = seg.label))


# 2b.  now split the merged road objects anywhere there is a road intersection ----


napa_sonoma_rds_intersections <- st_collection_extract(st_intersection(napa_sonoma_rds_arc_merged_clean), "POINT")

ggplot() +
  geom_sf(data = napa_sonoma_rds_intersection_splits %>% filter(label.city == "Adobe Rd_Penngrove"), aes(color = label.city.2)) +
  geom_sf(data = napa_sonoma_rds_intersections %>% filter(label.city == "Adobe Rd_Penngrove"))



napa_sonoma_rds_intersection_splits <- lwgeom::st_split(napa_sonoma_rds_arc_merged_clean, napa_sonoma_rds_intersections)
napa_sonoma_rds_intersection_splits <- st_collection_extract(napa_sonoma_rds_intersection_splits, "LINESTRING") %>% 
  group_by(label.city) %>% 
  mutate(label.city.2 = paste(label.city, row_number(), sep = "_")) %>% 
  ungroup()


saveRDS(napa_sonoma_rds_intersection_splits, here("data/napa_sonoma_rds_intersection_splits"))



# 3. NO RUN make a shapefile with a buffered polygon for each road segment ----

napa_sonoma_rds_equal_segs <- readRDS(here("data/napa_sonoma_rds_equal_segs")) %>% 
  bind_rows()


rds_equal_segs_buffer <- napa_sonoma_rds_equal_segs %>% 
  st_buffer(dist = 100, endCapStyle = "FLAT")


ggplot() +
  geom_sf(data = rds_equal_segs_buffer %>%  filter(str_detect(label, "Langtry Rd")), aes(color = seg.label)) +
  geom_sf(data = napa_sonoma_rds_equal_segs %>%  filter(str_detect(label, "Langtry Rd")))


st_write(rds_equal_segs_buffer, "data/rds_equal_segs_buffer.shp")
