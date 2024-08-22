


library(tidyverse)
library(here)
library(sf)

options(scipen = 999)
source(here("code/utilities.R"))





# 1. combine segments into a single object for each named road ----
napa_sonoma_rds_utm <- readRDS(here("data/napa_sonoma_rds_utm"))


# need to get only LINESTRINGS in order to break each road into equal length segments

# first need to join each named road together by label
# for some roads the segments line up well enough to create a LINESTRING
# but this still leaves ~773 MULTILINESTRINGs
summarised_roads <- napa_sonoma_rds_utm %>% 
  group_by(label) %>% 
  summarise() %>% 
  ungroup()

# peel off the LINESTRING roads that are ready to split
good_roads <- summarised_roads %>% 
  filter(st_geometry_type(.) == "LINESTRING")

# st_line_merge only works on MULTILINESTRINGs so need to filter those out, do st_line_merge, then add back to the LINESTRINGs.
# road2 still has 92 MULTILINESTRINGs
# I still don't know how summarise() and st_line_merge() work differently
merged_roads <- summarised_roads %>% 
  filter(st_geometry_type(.) == "MULTILINESTRING")  %>% 
  group_by(label) %>% 
  st_line_merge() %>% 
  ungroup()


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
  select(label, segment.length = dist, is.bridge, geometry = bridge) %>% 
  filter(!is.na(segment.length)) %>% 
  st_as_sf()


# here we only bridge gaps that are 10m or less
bridged_roads <- road_bridges %>% 
  filter(segment.length < 10) %>% 
  bind_rows(gap_roads) %>% 
  group_by(label) %>% 
  summarise() %>% 
  st_line_merge()

# peel off the new LINESTRING roads and add to the good roads object
good_roads <- bridged_roads %>% 
  filter(st_geometry_type(.) == "LINESTRING") %>% 
  bind_rows(good_roads) %>% 
  distinct()

# the remaining roads that are still MULTILINESTRING should just be treated as separate roads for the purpose of extracting habitat values
multi_seg_roads <- bridged_roads %>% 
  filter(st_geometry_type(.) == "MULTILINESTRING") %>%
  st_cast("LINESTRING") %>% 
  group_by(label) %>% 
  mutate(label = paste(label, row_number(), sep = "_")) %>% 
  ungroup()

# any MULTILINESTRING left?
multi_seg_roads %>% 
  filter(st_geometry_type(.) == "MULTILINESTRING") %>% nrow()


napa_sonoma_rds_utm_merged <- bind_rows(good_roads, multi_seg_roads) %>% 
  distinct()

napa_sonoma_rds_utm_merged %>% 
  filter(st_geometry_type(.) == "MULTILINESTRING") %>% nrow()

saveRDS(napa_sonoma_rds_utm_merged, here("data/napa_sonoma_rds_utm_merged"))


# 2. now split the merged road objects into equal length segments ----
# mean bbmm crossing segment is 1300 m so starting with that


napa_sonoma_rds_utm_merged <- readRDS(here("data/napa_sonoma_rds_utm_merged"))


make_road_pts <- function(zroad) {     
  road_in <- filter(napa_sonoma_rds_utm_merged, label == zroad)
  road_pts <- road_in %>%
    st_as_sfc() %>%   
    st_line_interpolate(dist = seq(0, as.numeric(st_length(road_in)), by = 1300)) 
}


zroad = napa_sonoma_rds_utm_merged[612,]
zz <- make_road_pts(zroad$label)


system.time(
  rd_pts <- map(napa_sonoma_rds_utm_merged$label, make_road_pts)
)
names(rd_pts) <- napa_sonoma_rds_utm_merged$label



split_roads <- function(zroad) {
  road_in <- filter(napa_sonoma_rds_utm_merged, label == zroad) 
  
  road_splitter <- rd_pts[[zroad]] %>% 
    st_buffer(., 0.01) %>% 
    st_combine() 
  
  
  rd_split <- lwgeom::st_split(road_in, road_splitter) %>% 
    st_collection_extract(., "LINESTRING") %>% 
    mutate(seg.length = st_length(.)) %>% 
    filter(as.numeric(seg.length) > 1) %>% 
    mutate(seg.label = paste(label, row_number(), sep = "_"))
  
}



system.time(
  napa_sonoma_rds_equal_segs <- map(napa_sonoma_rds_utm_merged$label, split_roads)
)
names(napa_sonoma_rds_equal_segs) <- napa_sonoma_rds_utm_merged$label


saveRDS(napa_sonoma_rds_equal_segs, here("data/napa_sonoma_rds_equal_segs"))



# 3. make a shapefile with a buffered polygon for each road segment ----

napa_sonoma_rds_equal_segs <- readRDS(here("data/napa_sonoma_rds_equal_segs")) %>% 
  bind_rows()


rds_equal_segs_buffer <- napa_sonoma_rds_equal_segs %>% 
  st_buffer(dist = 100, endCapStyle = "FLAT")


ggplot() +
  geom_sf(data = filter(rds_equal_segs_buffer, label == "Warm Springs Rd")) +
  geom_sf(data = filter(napa_sonoma_rds_equal_segs, label == "Warm Springs Rd"))


st_write(rds_equal_segs_buffer, "data/rds_equal_segs_buffer.shp")
