

library(tidyverse)
library(here)
library(sf)


monthly_mean_tot_lions <- st_read(here("data/shapefiles/monthly_mean_tot_lions.shp")) %>% 
  st_transform(crs = 26910)

# Define bounding box: xmin, ymin, xmax, ymax (in the same CRS as sf_object)
bbox <- st_bbox(c(xmin = -122.7996612, ymin = 38.2174861, xmax = -122.3248337, ymax = 38.4857256), crs = 4326)
bbox_sf <- st_as_sfc(bbox)%>% 
  st_transform(crs = 26910)

ggplot() +
  geom_sf(data = monthly_mean_tot_lions) +
  geom_sf(data = bbox_sf, color = "red")



slt_monthly_mean_tot_lions <- st_crop(monthly_mean_tot_lions, bbox_sf)

ggplot() +
  geom_sf(data = monthly_mean_tot_lions) +
  geom_sf(data = slt_monthly_mean_tot_lions, color = "blue") +
  geom_sf(data = bbox_sf, color = "red", fill = NA)


slt_monthly_mean_tot_lions %>% 
  st_write(here("data/data_for_SLT/slt_monthly_mean_road_crossings.shp"), append = FALSE) 


# raw GPS data

slt_study_area <- read_sf(here("data/shapefiles/slt_study_area/StudyArea.shp"))%>% 
  st_transform(crs = 26910)


analysis_table <- readRDS(here("data/analysis_table")) %>% 
  st_as_sf(x = .,
           coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  st_transform(crs = 26910) %>% 
  mutate(time = format(datetime.local, "%H:%M:%S"),
         time = as.character(time),
         datetime.local = as.character(datetime.local),
         date.local = as.character(date.local))

slt_raw_gps <- st_intersection(analysis_table, slt_study_area)

slt_raw_gps <- st_zm(slt_raw_gps, drop = TRUE, what = "ZM")


slt_raw_gps <- analysis_table[st_within(analysis_table, slt_study_area, sparse = FALSE)[,1], ]

slt_raw_gps <- analysis_table %>%
  filter(st_within(geometry, slt_study_area, sparse = FALSE)[,1])


ggplot() +
  #geom_sf(data = analysis_table) +
  #geom_sf(data = slt_raw_gps, color = "blue") +
  #geom_sf(data = bbox_sf, color = "red", fill = NA) +
  geom_sf(data = slt_study_area, color = "green", fill = NA)


slt_raw_gps %>% st_write(here("data/data_for_SLT/slt_raw_gps2.shp"), append = FALSE) 

st_write(slt_raw_gps, here("data/data_for_SLT/slt_raw_gps2.gpkg"), delete_layer = TRUE)



slt_raw_gps_g <- st_read(here("data/data_for_SLT/slt_raw_gps2.gpkg"))
