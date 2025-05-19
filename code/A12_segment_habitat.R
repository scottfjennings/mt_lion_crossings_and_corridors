

library(tidyverse)
library(here)
library(sf)
library(terra)
library(units)

options(scipen = 999)

#source(here("code/utilities.R"))
source(here("code/helper_data.R"))



# roads 
# this is just the segments in the combined homerange polygon for the 12 analysis lions
segments_in_combined_homeranges <- readRDS(here("data/segments_in_combined_homeranges"))



study_years <- readRDS(here("data/crossing_clusters_gps_1step")) %>% 
  mutate(year = year(datetime.local)#, 
         #puma = str_extract(crossing.step, "^[^_]+(?=_)"),
         #puma = ifelse(puma == "P5*", "P5", puma)
         ) %>% 
  ungroup() %>% 
  distinct(year) %>% 
  pull(year) # convert df column to string

# redoing this to NOT be by puma
# May 2025 realized doing this by puma means extracting habitat multiple time for each segment where home ranges overlap.
# quicker to just do each segment once

# reading in data takes some time so doing it once outside the function

combined_puma_homeranges_95 <- st_read(here("data/shapefiles/combined_puma_homeranges_95.shp"))
# this is to filter the habitat raster to speed processing time when extracting values around each road segment. need the 300 m buffer to get all the habitat for segments that are right on the homerange edge.
hab_masker <- st_buffer(combined_puma_homeranges_95, 300)


# mask habitat to home range. all 3 raster layers ----
#' get_hr_road_habitat
#' 
#' extract habitat values from USDA RAP layers 
#' this is the step where the roads get restricted to the home range of each mt lion
#'
#' @param zyear 
#'
#' @return
#' @export
#'
#' @examples
get_hr_road_habitat <- function(zyear) {
  
  zhab = rast(paste("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/Harvey_north bay habitat/RSF_Layers_", zyear, "_2024-03-19.TIF", sep = ""))
  
  
  #zhab = rast(here(paste("data/sonoma_napa_usda_rap/sonoma_napa_usda_rap_", zyear, ".TIF", sep = "")))
  # filter to just the layers needed to speed masking - looks like it is ~1/3 the time as the full habitat layers
  ind <- match(c("TRE","SHR", "Development"), names(zhab))
  ind <- ind[!is.na(ind)]
  zhab_filtered <- zhab[[ names(zhab)[ind] ]]
  
  
  zhab_cropped <- crop(zhab_filtered, hab_masker)
  zhab_cropped <- project(zhab_cropped, "EPSG:26910") # for some reason need to reproject after cropping
  hr_hab <- mask(zhab_cropped, hab_masker)
  
  buffer_roads <- function(zbuff) {
  buff_road <- segments_in_combined_homeranges %>% 
    group_by(seg.label) %>% 
    st_buffer(., zbuff, endCapStyle = "ROUND") %>% 
    mutate(buff = zbuff)
  }
  
  hr_road_buffers <- map_df(seq(30, 300, by = 30), buffer_roads)
  
  hr_road_buffers_df <- hr_road_buffers %>% 
    data.frame() %>% 
    select(road.label, seg.label, buff) %>% 
    mutate(ID = row_number())
  
  # extract habitat along roads
  
  
  tre_ext <- extract(hr_hab, hr_road_buffers, layer = "TRE")  %>% 
    full_join(hr_road_buffers_df) %>% 
    rename("TRE" = value, "ID.TRE" = ID) %>% 
    select(-layer)
  
  shr_ext <- extract(hr_hab, hr_road_buffers, layer = "SHR") %>% 
    #full_join(hr_road_buffers_df) %>% 
    rename("SHR" = value, "ID.SHR" = ID) %>% 
    select(-layer)
  
  dev_ext <- extract(hr_hab, hr_road_buffers, layer = "Development") %>% 
    #full_join(hr_road_buffers_df) %>% 
    rename("Development" = value, "ID.Development" = ID) %>% 
    select(-layer)
  
  tre_shr_dev <- bind_cols(tre_ext, shr_ext, dev_ext) %>% 
    mutate(across(c(TRE, SHR), ~./100),
           tre.shr = TRE + SHR)
  
  rds_buff_mean_tre_shr_dev <- tre_shr_dev %>% 
    group_by(road.label, seg.label, buff) %>% 
    summarise(mean.dev = mean(Development),
              mean.tre = mean(TRE),
              mean.shr = mean(SHR),
              mean.tre.shr = mean(tre.shr),
              num.cell = n()) %>% 
    ungroup() %>% 
    full_join(segments_in_combined_homeranges) %>% 
    st_as_sf() %>% 
    dplyr::mutate(year = zyear)
  
  return(rds_buff_mean_tre_shr_dev)
  }




system.time(
  all_hr_road_habitat  <- map_df(study_years, get_hr_road_habitat)
) # 606!!! 2.5% the time of doing it by individual (and for a larger area)


saveRDS(all_hr_road_habitat, here("data/all_hr_road_habitat_95"))

# checking below here
all_hr_road_habitat <- readRDS(here("data/all_hr_road_habitat_95"))


ggplot() +
  geom_sf(data = filter(all_hr_road_habitat, buff == 30), aes(color = mean.tre))




all_hr_road_habitat %>% 
  filter(buff == 60, animal.id %in% analysis_pumas) %>% 
  select(road.label, seg.label, mean.dev, geometry) %>% 
  distinct() %>% 
  st_write(here("data/shapefiles/predictor_variable_checking/dev60.shp"), append = FALSE)



all_hr_road_habitat %>%
  data.frame() %>% 
  dplyr::select(seg.label, animal.id, year, buff, mean.dev, mean.tre.shr) %>% 
  saveRDS(here("data/analysis_inputs/all_hr_road_habitat_df_95"))


homerange_segments <- all_hr_road_habitat %>% 
  data.frame() %>% 
  select(animal.id, seg.label) %>% 
  distinct(animal.id, seg.label)

saveRDS(homerange_segments, here("data/analysis_inputs/homerange_segments_95"))


## optional checking below

ggplot() +
  #geom_sf(data = puma_hr_uds, fill = NA) +
 # geom_sf(data = hab_masker, fill = NA) +
#  geom_sf(data = filter(napa_sonoma_rds_equal_segs, label == "Hwy 1"), aes(color = seg.label)) +
  geom_sf(data = filter(hr_road_buffers, seg.label == "Hwy 1"), fill = NA)  +
  geom_sf(data = filter(hr_roads, label == "Hwy 1"), aes(color = seg.label))


# there are a few bad road sections for P31 along Hwy 1 where some of the buffer area is in the ocean and we get NA habitat values
filter(all_hr_road_habitat, is.na(mean.dev) & !animal.id %in% hr_exclude_pumas) %>% distinct(seg.label) %>% paste(., collapse = ", ")
# I create a helper df for these in helper_data.R


