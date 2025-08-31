

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
all_analysis_segments <- readRDS(here("data/all_analysis_segments"))


# mask habitat to home range. all 3 raster layers ----
#' get_hr_road_habitat
#' 
#' extract habitat values from USDA RAP layers 
#' this is the step where the roads get restricted to the home range of each mt lion
#'
#' @param zyear year to be processed
#' @param zveg veg layer to extract
#'
#' @return data frame with the zveg value for each cell within each buffer polygon for each segment
#'
#' @examples
get_analysis_segment_habitat <- function(zyear, zveg) {
  
  seg_df <- all_analysis_segments %>% 
    filter(year == zyear)
  
  veg_layer = rast(here(paste("data/processed_habitat_rasters/", zveg, "_", zyear, ".TIF", sep = "")))

  buffer_roads <- function(zbuff) {
  buff_road <- seg_df %>% 
    group_by(seg.label) %>% 
    st_buffer(., zbuff, endCapStyle = "ROUND") %>% 
    mutate(buff = zbuff)
  }
  
  hr_road_buffers <- map_df(seq(30, 300, by = 30), buffer_roads)
  
  hr_road_buffers_df <- hr_road_buffers %>% 
    data.frame() %>% 
    select(seg.label, buff, year) %>% 
    mutate(ID = row_number())
  
  # extract habitat along roads
   veg_ext <- extract(veg_layer, hr_road_buffers)
  
  seg_veg <- veg_ext %>% 
    full_join(hr_road_buffers_df) %>% 
    mutate(veg.layer = zveg)
  
  return(seg_veg)
  }




system.time(
  all_analysis_segment_habitat  <- map2_df(seq(2016, 2023), "tre_shr", get_analysis_segment_habitat)
) # 606!!! 2.5% the time of doing it by individual (and for a larger area)
# 1003 Aug 2025



saveRDS(all_analysis_segment_habitat, here("data/all_analysis_segment_tre_shr"))


# checking below here
all_hr_road_habitat <- readRDS(here("data/all_hr_road_habitat_95"))


# how much do mean.tre.shr and mean.dev vary between years?
zz <- all_hr_road_habitat %>% 
  data.frame() %>% 
  select(seg.label, buff, year, mean.tre.shr, mean.dev) %>% 
  pivot_longer(cols = c(mean.tre.shr, mean.dev)) %>% 
  group_by(seg.label, buff, name) %>% 
  summarise(min.val = min(value),
            max.val = max(value),
            mean.val = mean(value)) %>% 
  ungroup() %>% 
  mutate(zdif = max.val - min.val)



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


