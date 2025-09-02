

library(tidyverse)
library(here)
library(sf)
library(terra)
library(units)

options(scipen = 999)

#source(here("code/utilities.R"))
source(here("code/helper_data.R"))


# read data ----
# this is all the used and available road segments for all lions
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
get_analysis_segment_mean_habitat <- function(zyear, zveg) {
  
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
  
  mean_veg <- seg_veg %>% 
    filter(!is.na(sum)) %>% 
    group_by(seg.label, year, buff) %>% 
    summarise(mean.veg = mean(sum, na.rm = TRUE)) %>% 
    ungroup()
  
  return(mean_veg)
  }




system.time(
  all_analysis_segments_mean_tre_shr  <- map2_df(seq(2016, 2023), "tre_shr", get_analysis_segment_mean_habitat)
) # 1166 Aug 2025

all_analysis_segments_mean_tre_shr %>% 
  count(seg.label, year) %>% 
  filter(n < 10) %>% 
  view()


all_analysis_segments_mean_tre_shr %>% 
  count(seg.label, year, buff) %>% 
  pivot_wider(id_cols = c(seg.label, year), names_from = buff, values_from = n) %>% 
  view()


saveRDS(all_analysis_segments_mean_tre_shr, here("data/all_analysis_segments_mean_tre_shr"))
all_analysis_segments_mean_tre_shr <- readRDS(here("data/all_analysis_segments_mean_tre_shr"))

all_analysis_segments_mean_tre_shr %>% 
  count(seg.label, year) %>% 
  filter(n < 10) %>% 
  nrow()

# all seg.label X year combos missing tre_she means are at small buffers
all_analysis_segments_mean_tre_shr %>% 
  count(seg.label, year) %>% 
  filter(n < 10) %>% 
  distinct(seg.label, year) %>% 
  left_join(all_analysis_segments_mean_tre_shr) %>% 
  group_by(seg.label, year) %>% 
  filter(buff == min(buff)) %>% 
  view()

all_analysis_segments_mean_tre_shr %>% 
  count(seg.label, year) %>% 
  filter(n < 10) %>% 
  distinct(seg.label, year) %>% 
  left_join(all_analysis_segments_mean_tre_shr) %>% 
  group_by(seg.label, year) %>% 
  filter(buff == max(buff)) %>% 
  view()


# now add back in segments that didn't make it through that process (I think these are mostly segments that had no TRE+SHR cells at a given buffer distance and forest threshold)
# Define buffer and threshold values
buffers = data.frame(buff = seq(30, 300, by = 30))


# Cross with your real seg.label-animal.id pairs
all_segments_cov_buffers <- all_analysis_segments %>%
  data.frame() %>% 
  select(seg.label, year, used.available, crossing.step) %>% 
  crossing(buffers)

all_segments_cov_buffers %>% 
  count(seg.label, year) %>% 
  filter(n < 10) %>% 
  nrow()

all_analysis_segments_mean_tre_shr_full <- all_analysis_segments_mean_tre_shr %>% 
  right_join(all_segments_cov_buffers) %>%
  mutate(mean.veg = replace_na(mean.veg, 0))


all_analysis_segments_mean_tre_shr_full %>%
  filter(is.na(mean.veg)) %>% 
  distinct(seg.label, year) %>% 
  left_join(all_analysis_segments_mean_tre_shr_full) %>% 
  view()

saveRDS(all_analysis_segments_mean_tre_shr_full, here("data/all_analysis_segments_mean_tre_shr_full"))



# OPTIONAL below here. checking ----

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


