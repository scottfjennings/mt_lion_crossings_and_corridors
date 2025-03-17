

library(tidyverse)
library(here)
library(sf)
library(terra)
library(units)
library(ctmm)

options(scipen = 999)

#source(here("code/utilities.R"))
source(here("code/helper_data.R"))


study_area_counties <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/data/study_area_counties")  %>% 
  filter(NAME %in% c("Napa", "Sonoma")) %>% 
  select(NAME) %>% 
  st_transform(crs = 26910)


# roads
napa_sonoma_rds_equal_segs <- readRDS(here("data/napa_sonoma_rds_equal_segs")) %>% 
  bind_rows() %>% 
  rename("road.seg.length" = seg.length)



puma_years <- readRDS(here("data/crossing_clusters_gps_1step")) %>% 
  mutate(year = year(datetime.local), 
         puma = str_extract(crossing.step, "^[^_]+(?=_)"),
         puma = ifelse(puma == "P5*", "P5", puma)) %>% 
  ungroup() %>% 
  distinct(puma, year)

# redoing this to be by puma

# reading in data takes some time so doing it once outside the function
hr_uds <- readRDS(here("model_objects/puma_hr_uds"))


# mask habitat to home range
#' get_hr_road_habitat
#' 
#' extract habitat values from USDA RAP layers 
#' this is the step where the roads get restricted to the home range of each mt lion
#'
#' @param zpuma 
#' @param zyear 
#'
#' @return
#' @export
#'
#' @examples
get_hr_road_habitat <- function(zpuma, zyear) {
  
  puma_hr_uds <- hr_uds[[zpuma]] %>% 
    as.sf(., DF = "PDF", level.UD = 0.999) %>% 
    st_transform(crs = 26910)   %>% 
    filter(str_detect(name, "est"))
  
  hab_masker <- st_buffer(puma_hr_uds, 5000)
  
  #zhab = rast(paste("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/Harvey_north bay habitat/RSF_Layers_", zyear, "_2024-03-19.TIF", sep = ""))
  
  zhab = rast(here(paste("data/sonoma_napa_usda_rap/sonoma_napa_usda_rap_", zyear, ".TIF", sep = "")))
  # filter to just the layers needed to speed masking - looks like it is ~1/3 the time as the full habitat layers
  ind <- match(c("TRE","SHR", "Development"), names(zhab))
  ind <- ind[!is.na(ind)]
  zhab_filtered <- zhab[[ names(zhab)[ind] ]]
  zhab_filtered <- project(zhab_filtered, "EPSG:26910")
  
  hr_hab <- mask(zhab_filtered, hab_masker)
  # mask roads to home range
  hr_roads <- st_intersection(napa_sonoma_rds_equal_segs, puma_hr_uds) %>% 
    mutate(hr.seg.length = st_length(.))
  
  buffer_roads <- function(zbuff) {
  buff_road <- hr_roads %>% 
    group_by(road.label, seg.label, road.seg.length, hr.seg.length) %>% 
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
    full_join(hr_roads) %>% 
    st_as_sf() %>% 
    dplyr::mutate(year = zyear,
                  animal.id = zpuma)
  
  return(rds_buff_mean_tre_shr_dev)
  }


system.time(
  all_hr_road_habitat  <- map2_df(puma_years$puma, puma_years$year, get_hr_road_habitat)
) # 11678 10/15/24; 23742.20  3/13/25

saveRDS(all_hr_road_habitat, here("data/all_hr_road_habitat"))
all_hr_road_habitat <- readRDS(here("data/all_hr_road_habitat"))


ggplot() +
  #geom_sf(data = puma_hr_uds, fill = NA) +
 # geom_sf(data = hab_masker, fill = NA) +
#  geom_sf(data = filter(napa_sonoma_rds_equal_segs, label == "Hwy 1"), aes(color = seg.label)) +
  geom_sf(data = filter(hr_road_buffers, seg.label == "Hwy 1"), fill = NA)  +
  geom_sf(data = filter(hr_roads, label == "Hwy 1"), aes(color = seg.label))


# there are a few bad road sections for P31 along Hwy 1 where some of the buffer area is in the ocean and we get NA habitat values
filter(all_hr_road_habitat, is.na(mean.dev) & !animal.id %in% hr_exclude_pumas) %>% distinct(seg.label) %>% paste(., collapse = ", ")
# I create a helper df for these in helper_data.R

