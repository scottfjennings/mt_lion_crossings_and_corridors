

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


step_years <- readRDS(here("data/crossing_clusters_gps_1step")) %>% 
  mutate(year = year(datetime.local)) %>% 
  distinct(crossing.step, year)


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
  
  hr_hab <- mask(zhab_filtered, hab_masker)
  # mask roads to home range
  hr_roads <- st_intersection(napa_sonoma_rds_equal_segs, puma_hr_uds) %>% 
    mutate(hr.seg.length = st_length(.))
  
  buffer_roads <- function(zbuff) {
  buff_road <- hr_roads %>% 
    group_by(label, seg.label, road.seg.length, hr.seg.length) %>% 
    st_buffer(., zbuff, endCapStyle = "ROUND") %>% 
    mutate(buff = zbuff)
  }
  
  hr_road_buffers <- map_df(seq(30, 300, by = 30), buffer_roads)
  
  hr_road_buffers_df <- hr_road_buffers %>% 
    data.frame() %>% 
    select(label, seg.label, buff) %>% 
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
    group_by(label, seg.label, buff) %>% 
    summarise(mean.dev = mean(Development),
              mean.tre = mean(TRE),
              mean.shr = mean(SHR),
              mean.tre.shr = mean(tre.shr),
              num.cell = n()) %>% 
    ungroup() %>% 
    full_join(hr_roads) %>% 
    st_as_sf() %>% 
    mutate(year = zyear,
           animal.id == zpuma)
  
  return(rds_buff_mean_tre_shr_dev)
  }


system.time(
  all_hr_road_habitat  <- map2_df(puma_years$puma, puma_years$year, get_hr_road_habitat)
)

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

# next to analysis8_find_best_hab_scale.R

########################################################################################################
############## NO RUN BELOW HERE
# this is the original, slower method for getting habitat values along roads

# habitat layers ----

#' mask_habitat_to_counties
#' 
#' mask habitat layers to just sonoma and napa counties for faster extraction at road segment buffers
#'
#' @param zyear which year of habitat layers do you want to mask
#'
#' @return saves new raster to this project directory
#'
#' @examples
mask_habitat_to_counties <- function(zyear) {
  zhab = rast(paste("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/Harvey_north bay habitat/RSF_Layers_", zyear, "_2024-03-19.TIF", sep = ""))
  
  zhab_counties <- mask(zhab, study_area_counties)

  writeRaster(zhab_counties, here(paste("data/sonoma_napa_usda_rap/hr_uds_usda_rap_", zyear, ".tif", sep = "")))
}

map(seq(2016, 2023), mask_habitat_to_counties)

ggplot() +
  tidyterra::geom_spatraster(data = zhab, aes(fill = Development))



# combine home ranges

hr_uds <- readRDS(here("model_objects/puma_hr_uds"))

ud_to_sf <- function(zpuma) {
  hr_uds[[zpuma]] %>% 
    as.sf(., DF = "PDF", level.UD = 0.999) %>% 
    st_transform(crs = 26910) %>% 
    mutate(animal.id = zpuma)
}

pumaz = names(hr_uds)
pumaz = pumaz[!pumaz %in% hr_exclude_pumas]

all_hr_uds <- map_df(pumaz, ud_to_sf)

all_hr_uds_merge_est <- all_hr_uds  %>% 
  filter(str_detect(name, "est")) %>% 
  summarise()

# see how it looks
ggplot() + 
  geom_sf(data = study_area_counties, fill = NA) + 
#  geom_sf(data = all_hr_uds  %>% filter(str_detect(name, "est")), fill = NA, aes(color = animal.id)) + 
  geom_sf(data = all_hr_uds_merge_est, fill = NA) +
  geom_sf(data = zhab_hrs, aes(color = TRE))



mask_habitat_to_home_ranges <- function(zyear) {
  
  zhab = rast(paste("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/Harvey_north bay habitat/RSF_Layers_", zyear, "_2024-03-19.TIF", sep = ""))
  
  zhab_hrs <- mask(zhab, all_hr_uds)
  
  writeRaster(zhab_counties, here(paste("data/sonoma_napa_usda_rap/hr_uds_usda_rap_", zyear, ".tif", sep = "")))
}



# next, extract habitat values at multiple scales for each year ----
#' get_rd_habitat
#' 
#' extract tree and shrub cover values for a given year in polygons defined by a given buffer around road segments.
#'
#' @param zyear the desired year
#' @param zbuff the desired buffer around the road
#'
#' @return
#' @details
#' need to define zroad as the desired road layer before calling this function. generally zroad = napa_sonoma_rds_equal_segs. in st_buffer, endCapStyle = "FLAT" and "SQUARE" makes weird shapes for some road segments. using endCapStyle = "ROUND" instead even though that includes some habitat from adjacent road segments
#' 
#'
#' @examples
get_rd_habitat <- function(zyear, zbuff) { 
  
zhab <- rast(here(paste("data/sonoma_napa_usda_rap/sonoma_napa_usda_rap_", zyear, ".tif", sep = "")))
  
zroad_buff <- zroad %>% 
  st_buffer(., zbuff, endCapStyle = "ROUND")

  tre_ext <- extract(zhab, zroad_buff, layer = "TRE") %>% 
    mutate(zindex = row_number()) %>% 
    pivot_wider(id_cols = c(ID, zindex), names_from = layer, values_from = value)
                                            
  shr_ext <- extract(zhab, zroad_buff, layer = "SHR") %>% 
    mutate(zindex = row_number()) %>% 
    pivot_wider(id_cols = c(ID, zindex), names_from = layer, values_from = value)
  
  
  dev_ext <- extract(zhab, zroad_buff, layer = "Development")
  dev_ext <- dev_ext %>% 
    mutate(zindex = row_number()) %>% 
    pivot_wider(id_cols = c(ID, zindex), names_from = layer, values_from = value)
  
tre_shr_ext <- full_join(tre_ext, shr_ext) %>% 
  full_join(dev_ext) %>% 
  select(-zindex) %>% 
  mutate(tre.shr = TRE + SHR)

rds_buff_mean_tre_shr <- tre_shr_ext %>% 
  group_by(ID) %>% 
  summarise(mead.dev = mean(Development),
            mean.tre = mean(TRE),
            mean.shr = mean(SHR),
            mean.tre.shr = mean(tre.shr),
            num.cell = n()) %>% 
  ungroup()%>% 
  full_join(zroad_buff %>% mutate(ID = row_number())) %>% 
  st_as_sf() %>% 
  mutate(buff = zbuff,
         year = zyear)
}

zz <- get_rd_habitat(2016, 30)

years_buffs <- expand.grid(year = seq(2016, 2023),
                           buffs = seq(30, 300, by = 30))


system.time(
  all_rd_seg_habitats <- map2(years_buffs$year, years_buffs$buffs, get_rd_habitat, .progress = TRUE) %>% 
  list_rbind()
)

xyear <- rep(years_buffs$year, each = 2309)
all_rd_seg_habitats <- all_rd_seg_habitats %>% 
  bind_cols(year = xyear)

saveRDS(all_rd_seg_habitats, here("data/all_rd_seg_habitats"))
#all_rd_seg_habitats <- readRDS(here("data/all_rd_seg_habitats"))








#' get_ud_rd_seg_habitat
#' 
#' filter road segment habitat rasters to each puma's home range
#' 
#' @param zpuma 
#' @param zyear 
#'
#' @return
#'
#' @examples
get_ud_rd_seg_habitat <- function(zpuma, zyear) {
hr_uds <- readRDS(here("model_objects/puma_hr_uds"))[[zpuma]] %>% 
  as.sf(., DF = "PDF", level.UD = 0.999) %>% 
  st_transform(crs = 26910) %>% 
  filter(str_detect(name, "est"))


all_rd_seg_habitats <- readRDS(here("data/all_rd_seg_habitats")) %>% 
  filter(year == zyear) %>% 
  st_as_sf()
  
ud_rd_seg_habitat <- st_intersection(hr_uds, all_rd_seg_habitats) %>% 
  mutate(animal.id = zpuma)
}

system.time(
ud_rd_seg_habitats <- map2_df(puma_years$puma, puma_years$year, get_ud_rd_seg_habitat)
)

saveRDS(ud_rd_seg_habitats, here("data/ud_rd_seg_habitats"))


# vizualize results

ud_rd_seg_habitats <- readRDS(here("data/ud_rd_seg_habitats"))


ggplot() +
  #geom_sf(data = hr_uds) +
  geom_sf(data = ud_rd_seg_habitats %>% filter(seg.label == "San Ramon Way_1")) +
  geom_sf(data = napa_sonoma_rds_equal_segs %>% filter(seg.label == "San Ramon Way_1"))


ggplot() +
#  geom_sf(data = filter(all_rd_seg_habitats, seg.label == "San Ramon Way_1"), fill = NA, aes(color = as.character(buff))) +
  geom_sf(data = zroad_buff, fill = NA) +
  geom_sf(data = filter(zroad,  label == "San Ramon Way"), aes(color = seg.label))
