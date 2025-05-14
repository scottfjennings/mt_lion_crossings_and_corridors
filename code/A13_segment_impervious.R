


# the Development layer in the USDA RAP rasters is potentially too coarse for this analysis. especially right along the roads in undeveloped areas where the pressence of just the road seems to be tipping the cell to "developed"

# the NLCD percent impervious data, which the RAP is derived from, provides the raw percent impervious for each cell.

library(FedData)
library(tigris)
library(sf)
library(terra)
library(tidyverse)
library(here)
library(ctmm)

source(here("code/helper_data.R"))
#
# download NLCD data ----
# 1. Get county boundaries for Sonoma and Napa
options(tigris_use_cache = TRUE)
ca_counties <- counties(state = "CA", cb = TRUE, year = 2020)

target_counties <- ca_counties %>%
  filter(NAME %in% c("Sonoma", "Napa"))

# Convert to terra-compatible sf object in NLCD CRS
target_proj <- st_transform(target_counties, crs = "EPSG:5070")

# 2. Download NLCD Impervious Surface (2019)
impervious_rast <- get_nlcd(
  template = target_proj,
  label = "Sonoma_Napa_Impervious",
  year = 2019,
  dataset = "impervious",
  landmass = "L48"
)

saveRDS(impervious_rast, here("data/sonoma_napa_nlcd/sonoma_napa_impervious2019_rast"))

writeRaster(
  impervious_rast,
  filename = here("data/shapefiles/predictor_variable_checking/impervious_sonoma_napa_2019.tif"),  # .tif is ideal
  filetype = "GTiff",    # Explicitly declare GeoTIFF
  overwrite = TRUE
)
# get average percent impervious in buffers around each road segment ----

# reading data
# home range utilization distributions from A2_fit_ctmm_homerange.R
hr_uds <- readRDS(here("model_objects/puma_hr_uds"))


# roads
napa_sonoma_rds_equal_segs <- readRDS(here("data/napa_sonoma_rds_equal_segs")) %>% 
  bind_rows() %>% 
  rename("road.seg.length" = seg.length)

# impervious raster from above
impervious_rast = readRDS(here("data/sonoma_napa_nlcd/sonoma_napa_impervious2019_rast"))

impervious_rast <- project(impervious_rast, "EPSG:26910")


# mask habitat to home range
#' get_hr_road_habitat
#' 
#' extract habitat values from USDA RAP layers 
#' this is the step where the roads get restricted to the home range of each mt lion
#'
#' @param zpuma 
#' @param zyear 
#' @param zlayer any layer in the Harvey_north bay habitat/RSF_Layers_ rasters. for this analysis one of c("TRE", "SHR", "Development")
#'
#' @return
#' @export
#'
#' @examples
get_hr_road_impervious <- function(zpuma) {
  
  # this is the home range polygon for zpuma
  puma_hr_uds <- hr_uds[[zpuma]] %>% 
    as.sf(., DF = "PDF", level.UD = 0.999) %>% 
    st_transform(crs = 26910)   %>% 
    filter(str_detect(name, "est"))
  
  # this is to filter the habitat raster to speed processing time when extracting values around each road segment. need the 500 m buffer to get all the habitat for segments that are right on the homerange edge.
  hab_masker <- st_buffer(puma_hr_uds, 300)
  
  # First crop to the extent of the home range polygon
  zhab_cropped <- crop(impervious_rast, hab_masker)
  zhab_cropped <- project(zhab_cropped, "EPSG:26910") # for some reason need to reproject after cropping
  
  # Then mask using the polygon shape, this sets cells outside hab_masker to NA
  hr_hab <- mask(zhab_cropped, hab_masker)
  
  # mask roads to home range
  hr_roads <- st_intersection(napa_sonoma_rds_equal_segs, puma_hr_uds) %>% 
    mutate(hr.seg.length = st_length(.))
  
  
  # Reproject the sf lines to match the CRS of the raster
  hr_roads <- st_transform(hr_roads, crs(hr_hab))
  
  # function to create buffer around road segment
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
  # add ID field because terra::extract() has this argument "ID - logical. Should an ID column be added? If so, the first column returned has the IDs (record numbers) of y" and adding the same field here allows joining in the next line
  
  # extract habitat along roads
  lyr_ext <- terra::extract(hr_hab, hr_road_buffers, cells = TRUE)  %>% 
    full_join(hr_road_buffers_df) %>% 
    rename("percent.impervious" = mrlc_download__NLCD_2019_Impervious_L48)
  
  # calculate mean per buffer per segment and rejoin with hr_roads to add geometry field back
  rds_buff_mean_lyr <- lyr_ext %>% 
    group_by(road.label, seg.label, buff) %>% 
    summarise(mean.percent.impervious = mean(percent.impervious),
              num.cell = n()) %>% 
    ungroup() %>% 
    full_join(hr_roads) %>% 
    st_as_sf() %>% 
    dplyr::mutate(animal.id = zpuma)
  
  return(rds_buff_mean_lyr)
}


ztest <- get_hr_road_impervious("P1")

system.time(
  all_hr_road_impervious  <- map_df(analysis_pumas, get_hr_road_impervious)
) #174



saveRDS(all_hr_road_impervious, here("data/all_hr_road_impervious"))
