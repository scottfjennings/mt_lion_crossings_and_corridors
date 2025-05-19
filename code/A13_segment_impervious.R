


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

combined_puma_homeranges_95 <- st_read(here("data/shapefiles/combined_puma_homeranges_95.shp"))

# this is to filter the habitat raster to speed processing time when extracting values around each road segment. need the 300 m buffer to get all the habitat for segments that are right on the homerange edge.
hab_masker <- st_buffer(combined_puma_homeranges_95, 300)


# roads 
# this is just the segments in the combined homerange polygon for the 12 analysis lions
segments_in_combined_homeranges <- readRDS(here("data/segments_in_combined_homeranges"))


# impervious raster from above
impervious_rast = readRDS(here("data/sonoma_napa_nlcd/sonoma_napa_impervious2019_rast"))

impervious_rast <- project(impervious_rast, "EPSG:26910")


  
  # First crop to the extent of the home range polygon
imperv_cropped <- crop(impervious_rast, hab_masker)
imperv_cropped <- project(imperv_cropped, "EPSG:26910") # for some reason need to reproject after cropping
  
  # Then mask using the polygon shape, this sets cells outside hab_masker to NA
  hr_imperv <- mask(imperv_cropped, hab_masker)
  
  
  # function to create buffer around road segment
  buffer_roads <- function(zbuff) {
    buff_road <- segments_in_combined_homeranges %>% 
      group_by(seg.label) %>% 
      st_buffer(., zbuff, endCapStyle = "ROUND") %>% 
      mutate(buff = zbuff)
  }
  
  hr_road_buffers <- map_df(seq(30, 300, by = 30), buffer_roads)
  
  hr_road_buffers_df <- hr_road_buffers %>% 
    data.frame() %>% 
    select(seg.label, buff) %>% 
    mutate(ID = row_number()) 
  # add ID field because terra::extract() has this argument "ID - logical. Should an ID column be added? If so, the first column returned has the IDs (record numbers) of y" and adding the same field here allows joining in the next line
  
  # extract habitat along roads
  lyr_ext <- terra::extract(hr_imperv, hr_road_buffers, cells = TRUE)  %>% 
    full_join(hr_road_buffers_df) %>% 
    rename("percent.impervious" = mrlc_download__NLCD_2019_Impervious_L48)
  
  # calculate mean per buffer per segment and rejoin with segments_in_combined_homeranges to add geometry field back
  all_hr_road_impervious <- lyr_ext %>%
    select(seg.label, percent.impervious, buff) %>% 
    group_by(seg.label, buff) %>% 
    summarise(mean.percent.impervious = mean(percent.impervious),
              max.percent.impervious = max(percent.impervious),
              med.percent.impervious = median(percent.impervious),
              num.cell = n(),
              num.cell.50 = sum(percent.impervious >= 50, na.rm = TRUE),
              num.cell.20 = sum(percent.impervious >= 20, na.rm = TRUE),
              num.cell.g0 = sum(percent.impervious > 0, na.rm = TRUE),
              num.cell.0 = sum(percent.impervious == 0, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(percent.dev.o.l.m.h = 100 * (num.cell.g0/num.cell),
           percent.dev.l.m.h = 100 * (num.cell.20/num.cell),
           percent.dev.m.h = 100 * (num.cell.50/num.cell),
           percent.undev = 100 * (num.cell.0/num.cell))
  
  

saveRDS(all_hr_road_impervious, here("data/all_hr_road_impervious"))
