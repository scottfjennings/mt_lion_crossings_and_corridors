


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
aLoCoH_hrs_99_combined_sonoma_napa <- readRDS(here("model_objects/a_LoCoH home ranges/aLoCoH_hrs_99_combined_sonoma_napa"))

# this is to filter the habitat raster to speed processing time when extracting values around each road segment. need the 300 m buffer to get all the habitat for segments that are right on the homerange edge.
hab_masker <- st_buffer(aLoCoH_hrs_99_combined_sonoma_napa, 300) %>% 
  st_transform(crs = 26910)


# roads 
all_analysis_segments <- readRDS(here("data/all_analysis_segments"))



# impervious raster from above
impervious_rast = readRDS(here("data/sonoma_napa_nlcd/sonoma_napa_impervious2019_rast"))

impervious_rast <- project(impervious_rast, "EPSG:26910")



  
  # mask habitat to home range. all 3 raster layers ----
  #' get_analysis_segment_impervious
  #' 
  #' extract habitat values from USDA RAP layers 
  #' this is the step where the roads get restricted to the home range of each mt lion
  #'
  #' @param zsegment 
  #' 
  #' @return data frame with the zveg value for each cell within each buffer polygon for each segment
  #'
  #' @examples
  get_analysis_segment_impervious <- function(zsegment) {
    
    seg_df <- all_analysis_segments %>% 
      filter(seg.label == zsegment)
    
    
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
    impervious_extract <- terra::extract(impervious_rast, hr_road_buffers)
    
    seg_impervious <- impervious_extract %>% 
      full_join(hr_road_buffers_df) %>% 
      rename("percent.impervious" = mrlc_download__NLCD_2019_Impervious_L48)
    
    seg_impervious_summarized <- seg_impervious %>%
      #select(seg.label, percent.impervious, buff) %>% 
      group_by(seg.label, buff) %>% 
      summarise(mean.percent.impervious = mean(percent.impervious, na.rm = TRUE),
                max.percent.impervious = max(percent.impervious, na.rm = TRUE),
                med.percent.impervious = median(percent.impervious, na.rm = TRUE),
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
    
    
    return(seg_impervious_summarized)
  }
  
  
system.time(  
all_analysis_segment_impervious <- map_df(distinct(all_analysis_segments, seg.label) %>% pull(seg.label), get_analysis_segment_impervious)  
) # 12641.08 
  
  
saveRDS(all_analysis_segment_impervious, here("data/all_analysis_segment_impervious"))

all_analysis_segment_impervious <- readRDS(here("data/all_analysis_segment_impervious"))

# mean percent not calculated for a small proportion:
filter(all_analysis_segment_impervious, is.na(mean.percent.impervious)) %>% nrow()

filter(all_analysis_segment_impervious, is.na(mean.percent.impervious)) %>%
  distinct(seg.label) %>% 
  left_join(all_analysis_segment_impervious) %>% 
  view()

# looks like problem was not having na.rm = TRUE in the function above
# fixed that but don't want to rerun every segment today (9/2/25)

# quick fix to avoid rerunning every segment
second_try_segs <- filter(all_analysis_segment_impervious, is.na(mean.percent.impervious)) %>%
  distinct(seg.label) %>% 
  pull(seg.label)

system.time(  
  second_try_segs_impervious <- map_df(second_try_segs, get_analysis_segment_impervious)  
) # 9554.26 

all_analysis_segment_impervious_full <- all_analysis_segment_impervious %>% 
  filter(!seg.label %in% second_try_segs) %>% 
  bind_rows(second_try_segs_impervious)
# end quick fix


# if redoing without the above quick fix above, change input df to all_analysis_segment_impervious
all_analysis_segment_impervious_full <- all_analysis_segment_impervious %>% 
  full_join(all_analysis_segments)

filter(all_analysis_segment_impervious_full, is.na(mean.percent.impervious)) %>% nrow()

saveRDS(all_analysis_segment_impervious_full, here("data/all_analysis_segment_impervious_full"))

