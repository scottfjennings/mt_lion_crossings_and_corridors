


library(terra)
library(here)
library(sf)



hab_masker <- st_read(here("data/shapefiles/combined_puma_homeranges_95.shp")) %>% 
  st_buffer(300)



create_tre_shr_raster <- function(zyear) {
  
  # Define input and output paths
  input_path <- paste0(
    "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/",
    "Harvey_north bay habitat/RSF_Layers_", zyear, "_2024-03-19.TIF"
  )
  output_dir <- here::here("data/processed_habitat_rasters")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Load habitat raster
  zhab <- terra::rast(input_path)
  
  # Extract layers
  tre_raw <- zhab[["TRE"]]
  shr_raw <- zhab[["SHR"]]
  
  # Project + mask
  process_layer <- function(layer) {
    layer %>%
      terra::crop(hab_masker) %>%
      terra::project("EPSG:26910") %>%
      terra::mask(hab_masker)
  }
  
  tre_masked <- process_layer(tre_raw)
  shr_masked <- process_layer(shr_raw)
  
  # Save individual layers
  tre_path <- file.path(output_dir, paste0("tre_", zyear, ".tif"))
  shr_path <- file.path(output_dir, paste0("shr_", zyear, ".tif"))
  sum_path <- file.path(output_dir, paste0("tre_shr_", zyear, ".tif"))
  
  terra::writeRaster(tre_masked, tre_path, overwrite = TRUE)
  terra::writeRaster(shr_masked, shr_path, overwrite = TRUE)
  
  # Sum and save combined raster
  sum_layer <- terra::app(c(tre_masked, shr_masked), sum)
  terra::writeRaster(sum_layer, sum_path, overwrite = TRUE)
  
  message("Saved TRE, SHR, and TRE+SHR rasters for ", zyear)
  return(invisible(list(tre = tre_path, shr = shr_path, sum = sum_path)))
}


create_tre_shr_raster(2017)

system.time(
map(seq(2016, 2023), create_tre_shr_raster)
) # 162
