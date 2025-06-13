

library(terra)
library(ggplot2)
library(dplyr)

combined_puma_homeranges_95 <- st_read(here("data/shapefiles/combined_puma_homeranges_95.shp"))
# this is to filter the habitat raster to speed processing time when extracting values around each road segment. need the 300 m buffer to get all the habitat for segments that are right on the homerange edge.
hab_masker <- st_buffer(combined_puma_homeranges_95, 300)


buffers_sf <- readRDS(here("data/precomputed_road_buffers.rds"))

segments_in_combined_homeranges <- readRDS(here("data/segments_in_combined_homeranges"))

plot_segment_habitat_raster_orig <- function(
    seg_label,
    zyear,
    veg_type = c("TRE", "TRE+SHR"),
    threshold = 50,
    buffer_m = 200
) {
  veg_type <- match.arg(veg_type)
  
  # Construct file path based on year and veg type
  base_path <- here::here("data", "processed_habitat_rasters")
  raster_filename <- switch(
    veg_type,
    "TRE" = paste0("tre_", zyear, ".tif"),
    "TRE+SHR" = paste0("tre_shr_", zyear, ".tif")
  )
  raster_path <- file.path(base_path, raster_filename)
  
  # Load preprocessed raster
  hab_layer <- terra::rast(raster_path)
  
  # Get precomputed buffer and segment
  buffer_all <- buffers_sf[[as.character(buffer_m)]]
  buffer_sf <- buffer_all[buffer_all$seg.label == seg_label, ]
  seg_sf <- segments_in_combined_homeranges[segments_in_combined_homeranges$seg.label == seg_label, ]
  
  # Convert sf to SpatVector
  seg_vect <- terra::vect(seg_sf)
  buffer_vect <- terra::vect(buffer_sf)
  
  # Crop and binarize
  hab_cropped <- terra::crop(hab_layer, buffer_vect)
  binary_hab <- terra::classify(hab_cropped > threshold, cbind(TRUE, 1), others = 0)
  binary_masked <- terra::mask(binary_hab, buffer_vect)
  
  # Raster to data frame
  df <- as.data.frame(binary_masked, xy = TRUE)
  names(df)[3] <- "value"
  
  # Plot
  ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = factor(value))) +
    scale_fill_manual(values = c("gray", "darkgreen"), labels = c("Non-woody", "Woody")) +
    geom_sf(data = buffer_sf, fill = NA, color = "black", linewidth = 0.5) +
    geom_sf(data = seg_sf, color = "red", linewidth = 1) +
    coord_sf(crs = st_crs(buffer_sf)) +
    labs(
      title = paste("Segment:", seg_label),
      subtitle = paste("Veg:", veg_type, "| Threshold:", threshold, "| Buffer:", buffer_m, "m", "| Year:", zyear),
      fill = "Cover"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

plot_segment_habitat_raster <- function(
    seg_label,
    zyear,
    veg_type = c("TRE", "TRE+SHR"),
    threshold = 50,
    buffer_m = 200
) {
  veg_type <- match.arg(veg_type)
  
  # Construct file path based on year and veg type
  base_path <- here::here("data", "processed_habitat_rasters")
  raster_filename <- switch(
    veg_type,
    "TRE" = paste0("tre_", zyear, ".tif"),
    "TRE+SHR" = paste0("tre_shr_", zyear, ".tif")
  )
  raster_path <- file.path(base_path, raster_filename)
  
  # Load preprocessed raster
  hab_layer <- terra::rast(raster_path)
  
  # Get precomputed buffer and segment
  buffer_all <- buffers_sf[[as.character(buffer_m)]]
  buffer_sf <- buffer_all[buffer_all$seg.label == seg_label, ]
  seg_sf <- segments_in_combined_homeranges[segments_in_combined_homeranges$seg.label == seg_label, ]
  
  # Convert sf to SpatVector
  seg_vect <- terra::vect(seg_sf)
  buffer_vect <- terra::vect(buffer_sf)
  
  # Crop and binarize
  hab_cropped <- terra::crop(hab_layer, buffer_vect)
  binary_hab <- terra::classify(hab_cropped > threshold, cbind(TRUE, 1), others = 0)
  binary_masked <- terra::mask(binary_hab, buffer_vect)
  
  # Raster to data frame
  df <- as.data.frame(binary_masked, xy = TRUE)
  names(df)[3] <- "value"
  
  # Lookup cohesion and np
  patch_metrics <- dplyr::filter(
    all_hr_road_patch_cohesion_treshr,
    seg.label == seg_label,
    year == zyear,
    buff == buffer_m,
    forest.threshold == threshold,
    layer == str_replace(veg_type, "\\+", "\\_")
  )
  
  cohesion_val <- round(patch_metrics$cohesion, 1)
  np_val <- patch_metrics$np
  patch_touches_road <- patch_metrics$patch.touches.road
  
  # Plot
  ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = factor(value))) +
    scale_fill_manual(values = c("gray", "darkgreen"), labels = c("Non-woody", "Woody")) +
    geom_sf(data = buffer_sf, fill = NA, color = "black", linewidth = 0.5) +
    geom_sf(data = seg_sf, color = "red", linewidth = 1) +
    coord_sf(crs = st_crs(buffer_sf)) +
    labs(
      title = paste("Segment:", seg_label),
      subtitle = paste(
        "Veg:", veg_type,
        "| Threshold:", threshold,
        "| Buffer:", buffer_m, "m",
        "| Year:", zyear, "\n",
        "| Cohesion:", cohesion_val,
        "| Patches:", np_val,
        "| Patch touches rd:", patch_touches_road
      ),
      fill = "Cover"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}



plot_segment_habitat_raster(seg_label = "Enterprise Rd_Glen Ellen_1_5", zyear = 2017, veg_type = "TRE+SHR", threshold = 75, buffer_m = 300)


# 1st St E_Sonoma_1_1 100 25 cohesion = 100
# Arnold Dr_Glen Ellen_1_1 100 25 cohesion = 44.691989
# Austin Way_Napa_1_1 100 25 cohesion = 0. seems good
# Brookshire Cir_Santa Rosa_1_2 100 25 cohesion = 0. seems good
# Bennett Valley Rd_Santa Rosa_1_9 100 50, cohesion = 100.







