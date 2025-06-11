

library(terra)
library(landscapemetrics)
library(ggplot2)
library(dplyr)

combined_puma_homeranges_95 <- st_read(here("data/shapefiles/combined_puma_homeranges_95.shp"))
# this is to filter the habitat raster to speed processing time when extracting values around each road segment. need the 300 m buffer to get all the habitat for segments that are right on the homerange edge.
hab_masker <- st_buffer(combined_puma_homeranges_95, 300)


segments_in_combined_homeranges <- readRDS(here("data/segments_in_combined_homeranges"))

plot_segment_habitat_raster <- function(
    seg_label,
    zyear,
    veg_type = c("TRE", "TRE+SHR"),
    threshold = 50,
    buffer_m = 200
) {
  
  
  veg_type <- match.arg(veg_type)
  
  # Load raster
  zhab <- terra::rast(paste0(
    "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/",
    "Harvey_north bay habitat/RSF_Layers_", zyear, "_2024-03-19.TIF"
  ))
  
  tre_layer <- zhab[["TRE"]]
  shr_layer <- zhab[["SHR"]]
  sum_layer <- tre_layer + shr_layer
  
  hab_layer <- if (veg_type == "TRE") tre_layer else sum_layer
  
  # Segment and buffer
  seg_vect <- terra::vect(segments_in_combined_homeranges)
  seg_vect <- seg_vect[seg_vect$seg.label == seg_label, ]
  
  # Reproject segment if CRS doesn't match raster
  if (!terra::crs(seg_vect) == terra::crs(hab_layer)) {
    seg_vect <- terra::project(seg_vect, hab_layer)
  }
  
  seg_buffer <- terra::buffer(seg_vect, width = buffer_m)
  
  # Crop and binarize
  hab_cropped <- terra::crop(hab_layer, seg_buffer)
  binary_hab <- terra::classify(hab_cropped > threshold, cbind(TRUE, 1), others = 0)
  binary_masked <- terra::mask(binary_hab, seg_buffer)
  
  # Convert to data frame for plotting
  df <- as.data.frame(binary_masked, xy = TRUE)
  names(df)[3] <- "value"
  
  # Convert geometries to sf (assume they’re in the same CRS as raster)
  seg_sf <- st_as_sf(seg_vect)
  buffer_sf <- st_as_sf(seg_buffer)
  
  # Plot
  ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = factor(value))) +
    scale_fill_manual(values = c("white", "darkgreen"), labels = c("Non-woody", "Woody")) +
    geom_sf(data = buffer_sf, fill = NA, color = "black", linewidth = 0.5) +
    geom_sf(data = seg_sf, color = "red", linewidth = 1) +
    coord_sf(crs = st_crs(buffer_sf)) +
    labs(
      title = paste("Segment:", seg_label),
      subtitle = paste("Veg:", veg_type, "| Threshold:", threshold, "| Buffer:", buffer_m, "m"),
      fill = "Cover"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}




plot_segment_habitat_raster(
  seg_label = "1st St E_Sonoma_1_1",
  zyear = 2016,
  veg_type = "TRE+SHR",
  threshold = 25,
  buffer_m = 100
)


# 1st St E_Sonoma_1_1 100 25 cohesion = 100
# Arnold Dr_Glen Ellen_1_1 100 25 cohesion = 44.691989
