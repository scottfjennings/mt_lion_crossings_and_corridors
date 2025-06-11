




library(terra)
library(sf)
library(landscapemetrics)
library(here)
library(tidyverse)


# roads 
# this is just the segments in the combined homerange polygon for the 12 analysis lions
segments_in_combined_homeranges <- readRDS(here("data/segments_in_combined_homeranges"))

# Set buffer distances
buffers <- c(100, 200, seq(30, 300, by = 30))

# Vectorize road segments (assuming segments_in_combined_homeranges is an sf object)
road_vect <- terra::vect(segments_in_combined_homeranges)

# Create a named list of buffers
road_segment_buffers <- purrr::map(buffers, function(b) {
  buf <- terra::buffer(road_vect, width = b)
  buf_sf <- sf::st_as_sf(buf)
  buf_sf$seg.label <- segments_in_combined_homeranges$seg.label
  buf_sf$buffer <- b
  buf_sf
})
names(road_segment_buffers) <- as.character(buffers)

# Optionally save to disk
saveRDS(road_segment_buffers, "precomputed_road_buffers.rds")
