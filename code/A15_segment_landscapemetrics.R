


library(terra)
library(sf)
library(landscapemetrics)
library(here)
library(tidyverse)

source(here("code/helper_data.R"))

# roads 
# this is just the segments in the combined homerange polygon for the 12 analysis lions
segments_in_combined_homeranges <- readRDS(here("data/segments_in_combined_homeranges"))

# smaller set of roads for testing the function
# segments_in_combined_homeranges <- segments_in_combined_homeranges[1:100,]


study_years <- readRDS(here("data/crossing_clusters_gps_1step")) %>% 
  mutate(year = year(datetime.local)#, 
         #puma = str_extract(crossing.step, "^[^_]+(?=_)"),
         #puma = ifelse(puma == "P5*", "P5", puma)
  ) %>% 
  ungroup() %>% 
  distinct(year) %>% 
  pull(year) # convert df column to string

# redoing this to NOT be by puma
# May 2025 realized doing this by puma means extracting habitat multiple time for each segment where home ranges overlap.
# quicker to just do each segment once

# reading in data takes some time so doing it once outside the function

combined_puma_homeranges_95 <- st_read(here("data/shapefiles/combined_puma_homeranges_95.shp"))
# this is to filter the habitat raster to speed processing time when extracting values around each road segment. need the 300 m buffer to get all the habitat for segments that are right on the homerange edge.
hab_masker <- st_buffer(combined_puma_homeranges_95, 300)

# precomputed segment buffers

buffers_sf <- readRDS(here("data/precomputed_road_buffers.rds"))


# functions to calculate various landscape metrics for each segment across various scales ----

# I tested a number of landscape metrics available through the landscapemetrics package
# eventually settled on cohesion as the one that best represented what Quinton thinks is important about landscape configuration: "mini-corridors" crossing open country that allow lions to move through road areas
# the cohesion function is first, functions below line 140 are retained for posterity



# calculate patch cohesion ----
#' get_hr_road_patch_cohesion_treshr
#'
#' @param zyear 
#'
#' @returns
#' @details
#' calculating cohesion at 3 buffer distances around segments and 3 thresholds for classifying a cell as tree+shrub.
#' spatial scale: because cohesion is calculated across multiple cells, doesn't add much information to calculate it at 30m intervals moving out from roads, just using 100, 200, 300
#' woody plant threshold: this is the % cover threshold for classifying a raster cell as woody or not. cohesion is calculated based on 1 vs 0 classification of each cell, not on the original % cover of the veg type of interest. currently calculating at 25, 50, 75%
#' 
#'
#' @examples
get_hr_road_patch_cohesion_treshr <- function(zyear, layer_type = "tre_shr") {
  
  thresholds <- c(25, 50, 75)
  buffers <- c(100, 200, 300)
  
  # Load preprocessed raster
  layer_path <- here("data/processed_habitat_rasters", paste0(layer_type, "_", zyear, ".tif"))
  if (!file.exists(layer_path)) stop("Raster file not found: ", layer_path)
  hr_hab <- terra::rast(layer_path)
  
  # Create binary habitat layers at each threshold
  binary_forest_list <- lapply(thresholds, function(thresh) {
    terra::classify(hr_hab > thresh, cbind(TRUE, 1), others = 0)
  })
  names(binary_forest_list) <- as.character(thresholds)
  
  # Loop over thresholds and buffers
  combos <- expand.grid(buff = buffers, forest.threshold = thresholds)
  
  results <- purrr::map_dfr(1:nrow(combos), function(i) {
    buff_dist <- combos$buff[i]
    threshold <- combos$forest.threshold[i]
    
    forest_binary <- binary_forest_list[[as.character(threshold)]]
    
    road_buffer_sf <- buffers_sf[[as.character(buff_dist)]]
    if (is.null(road_buffer_sf)) stop("Buffer ", buff_dist, " not found in buffers_sf")
    
    buffer_metrics <- purrr::map_dfr(1:nrow(road_buffer_sf), function(j) {
      one_buf <- road_buffer_sf[j, ]
      #one_buf <- filter(road_buffer_sf, seg.label == "Adobe Canyon Rd_Kenwood_1_2")
      one_vect <- terra::vect(one_buf)
      
      bin_crop <- terra::crop(forest_binary, one_vect)
      bin_mask <- terra::mask(bin_crop, one_vect)
      
      
      # Calculate area proportion of woody vegetation
      total_cells <- terra::global(!is.na(bin_mask), "sum", na.rm = TRUE)[[1]]
      woody_cells <- terra::global(bin_mask == 1, "sum", na.rm = TRUE)[[1]]
      woody_prop <- if (!is.na(total_cells) && total_cells > 0) woody_cells / total_cells else NA_real_
      
      # Convert the masked raster (binary woody cover within buffer) to patches (polygons)
      # Default to FALSE
      patch_touches_road <- FALSE
      
      road_seg <- segments_in_combined_homeranges %>% filter(seg.label == one_buf$seg.label)
  
      patch_check <- try({
        patch_rast <- landscapemetrics::get_patches(bin_mask, class = 1, directions = 8)[[1]]$class_1
        
        patches_vect <- terra::as.polygons(patch_rast)
        patches_vect <- patches_vect[!is.na(terra::values(patches_vect)[,1]), ]
        
        if (terra::nrow(patches_vect) > 0) {
          patches_sf <- sf::st_as_sf(patches_vect)
          patches_sf <- sf::st_transform(patches_sf, sf::st_crs(road_seg))
          
          if (nrow(patches_sf) > 0 && nrow(road_seg) == 1) {
            patch_touches_road <- any(sf::st_intersects(patches_sf, road_seg, sparse = FALSE))
          }
        }
      }, silent = TRUE)
      
      
      
      metrics_val <- tryCatch({
        coh <- landscapemetrics::lsm_c_cohesion(bin_mask)
        np <- landscapemetrics::lsm_c_np(bin_mask)
        
        left_join(
          coh %>% filter(class == 1) %>% select(class, cohesion = value),
          np %>% filter(class == 1) %>% select(class, np = value),
          by = "class"
        )
      }, error = function(e) tibble(class = NA, cohesion = NA, np = NA))
      
      metrics_val %>%
        mutate(
          seg.label = one_buf$seg.label,
          buff = buff_dist,
          forest.threshold = threshold,
          layer = toupper(layer_type),
          patch.touches.road = patch_touches_road,
          woody.prop = woody_prop
        )
    })
    
    buffer_metrics
  })
  
  results <- results %>%
    mutate(year = zyear)
  
  message("Values calculated for ", zyear)
  
  return(results)
}



system.time(
  all_hr_road_patch_cohesion_treshr <- get_hr_road_patch_cohesion_treshr(2017, layer_type = "tre_shr")
) # ~1000


system.time(
  all_hr_road_patch_cohesion_treshr  <- map_df(seq(2016, 2023), get_hr_road_patch_cohesion_treshr)
) # 7969


# now add back in segments that didn't make it through that process (I think these are mostly segments that had no TRE+SHR cells at a given buffer distance and forest threshold)
# Define buffer and threshold values
buffers <- c(100, 200, 300)
thresholds <- c(25, 50, 75)
years <- seq(2016, 2023)

# Create all combinations of buffer × threshold
buff_thresh_combos <- crossing(
  buff = buffers,
  forest.threshold = thresholds,
  year = years
)

# Cross with your real seg.label-animal.id pairs
lion_segments <- readRDS(here("data/segments_in_homeranges")) %>% 
  data.frame() %>% 
  select(animal.id = puma, seg.label)   %>% 
  filter(animal.id %in% analysis_pumas,
         !animal.id %in% few_crossings_pumas,
         !animal.id %in% hr_exclude_pumas,
         !seg.label %in% p31_exclude_segments) 

full_combos <- lion_segments%>%
  crossing(buff_thresh_combos)

# checking number of rows overall and per segment-year-lion
# there are 72 buff-forest.threshold-year combinations
nrow(full_combos)/72 == nrow(lion_segments) # should be TRUE
count(full_combos, animal.id, seg.label, year) %>% filter(n != 9) %>% nrow() # should be 0

#all_hr_road_patch_cohesion_treshr <- readRDS(here("data/all_hr_road_patch_cohesion_treshr"))
all_hr_road_patch_cohesion_treshr_full <- left_join(full_combos, all_hr_road_patch_cohesion_treshr)

# checking again
nrow(all_hr_road_patch_cohesion_treshr_full)/72 == nrow(lion_segments) # should be TRUE
count(all_hr_road_patch_cohesion_treshr_full, animal.id, seg.label, year) %>% filter(n != 9) %>% nrow() # should be 0


saveRDS(all_hr_road_patch_cohesion_treshr_full, here("data/all_hr_road_patch_cohesion_treshr_full"))

########################################
# calculate landscape contagion ----

get_hr_road_landscape_contag_treshr <- function(zyear) {
  thresholds <- c(25, 50, 75)
  buffers <- c(100, 200, 300)
  
  # Load and process habitat raster
  zhab <- terra::rast(paste0(
    "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/",
    "Harvey_north bay habitat/RSF_Layers_", zyear, "_2024-03-19.TIF"
  ))
  
  # Create summed TRE + SHR layer
  tre_layer <- zhab[["TRE"]]
  shr_layer <- zhab[["SHR"]]
  sum_layer <- tre_layer + shr_layer
  
  process_layer <- function(layer, name_suffix) {
    rast_cropped <- terra::crop(layer, hab_masker)
    rast_proj <- terra::project(rast_cropped, "EPSG:26910")
    hr_hab <- terra::mask(rast_proj, hab_masker)
    
    binary_forest_list <- lapply(thresholds, function(thresh) {
      terra::classify(hr_hab > thresh, cbind(TRUE, 1), others = 0)
    })
    names(binary_forest_list) <- as.character(thresholds)
    
    combos <- expand.grid(buff = buffers, forest.threshold = thresholds)
    
    results <- purrr::map_dfr(1:nrow(combos), function(i) {
      buff_dist <- combos$buff[i]
      threshold <- combos$forest.threshold[i]
      forest_binary <- binary_forest_list[[as.character(threshold)]]
      
      road_vect <- terra::vect(segments_in_combined_homeranges)
      road_buffer <- terra::buffer(road_vect, width = buff_dist)
      road_buffer_sf <- sf::st_as_sf(road_buffer)
      road_buffer_sf$seg.label <- segments_in_combined_homeranges$seg.label
      
      buffer_metrics <- purrr::map_dfr(1:nrow(road_buffer_sf), function(j) {
        one_buf <- road_buffer_sf[j, ]
        one_vect <- terra::vect(one_buf)
        bin_crop <- terra::crop(forest_binary, one_vect)
        bin_mask <- terra::mask(bin_crop, one_vect)
        
        contag_val <- tryCatch({
          landscapemetrics::lsm_l_contag(bin_mask) %>%
            dplyr::select(contagion = value)
        }, error = function(e) {
          tibble::tibble(contagion = NA_real_)
        })
        
        contag_val %>%
          dplyr::mutate(
            seg.label = one_buf$seg.label,
            buff = buff_dist,
            forest.threshold = threshold,
            layer = name_suffix
          )
      })
      
      dplyr::left_join(segments_in_combined_homeranges, buffer_metrics, by = "seg.label") %>%
        dplyr::filter(!is.na(buff))
    })
    
    return(results)
  }
  
  # Run for TRE and TRE+SHR
  res_tre <- process_layer(tre_layer, "TRE")
  res_sum <- process_layer(sum_layer, "TRE+SHR")
  
  results <- dplyr::bind_rows(res_tre, res_sum) %>%
    dplyr::mutate(year = zyear) %>%
    sf::st_as_sf()
  
  return(results)
}



system.time(
  zz <- get_hr_road_landscape_contag_treshr(2017)
) # 623


system.time(
  all_hr_road_patch_cohesion_treshr  <- map_df(seq(2016, 2023), get_hr_road_patch_cohesion_treshr)
) 



###############################################################################

################# as of June 2025, only run to here ###########################

###############################################################################

###############################################################################

###############################################################################


#' get_hr_road_landmetrics_tre
#' 
#' calculates lsm_c_ai, lsm_p_shape, lsm_p_para, and lsm_p_circle tree rasters for each segment at each of 3 scales and 3 % thresholds for classifying a raster cell as tree
#'
#' @param zyear 
#'
#' @returns
#'
#' @examples
get_hr_road_landmetrics_tre <- function(zyear) {
  thresholds <- c(25, 50, 75)
  buffers <- c(100, 200, 300)

  # Load and process habitat raster
  zhab <- terra::rast(paste0(
    "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/",
    "Harvey_north bay habitat/RSF_Layers_", zyear, "_2024-03-19.TIF"
  ))
  rast_layer <- zhab[["TRE"]]
  rast_cropped <- terra::crop(rast_layer, hab_masker)
  rast_proj <- terra::project(rast_cropped, "EPSG:26910")
  hr_hab <- terra::mask(rast_proj, hab_masker)

  # Precompute binary rasters for all thresholds
  binary_forest_list <- lapply(thresholds, function(thresh) terra::classify(hr_hab > thresh, cbind(TRUE, 1), others = 0))
  names(binary_forest_list) <- as.character(thresholds)


  combos <- expand.grid(buff = buffers, forest.threshold = thresholds)

  results <- purrr::map_dfr(1:nrow(combos), function(i) {
    buff_dist <- combos$buff[i]
    threshold <- combos$forest.threshold[i]

    forest_binary <- binary_forest_list[[as.character(threshold)]]

    road_vect <- terra::vect(segments_in_combined_homeranges)
    road_buffer <- terra::buffer(road_vect, width = buff_dist)
    road_buffer_sf <- sf::st_as_sf(road_buffer)

    road_buffer_sf$seg.label <- segments_in_combined_homeranges$seg.label

    buffer_metrics <- purrr::map_dfr(1:nrow(road_buffer_sf), function(j) {
      one_buf <- road_buffer_sf[j, ]
      one_vect <- terra::vect(one_buf)
      bin_crop <- terra::crop(forest_binary, one_vect)
      bin_mask <- terra::mask(bin_crop, one_vect)

      ai_val <- tryCatch({
        landscapemetrics::lsm_c_ai(bin_mask) %>%
          dplyr::rename("ai" = value) %>%
          dplyr::select(class, ai)
      }, error = function(e) tibble::tibble(class = NA, ai.val = NA))

      shape_vals <- tryCatch({
        dplyr::bind_rows(
          landscapemetrics::lsm_p_shape(bin_mask),
          landscapemetrics::lsm_p_para(bin_mask),
          landscapemetrics::lsm_p_circle(bin_mask)
        ) %>%
          dplyr::select(level, id, class, metric, value) %>%
          tidyr::pivot_wider(names_from = metric, values_from = value)
      }, error = function(e) tibble::tibble(level = NA, id = NA, class = NA, frac = NA, shape = NA, para = NA))


       dplyr::full_join(ai_val, shape_vals) %>% 
         tibble::tibble(
          seg.label = one_buf$seg.label,
          buff = buff_dist,
          forest.threshold = threshold)
    })

    dplyr::left_join(segments_in_combined_homeranges, buffer_metrics, by = c("seg.label")) %>%
      dplyr::filter(!is.na(buff))
  })

  results <- results %>%
    dplyr::mutate(animal.id = zpuma) %>%
    sf::st_as_sf()

  return(results)
}



system.time(
  zz <- get_hr_road_connectivity_multi_thresh("P1", 2017)
)


zz %>% 
  filter(class == 1, buff == 300, forest.threshold == 75) %>% st_write(here("data/shapefiles/predictor_variable_checking/p1_2017_ai.shp"), append = FALSE)



#' get_hr_road_landmetrics_treshr ----
#'
#' calculates lsm_c_ai, lsm_p_shape, lsm_p_para, and lsm_p_circle tree and tree+shrub rasters for each segment at each of 3 scales and 3 % thresholds for classifying a raster cell as tree or tree+shrub
#' 
#' 
#' @param zyear 
#'
#' @returns
#' @export
#'
#' @examples
get_hr_road_landmetrics_treshr <- function(zyear) {
  thresholds <- c(25, 50, 75)
  buffers <- c(100, 200, 300)
  
  
  # Load and process habitat raster
  zhab <- terra::rast(paste0(
    "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/",
    "Harvey_north bay habitat/RSF_Layers_", zyear, "_2024-03-19.TIF"
  ))
  
  # Create summed TRE + SHR layer
  tre_layer <- zhab[["TRE"]]
  shr_layer <- zhab[["SHR"]]
  sum_layer <- tre_layer + shr_layer
  
  process_layer <- function(layer, name_suffix) {
    rast_cropped <- terra::crop(layer, hab_masker)
    rast_proj <- terra::project(rast_cropped, "EPSG:26910")
    hr_hab <- terra::mask(rast_proj, hab_masker)
    
    binary_forest_list <- lapply(thresholds, function(thresh) terra::classify(hr_hab > thresh, cbind(TRUE, 1), others = 0))
    names(binary_forest_list) <- as.character(thresholds)
    
    combos <- expand.grid(buff = buffers, forest.threshold = thresholds)
    
    results <- purrr::map_dfr(1:nrow(combos), function(i) {
      buff_dist <- combos$buff[i]
      threshold <- combos$forest.threshold[i]
      
      forest_binary <- binary_forest_list[[as.character(threshold)]]
      
      road_vect <- terra::vect(segments_in_combined_homeranges)
      road_buffer <- terra::buffer(road_vect, width = buff_dist)
      road_buffer_sf <- sf::st_as_sf(road_buffer)
      road_buffer_sf$seg.label <- segments_in_combined_homeranges$seg.label
      
      buffer_metrics <- purrr::map_dfr(1:nrow(road_buffer_sf), function(j) {
        one_buf <- road_buffer_sf[j, ]
        one_vect <- terra::vect(one_buf)
        bin_crop <- terra::crop(forest_binary, one_vect)
        bin_mask <- terra::mask(bin_crop, one_vect)
        
        ai_val <- tryCatch({
          landscapemetrics::lsm_c_ai(bin_mask) %>%
            dplyr::rename("ai" = value) %>%
            dplyr::select(class, ai)
        }, error = function(e) tibble::tibble(class = NA, ai = NA))
        
        shape_vals <- tryCatch({
          dplyr::bind_rows(
            landscapemetrics::lsm_p_shape(bin_mask),
            landscapemetrics::lsm_p_para(bin_mask),
            landscapemetrics::lsm_p_circle(bin_mask)
          ) %>%
            dplyr::select(level, id, class, metric, value) %>%
            tidyr::pivot_wider(names_from = metric, values_from = value)
        }, error = function(e) tibble::tibble(level = NA, id = NA, class = NA, shape = NA, para = NA, circle = NA))
        
        dplyr::full_join(ai_val, shape_vals, by = "class") %>%
          dplyr::mutate(
            seg.label = one_buf$seg.label,
            buff = buff_dist,
            forest.threshold = threshold,
            layer = name_suffix
          )
      })
      
      dplyr::left_join(hr_roads, buffer_metrics, by = c("seg.label")) %>%
        dplyr::filter(!is.na(buff))
    })
    
    return(results)
  }
  
  # Run for TRE and TRE+SHR
  res_tre <- process_layer(tre_layer, "TRE")
  res_sum <- process_layer(sum_layer, "TRE+SHR")
  
  results <- dplyr::bind_rows(res_tre, res_sum) %>%
    dplyr::mutate(animal.id = zpuma,
                  year = zyear) %>%
    sf::st_as_sf()
  
  return(results)
}


system.time(
  zz <- get_hr_road_landmetrics_treshr("P1", 2017)
)


system.time(
  all_hr_road_landmetrics_treshr  <- map2_df(puma_years$puma, puma_years$year, get_hr_road_landmetrics_treshr)
) 

saveRDS(all_hr_road_landmetrics_treshr, here("data/all_hr_road_landmetrics_treshr"))


# summarize by segment

all_hr_road_landmetrics_treshr_summ <- all_hr_road_landmetrics_treshr %>% 
  data.frame() %>% 
  select(-geometry) %>% 
  group_by(animal.id, year, seg.label, class, buff, layer, forest.threshold, ai) %>% 
  summarise(mean.shape = mean(shape),
            max.shape = max(shape),
            mean.para = mean(para),
            max.para = max(para),
            mean.circle = mean(circle),
            max.circle = max(circle),
            num.patch = n()) %>% 
  ungroup()
  


all_hr_road_landmetrics_treshr_summ %>% 
  filter(buff == 300, animal.id %in% c("P1"), forest.threshold == 25, year == 2022, class == 1, layer == "TRE+SHR") %>% 
  select(seg.label, ai, contains("mean"), contains("max"), geometry) %>% 
  distinct() %>%  
  left_join(napa_sonoma_rds_equal_segs %>% select(seg.label, geometry)) %>% 
  st_write(here("data/shapefiles/predictor_variable_checking/P1_2022_landscapemetrics.shp"), append = FALSE)





all_hr_road_patch_cohesion_treshr %>% 
  filter(buff == 300, forest.threshold == 25, year == 2022, class == 1, layer == "TRE+SHR") %>%
  st_write(here("data/shapefiles/predictor_variable_checking/2022_cohesion.shp"), append = FALSE)


