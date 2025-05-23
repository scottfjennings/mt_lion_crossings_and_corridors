


library(terra)
library(sf)
library(dplyr)
library(stringr)
library(purrr)
library(furrr)
library(landscapemetrics)
library(here)
library(tidyverse)


# home range utilization distributions from A2_fit_ctmm_homerange.R
hr_uds <- readRDS(here("model_objects/puma_hr_uds"))


# roads
napa_sonoma_rds_equal_segs <- readRDS(here("data/napa_sonoma_rds_equal_segs")) %>% 
  bind_rows() %>% 
  rename("road.seg.length" = seg.length)

combined_hr <- st_read(here("data/shapefiles/combined_puma_homeranges_99.shp")) %>%
  st_transform(26910)

combined_hr_masker <- sf::st_buffer(combined_hr, 300)


hr_segs <- napa_sonoma_rds_equal_segs[sf::st_intersects(napa_sonoma_rds_equal_segs, combined_hr, sparse = FALSE), ]  %>%
  st_transform(26910)

puma_years <- readRDS(here("data/crossing_clusters_gps_1step")) %>% 
  mutate(year = year(datetime.local), 
         puma = str_extract(crossing.step, "^[^_]+(?=_)"),
         puma = ifelse(puma == "P5*", "P5", puma)) %>% 
  ungroup() %>% 
  distinct(puma, year)

# TRE layer only ----
get_hr_road_landmetrics_tre <- function(zpuma, zyear) {
  thresholds <- c(25, 50, 75)
  buffers <- c(100, 200, 300)

  # Get home range polygon
  puma_hr_uds <- hr_uds[[zpuma]] %>%
    ctmm::as.sf(DF = "PDF", level.UD = 0.999) %>%
    st_transform(26910) %>%
    dplyr::filter(stringr::str_detect(name, "est"))

  hab_masker <- sf::st_buffer(puma_hr_uds, 300)

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

  # Subset roads within home range using spatial filter
  hr_roads <- napa_sonoma_rds_equal_segs[sf::st_intersects(napa_sonoma_rds_equal_segs, puma_hr_uds, sparse = FALSE), ] %>%
    sf::st_transform(sf::st_crs(hr_hab)) %>%
    dplyr::mutate(hr.seg.length = sf::st_length(.)) %>%
    dplyr::select(seg.label, geometry)

  combos <- expand.grid(buff = buffers, forest.threshold = thresholds)

  results <- purrr::map_dfr(1:nrow(combos), function(i) {
    buff_dist <- combos$buff[i]
    threshold <- combos$forest.threshold[i]

    forest_binary <- binary_forest_list[[as.character(threshold)]]

    road_vect <- terra::vect(hr_roads)
    road_buffer <- terra::buffer(road_vect, width = buff_dist)
    road_buffer_sf <- sf::st_as_sf(road_buffer)

    road_buffer_sf$seg.label <- hr_roads$seg.label

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

    dplyr::left_join(hr_roads, buffer_metrics, by = c("seg.label")) %>%
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



# TRE and TRE + SHR ----
get_hr_road_landmetrics_treshr <- function(zpuma, zyear) {
  thresholds <- c(25, 50, 75)
  buffers <- c(100, 200, 300)
  
  # Get home range polygon
  puma_hr_uds <- hr_uds[[zpuma]] %>%
    ctmm::as.sf(DF = "PDF", level.UD = 0.999) %>%
    st_transform(26910) %>%
    dplyr::filter(stringr::str_detect(name, "est"))
  
  hab_masker <- sf::st_buffer(puma_hr_uds, 300)
  
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
    
    hr_roads <- napa_sonoma_rds_equal_segs[sf::st_intersects(napa_sonoma_rds_equal_segs, puma_hr_uds, sparse = FALSE), ] %>%
      sf::st_transform(sf::st_crs(hr_hab)) %>%
      dplyr::mutate(hr.seg.length = sf::st_length(.)) %>%
      dplyr::select(seg.label, geometry)
    
    combos <- expand.grid(buff = buffers, forest.threshold = thresholds)
    
    results <- purrr::map_dfr(1:nrow(combos), function(i) {
      buff_dist <- combos$buff[i]
      threshold <- combos$forest.threshold[i]
      
      forest_binary <- binary_forest_list[[as.character(threshold)]]
      
      road_vect <- terra::vect(hr_roads)
      road_buffer <- terra::buffer(road_vect, width = buff_dist)
      road_buffer_sf <- sf::st_as_sf(road_buffer)
      road_buffer_sf$seg.label <- hr_roads$seg.label
      
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




# calculate patch cohesion ----
get_hr_road_patch_cohesion_treshr <- function(zyear) {
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
    rast_cropped <- terra::crop(layer, combined_hr_masker)
    rast_proj <- terra::project(rast_cropped, "EPSG:26910")
    hr_hab <- terra::mask(rast_proj, combined_hr_masker)
    
    binary_forest_list <- lapply(thresholds, function(thresh) terra::classify(hr_hab > thresh, cbind(TRUE, 1), others = 0))
    names(binary_forest_list) <- as.character(thresholds)
    
    
    combos <- expand.grid(buff = buffers, forest.threshold = thresholds)
    
    results <- purrr::map_dfr(1:nrow(combos), function(i) {
      buff_dist <- combos$buff[i]
      threshold <- combos$forest.threshold[i]
      
      forest_binary <- binary_forest_list[[as.character(threshold)]]
      
      road_vect <- terra::vect(hr_segs)
      road_buffer <- terra::buffer(road_vect, width = buff_dist)
      road_buffer_sf <- sf::st_as_sf(road_buffer)
      road_buffer_sf$seg.label <- hr_segs$seg.label
      
      buffer_metrics <- purrr::map_dfr(1:nrow(road_buffer_sf), function(j) {
        one_buf <- road_buffer_sf[j, ]
        one_vect <- terra::vect(one_buf)
        bin_crop <- terra::crop(forest_binary, one_vect)
        bin_mask <- terra::mask(bin_crop, one_vect)
        
        cohesion_val <- tryCatch({
          landscapemetrics::lsm_c_cohesion(bin_mask) %>%
            dplyr::rename("cohesion" = value) %>%
            dplyr::select(class, cohesion)
        }, error = function(e) tibble::tibble(class = NA, cohesion = NA))
        
        
        cohesion_val %>%
          dplyr::mutate(
            seg.label = one_buf$seg.label,
            buff = buff_dist,
            forest.threshold = threshold,
            layer = name_suffix
          )
      })
      
      dplyr::left_join(hr_segs, buffer_metrics, by = c("seg.label")) %>%
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
  zz <- get_hr_road_patch_cohesion_treshr(2017)
)


system.time(
  all_hr_road_patch_cohesion_treshr  <- map_df(seq(2016, 2023), get_hr_road_patch_cohesion_treshr)
) 

saveRDS(all_hr_road_patch_cohesion_treshr, here("data/all_hr_road_patch_cohesion_treshr"))


all_hr_road_patch_cohesion_treshr %>% 
  filter(buff == 300, forest.threshold == 25, year == 2022, class == 1, layer == "TRE+SHR") %>%
  st_write(here("data/shapefiles/predictor_variable_checking/2022_cohesion.shp"), append = FALSE)


