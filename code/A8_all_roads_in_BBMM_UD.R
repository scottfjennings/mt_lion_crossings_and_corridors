

# use utilization distributions from BBMM (from analysis3_fit_BBMM.R) to ID probabilistic road crossing areas

# the main logic here is that for most crossing steps, the most parsimonious crossed section of road is the part that the straight line step crosses extending outward in each direction to the first road intersection. 
# this is most parsimonious because extending out past the first intersection would require that the mt lion made multiple crossings.
# however, for some steps (e.g. P13_37473_36228, P13_29892_22992) the straight line crosses multiple roads and a non-straight line crosses fewer roads, which would then be the most parsimonious path following the above logic.
# I'm not sure yet (10/11/24) how to find the least cost path crossing the fewest roads, so for now I think I'll filter out the steps that have multiple crossings.
# there remains the problem of roads that end inside the BBMM UD, giving the possibility for the mt lion to go around the end of the road with no crossing.
# I'm also not sure how to even ID all those situations, let alone how to fix them. 
# the solution to that might be related to the multiple crossing solution
# I need a method to find the path within the BBMM UD that crosses the fewest roads.

library(tidyverse)
library(here)
library(sf)
library(raster)
library(terra)
library(tidyterra)
library(igraph)

options(scipen = 999)

source(here("code/utilities.R"))
#
# read/prep data ----
crossing_clusters_gps <- readRDS(here("data/crossing_clusters_gps_1step")) %>% 
  arrange(crossing.step, datetime.local)

all_clusters_bbmm <- readRDS(here("model_objects/all_clusters_bbmm_1step"))
#all_clusters_bbmm <- readRDS(here("model_objects/all_clusters3_bbmm"))

# NO RUN - any errors on individual crossing steps are handled automatically now - 
#all_clusters_bbmm["P13_29892_23626"] <- NULL # this one is only 1 raster cell tall and causes problems

# NO RUN - any errors on individual crossing steps are handled automatically now - for the 1 step uds these steps also cause trouble
#all_clusters_bbmm["P1_37472_11247"] <- NULL
#all_clusters_bbmm["P1_37472_11248"] <- NULL
#all_clusters_bbmm["P1_37472_14839"] <- NULL
#all_clusters_bbmm["P1_37472_15955"] <- NULL
#all_clusters_bbmm["P2_9323_60474"] <- NULL
#all_clusters_bbmm["P33_40855_86744"] <- NULL
#all_clusters_bbmm["P41_44132_132816"] <- NULL
#all_clusters_bbmm["P41_44132_132817"] <- NULL

# need this to extract the road segments that were crossed
# road_crossing_steps$geometry is the geometry of the road segment that is crossed by the direct line of the step, not the geometry of the step itself
naive_crossings <- readRDS(here("data/naive_crossings_napa_sonoma_2hr")) 

# list of crossing steps to loop through below 
#crossing_steps <- prob_check %>% 
#  filter(crossing.step != "P13_29892_23626") # this one is only 1 raster cell tall and causes problems

crossing_steps <- names(all_clusters_bbmm)

# crossing_steps = crossing_steps[crossing_steps != "P13_29892_23626"] # this one is only 1 raster cell tall and causes problems


#
# OPTIONAL check how big the BBMM probability layer is. The probabilities seem to get funky with <50 cells. prob_checker() is in utilities.R ---- 
prob_check <- map_df(names(all_clusters_bbmm), prob_checker)
filter(prob_check, prob.size > 50) %>% nrow()

# OPTIONAL checking the relationship between step length of each cluster and the size of the resulting BBMM probability raster  ---- 
zz <- prob_check %>% 
  left_join(crossing_clusters_gps) %>% 
  select(prob.size, crossing.step, step.id) %>% 
  left_join(readRDS(here("data/puma_steps"))) %>% 
  mutate(step.dist = as.numeric(step.dist)) %>% 
  group_by(crossing.step, prob.size) %>% 
  summarise(mean.step.len = mean(step.dist),
            min.step.len = min(step.dist),
            max.step.len = max(step.dist)) %>% 
  ungroup() 

zz %>% 
  pivot_longer(cols = contains("step.len")) %>% 
  filter(prob.size > 500) %>% 
  ggplot() +
  geom_point(aes(x = prob.size, y = value, color = name)) +
  stat_smooth(aes(x = prob.size, y = value, color = name))

# 

# now the data processing steps ----
# 1. make BBMM Utilization Distribution. creates all_bbmm_ud ----
#' bbmm_to_UD 
#'
#' create a Utilization Distribution from a Brownian Bridge Movement Model object
#'
#' @param zbbmm a BBMM object from BBMM::brownian.bridge()
#' @param zlevel UD level in proportion (0-1) format 
#'
#' @return for each step, a df with a row for each cell in the BBMM UD at zlevel
#'
#' @examples bbmm_to_UD(all_clusters_bbmm[[1]])
bbmm_to_UD <- function(zbbmm, zlevel = .9) {

    
  # convert to data frame, sort on probability, and calculate cumulative probability to allow filtering to level UD level
  bbmm_ud <- do.call(cbind.data.frame, zbbmm) %>% 
    mutate(zindex = row_number()) %>% 
    arrange(-probability) %>% 
    mutate(prob.sum = cumsum(probability),
           level.lab = paste(zlevel * 100, "% UD", sep = "")) %>% 
    arrange(zindex) %>% 
    filter(prob.sum <= zlevel) %>% 
    select(-zindex, -prob.sum)
  return(bbmm_ud)
}


system.time(
  all_bbmm_ud <- map(all_clusters_bbmm, bbmm_to_UD), gcFirst = TRUE
)
# ~ 20 sec; 34
names(all_bbmm_ud) <- names(all_clusters_bbmm)
saveRDS(all_bbmm_ud, here("model_objects/all_bbmm_ud_1step"))

all_bbmm_ud <- readRDS(here("model_objects/all_bbmm_ud_1step"))

# 2. create a raster for each UD. creates all_ud_rast ---- 
#' UD_to_raster
#' 
#' create a raster of a Utilization Distribution from the BBMM model objects created in analysis3_fit_BBMM. then create a polygon from the outline of that raster should work on any output from BBMM::brownian.bridge(), but seems to fail if there is only a single row or column in the UD
#'
#' @param zud a data frame representing a Utilization Distribution. generally the result of bbmm_to_UD(). must have at least columns x, y, and probability
#'
#' @return a terra::as.polygons object projected as UTM zone 10N
#'
#' @examples ud_rast <- UD_to_raster(bbmm_ud)
UD_to_raster <- function(zud) {

  # make it raster, UTM  
  r <- terra::rast(cbind(zud$x, zud$y, ceiling(zud$probability)), type = "xyz")
  #r <- terra::rast(cbind(bbmm_df$x, bbmm_df$y, bbmm_df$probability), type = "xyz")
  crs(r) <- "EPSG:26910"
  # raster to polygon b/c for some reason can't do st_as_sf on a SpatRaster but can on a SpatVector
  rp <- terra::as.polygons(r, crs = 26910, values = TRUE, digits = 0, aggregate = TRUE)
  
return(rp)    
}


system.time(
  all_ud_rast_safe <- map(all_bbmm_ud, safely(UD_to_raster)), gcFirst = TRUE
)
# ~ 60 sec
names(all_ud_rast_safe) <- names(all_clusters_bbmm)

# these ud can't be rasterized by terra::rast because they only have a single x or y coordinate 
all_ud_rast_error <- map(all_ud_rast_safe, "error")[!sapply(map(all_ud_rast_safe, "error"), is.null)]

# filter out errors
all_ud_rast <- map(all_ud_rast_safe, "result")[sapply(map(all_ud_rast_safe, "error"), is.null)]

# need to wrap the rasters to saveRDS
# https://stackoverflow.com/questions/76517502/error-external-pointer-is-not-valid-when-saving-spatrasters-from-the-r-environm
wrapped_all_ud_rast <- lapply(all_ud_rast, wrap)

saveRDS(wrapped_all_ud_rast, here("model_objects/all_ud_rast_1step"))

# 3. id the section of road that is within the 90% UD. creates all_bbmm_roads ----

wrapped_all_ud_rast <- readRDS(here("model_objects/all_ud_rast_1step"))
all_ud_rast <- lapply(wrapped_all_ud_rast, unwrap)
crossing_steps <- names(all_ud_rast)
# napa_sonoma_rds_equal_segs <- readRDS(here("data/napa_sonoma_rds_equal_segs")) %>% bind_rows()

# read in road layer
# as of Jan 2025 using the merged road layer from A3_road_layers.R
final_cleaned_road_layer <- readRDS(here("data/final_cleaned_road_layer")) %>% 
  st_transform(crs = 26910) %>%  # to UTM 10N
  mutate(road.label = paste(label, leftcity, city.road.num, sep = "_"),
         road.length = st_length(.))

# excluding shorter road objects that are probably not very risky for mt lion.
ggplot() +
  geom_density(data = final_cleaned_road_layer, aes(x = as.numeric(road.length))) +
  geom_density(data = final_cleaned_road_layer %>% filter(as.numeric(road.length) > 500), aes(x = as.numeric(road.length)), color = "red") +
  geom_density(data = final_cleaned_road_layer %>% filter(as.numeric(road.length) > 1000), aes(x = as.numeric(road.length)), color = "blue")


final_cleaned_road_layer <- final_cleaned_road_layer %>% 
  filter(as.numeric(road.length) > 500)



# zcrossing.step = crossing_steps$crossing.step[272]

# zcrossing.step = "P1_23163_2781"


#  get_all_bbmm_roads() is in utilities.R
zcrossing.step = "P1_11082_9"

zz <- get_all_bbmm_roads(final_cleaned_road_layer, "P1_11082_9")

system.time(
  all_bbmm_roads_safe <- pmap(list(road_layer = list(final_cleaned_road_layer), crossing_steps), safely(get_all_bbmm_roads)), gcFirst = TRUE
) # ~420 sec, or 179
names(all_bbmm_roads_safe) <- crossing_steps

# these ud can't be rasterized by terra::rast because they only have a single x or y coordinate 
all_bbmm_roads_error <- map(all_bbmm_roads_safe, "error")[!sapply(map(all_bbmm_roads_safe, "error"), is.null)]

# filter out errors
all_bbmm_roads <- map(all_bbmm_roads_safe, "result")[sapply(map(all_bbmm_roads_safe, "error"), is.null)]


saveRDS(all_bbmm_roads, here("model_objects/all_bbmm_roads_1step"))
all_bbmm_roads <- readRDS(here("model_objects/all_bbmm_roads_1step"))

  


# new Aug 2025 checking if all lions with aLoCoH hr have bbmm roads ----
# all do but P30. P30 only had 3 crossings and not had step durations within bounds for BBMM calculations
names(all_bbmm_roads) %>% 
  data.frame() %>% 
  rename(crossing.step = 1 )%>%  
  animal.id_from_crossing.step() %>% 
  distinct(animal.id) %>% 
  mutate(bbmm.clusters = TRUE) %>% 
  full_join(readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs_clean")) %>%
              filter(iso_level == 0.99) %>% 
              distinct(ID) %>% 
              rename(animal.id = ID) %>% 
              mutate(hr = TRUE))

# new Aug 2025 adding naive crossed status here ----
# new analysis plan per conversation with Jon B is to use the whole length of road inside the BBMM as the crossed segment, 
# and not to have multiple equal-length-segments IDed as partially crossed for each step

system.time(
  all_bbmm_roads_df <- all_bbmm_roads %>% 
    bind_rows(.id = "crossing.step")
) # 10

# add column indicating if this BBMM rd is the naively crossed road (may be >1 naive rd)
# need left_join here because naive_crossings has steps that I couldn't calculate BBMM for
all_bbmm_roads_df_naive <- all_bbmm_roads_df %>% 
  left_join(naive_crossings %>% 
              data.frame() %>% 
              select(crossing.step = step.id, road.label) %>% 
              mutate(naive.rd = TRUE)) %>% 
  mutate(naive.rd = replace_na(naive.rd, FALSE))

# add column with number of naively crossed roads per crossing step
all_bbmm_roads_df_naive <- all_bbmm_roads_df_naive %>% 
  group_by(crossing.step) %>% 
  mutate(num.naive.rds = sum(naive.rd)) %>% 
  ungroup()


# count how many times each naive road was crossed by each crossing ----

# get naive segment crossings. ----


puma_steps <- readRDS(here("data/puma_steps"))

# until now only have whole road naive crossings, need to redo the spatial intersection to find the naive segment crossings
# this allows calculating how many times each segment (not entire roads) was naively crossed 

# segments crossed an even number of times likely were not true crossings; the cat instead likely e.g. went around the outside of a curve.

# first need to redo the intersection of the step and the roads like in A5_naive_road_crossing.R, but now using the equal length segments instead of final_clean_road_layer
#' get_naive_segment_crossings
#' 
#' check which road segments in each BBMM UD were actually crossed by the straight line of the step. Use the BBMM segments (not larger area) for quicker processing time
#' 
#
#' @param zcrossing.step 
#'
#' @returns list with an element for each crossing step, and for each crossing step returns the geometry for the actual naive crossing location (POINT) for each segment. This contrasts with utilities::get_all_bbmm_roads which returns the geometry for the crossed road (LINESTRING)
#'
#' @examples
get_naive_crossing_points <- function(zcrossing.step) {
  
  # get the crossing step GPS points. use the original puma_steps for this for easiest filtering of the start and end GPS point for each step
  crossing_step_wide <- filter(puma_steps, step.id == zcrossing.step)
  
  crossing_step <- bind_rows(crossing_step_wide %>% 
                               dplyr::select(step.id, datetime.local, easting, northing),
                             crossing_step_wide %>% 
                               select(step.id, datetime.local = datetime.local.end, easting = easting.end, northing = northing.end)) %>% 
    data.frame() %>% 
    st_as_sf(coords = c("easting", "northing"), crs = 26910)
  
  # make into a line for intersection
  crossing_step_line <- crossing_step %>%
    st_as_sf(coords = c("easting", "northing"), crs = 26910) %>%
    group_by(step.id) %>%
    dplyr::summarize(do_union=FALSE) %>% 
    st_cast("LINESTRING") %>% 
    rename(crossing.step = step.id)
  
  # get all naively crossed roads that are in this step's BBMM UD
  bbmm_ud_naive_roads <-   all_bbmm_roads_df_naive %>% 
    filter(crossing.step == zcrossing.step, naive.rd == TRUE) %>% 
    select(road.label, geometry)
  
  naive_crossing_points <- st_intersection(crossing_step_line, bbmm_ud_naive_roads) %>% 
    st_cast("POINT")
  
  return(naive_crossing_points)
}

system.time(
all_naive_crossing_points <- map_df(crossing_steps, get_naive_crossing_points)
) #122

num_naive_crossings <- all_naive_crossing_points %>% 
  data.frame() %>% 
  group_by(crossing.step) %>% 
  count(road.label) %>% 
  rename(crossings.per.naive.rd = n)

all_bbmm_roads_df_naive <- all_bbmm_roads_df_naive %>% 
  full_join(num_naive_crossings)

saveRDS(all_bbmm_roads_df_naive, here("data/all_bbmm_roads_df_naive"))

