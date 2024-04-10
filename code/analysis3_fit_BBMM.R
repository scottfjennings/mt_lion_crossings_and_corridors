

# fitting brownian bridge movement models

# objective is to develop probabilistic estimates of where each rod crossing occured.

# then evaluate the relative importance of covariates in predicting those crossings


library(tidyverse)
library(here)
library(sf)
library(move)
library(BBMM)

options(scipen = 999)


road_crossing_steps <- readRDS(here("data/road_crossing_steps_napa_sonoma"))
puma_steps <- readRDS(here("data/puma_steps")) %>% 
  mutate(step.dist.2hr = (step.dist/as.numeric(step.dur)) * 7200)

# first need to ID the ~2 steps before and after each crossing

# adapted from https://stackoverflow.com/questions/73142970/inspect-surroundings-area-neighbors-context-of-every-row-selected-by-a-filter-co

extract.with.context <- function(zstep) {
  
  match.idx  <- which(puma_steps$step.id == zstep)
  span       <- seq(from = -2, to = 2)
  extend.idx <- c(outer(match.idx, span, `+`))
  extend.idx <- Filter(function(i) i > 0 & i <= nrow(puma_steps), extend.idx)
  extend.idx <- sort(unique(extend.idx))
  
  neighbor_steps = puma_steps[extend.idx, , drop = FALSE] %>% 
    mutate(crossing.step = zstep)
return(neighbor_steps)
  }



system.time(
neighbor_steps_all <- map_df(distinct(road_crossing_steps, step.id)$step.id, extract.with.context), gcFirst = TRUE
)

# want all steps in a cluster to be close to 2 hours, currently using +- 10 min

ok_step_dur <- neighbor_steps_all %>% 
  dplyr::select(step.id, crossing.step, step.dur) %>% 
  mutate(ok.step.dur = between(as.numeric(step.dur), 6600, 7800)) %>% 
  group_by(crossing.step) %>% 
  mutate(all.step.dur.ok = all(ok.step.dur == TRUE)) %>% 
  ungroup()

ok_step_dur %>% 
  filter(all.step.dur.ok == TRUE) %>% 
  distinct(crossing.step) %>% nrow()

crossing_clusters <- ok_step_dur %>% 
  filter(all.step.dur.ok == TRUE) %>% left_join(neighbor_steps_all) %>%
  group_by(crossing.step) %>% 
  mutate(cluster.step.num = row_number()) %>% 
  ungroup()

# then fit the dBBMM to each of those 5 step clusters
# first need to decompose each step back into simple GPS points

crossing_clusters_gps <- crossing_clusters %>%  
  data.frame() %>% 
  dplyr::select(crossing.step, step.id, cluster.step.num, easting, easting.end, northing, northing.end, datetime.local, datetime.local.end) %>% 
  mutate(across(c(easting, easting.end, northing, northing.end, datetime.local, datetime.local.end), ~as.character(.))) %>% 
  pivot_longer(cols = c(easting, easting.end, northing, northing.end, datetime.local, datetime.local.end)) %>% 
  mutate(cluster.step.num = ifelse(cluster.step.num == 5 & str_detect(name, "end"), "5b", cluster.step.num),
         name = ifelse(cluster.step.num == "5b", str_replace(name, ".end", ""), name)) %>% 
  filter(!str_detect(name, ".end")) %>% 
  pivot_wider(id_cols = c(crossing.step, step.id, cluster.step.num), values_from = value, names_from = name) %>% 
  mutate(datetime.local = as.POSIXct(datetime.local),
         easting = as.numeric(easting),
         northing = as.numeric(northing)) %>% 
  dplyr::select(-cluster.step.num)
  

#' calc_crossing_bbmm
#' 
#' calculate a Brownian Bridge Movement Model for a cluster of GPS points around a road crossing. Also calculates the cumulative probability for filtering to xx% UD
#'
#' @param zcrossing.step 
#'
#' @return data frame with x and y coords, probability, cumulative probability, and crossing.step ID. the coordinates are the center of each 100m grid cell.
#' 
#' @details
#' relies on package BBMM which is not on CRAN as of April 2024. I downloaded version 3.0 from https://cran.r-project.org/src/contrib/Archive/BBMM/ on 4/10/24 and saved it to C:\Users\scott.jennings\OneDrive - Audubon Canyon Ranch\R_examples_resources\BBMM_archive
#' 
#' can install package from there with install.packages("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/R_examples_resources/BBMM_archive/BBMM_3.0.tar.gz", repos = NULL, type="source")
#' 
#'
#' @examples
calc_crossing_bbmm <- function(zcrossing.step) {
p1 <- crossing_clusters_gps %>% 
  filter(crossing.step == zcrossing.step) %>%
  arrange(datetime.local) %>% 
  mutate(time.lag = as.numeric(difftime(datetime.local, lag(datetime.local), units = "mins")))

bb <- brownian.bridge(x = p1$easting, y = p1$northing, time.lag = p1$time.lag[-1], location.error = 5, cell.size = 100, max.lag = 130, time.step = 5)

bb_df <- data.frame(x = bb$x, y = bb$y, probability = bb$probability) %>% 
  mutate(zindex = row_number()) %>% 
  arrange(-probability) %>% 
  mutate(prob.sum = cumsum(probability),
         crossing.step = zcrossing.step) 

}

crossing_steps <- distinct(crossing_clusters_gps, crossing.step)$crossing.step

system.time(
  all_clusters_bbmm <- map_df(crossing_steps, calc_crossing_bbmm), gcFirst = TRUE
)

saveRDS(all_clusters_bbmm, here("model_objects/all_clusters_bbmm"))


bbmm_plotter <- function(zcrossing.step) {
ggplot() +
  geom_rect(data = filter(all_clusters_bbmm, prob.sum < 0.9, crossing.step == zcrossing.step), aes(xmin = x - 50, xmax = x + 50, ymin = y - 50, ymax = y + 50, fill = probability)) +
  geom_path(data = filter(crossing_clusters_gps, crossing.step == zcrossing.step), aes(x = easting, y = northing)) +
  geom_point(data = filter(crossing_clusters_gps, crossing.step == zcrossing.step), aes(x = easting, y = northing)) +
    theme_bw() +
    labs(title = zcrossing.step)
}

bbmm_plotter(crossing_steps[1])
