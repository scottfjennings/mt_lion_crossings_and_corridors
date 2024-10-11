

# fitting brownian bridge movement models

# objective is to develop probabilistic estimates of where each rod crossing occured.

# then evaluate the relative importance of covariates in predicting those crossings


library(tidyverse)
library(here)
library(sf)
library(move)
library(BBMM)

source(here("code/utilities.R"))

options(scipen = 999)

# from analysis1_naive_road_crossing.R
naive_crossings <- readRDS(here("data/naive_crossings_napa_sonoma_2hr"))

# from prep_data1_gps_data.R
puma_steps <- readRDS(here("data/puma_steps")) %>% 
  mutate(step.dist.2hr = (step.dist/as.numeric(step.dur)) * 7200)

# from analysis2_crossing_step_clusters.R
crossing_clusters_gps <- readRDS(here("data/crossing_clusters_gps_1step")) 

# then fit the dBBMM to each of those 3 step clusters


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

bb <- brownian.bridge(x = p1$easting, y = p1$northing, time.lag = p1$time.lag[-1], location.error = 26.2, cell.size = 30, max.lag = 130, time.step = 5)

}



crossing_steps <- distinct(crossing_clusters_gps, crossing.step)$crossing.step

system.time(
  all_clusters_bbmm <- map(crossing_steps, calc_crossing_bbmm), gcFirst = TRUE
)
# 320 sec
names(all_clusters_bbmm) <- crossing_steps


# check if any BBMMs resulted in NAN probability for any cell. prob_checker() is in utilities.R
prob_check <- map_df(crossing_steps, prob_checker)


# nope, save output
saveRDS(all_clusters_bbmm, here("model_objects/all_clusters_bbmm_1step"))


