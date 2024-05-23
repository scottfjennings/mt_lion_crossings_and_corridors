

library(tidyverse)
library(here)
library(sf)
library(move)
library(ctmm)
library(terra)

source(here("code/utilities.R"))

options(scipen = 999)


crossing_steps <- readRDS(here("data/crossing_step_ids"))

# read steps and back dop
crossing_clusters_gps <- readRDS(here("data/crossing_clusters_gps")) %>% 
  separate(step.id, c("animal.id", "collar.id"), sep = "_", remove = FALSE) %>% 
  mutate(collar.id = as.numeric(collar.id),
         datetime.local = as.character(datetime.local)) %>% 
  left_join(readRDS(here("data/analysis_table")) %>% 
              dplyr::select(animal.id, collar.id, datetime.local, dop) %>% 
              mutate(datetime.local = as.character(datetime.local))) %>% 
  mutate(datetime.local = as.POSIXct(datetime.local))
#

#' ctmm_ud_sf
#' 
#' calculate a 90% UD based on the best supported movement model as implemented in ctmm
#' 
#' adapted from supplement to Fleming, C. H., W. F. Fagan, T. Mueller, K. A. Olson, P. Leimgruber, and J. M. Calabrese. 2016. Estimating where and how animals travel: an optimal framework for path reconstruction from autocorrelated tracking data. Ecology 97:576–582.

#'
#' @param zcrossing.step 
#'
#' @return
#' @export
#'
#' @examples
ctmm_ud_sf <- function(zcrossing.step) {
DATA <- filter(crossing_clusters_gps, crossing.step == zcrossing.step)
DATA <- move(x = DATA$easting, y = DATA$northing, time = DATA$datetime.local, proj = "+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs")
DATA <- as.telemetry(DATA)

# Now we are going to calculate and plot the variogram with a zoom slider.
# Also see vignette("variogram")
SVF <- variogram(DATA)
# This variogram is a bit erratic due to regular gaps in the data, but it is good enough to get a rough sense of what the movement parameters might be.

# Now we are going to guesstimate these same three parameters interactively.
GUESS1 <- variogram.fit(SVF, interactive = FALSE)
# After playing with the sliders, we can click the "save" button to store our guesstimate to the named "GUESS" variable.

# Now we are going to perform model selection.
FITS <- ctmm.select(DATA,GUESS1,verbose=TRUE)
#summary(FITS)

# By default our model does not include telemetry error, but Leroy has GPS DOP estimates that we can use to improve our model fit and Kriging estimates.
GUESS2 <- FITS[[1]]
GUESS2$error <- TRUE
FIT <- ctmm.fit(DATA,GUESS2)
# AIC improvement by including telemetry error.
FITS[[1]]$AICc - FIT$AICc

# Next we should compare this movement model back to the data and variogram to make sure that it looks reasonable.
#plot(DATA,CTMM=FIT)
#zoom(SVF,CTMM=FIT)

# Finally we can estimate the occurrence distribution with our selected model.
OD <- occurrence(DATA[4:5,], FIT)
# convert to sf object
od_sf <- as.sf(OD, level.UD = 0.9)
step_out <- list(SVF = SVF,
                 FITS = FITS,
                 OD = OD,
                 od_sf = od_sf)
}

test_steps = crossing_steps[1:5]

system.time(
ctmm_ud <- map(test_steps, safely(ctmm_ud_sf)), gcFirst = TRUE
)
names(ctmm_ud) <- test_steps

map(ctmm_ud, "error")


zoom(ctmm_ud[[5]]$SVF,CTMM=ctmm_ud[[5]]$FIT)
