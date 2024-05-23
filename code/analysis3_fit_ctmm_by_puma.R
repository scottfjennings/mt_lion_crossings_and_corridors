





library(tidyverse)
library(here)
library(sf)
library(move)
library(ctmm)
library(terra)

source(here("code/utilities.R"))

options(scipen = 999)



analysis_table <- readRDS(here("data/analysis_table"))

DATA <- filter(analysis_table, animal.id == "P1", collar.id == "11082")
DATA <- move(x = DATA$longitude, y = DATA$latitude, time = DATA$datetime.local, proj = "+proj=latlong +datum=NAD83")
DATA <- as.telemetry(DATA)

# Now we can plot the data
plot(DATA)
# The radius and translucence of each point corresponds to its telemetry error's variance.

# Now we are going to calculate and plot the variogram with a zoom slider.
# Also see vignette("variogram")
SVF <- variogram(DATA)
zoom(SVF,level=c(0.95,0.50))
# This variogram is a bit erratic due to regular gaps in the data, but it is good enough to get a rough sense of what the movement parameters might be.
# The variogram asymptotes at around 3 km^2, which corresponds to the variance and is evidence of range residence.
# It takes around 1-2 days for the variogram to turn over after its initial ramp upwards, which corresponds to the home-range crossing timescale.
# If we zoom in to the beginning of the variogram, then we can see that there is some miniscule upwards curvature for a couple of lags, which is evidence of continuous velocity.

# Now we are going to guesstimate these same three parameters interactively.
variogram.fit(SVF,level=c(0.95,0.50),name="GUESS1")
# After playing with the sliders, we can click the "save" button to store our guesstimate to the named "GUESS" variable.

# Now we are going to perform model selection.
FITS <- ctmm.select(DATA,GUESS1,verbose=TRUE)
summary(FITS)

# By default our model does not include telemetry error, but Leroy has GPS DOP estimates that we can use to improve our model fit and Kriging estimates.
GUESS2 <- FITS[[1]]
GUESS2$error <- TRUE
FIT <- ctmm.fit(DATA,GUESS2)
# AIC improvement by including telemetry error.
FITS[[1]]$AICc - FIT$AICc

# Next we should compare this movement model back to the data and variogram to make sure that it looks reasonable.
plot(DATA,CTMM=FIT)
zoom(SVF,CTMM=FIT)

# Finally we can estimate the occurrence distribution with our selected model.
OD <- occurrence(DATA[2:3,],GUESS2)
zoom(DATA,UD=OD,col.level=NA)
