

# fitting brownian bridge movement models

# objective is to develop probabilistic estimates of where each rod crossing occured.

# then evaluate the relative importance of covariates in predicting those crossings


library(tidyverse)
library(here)
library(sp)
library(move)



analysis_table <- readRDS(here("data/analysis_table"))


p1 <- analysis_table %>% 
  filter(animal.id == "P1", collar.id == 11082)

p1_move <- move(x = p1$longitude, y = p1$latitude, 
              time = p1$datetime.local, 
              proj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"), 
              data = p1, animal = p1$animal.id)

p1_prj <- spTransform(p1_move, 
                      CRSobj = CRS("+proj=utm +zone=10 +datum=WGS84"))

dBB.p1 <- brownian.bridge.dyn(p1_prj, ext=.85, raster=100, location.error=20)

UD_p1 <- getVolumeUD(dBB.p1)
