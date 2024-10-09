# Load the package and the buffalo tracking dataset
library(ctmm)
library(tidyverse)
library(here)
library(sf)
library(move)
library(terra)
library(lubridate)

# load and prep data ----

analysis_table <- readRDS(here("data/analysis_table")) %>% 
  mutate(animal.id = ifelse(animal.id == "P5*", "P5", animal.id)) %>% 
  dplyr::select(individual.local.identifier = animal.id, timestamp = datetime.local, location.long = longitude,
                location.lat = latitude) %>% 
  arrange(individual.local.identifier, timestamp) %>% 
  group_by(individual.local.identifier) %>% 
  mutate(zt = timestamp - lag(timestamp)) %>% 
  ungroup() %>% 
  filter(zt > 3600)

analysis_table_tl <- analysis_table %>% 
  as.telemetry()




plot(analysis_table)


# Estimate the empirical variograms ----

puma_variogram <- function(puma) {
  vg <- variogram(analysis_table_tl[[puma]])
}

vg_out <- map(names(analysis_table_tl), puma_variogram)
names(vg_out) <- names(analysis_table_tl)

# Visually inspect these for range - residency
par(mfrow =c(1,2))
plot(vg_out[[1]])
plot(vg_out[[2]])

# no range residency for P19, P21, P34, P36 (funky locations, in the ocean), P44
# iffy for P14, P24, P26, P37, P41 (started out range resident then ended just before 4 months, dependent kitten?)
# P6 ok but only 50 days

# Generate initial guesses of the parameter estimates ----
puma_guess <- function(puma) {
  guess_out <- ctmm.guess(analysis_table_tl[[puma]], interactive = FALSE)
}

puma_guesses <- map(names(analysis_table_tl), puma_guess)
names(puma_guesses) <- names(analysis_table_tl)



#Fit and select movement models for each animal ----

puma_mods <- function(puma) {
  FITS_1 <- ctmm.select(analysis_table_tl[[puma]],
                        puma_guesses[[puma]])
}

system.time(
puma_fits <- map(names(analysis_table_tl), puma_mods)
)
#4 hr 41 min
names(puma_fits) <- names(analysis_table_tl)


saveRDS(puma_fits, here("model_objects/puma_fits"))



# Estimate the HR estimates for each individual ----

puma_fits <- readRDS(here("model_objects/puma_fits"))

HR_UDS <- akde(analysis_table_tl,
               puma_fits)

saveRDS(HR_UDS, here("model_objects/puma_hr_uds"))
puma_hr_uds <- readRDS(here("model_objects/puma_hr_uds"))


#Plot the data and HR estimates
plot(analysis_table_tl[["P19"]],
     UD=HR_UDS[["P19"]],
     level.UD = 0.999)



study_area_counties <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/data/study_area_counties")  %>% 
  filter(NAME %in% c("Napa", "Sonoma")) %>% 
  dplyr::select(NAME) %>% 
  st_transform(crs = 26910)

kitty = "P41"


kitty_plotter <- function(kitty) {
zz <- as.sf(HR_UDS[[kitty]], DF = "PDF", level.UD = 0.999) %>% 
  st_transform(crs = 26910)

yy <- analysis_table %>% 
  filter(individual.local.identifier == kitty) %>% 
  st_as_sf(x = .,
           coords = c("location.long", "location.lat"),
           crs = 4326)%>% 
  st_transform(crs = 26910)

ggplot() +
  geom_sf(data = study_area_counties) +
  geom_sf(data = zz, fill = NA) +
  geom_sf(data = yy)
}


kitty_plotter("P1")


hr_exclude_pumas <- c("P36", #funky locations, in the ocean
                      "P14", # many points outside Sonoma/Napa
                      "P37", # not totally range resident and 100% HR covers much of sonoma 
                      "P19", # many points outside Sonoma/Napa
                      )


ggplot() +
  geom_sf(data = study_area_counties) +
  geom_sf(data = yy)
