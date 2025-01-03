
# basic initial cleaning of mountain lion GPS collar data
# 1. exclude initial data when P2, P6, and P19 were dependent with P1 based on their proximity to each other  
# 2. filter for higher accuracy GPS points
# 3. convert fixes to steps


# this requires:
# "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/data/lion_deployments.csv"
# "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/data/cleaned_collars"

# this creates:
# "data/p1_family_proximity.csv" - distances between P1 and her kittens
# "data/analysis_table" - the cleaned fixes with the dependent P2, P6, and P19 data filtered out
# "data/puma_steps" - the cleaned fixes converted to steps

# this optionally creates
# "figures/p1_family_proximity.png" - figure of distances between P1 and her kittens
# "data/pre_analysis_table" - uncleaned fixes with the dependent P2, P6, and P19 data filtered out



library(tidyverse)
library(here)
library(sf)

# this is needed for puma_proximity()
source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/code/mountain_lion_utilities.R")

# this is needed for exclude_pumas
source(here("code/helper_data.R"))



# number of days to trim from start and end of each collar deployment
start.buffer = 1
end.buffer = -2


# read data prepared in  C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/code/clean_raw_download.R
deployments <- read_csv("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/data/lion_deployments.csv")

collars <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/data/cleaned_collars") 

# first need to ID when P2, P6, and P19 were still dependent on P1 ----
# use puma_proximity() from mt_lion_data_work/code/mountain_lion_utilities.R



p1_family_proximity <- bind_rows(puma_proximity("P1", "P2"),
                                 puma_proximity("P1", "P6"),
                                 puma_proximity("P1", "P19")) %>% 
  arrange(round.datetime) %>%
  filter(!is.na(dist)) %>% 
  mutate(date.local = as.Date(round.datetime, "America/Los_Angeles"),
         week = format(date.local, "%Y%U")) %>% 
  group_by(cat2.id, week) %>%
  mutate(mean.weekly.dist = mean(dist),
         time.diff = cat1.time - cat2.time) %>% 
  ungroup()

write.csv(p1_family_proximity, here("data/p1_family_proximity.csv"), row.names = FALSE)

p1_family_proximity %>%  
  ggplot() +
  geom_line(aes(x = date.local, y = dist, color = cat2.id)) +
  geom_line(aes(x = date.local, y = mean.weekly.dist, color = cat2.id), linewidth = 2) +
  # stat_smooth(aes(x = date.local, y = dist, group = cat2.id), color = "gray10", linewidth = 2, se = FALSE, span = 0.2, method = "loess") +
  theme_bw() +
  scale_x_date(date_breaks = "1 month") +
  labs(x = "Date",
       y = "Distance (m)",
       title = "Distance between P1 and 3 of her kittens",
       color = "Kitten ID") +
  facet_wrap(~cat2.id, nrow = 3, scales = "free")

# ggsave(here("figures/p1_family_proximity.png"), width = 10)

# there seems to be a clear dependent/independent signal when the weekly average distance goes above 1 km (checking with Quinton about this as of 3/13/24)

independent_dates <- p1_family_proximity %>% 
  filter(mean.weekly.dist > 1000) %>% 
  group_by(cat2.id) %>% 
  filter(cat2.time == min(cat2.time)) %>% 
  select(animal.id = cat2.id, min.datetime = cat2.time)

# update the analysis_dates table ----
analysis_dates <- collars %>% 
  full_join(independent_dates) %>% 
  mutate(new.start = if_else(animal.id %in% independent_dates$animal.id, min.datetime, as.POSIXct(paste(collar.start + start.buffer, "00:00:01"), format = "%Y-%m-%d %H:%M:%S")),
         new.end = as.POSIXct(paste(collar.end + end.buffer, "23:59:59"), format = "%Y-%m-%d %H:%M:%S")) %>% 
  filter(!animal.id %in% exclude_pumas)


pre_analysis_table <- right_join(deployments, analysis_dates) %>% 
  select(animal.id, collar.id, datetime.local, date.local, collar.start, collar.end, latitude, longitude, altitude, dop, fix.type, sex, collar, age, new.start, new.end) %>% 
  mutate(keep.row = between(datetime.local, new.start, new.end))

# saveRDS(pre_analysis_table, here("data/pre_analysis_table"))
# don't think there's any reason to save this

# initially and arbitrarily used dop < 5 while getting the code to run, then switched to dop < 10 following 
# McClure, M. L., B. G. Dickson, and K. L. Nicholson. 2017. Modeling connectivity to identify current and future anthropogenic barriers to movement of large carnivores: A case study in the American Southwest. Ecology and Evolution 7:3762–3772.
# and
# D’eon, R. G., and D. Delparte. 2005. Effects of radio-collar position and orientation on GPS radio-collar performance, and the implications of PDOP in data screening. Journal of Applied Ecology 42:383–388.
# D’eon and Delparte found that eliminating 2D fixes improved accuracy similar to only using dop < 10, but that it eliminated 7.7% of the data whereas using dop < 10 eliminated only 1.3% of their data. In our dataset, filtering dop > 10 | fix.type %in% c("2D", "No Fix") causes a 2.1% data loss, which we feel is acceptable so we filtered based on both dop < 10 and fix type = 3D 


analysis_table <- pre_analysis_table %>% 
#analysis_table <- readRDS(here("data/pre_analysis_table")) %>% 
  filter(keep.row == TRUE, dop < 10, str_detect(fix.type, "3D")) %>% 
  select(animal.id, collar.id, datetime.local, date.local, latitude, longitude, altitude, dop, sex, collar, age)


saveRDS(analysis_table, here("data/analysis_table"))

# converting single row GPS data to paired-GPS steps ----

# load lion GPS data and convert to steps ----
analysis_table <- readRDS(here("data/analysis_table"))
#
# simpler way of creating steps, this doesn't resample so we keep all steps ----
# need to convert to sf object so can change lat/lon to UTM
puma_gps_utm <- analysis_table %>% 
  select(animal.id, collar.id, longitude, latitude, datetime.local) %>% 
  st_as_sf(x = .,
           coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  st_transform(crs = 26910) %>% 
  mutate(easting = st_coordinates(.)[,1],
         northing = st_coordinates(.)[,2]) 

# then converting fixes to steps
puma_steps <- puma_gps_utm %>% 
  data.frame() %>% 
  arrange(animal.id, datetime.local) %>% 
  group_by(animal.id, collar.id) %>% 
  mutate(datetime.local.end = lead(datetime.local),
         easting.end = lead(easting),
         northing.end = lead(northing),
         step.dur = datetime.local.end - datetime.local) %>% 
  ungroup() %>% 
  filter(!is.na(easting.end)) %>% 
  mutate(step.dist = st_distance(st_as_sf(., coords = c("easting", "northing"), crs=26910),
                                 st_as_sf(., coords = c("easting.end", "northing.end"), crs=26910), by_element = TRUE),
         step.id = paste(animal.id, collar.id, row_number(), sep = "_")) %>% 
  select(-geometry) 


saveRDS(puma_steps, here("data/puma_steps"))


