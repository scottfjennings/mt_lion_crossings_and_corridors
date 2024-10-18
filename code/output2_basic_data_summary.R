


library(tidyverse)
library(ggnewscale)
library(here)
library(sf)
#library(amt)

options(scipen = 999)

source(here("code/utilities.R"))
source(here("code/helper_data.R"))

# read data ----
# steps
puma_steps <- readRDS(here("data/puma_steps")) 

# number of lions 
distinct(puma_steps, animal.id) %>% 
  filter(!animal.id %in% exclude_pumas, !animal.id %in% hr_exclude_pumas) %>% nrow()

# date range
min(puma_steps$datetime.local)
max(puma_steps$datetime.local)

# number of crossing steps
readRDS(here("data/road_crossing_steps_napa_sonoma_2hr")) %>% 
  select(-geometry) %>% 
  data.frame() %>% 
  separate(step.id, c("animal.id", "collar.id", "step.num"), sep = "_") %>% 
  group_by(animal.id) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(mean.cross = mean(n),
         se.cross = sd(n)/sqrt(nrow(.))) %>% 
  arrange(-n) %>% 
  view()

#

readRDS(here("data/road_crossing_steps_napa_sonoma_2hr")) %>% 
  left_join(readRDS(here("data/puma_steps"))) %>% view()

# and county boundaries
study_area_counties <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/data/study_area_counties") %>% 
  filter(NAME %in% c("Napa", "Sonoma"))

roadkill <- read.csv("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/data/North Bay Puma Roadkill Data.csv") %>% 
  filter(county %in% c("Sonoma", "Napa")) 
 
roadkill_sf <- st_as_sf(roadkill,
                        coords = c("long", "lat"),
                        crs = 4326)
ggplot() +
  geom_sf(data = study_area_counties) +
  geom_sf(data = roadkill_sf)


# summarizing crossings

prob_crossings <- readRDS(here("model_objects/prob_crossings")) %>% 
  bind_rows(.id = "crossing.step")

named_road_crossings <- prob_crossings %>% 
  group_by(label) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(tot.crossings = sum(n),
         prop.crossings = n/tot.crossings) %>% 
  arrange(-n) 

road_segment_crossings <- prob_crossings %>% 
  group_by(label, objectid, class) %>% 
  count() %>%
  mutate(seg.length = st_length(.),
         crossing.density = n/(seg.length/100),
         crossing.density.out = case_when(as.numeric(crossing.density) > 500 ~ ">500",
                                          between(as.numeric(crossing.density), 250, 500) ~  "250-500",
                                          between(as.numeric(crossing.density), 100, 249) ~ "100-249",
                                          between(as.numeric(crossing.density), 50, 99) ~ "50-99",
                                          between(as.numeric(crossing.density), 25, 49) ~ "25-49",
                                          as.numeric(crossing.density) < 25 ~ "<25"),
         crossing.density.out = factor(crossing.density.out, levels = c(">500", "250-500", "100-249", "50-99", "25-49", "<25"))) %>% 
  arrange(crossing.density.out) 
 

  ggplot() +
  geom_sf(data = road_segment_crossings, aes(color = crossing.density.out)) +
    scale_color_brewer(palette = "Spectral")

road_segment_crossings %>% 
  group_by(class) %>% 
  summarise(mean.dens = mean(crossing.density)) %>% 
  view(  )


