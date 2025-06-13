


# create a df with every valid lion, year, month, segment combo

library(tidyverse)
library(here)
library(sf)

source(here("code/helper_data.R"))


lion_years_months <- readRDS(here("data/puma_steps")) %>% 
  mutate(year = year(datetime.local),
         month = month(datetime.local)) %>% 
  distinct(animal.id, year, month)  %>% 
  filter(animal.id %in% analysis_pumas,
         !animal.id %in% few_crossings_pumas,
         !animal.id %in% hr_exclude_pumas) 

# Cross with your real seg.label-animal.id pairs
lion_segments <- readRDS(here("data/segments_in_homeranges")) %>% 
  data.frame() %>% 
  select(animal.id = puma, seg.label)   %>% 
  filter(animal.id %in% analysis_pumas,
         !animal.id %in% few_crossings_pumas,
         !animal.id %in% hr_exclude_pumas,
         !seg.label %in% p31_exclude_segments) 

# full_combos is every segment in each lion's home range, duplicated for every buffer distance (n = 10) and year that there is data for that lion
full_lion_year_month_seg <- lion_segments%>%
  full_join(lion_years_months)
# there should be one row for each lion - year - month - segment
count(full_lion_year_month_seg, animal.id, seg.label, year, month) %>% filter(n != 1) %>% nrow()


saveRDS(full_lion_year_month_seg, here("data/full_lion_year_month_seg"))
