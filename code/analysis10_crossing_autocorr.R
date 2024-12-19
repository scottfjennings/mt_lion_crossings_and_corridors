


library(tidyverse)
library(here)
library(readxl)
library(sf)



source(here("code/utilities.R"))
source(here("code/helper_data.R"))

puma_sexes <- read_xlsx("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/data/COLLAR DATE INFO_31Dec2023.xlsx")

names(puma_sexes) <- tolower(names(puma_sexes))

puma_sexes <- puma_sexes %>%
  rename("animal.id" = `animal id`) %>% 
  distinct(animal.id, sex)

# final data prep ----
# not including varbs for traffic conditions for TWS/CADFW mt lion working group
bbmm_crossed_equal_seg <- readRDS(here("data/bbmm_crossed_equal_seg")) %>% 
  bind_rows(., .id = "crossing.step") %>%
  select(-X)
  

crossed_segs_clean <- bbmm_crossed_equal_seg %>% 
  animal.id_from_crossing.step() %>% 
  filter(!animal.id %in% hr_exclude_pumas,
         !seg.label %in% p31_exclude_segments#,
         #!is.na(classes)
  ) %>% 
  left_join(puma_sexes)


puma_steps <- readRDS(here("data/puma_steps"))


crossed_segs_clean_steps <- crossed_segs_clean %>% 
  left_join(puma_steps %>% 
              distinct(step.id, datetime.local, datetime.local.end) %>% 
              rename(crossing.step = step.id))

