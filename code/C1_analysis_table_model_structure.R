

# this is the first step for the main analysis
# this must be run before any other CX_... scripts

# here the final analysis table is built up by combining all predictors and response, final filtering



library(tidyverse)
library(here)
library(sf)
library(readxl)


source(here("code/helper_data.R"))


# used and available road segments ----
all_analysis_segments_by_lion_year <- readRDS(here("data/all_analysis_segments_by_lion_year"))

all_analysis_segments_by_lion_year <- all_analysis_segments_by_lion_year %>% 
  data.frame() %>% 
  select(-geometry) %>% 
  filter(!(animal.id == "P2" & year == 2016),
         !(animal.id == "P19" & year == 2019))
# 9/4/25 just realized P2 maybe not independent until 2017 and P19 not until 2020


all_analysis_segments_by_lion_year %>% 
  count(crossing.step, seg.label) %>% 
  filter(n > 1, !is.na(crossing.step)) %>%
  nrow()


all_analysis_segments_by_lion_year %>%
  count(animal.id, year, used.available) %>% 
  pivot_wider(id_cols = c(animal.id, year), names_from = used.available, values_from = n) %>% 
  view()



# lion sex and age ----
collar_info <- read_xlsx("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/data/COLLAR DATE INFO_31Dec2023.xlsx")

names(collar_info) <- tolower(names(collar_info))
names(collar_info) <- str_replace_all(names(collar_info), "# ", "")
names(collar_info) <- str_replace_all(names(collar_info), " ", "\\.")

collar_info <- collar_info %>%
  mutate(animal.id = ifelse(animal.id == "P5*", "P5", animal.id)) %>% 
  right_join(all_analysis_segments_by_lion_year %>% data.frame() %>% distinct(animal.id)) %>% 
  select(animal.id, sex, collar, collar.id, age, collar.start, collar.end)


# currently assuming age did not flip from SA to A for a collar period if collar was put on as SA.
# waiting to hear back from Q about possibly flipping ages for some lions
adult_collar_years <- collar_info %>% 
  filter(age == "ADULT") %>% 
  mutate(adult.start.year = year(collar.start)) %>% 
  group_by(animal.id) %>% 
  filter(adult.start.year == min(adult.start.year)) %>% 
  distinct(animal.id, adult.start.year)

puma_sexes <- collar_info %>% 
  distinct(animal.id, sex) %>% 
  full_join(adult_collar_years)

puma_sexes_ages <- all_analysis_segments_by_lion_year %>% 
  data.frame() %>% 
  distinct(animal.id, year) %>% 
  full_join(puma_sexes) %>% 
  mutate(adult.start.year = replace_na(adult.start.year, 2025), # just needs to be something after the end of the study
         age = ifelse(year < adult.start.year, "sub-adult", "adult")) %>% 
  mutate(age = case_when(animal.id == "P33" & year == 2023 ~ "adult", # adding a couple age adjustments based on a phone call with Q on 9/16/25
                         animal.id == "P36" & year == 2023 ~ "adult",
                         TRUE ~ age)) %>% 
  arrange(animal.id, year)

# landscape configuration ----

all_analysis_segments_cohesion_treshr <- readRDS(here("data/all_analysis_segments_cohesion_treshr_full")) %>% 
  filter(buff == 200, forest.threshold == 50) %>% # updated 9/16/25
  select(seg.label, year, cohesion)


all_analysis_segments_treshr_patchrd <- readRDS(here("data/all_analysis_segments_cohesion_treshr_full")) %>% 
  filter(buff == 200, forest.threshold == 25) %>% # updated 9/16/25
  select(seg.label, year, patchrd = patch.touches.road)



# landscape composition ----
all_analysis_segments_mean_tre_shr <- readRDS(here("data/all_analysis_segments_mean_tre_shr_full")) %>% 
  filter(buff == 120) %>% # updated 9/16/25
  select(seg.label, year, tre.shr = mean.veg)


all_analysis_segment_impervious <- readRDS(here("data/all_analysis_segment_impervious_full")) %>% 
  filter(buff == 90) %>%  # updated 9/16/25
  select(seg.label, year, impervious = mean.percent.impervious)


# road characteristics ----

bridges_per_segment <- readRDS(here("data/bridges_per_segment")) %>%  # updated 9/16/25
  select(seg.label, year, bridge.binary)

all_analysis_segments_class_clean <- readRDS(here("data/all_analysis_segments_class_clean")) %>% 
  select(seg.label, year, class)

all_analysis_segments_class_clean %>% 
  distinct(seg.label, class) %>% count(class)

all_analysis_segments_class_clean <- all_analysis_segments_class_clean %>% 
  mutate(class = case_when(class == "Access Road" ~ "Local",
                           class == "Freeway" ~ "Highway",
                           TRUE ~ class)) %>% 
  mutate(class = factor(class, levels = c("Local", "Collector", "Arterial", "Highway")))

all_analysis_segments_class_clean %>% 
  distinct(seg.label, class) %>% count(class)

# combine all ----

analysis_table <- all_analysis_segments_by_lion_year %>% 
  data.frame() %>% 
  select(seg.label, animal.id, year, used.available, seg.length) %>% 
  mutate(seg.length = as.numeric(seg.length),
         seg.length = round(seg.length, 0)) %>% 
  full_join(puma_sexes_ages) %>% 
  select(-adult.start.year)

all(count(analysis_table, animal.id, year) %>% arrange(animal.id, year) == count(analysis_table, animal.id, year) %>% arrange(animal.id, year))

analysis_table <- analysis_table %>% 
  left_join(all_analysis_segments_cohesion_treshr)

all(count(analysis_table, animal.id, year) %>% arrange(animal.id, year) == count(analysis_table, animal.id, year) %>% arrange(animal.id, year))

analysis_table <- analysis_table %>% 
  left_join(all_analysis_segments_treshr_patchrd)

all(count(analysis_table, animal.id, year) %>% arrange(animal.id, year) == count(analysis_table, animal.id, year) %>% arrange(animal.id, year))


analysis_table <- analysis_table %>% 
  left_join(all_analysis_segments_mean_tre_shr)

all(count(analysis_table, animal.id, year) %>% arrange(animal.id, year) == count(analysis_table, animal.id, year) %>% arrange(animal.id, year))

analysis_table <- analysis_table %>% 
  left_join(all_analysis_segment_impervious)

all(count(analysis_table, animal.id, year) %>% arrange(animal.id, year) == count(analysis_table, animal.id, year) %>% arrange(animal.id, year))

analysis_table <- analysis_table %>% 
  left_join(bridges_per_segment)

all(count(analysis_table, animal.id, year) %>% arrange(animal.id, year) == count(analysis_table, animal.id, year) %>% arrange(animal.id, year))

analysis_table <- analysis_table %>% 
  left_join(all_analysis_segments_class_clean)

all(count(analysis_table, animal.id, year) %>% arrange(animal.id, year) == count(analysis_table, animal.id, year) %>% arrange(animal.id, year))

analysis_table_complete <- analysis_table %>% 
  drop_na()


saveRDS(analysis_table_complete, here("data/analysis_table_complete"))

# if already combined and saved start here ----

analysis_table_complete <- readRDS(here("data/analysis_table_complete"))

analysis_table_complete_sp <- analysis_table_complete %>% 
  group_by(seg.label, year, used.available, cohesion, patchrd, tre.shr, impervious, bridge.binary, class) %>% 
  summarise(num.lions = n(),
            which.lions = paste(animal.id, collapse = "; ")) %>% 
  left_join(all_analysis_segments_by_lion_year %>% select(-seg.length))

st_write(analysis_table_complete_sp, here("data/shapefiles/analysis_table_complete_sp.shp"), append = FALSE)



analysis_table_complete <- analysis_table_complete %>% 
  mutate(road.label = sub("^((?:[^_]*_){2}[^_]*).*", "\\1", seg.label))

analysis_table_complete_checker <- analysis_table_complete %>% 
  pivot_longer(cols = c(cohesion, tre.shr, impervious), names_to = "varb") %>% 
  group_by(road.label, varb) %>% 
  summarise(mean.value = mean(value),
            range.value = max(value) - min(value))


analysis_table_checker2 <- analysis_table_complete_checker %>% 
  filter(range.value > 20) %>% 
  group_by(road.label) %>% 
  summarise(prolem.varbs = paste(varb, collapse = "; ")) %>% 
  ungroup() %>% 
  left_join(analysis_table_complete) %>% 
  filter(used.available == 0)
