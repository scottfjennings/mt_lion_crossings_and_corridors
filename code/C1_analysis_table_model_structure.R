

# this is the first step for the main analysis
# this must be run before any B4_... scripts, EXCEPT for B4_analyze_crossings_2step.R which uses a slightly different crossing_analysis_table and different model structures (different spatial scales for cev and tre.shr)

# here the final analysis table is built up by combining all predictors and response, final filtering
# also here is where the standard fixed effect model structure is set for use in any of the B4_... model fitting scripts





library(tidyverse)
library(here)
library(lme4)
library(AICcmodavg)
library(MuMIn)
library(readxl)
#library(merTools)
#library(sf)



source(here("code/helper_data.R"))

puma_sexes <- read_xlsx("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/data/COLLAR DATE INFO_31Dec2023.xlsx")

names(puma_sexes) <- tolower(names(puma_sexes))

puma_sexes <- puma_sexes %>%
  rename("animal.id" = `animal id`) %>% 
  distinct(animal.id, sex)

# final data prep ----
all_hr_road_habitat_df <- readRDS(here("data/analysis_inputs/all_hr_road_habitat_df")) %>% 
  filter(!animal.id %in% hr_exclude_pumas,
         !seg.label %in% p31_exclude_segments)

best_hab <- full_join(all_hr_road_habitat_df %>% 
                        filter(buff == 60) %>% 
                        select(seg.label, animal.id, year, dev.60 = mean.dev),
                      all_hr_road_habitat_df %>% 
                        filter(buff == 300) %>% 
                        select(seg.label, animal.id, year, tre.shr.300 = mean.tre.shr))


crossing_analysis_table <- readRDS(here("data/analysis_inputs/seg_crossing_sums_naive_roads_only")) %>% 
  filter(!animal.id %in% hr_exclude_pumas) %>% 
  left_join(puma_sexes) %>% 
  inner_join(best_hab) %>% 
  left_join(readRDS(here("data/analysis_inputs/streams_per_segment"))) %>% 
  filter(!animal.id %in% hr_exclude_pumas,
         !seg.label %in% p31_exclude_segments,
         !(is.na(dev.60) | is.na(tre.shr.300))) %>% # still a few lingering segments along the coast with NA habitat values
  filter(!class %in% c("Access Road", # only 5 total segments and none in any male home ranges so they break the 3 way interaction models
                       "Freeway"), # no actual crossings of freeway
         num.creek < 6) %>%  # only 7 segments have 6 or more crossings  
  mutate(#class = factor(class, levels = c("Local", "Collector", "Arterial", "Highway")),
         class = ifelse(class == "Local", "Local", "Not local"),
         crossed.bin = as.numeric(seg.wt.crossing > 0)) %>% 
  filter(!animal.id %in% few_crossings_pumas)





# optional data checking plots and summaries ----

crossing_analysis_table %>% 
  select(seg.label, class) %>% 
  distinct() %>%
  left_join(readRDS(here("data/all_hr_road_habitat")) %>% select(seg.label, geometry)) %>%
  sf::st_write(here("data/shapefiles/predictor_variable_checking/class.shp"), append = FALSE)


crossing_analysis_table %>% 
  #filter(seg.wt.crossing > 0) %>% 
  ggplot() +
  geom_density(aes(x = tre.shr.300)) +
  geom_density(aes(x = dev.60), color = "red") +
  xlim(0, 1)
  facet_wrap(~as.factor(num.creek))

  filter(crossing_analysis_table, seg.wt.crossing > 0, dev.60 < .25) %>% nrow()
  

  # any NA
  crossing_analysis_table %>%
    filter(if_any(everything(), is.na)) %>% nrow()
  # good 3/20/25
  
  # some raw visualization ----
  
  crossing_analysis_table %>% 
    ggplot() +
    geom_density(aes(x = dev.60, color = class))
  
  crossing_analysis_table %>% 
    distinct(seg.label, dev.60) %>% 
    filter(dev.60 < 0.25) %>% nrow()
  
  crossing_analysis_table %>% 
    distinct(seg.label, tre.shr.300) %>% 
    filter(tre.shr.300 > 0.75) %>% nrow()
  
  
  
  crossing_analysis_table %>%
    ggplot() +
    geom_point(aes(y = seg.raw.crossing, x = as.character(year), color = sex), alpha = 0.2) +
    stat_smooth(aes(y = seg.raw.crossing, x = as.character(year), color = sex)) +
    facet_wrap(~animal.id, scales = "free_y")
  # not much year pattern
  
  
  crossing_analysis_table %>% 
    select(class, dev.60, tre.shr.300, num.creek) %>%
    mutate(class = as.numeric(class)) %>% 
    cor()
  
  
# load candidate model fixed effects structure  ----
# load these here to use in multiple B4_... scripts. Easier to change candidate set in only one place than many.   
  
# not enough data for any num.creek interactions  
model_formulas_1step <- list(
  treshr300 = "tre.shr.300",
  dev60 = "dev.60",
  creek = "num.creek",
  class = "class",
  sex = "sex",
  
  # 2 way additive
  treshr300_creek = "tre.shr.300 + num.creek",
  dev60_creek = "dev.60 + num.creek",
  treshr300_sex = "tre.shr.300 + sex",
  dev60_sex = "dev.60 + sex",
  treshr300_class = "tre.shr.300 + class",
  dev60_class = "dev.60 + class",
  creek_sex = "num.creek + sex",
  class_sex = "class + sex",
  class_creek = "class + num.creek",
  
  # 3 way additive
  treshr300_creek_class = "tre.shr.300 + num.creek + class",
  treshr300_creek_sex = "tre.shr.300 + num.creek + sex",
  treshr300_class_sex = "tre.shr.300 + class + sex",
  dev60_creek_class = "dev.60 + num.creek + class",
  dev60_creek_sex = "dev.60 + num.creek + sex",
  dev60_class_sex = "dev.60 + class + sex",
  creek_class_sex = "num.creek + class + sex",
  
  # 2 way interactions
  treshr300.creek = "tre.shr.300 * num.creek",
  dev60.creek = "dev.60 * num.creek",
  treshr300.sex = "tre.shr.300 * sex",
  dev60.sex = "dev.60 * sex",
  treshr300.class = "tre.shr.300 * class",
  dev60.class = "dev.60 * class",
  creek.sex = "num.creek * sex",
  creek.class = "num.creek * class",
  class.sex = "class * sex",
  
  # 3 way interactions
  treshr300.creek.class = "tre.shr.300 * num.creek * class",
  treshr300.creek.sex = "tre.shr.300 * num.creek * sex",
  treshr300.class.sex = "tre.shr.300 * class * sex",
  dev60.creek.class = "dev.60 * num.creek * class",
  dev60.creek.sex = "dev.60 * num.creek * sex",
  dev60.class.sex = "dev.60 * class * sex",
  creek.class.sex = "num.creek * class * sex",
  
  # Null
  intercept = "1"
)
  