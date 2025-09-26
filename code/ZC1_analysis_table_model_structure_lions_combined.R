

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
# 9/4/25 just realized P2 maybe not independent until 2016 and P19 not until 2020


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
  arrange(animal.id, year)

# landscape configuration ----

all_analysis_segments_cohesion_treshr_patchrd <- readRDS(here("data/all_analysis_segments_cohesion_treshr_full")) %>% 
  filter(buff == 300, forest.threshold == 25) %>% 
  select(seg.label, year, cohesion, patchrd = patch.touches.road) %>% 
  mutate(cohesion = scale(cohesion)[,1],
         patchrd = scale(patchrd)[,1])


# landscape composition ----
all_analysis_segments_mean_tre_shr <- readRDS(here("data/all_analysis_segments_mean_tre_shr_full")) %>% 
  filter(buff == 60) %>% 
  select(seg.label, year, tre.shr = mean.veg) %>% 
  mutate(tre.shr = scale(tre.shr)[,1])


all_analysis_segment_impervious <- readRDS(here("data/all_analysis_segment_impervious_full")) %>% 
  filter(buff == 60) %>% 
  select(seg.label, year, impervious = mean.percent.impervious) %>% 
  mutate(impervious = scale(impervious)[,1])


# road characteristics ----

streams_per_segment <- readRDS(here("data/streams_per_segment")) %>% 
  select(seg.label, year, riparian.binary)

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
  left_join(all_analysis_segments_cohesion_treshr_patchrd)

all(count(analysis_table, animal.id, year) %>% arrange(animal.id, year) == count(analysis_table, animal.id, year) %>% arrange(animal.id, year))

analysis_table <- analysis_table %>% 
  left_join(all_analysis_segments_mean_tre_shr)

all(count(analysis_table, animal.id, year) %>% arrange(animal.id, year) == count(analysis_table, animal.id, year) %>% arrange(animal.id, year))

analysis_table <- analysis_table %>% 
  left_join(all_analysis_segment_impervious)

all(count(analysis_table, animal.id, year) %>% arrange(animal.id, year) == count(analysis_table, animal.id, year) %>% arrange(animal.id, year))

analysis_table <- analysis_table %>% 
  left_join(streams_per_segment)

all(count(analysis_table, animal.id, year) %>% arrange(animal.id, year) == count(analysis_table, animal.id, year) %>% arrange(animal.id, year))

analysis_table <- analysis_table %>% 
  left_join(all_analysis_segments_class_clean)

all(count(analysis_table, animal.id, year) %>% arrange(animal.id, year) == count(analysis_table, animal.id, year) %>% arrange(animal.id, year))

analysis_table_complete <- analysis_table %>% 
  drop_na()


saveRDS(analysis_table_complete, here("data/analysis_table_complete"))

# if already combined and saved start here ----

analysis_table_complete <- readRDS(here("data/analysis_table_complete"))


# optional data checking plots and summaries ----


analysis_table_complete %>% 
  count(used.available)


analysis_table_complete %>%
  pivot_longer(cols = c(tre.shr, impervious, cohesion)) %>% 
  ggplot() +
  geom_density(aes(x = value, color = name)) +
  facet_wrap(~name)

  

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
  
  
  # define models
  # response is monthly.seg.raw.crossing
  # using offset(log(monthly.seg.wt.crossing + 0.0001))
  # including random effect (1|animal.id)
  
  # fixed variables
  # cohesion.200.50
  # mean.tre.shr.210
  # mean.dev.300
  # num.creek.bin
  # class
  # sex
  
# code to generate exploratory models ----
  # Define variable sets
  core_vars <- c("cohesion"
                 , "patchrd"
                 , "tre.shr"
                 , "impervious" 
                 , "riparian.binary"
                 , "class"
                 , "sex"
                 , "age"
                 )
  
  
  restricted_set <- c("mean.tre.shr.300", "mean.dev.60")
  
make_candidate_set <- function(zvars, zrestricted) {
   # All combinations of 1–6 predictors
  model_combos <- unlist(lapply(1:length(zvars), function(k) {
    combn(zvars, k, simplify = FALSE)
  }), recursive = FALSE)
  
  # Filter to enforce only zero or one restricted variable per model
  valid_combos <- model_combos %>%
    keep(~ sum(.x %in% zrestricted) <= 1)

  
  # Create formula strings with response variable
  #formula_strs <- map_chr(expanded_combos, ~ paste(paste(.x, collapse = " + ")))
  formula_strs <- map_chr(valid_combos, ~ paste(paste(.x, collapse = " + ")))
  
  # Name them
  names(formula_strs) <- map_chr(valid_combos, ~ paste(paste(.x, collapse = "_")))
  names(formula_strs) <- str_replace_all(names(formula_strs), "cohesion.200.25", "cohesion")
  names(formula_strs) <- str_replace_all(names(formula_strs), "mean.dev.60", "dev")
  names(formula_strs) <- str_replace_all(names(formula_strs), "mean.tre.shr.300|mean.tre.shr.120|mean.tre.shr.30", "woody")
  names(formula_strs) <- str_replace_all(names(formula_strs), "num.creek.bin", "creek")
  names(formula_strs) <- str_replace_all(names(formula_strs), "patch.touches.road", "patchrd")
  
  # Generate named list as character vector definitions
  named_list_code <- map2_chr(
    names(formula_strs), formula_strs,
    ~ paste0("  ", .x, ' = "', .y, '"')
  )
  
  # Wrap in list() and collapse
out_mods <- cat("exploration_model_formulas <- list(\n",
      paste(named_list_code, collapse = ",\n"),
      "\n)")
return(out_mods)
}
  
make_candidate_set(core_vars, restricted_set)
  
  
  

  
  # defining structures for exploratory models ----
  # this is the exploratory candidate list generated by the chunk above
  
  
exploration_model_formulas <- list(
  cohesion = "cohesion.200.25",
  patchrd = "patch.touches.road",
  woody = "mean.tre.shr.30",
  dev = "mean.dev.60",
  creek = "num.creek.bin",
  class = "class",
  cohesion_patchrd = "cohesion.200.25 + patch.touches.road",
  cohesion_woody = "cohesion.200.25 + mean.tre.shr.30",
  cohesion_dev = "cohesion.200.25 + mean.dev.60",
  cohesion_creek = "cohesion.200.25 + num.creek.bin",
  cohesion_class = "cohesion.200.25 + class",
  patchrd_woody = "patch.touches.road + mean.tre.shr.30",
  patchrd_dev = "patch.touches.road + mean.dev.60",
  patchrd_creek = "patch.touches.road + num.creek.bin",
  patchrd_class = "patch.touches.road + class",
  woody_dev = "mean.tre.shr.30 + mean.dev.60",
  woody_creek = "mean.tre.shr.30 + num.creek.bin",
  woody_class = "mean.tre.shr.30 + class",
  dev_creek = "mean.dev.60 + num.creek.bin",
  dev_class = "mean.dev.60 + class",
  creek_class = "num.creek.bin + class",
  cohesion_patchrd_woody = "cohesion.200.25 + patch.touches.road + mean.tre.shr.30",
  cohesion_patchrd_dev = "cohesion.200.25 + patch.touches.road + mean.dev.60",
  cohesion_patchrd_creek = "cohesion.200.25 + patch.touches.road + num.creek.bin",
  cohesion_patchrd_class = "cohesion.200.25 + patch.touches.road + class",
  cohesion_woody_dev = "cohesion.200.25 + mean.tre.shr.30 + mean.dev.60",
  cohesion_woody_creek = "cohesion.200.25 + mean.tre.shr.30 + num.creek.bin",
  cohesion_woody_class = "cohesion.200.25 + mean.tre.shr.30 + class",
  cohesion_dev_creek = "cohesion.200.25 + mean.dev.60 + num.creek.bin",
  cohesion_dev_class = "cohesion.200.25 + mean.dev.60 + class",
  cohesion_creek_class = "cohesion.200.25 + num.creek.bin + class",
  patchrd_woody_dev = "patch.touches.road + mean.tre.shr.30 + mean.dev.60",
  patchrd_woody_creek = "patch.touches.road + mean.tre.shr.30 + num.creek.bin",
  patchrd_woody_class = "patch.touches.road + mean.tre.shr.30 + class",
  patchrd_dev_creek = "patch.touches.road + mean.dev.60 + num.creek.bin",
  patchrd_dev_class = "patch.touches.road + mean.dev.60 + class",
  patchrd_creek_class = "patch.touches.road + num.creek.bin + class",
  woody_dev_creek = "mean.tre.shr.30 + mean.dev.60 + num.creek.bin",
  woody_dev_class = "mean.tre.shr.30 + mean.dev.60 + class",
  woody_creek_class = "mean.tre.shr.30 + num.creek.bin + class",
  dev_creek_class = "mean.dev.60 + num.creek.bin + class",
  cohesion_patchrd_woody_dev = "cohesion.200.25 + patch.touches.road + mean.tre.shr.30 + mean.dev.60",
  cohesion_patchrd_woody_creek = "cohesion.200.25 + patch.touches.road + mean.tre.shr.30 + num.creek.bin",
  cohesion_patchrd_woody_class = "cohesion.200.25 + patch.touches.road + mean.tre.shr.30 + class",
  cohesion_patchrd_dev_creek = "cohesion.200.25 + patch.touches.road + mean.dev.60 + num.creek.bin",
  cohesion_patchrd_dev_class = "cohesion.200.25 + patch.touches.road + mean.dev.60 + class",
  cohesion_patchrd_creek_class = "cohesion.200.25 + patch.touches.road + num.creek.bin + class",
  cohesion_woody_dev_creek = "cohesion.200.25 + mean.tre.shr.30 + mean.dev.60 + num.creek.bin",
  cohesion_woody_dev_class = "cohesion.200.25 + mean.tre.shr.30 + mean.dev.60 + class",
  cohesion_woody_creek_class = "cohesion.200.25 + mean.tre.shr.30 + num.creek.bin + class",
  cohesion_dev_creek_class = "cohesion.200.25 + mean.dev.60 + num.creek.bin + class",
  patchrd_woody_dev_creek = "patch.touches.road + mean.tre.shr.30 + mean.dev.60 + num.creek.bin",
  patchrd_woody_dev_class = "patch.touches.road + mean.tre.shr.30 + mean.dev.60 + class",
  patchrd_woody_creek_class = "patch.touches.road + mean.tre.shr.30 + num.creek.bin + class",
  patchrd_dev_creek_class = "patch.touches.road + mean.dev.60 + num.creek.bin + class",
  woody_dev_creek_class = "mean.tre.shr.30 + mean.dev.60 + num.creek.bin + class",
  cohesion_patchrd_woody_dev_creek = "cohesion.200.25 + patch.touches.road + mean.tre.shr.30 + mean.dev.60 + num.creek.bin",
  cohesion_patchrd_woody_dev_class = "cohesion.200.25 + patch.touches.road + mean.tre.shr.30 + mean.dev.60 + class",
  cohesion_patchrd_woody_creek_class = "cohesion.200.25 + patch.touches.road + mean.tre.shr.30 + num.creek.bin + class",
  cohesion_patchrd_dev_creek_class = "cohesion.200.25 + patch.touches.road + mean.dev.60 + num.creek.bin + class",
  cohesion_woody_dev_creek_class = "cohesion.200.25 + mean.tre.shr.30 + mean.dev.60 + num.creek.bin + class",
  patchrd_woody_dev_creek_class = "patch.touches.road + mean.tre.shr.30 + mean.dev.60 + num.creek.bin + class",
  cohesion_patchrd_woody_dev_creek_class = "cohesion.200.25 + patch.touches.road + mean.tre.shr.30 + mean.dev.60 + num.creek.bin + class",
  intercept = "1"
)

#########################
  
  exploration_model_formulas <- list(
    cohesion = "cohesion.200.50",
    patchrd = "patch.touches.road",
    woody = "mean.tre.shr.210",
    dev = "mean.dev.300",
    creek = "num.creek.bin",
    class = "class",
    sex = "sex",
    cohesion_patchrd = "cohesion.200.50 + patch.touches.road",
    cohesion_creek = "cohesion.200.50 + num.creek.bin",
    cohesion_class = "cohesion.200.50 + class",
    cohesion_sex = "cohesion.200.50 + sex",
    patchrd_woody = "patch.touches.road + mean.tre.shr.210",
    patchrd_dev = "patch.touches.road + mean.dev.300",
    patchrd_creek = "patch.touches.road + num.creek.bin",
    patchrd_class = "patch.touches.road + class",
    patchrd_sex = "patch.touches.road + sex",
    woody_creek = "mean.tre.shr.210 + num.creek.bin",
    woody_class = "mean.tre.shr.210 + class",
    woody_sex = "mean.tre.shr.210 + sex",
    dev_creek = "mean.dev.300 + num.creek.bin",
    dev_class = "mean.dev.300 + class",
    dev_sex = "mean.dev.300 + sex",
    creek_class = "num.creek.bin + class",
    creek_sex = "num.creek.bin + sex",
    class_sex = "class + sex",
    cohesion_patchrd_creek = "cohesion.200.50 + patch.touches.road + num.creek.bin",
    cohesion_patchrd_class = "cohesion.200.50 + patch.touches.road + class",
    cohesion_patchrd_sex = "cohesion.200.50 + patch.touches.road + sex",
    cohesion_creek_class = "cohesion.200.50 + num.creek.bin + class",
    cohesion_creek_sex = "cohesion.200.50 + num.creek.bin + sex",
    cohesion_class_sex = "cohesion.200.50 + class + sex",
    patchrd_woody_creek = "patch.touches.road + mean.tre.shr.210 + num.creek.bin",
    patchrd_woody_class = "patch.touches.road + mean.tre.shr.210 + class",
    patchrd_woody_sex = "patch.touches.road + mean.tre.shr.210 + sex",
    patchrd_dev_creek = "patch.touches.road + mean.dev.300 + num.creek.bin",
    patchrd_dev_class = "patch.touches.road + mean.dev.300 + class",
    patchrd_dev_sex = "patch.touches.road + mean.dev.300 + sex",
    patchrd_creek_class = "patch.touches.road + num.creek.bin + class",
    patchrd_creek_sex = "patch.touches.road + num.creek.bin + sex",
    patchrd_class_sex = "patch.touches.road + class + sex",
    woody_creek_class = "mean.tre.shr.210 + num.creek.bin + class",
    woody_creek_sex = "mean.tre.shr.210 + num.creek.bin + sex",
    woody_class_sex = "mean.tre.shr.210 + class + sex",
    dev_creek_class = "mean.dev.300 + num.creek.bin + class",
    dev_creek_sex = "mean.dev.300 + num.creek.bin + sex",
    dev_class_sex = "mean.dev.300 + class + sex",
    creek_class_sex = "num.creek.bin + class + sex",
    cohesion_patchrd_creek_class = "cohesion.200.50 + patch.touches.road + num.creek.bin + class",
    cohesion_patchrd_creek_sex = "cohesion.200.50 + patch.touches.road + num.creek.bin + sex",
    cohesion_patchrd_class_sex = "cohesion.200.50 + patch.touches.road + class + sex",
    cohesion_creek_class_sex = "cohesion.200.50 + num.creek.bin + class + sex",
    patchrd_woody_creek_class = "patch.touches.road + mean.tre.shr.210 + num.creek.bin + class",
    patchrd_woody_creek_sex = "patch.touches.road + mean.tre.shr.210 + num.creek.bin + sex",
    patchrd_woody_class_sex = "patch.touches.road + mean.tre.shr.210 + class + sex",
    patchrd_dev_creek_class = "patch.touches.road + mean.dev.300 + num.creek.bin + class",
    patchrd_dev_creek_sex = "patch.touches.road + mean.dev.300 + num.creek.bin + sex",
    patchrd_dev_class_sex = "patch.touches.road + mean.dev.300 + class + sex",
    patchrd_creek_class_sex = "patch.touches.road + num.creek.bin + class + sex",
    woody_creek_class_sex = "mean.tre.shr.210 + num.creek.bin + class + sex",
    dev_creek_class_sex = "mean.dev.300 + num.creek.bin + class + sex",
    cohesion_patchrd_creek_class_sex = "cohesion.200.50 + patch.touches.road + num.creek.bin + class + sex",
    patchrd_woody_creek_class_sex = "patch.touches.road + mean.tre.shr.210 + num.creek.bin + class + sex",
    patchrd_dev_creek_class_sex = "patch.touches.road + mean.dev.300 + num.creek.bin + class + sex", 
    
    intercept = "1"
  )
  
  
  
  exploration_model_formulas <- list(
    cohesion = "cohesion.200.50",
    patchrd = "patch.touches.road",
    woody = "mean.tre.shr.210",
    dev = "mean.dev.300",
    creek = "num.creek.bin",
    class = "class",
    sex = "sex",
    cohesion_patchrd = "cohesion.200.50 + patch.touches.road",
    cohesion_woody = "cohesion.200.50 + mean.tre.shr.210",
    cohesion_dev = "cohesion.200.50 + mean.dev.300",
    cohesion_creek = "cohesion.200.50 + num.creek.bin",
    cohesion_class = "cohesion.200.50 + class",
    cohesion_sex = "cohesion.200.50 + sex",
    patchrd_woody = "patch.touches.road + mean.tre.shr.210",
    patchrd_dev = "patch.touches.road + mean.dev.300",
    patchrd_creek = "patch.touches.road + num.creek.bin",
    patchrd_class = "patch.touches.road + class",
    patchrd_sex = "patch.touches.road + sex",
    woody_creek = "mean.tre.shr.210 + num.creek.bin",
    woody_class = "mean.tre.shr.210 + class",
    woody_sex = "mean.tre.shr.210 + sex",
    dev_creek = "mean.dev.300 + num.creek.bin",
    dev_class = "mean.dev.300 + class",
    dev_sex = "mean.dev.300 + sex",
    creek_class = "num.creek.bin + class",
    creek_sex = "num.creek.bin + sex",
    class_sex = "class + sex",
    cohesion_patchrd_woody = "cohesion.200.50 + patch.touches.road + mean.tre.shr.210",
    cohesion_patchrd_dev = "cohesion.200.50 + patch.touches.road + mean.dev.300",
    cohesion_patchrd_creek = "cohesion.200.50 + patch.touches.road + num.creek.bin",
    cohesion_patchrd_class = "cohesion.200.50 + patch.touches.road + class",
    cohesion_patchrd_sex = "cohesion.200.50 + patch.touches.road + sex",
    cohesion_woody_creek = "cohesion.200.50 + mean.tre.shr.210 + num.creek.bin",
    cohesion_woody_class = "cohesion.200.50 + mean.tre.shr.210 + class",
    cohesion_woody_sex = "cohesion.200.50 + mean.tre.shr.210 + sex",
    cohesion_dev_creek = "cohesion.200.50 + mean.dev.300 + num.creek.bin",
    cohesion_dev_class = "cohesion.200.50 + mean.dev.300 + class",
    cohesion_dev_sex = "cohesion.200.50 + mean.dev.300 + sex",
    cohesion_creek_class = "cohesion.200.50 + num.creek.bin + class",
    cohesion_creek_sex = "cohesion.200.50 + num.creek.bin + sex",
    cohesion_class_sex = "cohesion.200.50 + class + sex",
    patchrd_woody_creek = "patch.touches.road + mean.tre.shr.210 + num.creek.bin",
    patchrd_woody_class = "patch.touches.road + mean.tre.shr.210 + class",
    patchrd_woody_sex = "patch.touches.road + mean.tre.shr.210 + sex",
    patchrd_dev_creek = "patch.touches.road + mean.dev.300 + num.creek.bin",
    patchrd_dev_class = "patch.touches.road + mean.dev.300 + class",
    patchrd_dev_sex = "patch.touches.road + mean.dev.300 + sex",
    patchrd_creek_class = "patch.touches.road + num.creek.bin + class",
    patchrd_creek_sex = "patch.touches.road + num.creek.bin + sex",
    patchrd_class_sex = "patch.touches.road + class + sex",
    woody_creek_class = "mean.tre.shr.210 + num.creek.bin + class",
    woody_creek_sex = "mean.tre.shr.210 + num.creek.bin + sex",
    woody_class_sex = "mean.tre.shr.210 + class + sex",
    dev_creek_class = "mean.dev.300 + num.creek.bin + class",
    dev_creek_sex = "mean.dev.300 + num.creek.bin + sex",
    dev_class_sex = "mean.dev.300 + class + sex",
    creek_class_sex = "num.creek.bin + class + sex",
    cohesion_patchrd_woody_creek = "cohesion.200.50 + patch.touches.road + mean.tre.shr.210 + num.creek.bin",
    cohesion_patchrd_woody_class = "cohesion.200.50 + patch.touches.road + mean.tre.shr.210 + class",
    cohesion_patchrd_woody_sex = "cohesion.200.50 + patch.touches.road + mean.tre.shr.210 + sex",
    cohesion_patchrd_dev_creek = "cohesion.200.50 + patch.touches.road + mean.dev.300 + num.creek.bin",
    cohesion_patchrd_dev_class = "cohesion.200.50 + patch.touches.road + mean.dev.300 + class",
    cohesion_patchrd_dev_sex = "cohesion.200.50 + patch.touches.road + mean.dev.300 + sex",
    cohesion_patchrd_creek_class = "cohesion.200.50 + patch.touches.road + num.creek.bin + class",
    cohesion_patchrd_creek_sex = "cohesion.200.50 + patch.touches.road + num.creek.bin + sex",
    cohesion_patchrd_class_sex = "cohesion.200.50 + patch.touches.road + class + sex",
    cohesion_woody_creek_class = "cohesion.200.50 + mean.tre.shr.210 + num.creek.bin + class",
    cohesion_woody_creek_sex = "cohesion.200.50 + mean.tre.shr.210 + num.creek.bin + sex",
    cohesion_woody_class_sex = "cohesion.200.50 + mean.tre.shr.210 + class + sex",
    cohesion_dev_creek_class = "cohesion.200.50 + mean.dev.300 + num.creek.bin + class",
    cohesion_dev_creek_sex = "cohesion.200.50 + mean.dev.300 + num.creek.bin + sex",
    cohesion_dev_class_sex = "cohesion.200.50 + mean.dev.300 + class + sex",
    cohesion_creek_class_sex = "cohesion.200.50 + num.creek.bin + class + sex",
    patchrd_woody_creek_class = "patch.touches.road + mean.tre.shr.210 + num.creek.bin + class",
    patchrd_woody_creek_sex = "patch.touches.road + mean.tre.shr.210 + num.creek.bin + sex",
    patchrd_woody_class_sex = "patch.touches.road + mean.tre.shr.210 + class + sex",
    patchrd_dev_creek_class = "patch.touches.road + mean.dev.300 + num.creek.bin + class",
    patchrd_dev_creek_sex = "patch.touches.road + mean.dev.300 + num.creek.bin + sex",
    patchrd_dev_class_sex = "patch.touches.road + mean.dev.300 + class + sex",
    patchrd_creek_class_sex = "patch.touches.road + num.creek.bin + class + sex",
    woody_creek_class_sex = "mean.tre.shr.210 + num.creek.bin + class + sex",
    dev_creek_class_sex = "mean.dev.300 + num.creek.bin + class + sex",
    cohesion_patchrd_woody_creek_class = "cohesion.200.50 + patch.touches.road + mean.tre.shr.210 + num.creek.bin + class",
    cohesion_patchrd_woody_creek_sex = "cohesion.200.50 + patch.touches.road + mean.tre.shr.210 + num.creek.bin + sex",
    cohesion_patchrd_woody_class_sex = "cohesion.200.50 + patch.touches.road + mean.tre.shr.210 + class + sex",
    cohesion_patchrd_dev_creek_class = "cohesion.200.50 + patch.touches.road + mean.dev.300 + num.creek.bin + class",
    cohesion_patchrd_dev_creek_sex = "cohesion.200.50 + patch.touches.road + mean.dev.300 + num.creek.bin + sex",
    cohesion_patchrd_dev_class_sex = "cohesion.200.50 + patch.touches.road + mean.dev.300 + class + sex",
    cohesion_patchrd_creek_class_sex = "cohesion.200.50 + patch.touches.road + num.creek.bin + class + sex",
    cohesion_woody_creek_class_sex = "cohesion.200.50 + mean.tre.shr.210 + num.creek.bin + class + sex",
    cohesion_dev_creek_class_sex = "cohesion.200.50 + mean.dev.300 + num.creek.bin + class + sex",
    patchrd_woody_creek_class_sex = "patch.touches.road + mean.tre.shr.210 + num.creek.bin + class + sex",
    patchrd_dev_creek_class_sex = "patch.touches.road + mean.dev.300 + num.creek.bin + class + sex",
    cohesion_patchrd_woody_creek_class_sex = "cohesion.200.50 + patch.touches.road + mean.tre.shr.210 + num.creek.bin + class + sex",
    cohesion_patchrd_dev_creek_class_sex = "cohesion.200.50 + patch.touches.road + mean.dev.300 + num.creek.bin + class + sex", 
    
    intercept = "1"
  )
  