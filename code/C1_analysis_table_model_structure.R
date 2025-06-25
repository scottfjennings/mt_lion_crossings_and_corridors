

# this is the first step for the main analysis
# this must be run before any other CX_... scripts

# here the final analysis table is built up by combining all predictors and response, final filtering



library(tidyverse)
library(here)
library(sf)
library(readxl)


source(here("code/helper_data.R"))


# lion sex ----
puma_sexes <- read_xlsx("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/data/COLLAR DATE INFO_31Dec2023.xlsx")

names(puma_sexes) <- tolower(names(puma_sexes))

puma_sexes <- puma_sexes %>%
  rename("animal.id" = `animal id`) %>% 
  distinct(animal.id, sex)   %>% 
  filter(animal.id %in% analysis_pumas,
         !animal.id %in% few_crossings_pumas,
         !animal.id %in% hr_exclude_pumas)


# full lion year month segment for just segmenst outside continuous development. this used to double check all the right segments move forward to the main analysis ----
analysis_lion_year_month_seg <- readRDS(here("data/full_lion_year_month_seg")) %>% 
  full_join(readRDS(here("data/hr_segments_prop_in_developed"))) %>% 
  data.frame() %>%  
  filter(prop.seg.in.dev50 == 0) %>% 
  select(animal.id, seg.label, year, month) %>% 
  filter(animal.id %in% analysis_pumas,
         !animal.id %in% few_crossings_pumas,
         !animal.id %in% hr_exclude_pumas,
         !seg.label %in% p31_exclude_segments)
  

# full lion year segment for just segments outside continuous development. this used to double check all the right segments move forward to the main analysis ----
analysis_lion_year_seg <- readRDS(here("data/full_lion_year_month_seg")) %>% 
  group_by(animal.id, year, seg.label) %>% 
  summarise(num.months = n()) %>% 
  full_join(readRDS(here("data/hr_segments_prop_in_developed"))) %>% 
  data.frame() %>%  
  filter(prop.seg.in.dev50 == 0) %>% 
  select(animal.id, seg.label, year, num.months) %>% 
  filter(animal.id %in% analysis_pumas,
         !animal.id %in% few_crossings_pumas,
         !animal.id %in% hr_exclude_pumas,
         !seg.label %in% p31_exclude_segments)


###
# read df with the proportion of each segment in continuous areas of moderate or high development, from B1_clip_roads_by_impervious.R
hr_segments_prop_in_developed <- readRDS(here("data/hr_segments_prop_in_developed")) %>% 
  data.frame() %>% 
  select(-geometry, -seg.length)   %>% 
  filter(animal.id %in% analysis_pumas,
         !animal.id %in% few_crossings_pumas,
         !animal.id %in% hr_exclude_pumas,
         !seg.label %in% p31_exclude_segments)


summed_crossings <- readRDS(here("data/analysis_inputs/annual_seg_crossings_naive_roads_only_0s_lions_combined")) %>%  
  select(year, seg.label, which.steps, seg.wt = annual.seg.wt.crossing, raw.crossing = annual.seg.raw.crossing, num.lion.months, num.lions)


# adding hr_segments_prop_in_developed shouldn't change the number of rows since it is derived from segments_in_homerange
main_analysis_df_pre <- summed_crossings %>% 
  left_join(hr_segments_prop_in_developed %>% distinct(seg.label, prop.seg.in.dev20, prop.seg.in.dev50))



# habitat composition. veg and development ----
# using 30m and 300m buffer distances 
# by month
#monthly_analysis_hab_composition <- readRDS(here("data/analysis_inputs/composition_scale_df")) %>% 
#  pivot_longer(cols = contains("mean."), names_to = "varb") %>% 
#  mutate(varb.buff = paste(varb, buff, sep = ".")) %>% 
#  filter(varb.buff %in% c("mean.tre.shr.210", "mean.dev.300")) %>% 
#  pivot_wider(id_cols = c(animal.id, seg.label, year, month, monthly.seg.raw.crossing, monthly.seg.wt.crossing), names_from = varb.buff, values_from = value) %>% 
#  right_join(analysis_lion_year_month_seg)
# by year
#annual_analysis_hab_composition <- readRDS(here("data/analysis_inputs/composition_scale_df")) %>% 
#  pivot_longer(cols = contains("mean."), names_to = "varb") %>% 
#  mutate(varb.buff = paste(varb, buff, sep = ".")) %>% 
#  filter(varb.buff %in% c("mean.tre.shr.210", "mean.dev.300")) %>% 
#  pivot_wider(id_cols = c(animal.id, seg.label, year, month, monthly.seg.raw.crossing, monthly.seg.wt.crossing), names_from = varb.buff, values_from = value) %>%
#  group_by(animal.id, year, seg.label, mean.tre.shr.210, mean.dev.300) %>% 
#  summarise(annual.seg.raw.crossing = sum(monthly.seg.raw.crossing),
#            annual.seg.wt.crossing = sum(monthly.seg.wt.crossing)) %>% 
#  ungroup() %>% 
#  right_join(analysis_lion_year_seg)



analysis_hab_composition <- readRDS(here("data/analysis_inputs/composition_scale_df_lions_combined")) %>% 
  pivot_longer(cols = contains("mean."), names_to = "varb") %>% 
  mutate(varb.buff = paste(varb, buff, sep = ".")) %>% 
  filter(varb.buff %in% c("mean.tre.shr.300", "mean.dev.60")) %>% 
  pivot_wider(id_cols = c(seg.label, year, raw.crossing, seg.wt), names_from = varb.buff, values_from = value) %>% 
  right_join(main_analysis_df_pre) %>% 
  filter(prop.seg.in.dev50 == 0)

# do any segments have missing composition values?
monthly_analysis_hab_composition %>% summarise(across(starts_with("mean."), ~ any(is.na(.)))) # should all be FALSE
annual_analysis_hab_composition %>% summarise(across(starts_with("mean."), ~ any(is.na(.)))) # should all be FALSE
analysis_hab_composition %>% summarise(across(starts_with("mean."), ~ any(is.na(.)))) # should all be FALSE as long as prop.seg.in.dev50 filter is used above


# habitat configuration. cohesion ----
# by month
#monthly_analysis_hab_configuration <- readRDS(here("data/analysis_inputs/configuration_scale_df")) %>% 
#  filter(buff == 200, forest.threshold == 50) %>% 
#  right_join(analysis_lion_year_month_seg) %>% 
#  rename(cohesion.200.50 = cohesion) %>% # renaming to include buff ad threshold as a reminder
#  select(animal.id, year, month, seg.label, cohesion.200.50, monthly.seg.raw.crossing, monthly.seg.wt.crossing, patch.touches.road, np)
# by year
#annual_analysis_hab_configuration <- readRDS(here("data/analysis_inputs/configuration_scale_df")) %>% 
#  filter(buff == 200, forest.threshold == 50) %>% 
#  right_join(analysis_lion_year_seg) %>% 
#  rename(cohesion.200.50 = cohesion) %>% # renaming to include buff ad threshold as a reminder
#  select(animal.id, year, month, seg.label, cohesion.200.50, monthly.seg.raw.crossing, monthly.seg.wt.crossing, patch.touches.road, np) %>% 
#  group_by(animal.id, year, seg.label, cohesion.200.50, patch.touches.road, np) %>% 
#  summarise(annual.seg.raw.crossing = sum(monthly.seg.raw.crossing),
#            annual.seg.wt.crossing = sum(monthly.seg.wt.crossing)) %>% 
#  ungroup() 

analysis_hab_configuration <- readRDS(here("data/analysis_inputs/configuration_scale_df_lions_combined")) %>% 
  filter(buff == 100, forest.threshold == 25) %>% 
  right_join(main_analysis_df_pre) %>% 
  filter(prop.seg.in.dev50 == 0) %>% 
  rename(cohesion.100.25 = cohesion) %>% # renaming to include buff ad threshold as a reminder
  select(year, seg.label, cohesion.100.25, raw.crossing, seg.wt, patch.touches.road, np) 


# do any segments have missing cohesion values?
#monthly_analysis_hab_configuration %>% filter(is.na(cohesion.200.50)) %>% nrow()
#annual_analysis_hab_configuration %>% filter(is.na(cohesion.200.50)) %>% nrow()
analysis_hab_configuration %>% filter(is.na(cohesion.100.25)) %>% nrow()


#monthly_analysis_table <- full_join(monthly_analysis_hab_configuration, monthly_analysis_hab_composition)%>% 
#  left_join(readRDS(here("data/analysis_inputs/streams_per_segment"))) %>% 
#  left_join(puma_sexes) %>% 
#  left_join(readRDS(here("data/seg_midpoints_road_class")))

#annual_analysis_table <- full_join(annual_analysis_hab_configuration, annual_analysis_hab_composition)%>% 
#  left_join(readRDS(here("data/analysis_inputs/streams_per_segment"))) %>% 
#  left_join(puma_sexes) %>% 
#  left_join(readRDS(here("data/seg_midpoints_road_class")))

analysis_table <- full_join(analysis_hab_configuration, analysis_hab_composition) %>% 
  left_join(readRDS(here("data/analysis_inputs/streams_per_segment"))) %>% 
  left_join(readRDS(here("data/seg_midpoints_road_class"))) %>% 
  mutate(bin.crossing = ifelse(raw.crossing == 0, raw.crossing, 1))



monthly_analysis_table %>% summarise(across(c(starts_with("mean."), cohesion.200.50, num.creek, sex, class), ~ any(is.na(.)))) # all should be FALSE
annual_analysis_table %>% summarise(across(c(starts_with("mean."), cohesion.200.50, num.creek, sex, class), ~ any(is.na(.)))) # all should be FALSE


monthly_analysis_table %>% distinct(seg.label, class) %>% count(class)
monthly_analysis_table %>% distinct(animal.id, seg.label, num.creek) %>% count(animal.id, num.creek) %>% filter(num.creek > 5)

main_analysis_table %>%
  filter(num.creek.bin == 3, monthly.seg.raw.crossing > 0) %>%
  ggplot(aes(x = monthly.seg.raw.crossing, fill = factor(num.creek))) +
  geom_histogram(binwidth = 1, boundary = 0, color = "black", position = "stack") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(x = "Monthly Segment Crossings", y = "Count", fill = "Number of Creeks")



monthly_analysis_table <- monthly_analysis_table %>% 
  mutate(class = ifelse(class == "Access Road", "Local", class)) %>%  # only 1 segment and only in P31's HR
  mutate(across(starts_with("mean."), ~ .x * 100)) %>% 
  mutate(class = relevel(factor(class), ref = "Local")) # using Local as the reference level because it is most common, is possibly more likely to be crossed, adn becasue it will be handy to compare the likely busier roads to Local


saveRDS(monthly_analysis_table, here("data/analysis_inputs/monthly_analysis_table"))

# hoping that agregating crossings to annual level may help zero inflation issues

annual_analysis_table <- annual_analysis_table %>% 
  mutate(class = ifelse(class == "Access Road", "Local", class)) %>%  # only 1 segment and only in P31's HR
  mutate(across(starts_with("mean."), ~ .x * 100)) %>% 
  mutate(class = relevel(factor(class), ref = "Local")) # using Local as the reference level because it is most common, is possibly more likely to be crossed, adn becasue it will be handy to compare the likely busier roads to Local


saveRDS(annual_analysis_table, here("data/analysis_inputs/annual_analysis_table"))


combined_lions_analysis_table <- analysis_table %>% 
  mutate(class = ifelse(class == "Access Road", "Local", class)) %>%  # only 1 segment and only in P31's HR
  mutate(across(starts_with("mean."), ~ .x * 100)) %>% 
  mutate(class = relevel(factor(class), ref = "Local")) # using Local as the reference level because it is most common, is possibly more likely to be crossed, adn becasue it will be handy to compare the likely busier roads to Local


saveRDS(combined_lions_analysis_table, here("data/analysis_inputs/combined_lions_analysis_table"))



# optional data checking plots and summaries ----



main_analysis_table %>%
  pivot_longer(cols = c(mean.tre.shr.210, mean.dev.300, cohesion.200.50)) %>% 
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
  core_vars <- c("cohesion.200.50", "patch.touches.road", "mean.tre.shr.210", "mean.dev.300", 
                 "num.creek.bin", "class", "sex")
  
  restricted_set <- c("mean.tre.shr.210", "mean.dev.300", "cohesion.200.50")
  
  # All combinations of 1–6 predictors
  model_combos <- unlist(lapply(1:length(core_vars), function(k) {
    combn(core_vars, k, simplify = FALSE)
  }), recursive = FALSE)
  
  # Filter to enforce only zero or one restricted variable per model
  valid_combos <- model_combos %>%
    keep(~ sum(.x %in% restricted_set) <= 1)
  
  # Add patch.touches.road only if cohesion.200.50 is present
  expanded_combos <- map(valid_combos, function(vars) {
    if ("cohesion.200.50" %in% vars) {
      list(vars, c(vars, "patch.touches.road"))
    } else {
      list(vars)
    }
  }) %>% flatten()
  
  # Create formula strings with response variable
  formula_strs <- map_chr(expanded_combos, ~ paste(paste(.x, collapse = " + ")))
  formula_strs <- map_chr(valid_combos, ~ paste(paste(.x, collapse = " + ")))
  
  # Name them
  names(formula_strs) <- map_chr(valid_combos, ~ paste(paste(.x, collapse = "_")))
  names(formula_strs) <- str_replace_all(names(formula_strs), "cohesion.200.50", "cohesion")
  names(formula_strs) <- str_replace_all(names(formula_strs), "mean.dev.300", "dev")
  names(formula_strs) <- str_replace_all(names(formula_strs), "mean.tre.shr.210", "woody")
  names(formula_strs) <- str_replace_all(names(formula_strs), "num.creek.bin", "creek")
  names(formula_strs) <- str_replace_all(names(formula_strs), "patch.touches.road", "patchrd")
  
  bind_cols(names(formula_strs), formula_strs) %>% print(n = Inf)
  
  # Generate named list as character vector definitions
  named_list_code <- map2_chr(
    names(formula_strs), formula_strs,
    ~ paste0("  ", .x, ' = "', .y, '"')
  )
  
  # Wrap in list() and collapse
  cat("exploration_model_formulas <- list(\n",
      paste(named_list_code, collapse = ",\n"),
      "\n)")
  
  
  
  
  
  
  

  
  # defining structures for exploratory models ----
  # this is the exploratory candidate list generated by the chunk above
  
  exploration_model_formulas <- list(
    cohesion = "cohesion.200.50",
   # patchrd = "patch.touches.road",
    woody = "mean.tre.shr.210",
    dev = "mean.dev.300",
    creek = "num.creek.bin",
    class = "class",
    sex = "sex",
    #cohesion_patchrd = "cohesion.200.50 + patch.touches.road",
    cohesion_creek = "cohesion.200.50 + num.creek.bin",
    cohesion_class = "cohesion.200.50 + class",
    cohesion_sex = "cohesion.200.50 + sex",
    #patchrd_woody = "patch.touches.road + mean.tre.shr.210",
    #patchrd_dev = "patch.touches.road + mean.dev.300",
    #patchrd_creek = "patch.touches.road + num.creek.bin",
    #patchrd_class = "patch.touches.road + class",
    #patchrd_sex = "patch.touches.road + sex",
    woody_creek = "mean.tre.shr.210 + num.creek.bin",
    woody_class = "mean.tre.shr.210 + class",
    woody_sex = "mean.tre.shr.210 + sex",
    dev_creek = "mean.dev.300 + num.creek.bin",
    dev_class = "mean.dev.300 + class",
    dev_sex = "mean.dev.300 + sex",
    creek_class = "num.creek.bin + class",
    creek_sex = "num.creek.bin + sex",
    class_sex = "class + sex",
    #cohesion_patchrd_creek = "cohesion.200.50 + patch.touches.road + num.creek.bin",
    #cohesion_patchrd_class = "cohesion.200.50 + patch.touches.road + class",
    #cohesion_patchrd_sex = "cohesion.200.50 + patch.touches.road + sex",
    cohesion_creek_class = "cohesion.200.50 + num.creek.bin + class",
    cohesion_creek_sex = "cohesion.200.50 + num.creek.bin + sex",
    cohesion_class_sex = "cohesion.200.50 + class + sex",
    #patchrd_woody_creek = "patch.touches.road + mean.tre.shr.210 + num.creek.bin",
    #patchrd_woody_class = "patch.touches.road + mean.tre.shr.210 + class",
    #patchrd_woody_sex = "patch.touches.road + mean.tre.shr.210 + sex",
    #patchrd_dev_creek = "patch.touches.road + mean.dev.300 + num.creek.bin",
    #patchrd_dev_class = "patch.touches.road + mean.dev.300 + class",
    #patchrd_dev_sex = "patch.touches.road + mean.dev.300 + sex",
    #patchrd_creek_class = "patch.touches.road + num.creek.bin + class",
    #patchrd_creek_sex = "patch.touches.road + num.creek.bin + sex",
    #patchrd_class_sex = "patch.touches.road + class + sex",
    woody_creek_class = "mean.tre.shr.210 + num.creek.bin + class",
    woody_creek_sex = "mean.tre.shr.210 + num.creek.bin + sex",
    woody_class_sex = "mean.tre.shr.210 + class + sex",
    dev_creek_class = "mean.dev.300 + num.creek.bin + class",
    dev_creek_sex = "mean.dev.300 + num.creek.bin + sex",
    dev_class_sex = "mean.dev.300 + class + sex",
    creek_class_sex = "num.creek.bin + class + sex",
    #cohesion_patchrd_creek_class = "cohesion.200.50 + patch.touches.road + num.creek.bin + class",
    #cohesion_patchrd_creek_sex = "cohesion.200.50 + patch.touches.road + num.creek.bin + sex",
    #cohesion_patchrd_class_sex = "cohesion.200.50 + patch.touches.road + class + sex",
    cohesion_creek_class_sex = "cohesion.200.50 + num.creek.bin + class + sex",
    #patchrd_woody_creek_class = "patch.touches.road + mean.tre.shr.210 + num.creek.bin + class",
    #patchrd_woody_creek_sex = "patch.touches.road + mean.tre.shr.210 + num.creek.bin + sex",
    #patchrd_woody_class_sex = "patch.touches.road + mean.tre.shr.210 + class + sex",
    #patchrd_dev_creek_class = "patch.touches.road + mean.dev.300 + num.creek.bin + class",
    #patchrd_dev_creek_sex = "patch.touches.road + mean.dev.300 + num.creek.bin + sex",
    #patchrd_dev_class_sex = "patch.touches.road + mean.dev.300 + class + sex",
    #patchrd_creek_class_sex = "patch.touches.road + num.creek.bin + class + sex",
    woody_creek_class_sex = "mean.tre.shr.210 + num.creek.bin + class + sex",
    dev_creek_class_sex = "mean.dev.300 + num.creek.bin + class + sex",
    cohesion_patchrd_creek_class_sex = "cohesion.200.50 + patch.touches.road + num.creek.bin + class + sex",
    #patchrd_woody_creek_class_sex = "patch.touches.road + mean.tre.shr.210 + num.creek.bin + class + sex",
    #patchrd_dev_creek_class_sex = "patch.touches.road + mean.dev.300 + num.creek.bin + class + sex", 
    
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
  