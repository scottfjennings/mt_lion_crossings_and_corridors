




library(tidyverse)
library(here)
library(sf)
library(lme4)
library(AICcmodavg)

options(scipen = 999)
source(here("code/helper_data.R"))

# read the road segment habitat values masked to each puma's home range
# this has a row for each road segment in each animal's home range, for each year the puma was tracked and each buffer distance (30-300, by 30)
# so this df has many more rows than the others



all_hr_road_habitat_df <- readRDS(here("data/all_hr_road_impervious")) %>% 
  data.frame() %>% 
  select(seg.label, animal.id, buff, "mean.imperv" = mean.percent.impervious) %>% 
  full_join(readRDS(here("data/analysis_inputs/all_hr_road_habitat_df"))) %>% 
  filter(!animal.id %in% hr_exclude_pumas,
         !seg.label %in% p31_exclude_segments) %>% 
  mutate(mean.imperv = mean.imperv/100)



# testing landscapemetrics scales ----
# here testing buffer distances and the scales at which cells are classified as forest or not. landscapemetrics extracted from A15_segment_landscapemetrics.R


# checking prediction of crossed vs not crossed ----

# trying lmm as for bridges_streams

#' fit_landscapemetrics_scale_mixed_mods_crossed_not
#'
#' @param zhab either mean.dev or mean.tre.shr, the mean development or tree+shrub cover within each buffer distance around each segment
#'
#' @returns list with an element for each model and one for the AIC table comparing all models
#'
#' @examples
fit_landscapemetrics_scale_mixed_mods_crossed_not <- function(zhab) {
  # Dynamically set the response variable in the formula
  zmods <- list(
    "mod30" = glmer(as.formula(paste("crossed.bin ~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 30), family = "binomial"),
    "mod60" = glmer(as.formula(paste("crossed.bin ~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 60), family = "binomial"),
    "mod90" = glmer(as.formula(paste("crossed.bin ~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 90), family = "binomial"),
    "mod120" = glmer(as.formula(paste("crossed.bin ~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 120), family = "binomial"),
    "mod150" = glmer(as.formula(paste("crossed.bin ~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 150), family = "binomial"),
    "mod180" = glmer(as.formula(paste("crossed.bin ~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 180), family = "binomial"),
    "mod210" = glmer(as.formula(paste("crossed.bin ~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 210), family = "binomial"),
    "mod240" = glmer(as.formula(paste("crossed.bin ~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 240), family = "binomial"),
    "mod270" = glmer(as.formula(paste("crossed.bin ~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 270), family = "binomial"),
    "mod300" = glmer(as.formula(paste("crossed.bin ~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 300), family = "binomial")
  )
  
  
  
  names(zmods) <-  paste(zhab, seq(30, 300, length.out = 10), sep = "")
  
  
  zmods$aic <- aictab(zmods, names(zmods)) %>% 
    data.frame() %>% 
    mutate(across(c(AICc, Delta_AICc, ModelLik, AICcWt), ~round(., 3)))
  
  return(zmods)
  
}


dev_scale_mods_crossed_not <- fit_scale_mixed_mods_crossed_not("mean.dev")
dev_scale_mods_crossed_not$aic
summary(dev_scale_mods_crossed_not$mean.dev90)


treshr_scale_mods_crossed_not <- fit_scale_mixed_mods_crossed_not("mean.tre.shr")
treshr_scale_mods_crossed_not$aic
summary(treshr_scale_mods_crossed_not$mean.tre.shr300)

