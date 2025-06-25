




library(tidyverse)
library(here)
library(sf)
library(lme4)
library(AICcmodavg)

options(scipen = 999)
source(here("code/helper_data.R"))

# testing the relative importance of "cohesion" of woody plant cover calculated at different spatial scales and different thresholds for classifying a 30m pixel as woody:
# see A15_segment_landscapemetrics.R for more details on cohesion calculation


# read df with the proportion of each segment in continuous areas of moderate or high development, from B1_clip_roads_by_impervious.R
hr_segments_prop_in_developed <- readRDS(here("data/hr_segments_prop_in_developed")) %>% 
  data.frame() %>% 
  select(-geometry, -seg.length)   %>% 
  filter(animal.id %in% analysis_pumas,
         !animal.id %in% few_crossings_pumas,
         !animal.id %in% hr_exclude_pumas,
         !seg.label %in% p31_exclude_segments)


# read segment crossing values from A11_sum_segment_crossings.R
# this has just the lion X months that there is real data for
# this is crossings summed for each segmentXmonthXlion combination, only considering naive crossed roads
# the 0s ending to the file name indicates this has the uncrossed segments added back in
#summed_crossings <- readRDS(here("data/analysis_inputs/monthly_seg_crossings_naive_roads_only_0s")) %>% 
#  select(animal.id, year, month, seg.label, which.steps, monthly.seg.wt.crossing, monthly.seg.raw.crossing)

summed_crossings <- readRDS(here("data/analysis_inputs/annual_seg_crossings_naive_roads_only_0s_lions_combined")) %>%  
  select(year, seg.label, which.steps, seg.wt = annual.seg.wt.crossing, raw.crossing = annual.seg.raw.crossing, num.lion.months, num.lions)


# adding hr_segments_prop_in_developed shouldn't change the number of rows since it is derived from segments_in_homerange
configuration_scale_df_pre <- summed_crossings %>% 
  left_join(hr_segments_prop_in_developed %>% distinct(seg.label, prop.seg.in.dev20, prop.seg.in.dev50))


# read the df with cohesion values from A15_segment_landscapemetrics.R
# the _full end to the file name indicates thsi has the segments with no woody habitat added back in. so far those have cohesion values = NA, but these will need to be changed to something, I guess 0
all_hr_road_patch_cohesion_treshr_full <- readRDS(here("data/all_hr_road_patch_cohesion_treshr_full")) %>% 
  select(-class, -layer) %>% 
  mutate(cohesion = replace_na(cohesion, 0),
         np = replace_na(np, 0),
         patch.touches.road = replace_na(patch.touches.road, FALSE),
         woody.prop = replace_na(woody.prop, 0))

all_hr_road_patch_cohesion_treshr_full <- all_hr_road_patch_cohesion_treshr_full %>% 
  distinct(seg.label, year, buff, forest.threshold, cohesion, np, patch.touches.road, woody.prop)


# all_hr_road_patch_cohesion_treshr_full has a record for each lion - year - segment - buffer - threshold
# this chunk expands that out to each month there is data for each lion, and excludes segments in continuous developed areas

configuration_scale_df <- configuration_scale_df_pre %>% 
  left_join(all_hr_road_patch_cohesion_treshr_full)


# remove segments that are in continuous developed areas
configuration_scale_df <- configuration_scale_df %>% 
  filter(prop.seg.in.dev50 == 0) %>% 
  mutate(bin.crossing = ifelse(raw.crossing == 0, raw.crossing, 1)) %>% 
  mutate(scale.group = paste(buff, forest.threshold, sep = "_"))


# check that we have the right number of rows
# counting how many months for each animal.id, seg.label, year, in the second line need to divide by # of buff-threshold combos
all(count(analysis_lion_year_month_seg, animal.id, seg.label, year) %>% arrange(animal.id, seg.label, year) == 
    count(all_hr_road_patch_cohesion_treshr_full_months, animal.id, seg.label, year) %>% mutate(n = n/9) %>% arrange(animal.id, seg.label, year))

nrow(analysis_lion_year_month_seg) == nrow(all_hr_road_patch_cohesion_treshr_full_months)/9




all_hr_road_patch_cohesion_treshr_full_months_crossings <- monthly_seg_crossings_naive_roads_only_0s %>% 
  right_join(all_hr_road_patch_cohesion_treshr_full_months) # right_join because all_hr_road_patch_cohesion_treshr_full_months excludes segments in continuous developed area

nrow(all_hr_road_patch_cohesion_treshr_full_months_crossings) == nrow(all_hr_road_patch_cohesion_treshr_full_months)

# there are some segments with high cohesion values but where the woody cover still doesn't overlap the road
# this duplicates all_hr_road_patch_cohesion_treshr_full_months, but sets cohesion = 0 for all segments where woody cover doesn't overlap the road
# then below I can test which one better predicts crossings as part of the scale selection.
configuration_scale_df <- bind_rows(all_hr_road_patch_cohesion_treshr_full_months_crossings %>% 
                                                                          mutate(which.cohesion = "all"), 
                                                                        all_hr_road_patch_cohesion_treshr_full_months_crossings %>% 
                                                                          mutate(cohesion = ifelse(patch.touches.road == TRUE, cohesion, 0),
                                                                                 which.cohesion = "touchrd"))%>% 
  mutate(scale.group = paste(buff, forest.threshold, which.cohesion, sep = "_"))


configuration_scale_df <- all_hr_road_patch_cohesion_treshr_full_months_crossings %>% 
  mutate(scale.group = paste(buff, forest.threshold, sep = "_"))

saveRDS(configuration_scale_df, here("data/analysis_inputs/configuration_scale_df_lions_combined"))


configuration_scale_df <- readRDS(here("data/analysis_inputs/configuration_scale_df"))

##########################
# fitting modles with no random effects
#' fit_landscapemetrics_scale_mixed_mods_offset
#'
#' @param zhab either mean.dev or mean.tre.shr, the mean development or tree+shrub cover within each buffer distance around each segment
#'
#' @returns list with an element for each model and one for the AIC table comparing all models
#'
#' @examples
fit_landscapemetrics_scale_mods_offset_logreg <- function(zhab) {
  # Dynamically set the predictor variable in the formula
  
  ####################################
  scales <- distinct(configuration_scale_df, scale.group) %>% pull(scale.group)
  
  zmods <- lapply(scales, function(s) {
    df_scale <- dplyr::filter(configuration_scale_df, scale.group == s)
    glm(
      formula = as.formula(paste0(
        "bin.crossing ~ ", zhab,
        " + offset(log(seg.wt + 0.0001))"
      )),
      data = df_scale,
      family = binomial
    )
  })
  
  names(zmods) <- paste(zhab, scales, sep = "")
  
  zmods$aic <- aictab(zmods, names(zmods)) %>%
    data.frame() %>%
    dplyr::mutate(across(c(AICc, Delta_AICc, ModelLik, AICcWt), \(x) round(x, 3)))
  
  return(zmods)
}


cohesion_scale_mods_offset <- fit_landscapemetrics_scale_mods_offset_logreg("cohesion")
cohesion_scale_mods_offset$aic

#Modnames K    AICc Delta_AICc ModelLik AICcWt        LL    Cum.Wt
#1 cohesion100_25 2 116.363      0.000    1.000  0.157 -56.18064 0.1569199
#7 cohesion300_25 2 116.567      0.203    0.903  0.142 -56.28227 0.2986760
#4 cohesion200_25 2 116.598      0.235    0.889  0.140 -56.29803 0.4382162
#8 cohesion300_50 2 116.974      0.610    0.737  0.116 -56.48581 0.5538663
#5 cohesion200_50 2 117.090      0.727    0.695  0.109 -56.54393 0.6629862
#2 cohesion100_50 2 117.362      0.999    0.607  0.095 -56.68014 0.7582109
#9 cohesion300_75 2 117.671      1.308    0.520  0.082 -56.83457 0.8398092
#6 cohesion200_75 2 117.685      1.322    0.516  0.081 -56.84161 0.9208352
#3 cohesion100_75 2 117.732      1.368    0.504  0.079 -56.86485 1.0000000
 

# all scales have dAICc < 2

# going to use 300_25 because it is the largest area (matching composition) and the lowest threshold


#' fit_landscapemetrics_scale_mixed_mods_offset
#'
#' @param zhab either mean.dev or mean.tre.shr, the mean development or tree+shrub cover within each buffer distance around each segment
#'
#' @returns list with an element for each model and one for the AIC table comparing all models
#'
#' @examples
fit_landscapemetrics_scale_mods_offset_negbin <- function(zhab) {
  # Dynamically set the predictor variable in the formula
  
  ####################################
  scales <- distinct(configuration_scale_df, scale.group) %>% pull(scale.group)
  
  zmods <- lapply(scales, function(s) {
    df_scale <- dplyr::filter(configuration_scale_df, scale.group == s)
    MASS::glm.nb(
      formula = as.formula(paste0(
        "raw.crossing ~ ", zhab,
        " + offset(log(seg.wt + 0.0001))"
      )),
      data = df_scale
    )
  })
  
  names(zmods) <- paste(zhab, scales, sep = "")
  
  zmods$aic <- aictab(zmods, names(zmods)) %>%
    data.frame() %>%
    dplyr::mutate(across(c(AICc, Delta_AICc, ModelLik, AICcWt), \(x) round(x, 3)))
  
  return(zmods)
}


cohesion_scale_mods_offset_nb <- fit_landscapemetrics_scale_mods_offset_negbin("cohesion")
cohesion_scale_mods_offset_nb$aic

#Modnames K     AICc Delta_AICc ModelLik AICcWt        LL    Cum.Wt
#4 cohesion200_25 3 6614.752      0.000    1.000  0.145 -3304.374 0.1451325
#6 cohesion200_75 3 6615.054      0.302    0.860  0.125 -3304.525 0.2699053
#5 cohesion200_50 3 6615.081      0.329    0.848  0.123 -3304.538 0.3930077
#7 cohesion300_25 3 6615.101      0.349    0.840  0.122 -3304.549 0.5148729
#3 cohesion100_75 3 6615.442      0.691    0.708  0.103 -3304.719 0.6176291
#2 cohesion100_50 3 6615.505      0.753    0.686  0.100 -3304.750 0.7172294
#1 cohesion100_25 3 6615.565      0.813    0.666  0.097 -3304.780 0.8138836
#8 cohesion300_50 3 6615.602      0.851    0.654  0.095 -3304.799 0.9087406
#9 cohesion300_75 3 6615.680      0.928    0.629  0.091 -3304.838 1.0000000


# all scales have dAICc < 2
 


###########################
# models with random effect for animal.id


#' fit_landscapemetrics_scale_mixed_mods_offset
#'
#' @param zhab either mean.dev or mean.tre.shr, the mean development or tree+shrub cover within each buffer distance around each segment
#'
#' @returns list with an element for each model and one for the AIC table comparing all models
#'
#' @examples
fit_landscapemetrics_scale_mixed_mods_offset <- function(zhab) {
  # Dynamically set the predictor variable in the formula

  ####################################
  scales <- distinct(configuration_scale_df, scale.group) %>% pull(scale.group)
  
  zmods <- lapply(scales, function(s) {
    df_scale <- dplyr::filter(configuration_scale_df, scale.group == s)
    glmer(
      formula = as.formula(paste0(
        "monthly.seg.raw.crossing ~ ", zhab,
        " + offset(log(monthly.seg.wt.crossing + 0.0001)) + (1|animal.id)"
      )),
      data = df_scale,
      family = poisson
    )
  })
  
  names(zmods) <- paste(zhab, scales, sep = "")
  
  zmods$aic <- aictab(zmods, names(zmods)) %>%
    data.frame() %>%
    dplyr::mutate(across(c(AICc, Delta_AICc, ModelLik, AICcWt), \(x) round(x, 3)))
  
  return(zmods)
}


cohesion_scale_mods_offset <- fit_landscapemetrics_scale_mixed_mods_offset("cohesion")
cohesion_scale_mods_offset$aic

#Modnames K     AICc Delta_AICc ModelLik AICcWt        LL    Cum.Wt
#5 cohesion200_50 3 16415.99      0.000    1.000  0.169 -8204.992 0.1690299
#4 cohesion200_25 3 16416.00      0.019    0.991  0.167 -8205.002 0.3364609
#7 cohesion300_25 3 16416.17      0.186    0.911  0.154 -8205.086 0.4904582
#6 cohesion200_75 3 16416.34      0.351    0.839  0.142 -8205.168 0.6322578
#2 cohesion100_50 3 16416.97      0.984    0.611  0.103 -8205.484 0.7356125
#3 cohesion100_75 3 16417.39      1.403    0.496  0.084 -8205.694 0.8194449
#8 cohesion300_50 3 16417.47      1.481    0.477  0.081 -8205.733 0.9000710
#9 cohesion300_75 3 16418.25      2.263    0.322  0.055 -8206.124 0.9545824
#1 cohesion100_25 3 16418.61      2.628    0.269  0.045 -8206.307 1.0000000


summary(cohesion_scale_mods_offset$cohesion200_25)


# 200m buffer with 50% cover threshold for being classified as woody is the best supported, but only barely.
# all except 300, 75% and 100, 25% have dAICc < 2