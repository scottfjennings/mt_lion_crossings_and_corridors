




library(tidyverse)
library(here)
library(sf)
library(lme4)
library(AICcmodavg)

options(scipen = 999)
source(here("code/helper_data.R"))

# testing the relative importance of "cohesion" of woody plant cover calculated at different spatial scales and different thresholds for classifying a 30m pixel as woody:
# see A15_segment_landscapemetrics.R for more details on cohesion calculation


all_analysis_segments_cohesion_treshr <- readRDS(here("data/all_analysis_segments_cohesion_treshr"))

# read segment crossing values from A10_combine_analysis_segments.R

all_analysis_segments_by_lion <- readRDS(here("data/all_analysis_segments_by_lion")) %>% 
  data.frame() %>% 
  select(seg.label, year, used.available, animal.id)

configuration_scale_analysis_df <- full_join(all_analysis_segments_by_lion, all_analysis_segments_cohesion_treshr)

configuration_scale_analysis_df %>% 
  count(seg.label, year, animal.id) %>% 
  filter(n != 9) %>% 
  nrow()

configuration_scale_analysis_df %>% 
  count(seg.label, year, animal.id) %>% 
  filter(n != 9) %>% 
  distinct(seg.label, animal.id) %>% 
  view()

full_join(all_analysis_segments_by_lion, all_analysis_segments_cohesion_treshr) %>% 
  filter(!is.na(cohesion)) %>% 
  count(buff, forest.threshold) %>% 
  arrange(n)



configuration_scale_analysis_df <- configuration_scale_analysis_df %>% 
  filter(!is.na(cohesion)) %>% 
  group_by(seg.label, year, animal.id) %>% 
  mutate(num.rec = n()) %>% 
  ungroup() %>% 
  filter(num.rec == 9) %>% 
  group_by(buff, forest.threshold) %>% 
  mutate(cohesion = scale(cohesion)[,1]) %>% 
  ungroup()


#' fit_scale_mixed_mods
#'
#' @param zcross either seg.raw.crossing or seg.wt.crossing, the average number of crossings per segment per mt lion per month
#' @param zhab either mean.dev or mean.tre.shr, the mean development or tree+shrub cover within each buffer distance around each segment
#'
#' @returns list with an element for each model and one for the AIC table comparing all models
#' 
#' 
#' @details
#' this function uses the summed monthly segment proportion as an offset, with the summed raw crossing values (=1 for each time a segment was inside a BBMM UD) as the response variable. the advantage of the offset, vs using the summed proportions as a scaled crossing value, is that is weights the importance of the predictor variables while keeping the response on a scale that works properly for Poiss or NB error distribution. 
#' 
#'
#' @examples
fit_configuration_scale_mods <- function(zconfig) {
  scales <- c(100, 200, 300)
  forest.thresholds = c(25, 50, 75)
  
  zmods <- lapply(scales, forest.thresholds, function(s, t) {
    df_scale <- dplyr::filter(configuration_scale_analysis_df, buff == s, forest.threshold == t)
    glmer(
      formula = as.formula(paste0(
        "used.available ~ ", zconfig, "+ (1|animal.id)")),
      , data = df_scale, family = binomial(link = "logit")
    )
  })
  
  names(zmods) <- paste(zhab, scales, sep = "")
  
  zmods$aic <- aictab(zmods, names(zmods)) %>%
    data.frame() %>%
    dplyr::mutate(across(c(AICc, Delta_AICc, ModelLik, AICcWt), \(x) round(x, 3)))
  
  return(zmods)
}



fit_configuration_scale_mods <- function(zconfig) {
  scales <- c(100, 200, 300)
  forest.thresholds <- c(25, 50, 75)
  
  combos <- expand.grid(buff = scales, forest.threshold = forest.thresholds)
  
  zmods <- apply(combos, 1, function(row) {
    s <- row["buff"]
    t <- row["forest.threshold"]
    
    df_scale <- dplyr::filter(configuration_scale_analysis_df, buff == s, forest.threshold == t)
    
    glmer(
      formula = as.formula(paste0("used.available ~ ", zconfig, " + (1|animal.id)")),
      data = df_scale,
      family = binomial(link = "logit")
    )
  })
  
  names(zmods) <- paste0(zconfig, "_", combos$buff, "_", combos$forest.threshold)
  
  zmods$aic <- aictab(zmods, names(zmods)) %>%
    data.frame() %>%
    dplyr::mutate(across(c(AICc, Delta_AICc, ModelLik, AICcWt), \(x) round(x, 3)))
  
  return(zmods)
}



configuration_scale_mods <- fit_configuration_scale_mods("cohesion")
configuration_scale_mods$aic
# 9/3/25 results
#       Modnames K     AICc Delta_AICc ModelLik AICcWt        LL    Cum.Wt
#3 cohesion_300_25 3 12838.30      0.000    1.000  0.998 -6416.148 0.9979775
#1 cohesion_100_25 3 12850.93     12.637    0.002  0.002 -6422.466 0.9997762
#4 cohesion_100_50 3 12856.02     17.720    0.000  0.000 -6425.008 0.9999178
#2 cohesion_200_25 3 12857.24     18.945    0.000  0.000 -6425.620 0.9999946
#6 cohesion_300_50 3 12862.56     24.265    0.000  0.000 -6428.280 1.0000000
#5 cohesion_200_50 3 12882.32     44.023    0.000  0.000 -6438.159 1.0000000
#7 cohesion_100_75 3 12886.10     47.801    0.000  0.000 -6440.048 1.0000000
#9 cohesion_300_75 3 12891.92     53.625    0.000  0.000 -6442.960 1.0000000
#8 cohesion_200_75 3 12898.11     59.816    0.000  0.000 -6446.056 1.0000000
 


patchrd_scale_mods <- fit_configuration_scale_mods("patch.touches.road")
patchrd_scale_mods$aic
# 9/3/25 results - duplicate models because it's the same patches touching or not touching at different buffer distances for a given forest threshold
#                   Modnames K     AICc Delta_AICc ModelLik AICcWt        LL    Cum.Wt
#4 patch.touches.road_100_50 3 12855.72      0.000        1  0.333 -6424.860 0.3332672
#5 patch.touches.road_200_50 3 12855.72      0.000        1  0.333 -6424.860 0.6665344
#6 patch.touches.road_300_50 3 12855.72      0.000        1  0.333 -6424.860 0.9998016
#1 patch.touches.road_100_25 3 12872.77     17.051        0  0.000 -6433.385 0.9998677
#2 patch.touches.road_200_25 3 12872.77     17.051        0  0.000 -6433.385 0.9999338
#3 patch.touches.road_300_25 3 12872.77     17.051        0  0.000 -6433.385 0.9999999
#7 patch.touches.road_100_75 3 12887.99     32.267        0  0.000 -6440.993 0.9999999
#8 patch.touches.road_200_75 3 12887.99     32.267        0  0.000 -6440.993 1.0000000
#9 patch.touches.road_300_75 3 12887.99     32.267        0  0.000 -6440.993 1.0000000
 

