




library(tidyverse)
library(here)
library(sf)
library(lme4)
library(AICcmodavg)

options(scipen = 999)
source(here("code/helper_data.R"))

# testing the relative importance of "cohesion" of woody plant cover calculated at different spatial scales and different thresholds for classifying a 30m pixel as woody:
# see A15_segment_landscapemetrics.R for more details on cohesion calculation


all_analysis_segments_cohesion_treshr_full <- readRDS(here("data/all_analysis_segments_cohesion_treshr_full"))

all_analysis_segments_cohesion_treshr_full <- all_analysis_segments_cohesion_treshr_full %>% 
  data.frame() %>% 
  select(cohesion, seg.label, buff, forest.threshold, layer, patch.touches.road, year, used.available, crossing.step, naive.rd, crossings.per.naive.rd)


# read segment crossing values from A10_combine_analysis_segments.R

all_analysis_segments_by_lion_year <- readRDS(here("data/all_analysis_segments_by_lion_year")) %>% 
  data.frame() %>% 
  select(seg.label, year, used.available, animal.id)

configuration_scale_analysis_df <- full_join(all_analysis_segments_by_lion_year, all_analysis_segments_cohesion_treshr_full)

configuration_scale_analysis_df <- distinct(configuration_scale_analysis_df)


configuration_scale_analysis_df %>% 
  count(seg.label, year, animal.id) %>% 
  filter(n != 9) %>% 
  nrow()

configuration_scale_analysis_df %>% 
  count(seg.label, year, animal.id) %>% 
  filter(n != 9) %>% 
  distinct(seg.label, animal.id) %>% 
  view()




configuration_scale_analysis_df <- configuration_scale_analysis_df %>% 
#  filter(!is.na(cohesion)) %>% 
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
# 9/16/25 results with 100% HR
#           Modnames K     AICc Delta_AICc ModelLik AICcWt        LL    Cum.Wt
#5 cohesion_200_50 3 18045.33      0.000        1      1 -9019.665 0.9999957
#6 cohesion_300_50 3 18070.68     25.352        0      0 -9032.340 0.9999988
#4 cohesion_100_50 3 18072.61     27.278        0      0 -9033.303 1.0000000
#1 cohesion_100_25 3 18110.49     65.164        0      0 -9052.247 1.0000000
#2 cohesion_200_25 3 18126.55     81.215        0      0 -9060.272 1.0000000
#3 cohesion_300_25 3 18133.48     88.150        0      0 -9063.739 1.0000000
#9 cohesion_300_75 3 18139.77     94.444        0      0 -9066.886 1.0000000
#8 cohesion_200_75 3 18154.56    109.232        0      0 -9074.281 1.0000000
#7 cohesion_100_75 3 18178.94    133.609        0      0 -9086.469 1.0000000



patchrd_scale_mods <- fit_configuration_scale_mods("patch.touches.road")
patchrd_scale_mods$aic
# duplicate models because it's the same patches touching or not touching at different buffer distances for a given forest threshold

# 9/16/25 results with 100% HR
#                Modnames K     AICc Delta_AICc ModelLik AICcWt        LL    Cum.Wt
#1 patch.touches.road_100_25 3 17993.93      0.000        1  0.333 -8993.966 0.3333333
#2 patch.touches.road_200_25 3 17993.93      0.000        1  0.333 -8993.966 0.6666667
#3 patch.touches.road_300_25 3 17993.93      0.000        1  0.333 -8993.966 1.0000000
#4 patch.touches.road_100_50 3 18074.07     80.141        0  0.000 -9034.036 1.0000000
#5 patch.touches.road_200_50 3 18074.07     80.141        0  0.000 -9034.036 1.0000000
#6 patch.touches.road_300_50 3 18074.07     80.141        0  0.000 -9034.036 1.0000000
#7 patch.touches.road_100_75 3 18127.21    133.278        0  0.000 -9060.605 1.0000000
#8 patch.touches.road_200_75 3 18127.21    133.278        0  0.000 -9060.605 1.0000000
#9 patch.touches.road_300_75 3 18127.21    133.278        0  0.000 -9060.605 1.0000000