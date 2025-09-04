
library(tidyverse)
library(here)
library(sf)
library(sp)
library(lme4)
library(gstat)
library(AICcmodavg)

options(scipen = 999)
source(here("code/helper_data.R"))



# read the road segment habitat values from A12_segment_habitat.R
# this has a row for each road segment in the combined home range polygon, for each year and each buffer distance (30-300, by 30)
# so this df has many more rows than the others

all_analysis_segment_impervious <- readRDS(here("data/all_analysis_segment_impervious")) %>% 
  select(seg.label, buff, mean.percent.impervious)

all_analysis_segments_mean_tre_shr <- readRDS(here("data/all_analysis_segments_mean_tre_shr")) %>% 
  select(seg.label, year, buff, mean.tre.shr = mean.veg)

# read segment crossing values from A10_combine_analysis_segments.R

all_analysis_segments_by_lion <- readRDS(here("data/all_analysis_segments_by_lion")) %>% 
  data.frame() %>% 
  select(seg.label, year, used.available, animal.id)

composition_scale_analysis_df <- full_join(all_analysis_segments_by_lion, all_analysis_segments_mean_tre_shr) %>% 
  full_join(all_analysis_segment_impervious)

composition_scale_analysis_df <- composition_scale_analysis_df %>% 
  group_by(seg.label, year, animal.id) %>% 
  mutate(num.rec = n()) %>% 
  ungroup() %>% 
  filter(num.rec == 10) %>% 
  group_by(buff) %>% 
  mutate(mean.tre.shr = scale(mean.tre.shr)[,1],
         mean.percent.impervious = scale(mean.percent.impervious)[,1]) %>% 
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
fit_composition_scale_mods <- function(zhab) {
  scales <- seq(30, 300, by = 30)
  
  zmods <- lapply(scales, function(s) {
    df_scale <- dplyr::filter(composition_scale_analysis_df, buff == s)
    glmer(
      formula = as.formula(paste0(
        "used.available ~ ", zhab, "+ (1|animal.id)")),
      , data = df_scale, family = binomial(link = "logit")
    )
  })
  
  names(zmods) <- paste(zhab, scales, sep = "")
  
  zmods$aic <- aictab(zmods, names(zmods)) %>%
    data.frame() %>%
    dplyr::mutate(across(c(AICc, Delta_AICc, ModelLik, AICcWt), \(x) round(x, 3)))
  
  return(zmods)
}


tre_shr_scale_mods <- fit_composition_scale_mods("mean.tre.shr")
tre_shr_scale_mods$aic
# 9/3/25 results
#           Modnames K     AICc Delta_AICc ModelLik AICcWt        LL    Cum.Wt
#3   mean.tre.shr90 3 44868.29      0.000    1.000  0.396 -22431.14 0.3958332
#2   mean.tre.shr60 3 44869.06      0.776    0.679  0.269 -22431.53 0.6644077
#7  mean.tre.shr210 3 44869.09      0.808    0.668  0.264 -22431.55 0.9287166
#4  mean.tre.shr120 3 44873.16      4.870    0.088  0.035 -22433.58 0.9633852
#6  mean.tre.shr180 3 44874.03      5.742    0.057  0.022 -22434.01 0.9858065
#8  mean.tre.shr240 3 44875.66      7.371    0.025  0.010 -22434.83 0.9957361
#5  mean.tre.shr150 3 44878.14      9.854    0.007  0.003 -22436.07 0.9986054
#9  mean.tre.shr270 3 44880.22     11.938    0.003  0.001 -22437.11 0.9996173
#10 mean.tre.shr300 3 44882.17     13.883    0.001  0.000 -22438.08 1.0000000
#1   mean.tre.shr30 3 44946.29     78.002    0.000  0.000 -22470.14 1.0000000



impervious_scale_mods <- fit_composition_scale_mods("mean.percent.impervious")
impervious_scale_mods$aic
# 9/3/25 results
#                     Modnames K     AICc Delta_AICc ModelLik AICcWt        LL    Cum.Wt
#2   mean.percent.impervious60 3 43219.96      0.000    1.000  0.997 -21606.98 0.9966108
#1   mean.percent.impervious30 3 43231.33     11.368    0.003  0.003 -21612.66 1.0000000
#3   mean.percent.impervious90 3 43295.78     75.817    0.000  0.000 -21644.89 1.0000000
#4  mean.percent.impervious120 3 43337.45    117.489    0.000  0.000 -21665.72 1.0000000
#5  mean.percent.impervious150 3 43370.52    150.563    0.000  0.000 -21682.26 1.0000000
#6  mean.percent.impervious180 3 43394.41    174.455    0.000  0.000 -21694.21 1.0000000
#7  mean.percent.impervious210 3 43415.61    195.647    0.000  0.000 -21704.80 1.0000000
#8  mean.percent.impervious240 3 43423.87    203.912    0.000  0.000 -21708.93 1.0000000
#9  mean.percent.impervious270 3 43433.62    213.666    0.000  0.000 -21713.81 1.0000000
#10 mean.percent.impervious300 3 43442.52    222.565    0.000  0.000 -21718.26 1.0000000
 

