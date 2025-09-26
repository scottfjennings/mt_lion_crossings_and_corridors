
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

all_analysis_segment_impervious_full <- readRDS(here("data/all_analysis_segment_impervious_full")) %>% 
  select(seg.label, buff, mean.percent.impervious)

all_analysis_segments_mean_tre_shr_full <- readRDS(here("data/all_analysis_segments_mean_tre_shr_full")) %>% 
  select(seg.label, year, buff, mean.tre.shr = mean.veg)

# read segment crossing values from A10_combine_analysis_segments.R

all_analysis_segments_by_lion_year <- readRDS(here("data/all_analysis_segments_by_lion_year")) %>% 
  data.frame() %>% 
  select(seg.label, year, used.available, animal.id)

composition_scale_analysis_df <- full_join(all_analysis_segments_by_lion_year, all_analysis_segments_mean_tre_shr_full) %>% 
  full_join(all_analysis_segment_impervious_full)

composition_scale_analysis_df <- composition_scale_analysis_df %>% 
  distinct() %>% 
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

# 9/16/25 results with 100% HR
#            Modnames K     AICc Delta_AICc ModelLik AICcWt        LL    Cum.Wt
#4  mean.tre.shr120 3 18228.76      0.000    1.000  0.172 -9111.381 0.1722458
#5  mean.tre.shr150 3 18229.09      0.324    0.850  0.146 -9111.543 0.3187269
#7  mean.tre.shr210 3 18229.49      0.727    0.695  0.120 -9111.744 0.4385078
#6  mean.tre.shr180 3 18229.50      0.734    0.693  0.119 -9111.748 0.5578684
#10 mean.tre.shr300 3 18229.57      0.808    0.668  0.115 -9111.785 0.6728794
#8  mean.tre.shr240 3 18229.86      1.094    0.579  0.100 -9111.928 0.7725644
#3   mean.tre.shr90 3 18229.91      1.146    0.564  0.097 -9111.954 0.8697010
#9  mean.tre.shr270 3 18229.97      1.210    0.546  0.094 -9111.986 0.9637687
#2   mean.tre.shr60 3 18232.20      3.433    0.180  0.031 -9113.098 0.9947155
#1   mean.tre.shr30 3 18235.73      6.968    0.031  0.005 -9114.865 1.0000000


impervious_scale_mods <- fit_composition_scale_mods("mean.percent.impervious")
impervious_scale_mods$aic

# 9/16/25 results with 100% HR
#                      Modnames K     AICc Delta_AICc ModelLik AICcWt        LL    Cum.Wt
#3   mean.percent.impervious90 3 18209.78      0.000    1.000  0.216 -9101.892 0.2163142
#7  mean.percent.impervious210 3 18210.63      0.847    0.655  0.142 -9102.315 0.3579400
#6  mean.percent.impervious180 3 18210.67      0.881    0.644  0.139 -9102.332 0.4971599
#5  mean.percent.impervious150 3 18210.72      0.931    0.628  0.136 -9102.357 0.6329477
#4  mean.percent.impervious120 3 18210.74      0.959    0.619  0.134 -9102.371 0.7668794
#8  mean.percent.impervious240 3 18211.54      1.756    0.416  0.090 -9102.770 0.8567952
#9  mean.percent.impervious270 3 18212.60      2.811    0.245  0.053 -9103.297 0.9098583
#2   mean.percent.impervious60 3 18212.66      2.878    0.237  0.051 -9103.331 0.9611655
#10 mean.percent.impervious300 3 18213.72      3.937    0.140  0.030 -9103.860 0.9913809
#1   mean.percent.impervious30 3 18216.23      6.446    0.040  0.009 -9105.114 1.0000000
