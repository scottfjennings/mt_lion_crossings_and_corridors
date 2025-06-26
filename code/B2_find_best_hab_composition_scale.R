
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
# this has a row for each road segment in the combined 95% home range polygon, for each year and each buffer distance (30-300, by 30)
# so this df has many more rows than the others
# it also has 
all_hr_road_habitat_95 <- readRDS(here("data/all_hr_road_habitat_95")) %>%
  data.frame() %>% 
  select(seg.label, mean.tre.shr, mean.dev, year, buff)


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
    select(year, seg.label, which.steps, seg.wt = annual.seg.wt.crossing, raw.crossing = annual.seg.raw.crossing, num.lion.months, num.lions, num.lions.crossing)
  

#puma_years <- seg_crossing_sums_naive_roads_only %>% 
#  distinct(animal.id, year)

# adding hr_segments_prop_in_developed shouldn't change the number of rows since it is derived from segments_in_homerange
composition_scale_df_pre <- summed_crossings %>% 
  left_join(hr_segments_prop_in_developed %>% distinct(seg.label, prop.seg.in.dev20, prop.seg.in.dev50))


# all_hr_road_habitat_95 has habitat values for some invalid puma X segment X year combinations
# need to use left_join(all_hr_road_habitat_95) to ensure just valid puma X segment X year combinations
# this is an expected many-to-many join because each segment may show up multiple times per year (in different months for same lion and in different lions HR) AND all_hr_road_habitat_95 has habitat at 10 spatial scales for each segment.
# should add 10X rows (for 10 buffer distances)
composition_scale_df <- composition_scale_df_pre %>% 
  left_join(all_hr_road_habitat_95)

# remove segments that are in continuous developed areas
composition_scale_df <- composition_scale_df %>% 
  filter(prop.seg.in.dev50 == 0) %>% 
  mutate(bin.crossing = ifelse(raw.crossing == 0, raw.crossing, 1))

saveRDS(composition_scale_df, here("data/analysis_inputs/composition_scale_df_lions_combined"))

composition_scale_df <- readRDS(here("data/analysis_inputs/composition_scale_df"))



# couple plots to check data ----

all_hr_road_habitat_95 %>% 
  select(year, seg.label, buff, mean.dev) %>% 
  mutate(buff = paste("b", buff, sep = "_")) %>% 
  pivot_wider(id_cols = c(year, seg.label), names_from = buff, values_from = mean.dev)%>% 
  select(-year, -seg.label) %>% 
  plot()



# similar distance buffers have more similar values of mean.tre.shr and mean.dev, although this is stronger for mean.tre.shr 


# modeling to select the best scale for both landscape composition predictors





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
fit_scale_mods_logreg <- function(zhab) {
  scales <- seq(30, 300, by = 30)
  
  zmods <- lapply(scales, function(s) {
    df_scale <- dplyr::filter(composition_scale_df, buff == s)
    glm(
      formula = as.formula(paste0(
        "bin.crossing ~ ", zhab)),
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




dev_scale_mods_logreg <- fit_scale_mods_logreg("mean.dev")
dev_scale_mods_logreg$aic

#     Modnames K     AICc Delta_AICc ModelLik AICcWt        LL    Cum.Wt
#2   mean.dev60 2 6503.569      0.000    1.000  0.992 -3249.784 0.9923545
#3   mean.dev90 2 6513.301      9.732    0.008  0.008 -3254.650 0.9999992
#4  mean.dev120 2 6531.551     27.982    0.000  0.000 -3263.774 1.0000000
#5  mean.dev150 2 6549.460     45.891    0.000  0.000 -3272.729 1.0000000
#6  mean.dev180 2 6563.920     60.351    0.000  0.000 -3279.959 1.0000000
#7  mean.dev210 2 6574.407     70.838    0.000  0.000 -3285.203 1.0000000
#8  mean.dev240 2 6580.004     76.434    0.000  0.000 -3288.001 1.0000000
#9  mean.dev270 2 6583.816     80.246    0.000  0.000 -3289.907 1.0000000
#10 mean.dev300 2 6586.301     82.731    0.000  0.000 -3291.149 1.0000000
#1   mean.dev30 2 6637.304    133.735    0.000  0.000 -3316.651 1.0000000
 

treshr_scale_mods_logreg <- fit_scale_mods_logreg("mean.tre.shr")
treshr_scale_mods_logreg$aic

#         Modnames K     AICc Delta_AICc ModelLik AICcWt        LL    Cum.Wt
#10 mean.tre.shr300 2 7042.009      0.000    1.000  0.104 -3519.003 0.1037675
#9  mean.tre.shr270 2 7042.013      0.004    0.998  0.104 -3519.005 0.2073279
#8  mean.tre.shr240 2 7042.051      0.042    0.979  0.102 -3519.025 0.3089325
#7  mean.tre.shr210 2 7042.067      0.058    0.972  0.101 -3519.032 0.4097527
#2   mean.tre.shr60 2 7042.071      0.062    0.970  0.101 -3519.034 0.5103626
#6  mean.tre.shr180 2 7042.081      0.072    0.965  0.100 -3519.040 0.6104552
#3   mean.tre.shr90 2 7042.103      0.094    0.954  0.099 -3519.050 0.7094579
#5  mean.tre.shr150 2 7042.116      0.107    0.948  0.098 -3519.057 0.8078197
#4  mean.tre.shr120 2 7042.128      0.119    0.942  0.098 -3519.063 0.9055776
#1   mean.tre.shr30 2 7042.198      0.189    0.910  0.094 -3519.098 1.0000000
 


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
fit_scale_mods_pois <- function(zhab) {
  scales <- seq(30, 300, by = 30)
  
  zmods <- lapply(scales, function(s) {
    df_scale <- dplyr::filter(composition_scale_df, buff == s)
    glm(
      formula = as.formula(paste0(
        "num.lions.crossing ~ ", zhab)),
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



dev_scale_mods_pois <- fit_scale_mods_pois("mean.dev")
dev_scale_mods_pois$aic

#    Modnames K     AICc Delta_AICc ModelLik AICcWt        LL    Cum.Wt
#2   mean.dev60 2 9101.883      0.000    1.000  0.997 -4548.941 0.9966072
#3   mean.dev90 2 9113.249     11.366    0.003  0.003 -4554.623 0.9999997
#4  mean.dev120 2 9131.679     29.795    0.000  0.000 -4563.838 1.0000000
#5  mean.dev150 2 9151.616     49.732    0.000  0.000 -4573.807 1.0000000
#6  mean.dev180 2 9167.177     65.294    0.000  0.000 -4581.588 1.0000000
#7  mean.dev210 2 9177.838     75.954    0.000  0.000 -4586.918 1.0000000
#8  mean.dev240 2 9182.944     81.060    0.000  0.000 -4589.471 1.0000000
#9  mean.dev270 2 9187.186     85.303    0.000  0.000 -4591.592 1.0000000
#10 mean.dev300 2 9189.710     87.827    0.000  0.000 -4592.854 1.0000000
#1   mean.dev30 2 9257.873    155.990    0.000  0.000 -4626.935 1.0000000

summary(dev_scale_mods_pois$mean.dev60)

treshr_scale_mods_pois <- fit_scale_mods_pois("mean.tre.shr")
treshr_scale_mods_pois$aic

#          Modnames K     AICc Delta_AICc ModelLik AICcWt        LL    Cum.Wt
#4  mean.tre.shr120 2 9610.093      0.000    1.000  0.105 -4803.045 0.1045062
#5  mean.tre.shr150 2 9610.093      0.000    1.000  0.105 -4803.045 0.2090079
#3   mean.tre.shr90 2 9610.129      0.037    0.982  0.103 -4803.064 0.3116101
#1   mean.tre.shr30 2 9610.144      0.051    0.975  0.102 -4803.071 0.4134753
#6  mean.tre.shr180 2 9610.147      0.055    0.973  0.102 -4803.073 0.5151558
#7  mean.tre.shr210 2 9610.187      0.095    0.954  0.100 -4803.093 0.6148333
#2   mean.tre.shr60 2 9610.198      0.105    0.949  0.099 -4803.098 0.7139961
#8  mean.tre.shr240 2 9610.239      0.147    0.929  0.097 -4803.119 0.8111062
#9  mean.tre.shr270 2 9610.295      0.202    0.904  0.094 -4803.146 0.9055615
#10 mean.tre.shr300 2 9610.295      0.203    0.904  0.094 -4803.147 1.0000000

# not worrying about spatial autocorrelation at this stage
# for both compositions, all spatial scales dAICc < 2 when considering crossings for all animals lumped. 
# just going to use 300m for both to capture the largest area


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
fit_scale_mixed_mods_offset <- function(zhab) {
  scales <- seq(30, 300, by = 30)
  
  zmods <- lapply(scales, function(s) {
    df_scale <- dplyr::filter(composition_scale_df, buff == s)
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




dev_scale_mods_offset <- fit_scale_mixed_mods_offset("mean.dev")
dev_scale_mods_offset$aic



summary(dev_scale_mods_offset$mean.dev30)
# 300m best for mean.dev (june 2025)
# but 30m has max dAICc at only 0.9, so going to use 30 and 300 for dev
#Modnames K     AICc Delta_AICc ModelLik AICcWt        LL    Cum.Wt
#10 mean.dev300 3 16417.26      0.000    1.000  0.116 -8205.631 0.1156502
#3   mean.dev90 3 16417.28      0.019    0.990  0.115 -8205.640 0.2301951
#4  mean.dev120 3 16417.36      0.100    0.951  0.110 -8205.680 0.3402303
#2   mean.dev60 3 16417.37      0.110    0.947  0.109 -8205.685 0.4497027
#9  mean.dev270 3 16417.40      0.135    0.935  0.108 -8205.698 0.5578103
#8  mean.dev240 3 16417.53      0.267    0.875  0.101 -8205.764 0.6589984
#5  mean.dev150 3 16417.75      0.485    0.785  0.091 -8205.873 0.7497382
#7  mean.dev210 3 16417.75      0.492    0.782  0.090 -8205.877 0.8401630
#6  mean.dev180 3 16417.82      0.557    0.757  0.088 -8205.909 0.9277155
#1   mean.dev30 3 16418.20      0.940    0.625  0.072 -8206.101 1.0000000

treshr_scale_mods_offset <- fit_scale_mixed_mods_offset("mean.tre.shr")
treshr_scale_mods_offset$aic
#Modnames K     AICc Delta_AICc ModelLik AICcWt        LL    Cum.Wt
#7  mean.tre.shr210 3 16418.07      0.000    1.000  0.107 -8206.036 0.1074743
#8  mean.tre.shr240 3 16418.07      0.000    1.000  0.107 -8206.036 0.2149423
#6  mean.tre.shr180 3 16418.09      0.014    0.993  0.107 -8206.043 0.3216872
#5  mean.tre.shr150 3 16418.12      0.049    0.976  0.105 -8206.061 0.4265419
#9  mean.tre.shr270 3 16418.16      0.084    0.959  0.103 -8206.078 0.5295989
#4  mean.tre.shr120 3 16418.20      0.129    0.937  0.101 -8206.101 0.6303482
#10 mean.tre.shr300 3 16418.22      0.149    0.928  0.100 -8206.111 0.7300948
#3   mean.tre.shr90 3 16418.32      0.248    0.883  0.095 -8206.160 0.8250145
#2   mean.tre.shr60 3 16418.45      0.380    0.827  0.089 -8206.226 0.9138708
#1   mean.tre.shr30 3 16418.51      0.443    0.801  0.086 -8206.258 1.0000000


summary(treshr_scale_mods_offset$mean.tre.shr120)

# 210m best (barely) for tre.shr (june 2025)
# but really, all scales are approximately equally supported: max aic wt = .107, min = 0.086 with dAICc = 0.443
# going to use use 30m and 300m in analysis to allow possible new relative importance to emerge when other varbs are added.


###############################################################################

################# as of June 2025, only run to here ###########################

###############################################################################

###############################################################################

###############################################################################


# trying lmm as for bridges_streams ----

#' fit_scale_mixed_mods
#'
#' @param zcross either seg.raw.crossing or seg.wt.crossing, the average number of crossings per segment per mt lion per month
#' @param zhab either mean.dev or mean.tre.shr, the mean development or tree+shrub cover within each buffer distance around each segment
#'
#' @returns list with an element for each model and one for the AIC table comparing all models
#'
#' @examples
fit_scale_mixed_mods <- function(zcross, zhab) {
  # Dynamically set the response variable in the formula
zmods <- list(
  "mod30" = lmer(as.formula(paste(zcross, "~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 30), REML = FALSE),
  "mod60" = lmer(as.formula(paste(zcross, "~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 60), REML = FALSE),
  "mod90" = lmer(as.formula(paste(zcross, "~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 90), REML = FALSE),
  "mod120" = lmer(as.formula(paste(zcross, "~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 120), REML = FALSE),
  "mod150" = lmer(as.formula(paste(zcross, "~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 150), REML = FALSE),
  "mod180" = lmer(as.formula(paste(zcross, "~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 180), REML = FALSE),
  "mod210" = lmer(as.formula(paste(zcross, "~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 210), REML = FALSE),
  "mod240" = lmer(as.formula(paste(zcross, "~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 240), REML = FALSE),
  "mod270" = lmer(as.formula(paste(zcross, "~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 270), REML = FALSE),
  "mod300" = lmer(as.formula(paste(zcross, "~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 300), REML = FALSE)
)



names(zmods) <-  paste(zhab, seq(30, 300, length.out = 10), sep = "")


zmods$aic <- aictab(zmods, names(zmods)) %>% 
  data.frame() %>% 
  mutate(across(c(AICc, Delta_AICc, ModelLik, AICcWt), ~round(., 3)))

return(zmods)

}


# checking prediction of num crossings with crossed and not crossed segments 
raw_dev_scale_mods <- fit_scale_mixed_mods("seg.raw.crossing", "mean.dev")
raw_dev_scale_mods$aic
summary(raw_dev_scale_mods$mean.dev60)

wt_dev_scale_mods <- fit_scale_mixed_mods("seg.wt.crossing", "mean.dev")
wt_dev_scale_mods$aic
summary(wt_dev_scale_mods$mean.dev60)


raw_treshr_scale_mods <- fit_scale_mixed_mods("seg.raw.crossing", "mean.tre.shr")
raw_treshr_scale_mods$aic
summary(raw_treshr_scale_mods$mean.tre.shr300)


wt_treshr_scale_mods <- fit_scale_mixed_mods("seg.wt.crossing", "mean.tre.shr")
wt_treshr_scale_mods$aic
summary(wt_treshr_scale_mods$mean.tre.shr300)


raw_imperv_scale_mods <- fit_scale_mixed_mods("seg.raw.crossing", "mean.imperv")
raw_imperv_scale_mods$aic
summary(raw_imperv_scale_mods$mean.imperv30)

wt_imperv_scale_mods <- fit_scale_mixed_mods("seg.wt.crossing", "mean.imperv")
wt_imperv_scale_mods$aic
summary(wt_imperv_scale_mods$mean.imperv30)


aictab(list(wt_dev_scale_mods$mean.dev60, wt_imperv_scale_mods$mean.imperv30), c("dev", "imperv"))

# 60m buffer best for development and 300m best for tree+shrub cover
# best development mod way better than best impervious surface model. similar coefs and CI for both models. going to stick with dev.30


# checking prediction of crossed vs not crossed ----

# trying lmm as for bridges_streams

#' fit_scale_mixed_mods_crossed_not
#'
#' @param zcross either seg.raw.crossing or seg.wt.crossing, the average number of crossings per segment per mt lion per month
#' @param zhab either mean.dev or mean.tre.shr, the mean development or tree+shrub cover within each buffer distance around each segment
#'
#' @returns list with an element for each model and one for the AIC table comparing all models
#'
#' @examples
fit_scale_mixed_mods_crossed_not <- function(zhab) {
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


# 90m buffer best for development and 300m best for tree+shrub cover


# diagnostics for spatial autocorr


zz <- filter(seg_crossing_sums_naive_roads_only_hab, buff == 90, !is.na(mean.dev))

mod90 = glmer(crossed.bin ~ mean.dev + (1|animal.id), data = zz, family = "binomial")

resid_vals <- residuals(mod90, type = "pearson")


resid_test <- zz %>% 
  mutate(resid = residuals(mod90, type = "pearson"))

ggplot() +
  geom_sf(data = resid_test, aes(color = resid, size = resid), alpha = 0.2)


# Moran’s I with ape

library(ape)
library(spdep)
# Create neighbor list: neighbors within 1000m
nb <- dnearneigh(coords, d1 = 0, d2 = 1000)

# Create spatial weights list
lw <- nb2listw(nb, style = "B")  # binary weights


# Moran's I test
moran_result <- Moran.I(resid_vals, lw)
print(moran_result)

##### Variogram using gstat

library(gstat)
library(sp)

# Build SpatialPointsDataFrame using coordinates and residuals
spdf <- SpatialPointsDataFrame(
  coords = coords,
  data = data.frame(resid = resid_vals),
  proj4string = CRS(st_crs(seg_crossing_sums_naive_roads_only_hab)$wkt)
)

# Compute variogram
vgm_emp <- variogram(resid ~ 1, data = spdf)
plot(vgm_emp)


# checking prediction of num crossed for crossed roads only ----

# trying lmm as for bridges_streams

#' fit_scale_mixed_mods
#'
#' @param zcross either seg.raw.crossing or seg.wt.crossing, the average number of crossings per segment per mt lion per month
#' @param zhab either mean.dev or mean.tre.shr, the mean development or tree+shrub cover within each buffer distance around each segment
#'
#' @returns list with an element for each model and one for the AIC table comparing all models
#'
#' @examples
fit_scale_mixed_mods_crossed <- function(zcross, zhab) {
  # Dynamically set the response variable in the formula
  zmods <- list(
    "mod30" = lmer(as.formula(paste(zcross, "~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 30, seg.wt.crossing > 0), REML = FALSE),
    "mod60" = lmer(as.formula(paste(zcross, "~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 60, seg.wt.crossing > 0), REML = FALSE),
    "mod90" = lmer(as.formula(paste(zcross, "~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 90, seg.wt.crossing > 0), REML = FALSE),
    "mod120" = lmer(as.formula(paste(zcross, "~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 120, seg.wt.crossing > 0), REML = FALSE),
    "mod150" = lmer(as.formula(paste(zcross, "~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 150, seg.wt.crossing > 0), REML = FALSE),
    "mod180" = lmer(as.formula(paste(zcross, "~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 180, seg.wt.crossing > 0), REML = FALSE),
    "mod210" = lmer(as.formula(paste(zcross, "~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 210, seg.wt.crossing > 0), REML = FALSE),
    "mod240" = lmer(as.formula(paste(zcross, "~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 240, seg.wt.crossing > 0), REML = FALSE),
    "mod270" = lmer(as.formula(paste(zcross, "~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 270, seg.wt.crossing > 0), REML = FALSE),
    "mod300" = lmer(as.formula(paste(zcross, "~", zhab, "+ (1|animal.id)")), data = filter(seg_crossing_sums_naive_roads_only_hab, buff == 300, seg.wt.crossing > 0), REML = FALSE)
  )
  
  names(zmods) <-  paste(zhab, seq(30, 300, length.out = 10), sep = "")
  
  zmods$aic <- aictab(zmods, names(zmods)) %>% 
    data.frame() %>% 
    mutate(across(c(AICc, Delta_AICc, ModelLik, AICcWt), ~round(., 3)))
  
  return(zmods)
}


dev_scale_mods_crossed <- fit_scale_mixed_mods_crossed("seg.wt.crossing", "mean.dev")
dev_scale_mods_crossed$aic
summary(dev_scale_mods_crossed$mean.dev30)


treshr_scale_mods_crossed <- fit_scale_mixed_mods_crossed("seg.wt.crossing", "mean.tre.shr")
treshr_scale_mods_crossed$aic
summary(treshr_scale_mods_crossed$mean.tre.shr60)


# 30m buffer best for development and 60m best for tree+shrub cover





# ADDING SPATIAL AUTOCORRELATION ----
fit_scale_spatial_mixed_mods_crossed_not_SpAuto <- function(zhab) {

  # Spatial + random effect logistic model across buffer distances
  buffers <- seq(30, 300, by = 30)
  zmods <- list()
  
  for (b in buffers) {
    dat <- seg_crossing_sums_naive_roads_only_hab %>% 
      filter(buff == b) %>%
      mutate(
        # Required spatial structure
        pos = numFactor(x, y),
        dummy_group = factor("all")
      )
    
    form <- as.formula(paste("crossed.bin ~", zhab, "+ exp(pos + 0 | dummy_group) + (1 | animal.id)"))
    
    zmods[[paste0("mod", b)]] <- glmmTMB(
      formula = form,
      family = binomial,
      data = dat
    )
  }
  
  # Label models by variable name and buffer
  names(zmods) <- paste(zhab, buffers, sep = "")
  
  # Add AICc table
  zmods$aic <- aictab(zmods, names(zmods)) %>%
    data.frame() %>%
    mutate(across(c(AICc, Delta_AICc, ModelLik, AICcWt), ~ round(., 3)))
  
  return(zmods)
}
