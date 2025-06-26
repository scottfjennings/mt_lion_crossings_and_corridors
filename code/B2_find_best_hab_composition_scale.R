
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
      family = binomial,
      weights = num.lion.months
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

#         Modnames K     AICc Delta_AICc ModelLik AICcWt        LL Cum.Wt
#2   mean.dev60 2 84282.28      0.000        1      1 -42139.14      1
#3   mean.dev90 2 84451.85    169.566        0      0 -42223.92      1
#4  mean.dev120 2 84759.32    477.044        0      0 -42377.66      1
#5  mean.dev150 2 85069.14    786.861        0      0 -42532.57      1
#6  mean.dev180 2 85314.18   1031.903        0      0 -42655.09      1
#7  mean.dev210 2 85482.49   1200.208        0      0 -42739.24      1
#8  mean.dev240 2 85585.90   1303.618        0      0 -42790.95      1
#9  mean.dev270 2 85651.78   1369.499        0      0 -42823.89      1
#10 mean.dev300 2 85697.23   1414.948        0      0 -42846.61      1
#1   mean.dev30 2 86484.93   2202.655        0      0 -43240.47      1
 
 

treshr_scale_mods_logreg <- fit_scale_mods_logreg("mean.tre.shr")
treshr_scale_mods_logreg$aic
#          Modnames K     AICc Delta_AICc ModelLik AICcWt        LL    Cum.Wt
#1   mean.tre.shr30 2 93710.12      0.000    1.000  0.820 -46853.06 0.8203813
#2   mean.tre.shr60 2 93713.24      3.123    0.210  0.172 -46854.62 0.9925420
#3   mean.tre.shr90 2 93720.71     10.589    0.005  0.004 -46858.35 0.9966593
#10 mean.tre.shr300 2 93722.33     12.218    0.002  0.002 -46859.17 0.9984830
#9  mean.tre.shr270 2 93723.30     13.184    0.001  0.001 -46859.65 0.9996081
#8  mean.tre.shr240 2 93726.93     16.809    0.000  0.000 -46861.46 0.9997917
#4  mean.tre.shr120 2 93727.96     17.844    0.000  0.000 -46861.98 0.9999012
#7  mean.tre.shr210 2 93729.28     19.164    0.000  0.000 -46862.64 0.9999577
#6  mean.tre.shr180 2 93730.80     20.684    0.000  0.000 -46863.40 0.9999842
#5  mean.tre.shr150 2 93731.83     21.715    0.000  0.000 -46863.91 1.0000000
 


#' fit_scale_mods_pois
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
      family = poisson,
      weights = num.lion.months
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

#     Modnames K     AICc Delta_AICc ModelLik AICcWt        LL Cum.Wt
#2   mean.dev60 2 135436.8      0.000        1      1 -67716.41      1
#3   mean.dev90 2 135609.3    172.530        0      0 -67802.67      1
#4  mean.dev120 2 135889.4    452.621        0      0 -67942.72      1
#5  mean.dev150 2 136198.7    761.856        0      0 -68097.33      1
#6  mean.dev180 2 136429.0    992.181        0      0 -68212.50      1
#7  mean.dev210 2 136576.1   1139.256        0      0 -68286.03      1
#8  mean.dev240 2 136648.5   1211.664        0      0 -68322.24      1
#9  mean.dev270 2 136712.8   1275.966        0      0 -68354.39      1
#10 mean.dev300 2 136750.5   1313.664        0      0 -68373.24      1
#1   mean.dev30 2 138087.3   2650.463        0      0 -69041.64      1

summary(dev_scale_mods_pois$mean.dev60)

treshr_scale_mods_pois <- fit_scale_mods_pois("mean.tre.shr")
treshr_scale_mods_pois$aic

#          Modnames K     AICc Delta_AICc ModelLik AICcWt        LL    Cum.Wt
#1   mean.tre.shr30 2 143990.9      0.000    1.000  0.688 -71993.44 0.6879676
#2   mean.tre.shr60 2 143993.8      2.902    0.234  0.161 -71994.90 0.8491518
#3   mean.tre.shr90 2 143996.8      5.873    0.053  0.036 -71996.38 0.8856506
#10 mean.tre.shr300 2 143997.2      6.326    0.042  0.029 -71996.61 0.9147478
#9  mean.tre.shr270 2 143997.3      6.423    0.040  0.028 -71996.66 0.9424762
#8  mean.tre.shr240 2 143998.3      7.444    0.024  0.017 -71997.17 0.9591120
#4  mean.tre.shr120 2 143998.7      7.855    0.020  0.014 -71997.37 0.9726608
#7  mean.tre.shr210 2 143999.2      8.329    0.016  0.011 -71997.61 0.9833479
#6  mean.tre.shr180 2 143999.7      8.762    0.013  0.009 -71997.83 0.9919558
#5  mean.tre.shr150 2 143999.8      8.898    0.012  0.008 -71997.89 1.0000000

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
