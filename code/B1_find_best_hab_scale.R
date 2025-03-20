
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

all_hr_road_habitat_df <- readRDS(here("data/analysis_inputs/all_hr_road_habitat_df")) %>% 
  filter(!animal.id %in% hr_exclude_pumas,
         !seg.label %in% p31_exclude_segments)


# couple plots to check data
all_hr_road_habitat_df %>%
  select(year, seg.label, animal.id, buff, mean.tre.shr) %>% 
  mutate(buff = paste("b", buff, sep = "_")) %>% 
  pivot_wider(id_cols = c(year, animal.id, seg.label), names_from = buff, values_from = mean.tre.shr)%>% 
  select(-animal.id, -year, -seg.label) %>% 
  plot()

all_hr_road_habitat_df %>% 
  select(year, seg.label, animal.id, buff, mean.dev) %>% 
  mutate(buff = paste("b", buff, sep = "_")) %>% 
  pivot_wider(id_cols = c(year, animal.id, seg.label), names_from = buff, values_from = mean.dev)%>% 
  select(-animal.id, -year, -seg.label) %>% 
  plot()

# similar distance buffers have more similar values of mean.tre.shr and mean.dev, although this is stronger for mean.tre.shr 

# read the summed segment crossings
# for now using the moderately filtered crossings, those that consider only the naively crossed road within the BBMM
########## will ultimately compare results from all three filter levels ##########
# seg_crossing_sums <- readRDS(here("data/analysis_inputs/seg_crossing_sums_naive_segs_only")) # more strictly filtered crossings
seg_crossing_sums_naive_roads_only <- readRDS(here("data/analysis_inputs/seg_crossing_sums_naive_roads_only")) %>% 
  filter(!animal.id %in% hr_exclude_pumas)


seg_crossing_sums_naive_roads_only_hab <- all_hr_road_habitat_df %>% 
  full_join(seg_crossing_sums_naive_roads_only)


#
# for summed crossings; selecting the best spatial scale for each predictor ----
# trying lmm as for bridges_streams

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

# 60m buffer best for development and 300m best for tree+shrub cover