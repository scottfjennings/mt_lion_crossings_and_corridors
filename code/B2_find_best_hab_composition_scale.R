
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




########## will ultimately compare results from all three filter levels ##########
# seg_crossing_sums <- readRDS(here("data/analysis_inputs/seg_crossing_sums_naive_segs_only")) # more strictly filtered crossings
seg_crossing_sums_naive_roads_only <- readRDS(here("data/analysis_inputs/seg_crossing_sums_naive_roads_only")) %>% 
  filter(!animal.id %in% hr_exclude_pumas)


seg_crossing_sums_naive_roads_only_hab <- all_hr_road_habitat_df %>% 
  full_join(seg_crossing_sums_naive_roads_only) %>% 
  mutate(crossed.bin = as.numeric(seg.wt.crossing > 0)) %>% 
  filter(!animal.id %in% few_crossings_pumas) %>% 
  left_join(seg_midpoints) %>% 
  st_as_sf()


# couple plots to check data ----
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


all_hr_road_habitat_df %>% 
  distinct(year, seg.label, buff, mean.dev, mean.imperv) %>% 
  ggplot() +
  geom_point(aes(x = mean.dev, y = mean.imperv/100)) +
  geom_abline() +
  facet_wrap(~buff)


# similar distance buffers have more similar values of mean.tre.shr and mean.dev, although this is stronger for mean.tre.shr 

# read the summed segment crossings
# for now using the moderately filtered crossings, those that consider only the naively crossed road within the BBMM


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
