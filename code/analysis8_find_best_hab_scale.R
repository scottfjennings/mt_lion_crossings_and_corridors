
library(tidyverse)
library(here)
library(sf)
library(ctmm)
library(AICcmodavg)

options(scipen = 999)
source(here("code/helper_data.R"))

# read the road segment habitat values masked to each puma's home range
# this has a row for each road segment in each animal's home range, for each year the puma was tracked and each buffer distance (30-300, by 30)
# so this df has many more rows than the others

all_hr_road_habitat <- readRDS(here("data/all_hr_road_habitat")) %>% 
  filter(!animal.id %in% hr_exclude_pumas,
         !seg.label %in% p31_exclude_segments)


all_hr_road_habitat %>% 
  data.frame() %>% 
  select(year, seg.label, animal.id, buff, mean.tre.shr) %>% 
  mutate(buff = paste("b", buff, sep = "_")) %>% 
  pivot_wider(id_cols = c(year, animal.id, seg.label), names_from = buff, values_from = mean.tre.shr)%>% 
  select(-animal.id, -year, -seg.label) %>% 
  plot()

all_hr_road_habitat %>% 
  data.frame() %>% 
  select(year, seg.label, animal.id, buff, mean.dev) %>% 
  mutate(buff = paste("b", buff, sep = "_")) %>% 
  pivot_wider(id_cols = c(year, animal.id, seg.label), names_from = buff, values_from = mean.dev)%>% 
  select(-animal.id, -year, -seg.label) %>% 
  plot()

# similar distance buffers have more similar values of mean.tre.shr and mean.dev, although this is stronger for mean.tre.shr 

# read the BBMM weighted road segment crossing densities
wt_road_crossed_segs <- readRDS(here("data/wt_road_crossed_segs")) %>% 
  filter(!animal.id %in% hr_exclude_pumas,
         !seg.label %in% p31_exclude_segments)


# count the number of raw and weighted crossings per segment
sum_wt_road_crossed_segs <- wt_road_crossed_segs %>% 
  data.frame() %>% 
  select(-geometry) %>% 
  mutate(animal.id = ifelse(animal.id == "P5*", "P5", animal.id)) %>% 
  group_by(year, animal.id, seg.label) %>% 
  summarise(tot.raw.cross = sum(raw.crossing),
            tot.wt.cross = sum(weighted.crossing)) %>% 
  ungroup() 





#
# for summed crossings; selecting the best spatial scale for each predictor ----
ud_hab_sum_wt_crossed_segs_longer <- all_hr_road_habitat %>%
  data.frame() %>%
  select(mean.dev, mean.tre.shr, seg.label, buff, year, animal.id) %>% 
  full_join(sum_wt_road_crossed_segs) %>% 
  mutate(across(c(tot.raw.cross, tot.wt.cross), ~replace_na(., 0))) %>% 
  filter(!is.na(seg.label)) %>% 
  pivot_longer(cols = c("mean.dev", "mean.tre.shr"), names_to = "variable", values_to = "hab.value")

puma_varbs <- ud_hab_sum_wt_crossed_segs_longer %>% 
  distinct(animal.id, variable)


ud_hab_sum_wt_crossed_segs_longest <- ud_hab_sum_wt_crossed_segs_longer %>% 
  pivot_longer(cols = c("tot.raw.cross", "tot.wt.cross"), names_to = "which.cross", values_to = "cross.value")

puma_varbs_cross <- ud_hab_sum_wt_crossed_segs_longest %>% 
  distinct(animal.id, variable, which.cross)

# raw and weighted crossings in the same function ----

fit_scale_mods <- function(zpuma, zvarb, zcross) {
  seg_filt <- ud_hab_sum_wt_crossed_segs_longest %>% 
    filter(animal.id == zpuma, variable == zvarb, which.cross == zcross, buff == 30) %>% 
    distinct(seg.label)
  
  df <- ud_hab_sum_wt_crossed_segs_longest %>% 
    filter(animal.id == zpuma, variable == zvarb, which.cross == zcross) %>% 
    right_join(seg_filt)
  
  scale_mods <- list(
    "mod30" = lm(cross.value ~ hab.value + year, data = filter(df, buff == 30)),
    "mod60" = lm(cross.value ~ hab.value + year, data = filter(df, buff == 60)),
    "mod90" = lm(cross.value ~ hab.value + year, data = filter(df, buff == 90)),
    "mod120" = lm(cross.value ~ hab.value + year, data = filter(df, buff == 120)),
    "mod150" = lm(cross.value ~ hab.value + year, data = filter(df, buff == 150)),
    "mod180" = lm(cross.value ~ hab.value + year, data = filter(df, buff == 180)),
    "mod210" = lm(cross.value ~ hab.value + year, data = filter(df, buff == 210)),
    "mod240" = lm(cross.value ~ hab.value + year, data = filter(df, buff == 240)),
    "mod270" = lm(cross.value ~ hab.value + year, data = filter(df, buff == 270)),
    "mod300" = lm(cross.value ~ hab.value + year, data = filter(df, buff == 300))
  )
  
  scale_aic <- aictab(scale_mods, names(scale_mods)) %>% 
    data.frame() %>% 
    mutate(animal.id = zpuma,
           variable = zvarb,
           which.cross = zcross)
  
  return(scale_aic)
}

system.time(
hab_scale_aic <- pmap_df(list(puma_varbs_cross$animal.id, puma_varbs_cross$variable, puma_varbs_cross$which.cross), fit_scale_mods)
) # 7 sec

all_aic_viewer <- function(zvarb, zcross, zaic = Inf) {
hab_scale_aic %>% 
  filter(variable == zvarb
         , which.cross == zcross
         , Delta_AICc <= zaic
         ) %>% 
  select(Modnames, Delta_AICc, animal.id) %>%
  arrange(Modnames, animal.id) %>% 
  pivot_wider(id_cols = c(animal.id), names_from = Modnames, values_from = Delta_AICc) %>% 
    select(animal.id, mod30, mod60, mod90, mod120, mod150, mod180, mod210, mod240, mod270, mod300)
}

all_aic_summer <- function(zvarb, zcross) {
  hab_scale_aic %>%  
    filter(variable == zvarb, which.cross == zcross) %>% 
    group_by(Modnames) %>% 
    summarise(tot.delta = sum(Delta_AICc)) %>% 
    ungroup() %>% 
    arrange(tot.delta)
}

# "mean.dev" and "tot.raw.cross"
all_aic_viewer("mean.dev", "tot.raw.cross") %>% view()
all_aic_summer("mean.dev", "tot.raw.cross")
# mod300 is most best for mean.dev across all lions


# "mean.dev" and "tot.wt.cross"
all_aic_viewer("mean.dev", "tot.wt.cross") %>% view()
all_aic_summer("mean.dev", "tot.wt.cross")
# mod60 is most best across all lions

# "mean.dev" and "tot.raw.cross"
all_aic_viewer("mean.tre.shr", "tot.raw.cross") %>% view()
all_aic_summer("mean.tre.shr", "tot.raw.cross")
# mod300 is most best across all lions but mod30 is good for a couple lions that mod300 isn't good for so using mod30 and mod300


# "mean.dev" and "tot.raw.cross"
all_aic_viewer("mean.tre.shr", "tot.wt.cross") %>% view()
all_aic_summer("mean.tre.shr", "tot.wt.cross")
# mod30 and mod300 are most best across all lions



# filter the best scales and save ----
habitat_varbs_scales <- ud_hab_sum_wt_crossed_segs_longer %>% 
  filter((variable == "mean.dev" & buff %in% c(60, 300)) | (variable == "mean.tre.shr" & buff %in% c(30, 300))) %>% 
  mutate(variable.buff = paste(str_replace(variable, "mean.", ""), buff, sep = ".")) %>% 
  pivot_wider(id_cols = c(animal.id, seg.label, year, tot.raw.cross, tot.wt.cross), names_from = variable.buff, values_from = hab.value)

saveRDS(habitat_varbs_scales, here("data/habitat_varbs_scales"))

habitat_varbs_scales <- readRDS(here("data/habitat_varbs_scales"))


##########################################################################
###########     NO RUN BELOW HERE
###########     this was an older version of finding the best scale. keeping for now (Sep 2024) but probably fine to delete soon

# first the raw crossings ----

fit_scale_mods_sum_raw <- function(zpuma, zvarb) {
  seg_filt <- ud_hab_sum_wt_crossed_segs_longer %>% 
    filter(animal.id == zpuma, variable == zvarb, buff == 30) %>% 
    distinct(seg.label)
  
  df <- ud_hab_sum_wt_crossed_segs_longer %>% 
    filter(animal.id == zpuma, variable == zvarb) %>% 
    right_join(seg_filt)
  
  scale_mods <- list(
    "mod30" = lm(tot.raw.cross ~ hab.value + year, data = filter(df, buff == 30)),
    "mod60" = lm(tot.raw.cross ~ hab.value + year, data = filter(df, buff == 60)),
    "mod90" = lm(tot.raw.cross ~ hab.value + year, data = filter(df, buff == 90)),
    "mod120" = lm(tot.raw.cross ~ hab.value + year, data = filter(df, buff == 120)),
    "mod150" = lm(tot.raw.cross ~ hab.value + year, data = filter(df, buff == 150)),
    "mod180" = lm(tot.raw.cross ~ hab.value + year, data = filter(df, buff == 180)),
    "mod210" = lm(tot.raw.cross ~ hab.value + year, data = filter(df, buff == 210)),
    "mod240" = lm(tot.raw.cross ~ hab.value + year, data = filter(df, buff == 240)),
    "mod270" = lm(tot.raw.cross ~ hab.value + year, data = filter(df, buff == 270)),
    "mod300" = lm(tot.raw.cross ~ hab.value + year, data = filter(df, buff == 300))
  )
  
  scale_aic <- aictab(scale_mods, names(scale_mods)) %>% 
    data.frame() %>% 
    mutate(animal.id = zpuma,
           variable = zvarb)
  
  return(scale_aic)
}

summed_raw_hab_scale_aic <- map2_df(puma_varbs$animal.id, puma_varbs$variable, fit_scale_mods_sum_raw)



summed_raw_hab_scale_aic %>% 
  filter(variable == "mean.dev", !animal.id %in% hr_exclude_pumas, Delta_AICc <= 10) %>% 
  select(Modnames, Delta_AICc, animal.id) %>%
  arrange(Modnames, animal.id) %>% 
  pivot_wider(id_cols = c(animal.id), names_from = Modnames, values_from = Delta_AICc) %>% 
  view()

summed_raw_hab_scale_aic %>% 
  filter(Delta_AICc <= 2, variable == "mean.dev") %>% 
  count(Modnames)

summed_raw_hab_scale_aic %>%  filter(variable == "mean.dev", Modnames %in% c("mod90", "mod120")) %>% group_by(Modnames) %>% summarise(tot.delta = sum(Delta_AICc))

# mod120 is most good across all pumas for % development


summed_raw_hab_scale_aic %>% 
  filter(variable == "mean.tre.shr", 
         !animal.id %in% hr_exclude_pumas#, 
         #         Delta_AICc <= 10
  ) %>% 
  select(Modnames, Delta_AICc, animal.id) %>%
  arrange(Modnames, animal.id) %>% 
  pivot_wider(id_cols = c(animal.id), names_from = Modnames, values_from = Delta_AICc) %>% 
  view()

summed_raw_hab_scale_aic %>%  filter(variable == "mean.tre.shr", 
                                     #                                   Modnames %in% c("mod30", "mod210", "mod240", "mod270", "mod300"),
                                     !animal.id %in% hr_exclude_pumas) %>% 
  group_by(Modnames) %>% 
  summarise(tot.delta = sum(Delta_AICc)) %>% 
  ungroup() %>% 
  arrange(tot.delta)

summed_raw_hab_scale_aic %>% 
  filter(Delta_AICc <= 2, variable == "mean.dev") %>% 
  count(Modnames)

# mod30 and mod300 are most good for tree/shrub cover

# then the weighted crossings ----

fit_scale_mods <- function(zpuma, zvarb) {
  seg_filt <- ud_hab_sum_wt_crossed_segs_longer %>% 
    filter(animal.id == zpuma, variable == zvarb, buff == 30) %>% 
    distinct(seg.label)
  
  df <- ud_hab_sum_wt_crossed_segs_longer %>% 
    filter(animal.id == zpuma, variable == zvarb) %>% 
    right_join(seg_filt)
  
  scale_mods <- list(
    "mod30" = lm(tot.wt.cross ~ hab.value + year, data = filter(df, buff == 30)),
    "mod60" = lm(tot.wt.cross ~ hab.value + year, data = filter(df, buff == 60)),
    "mod90" = lm(tot.wt.cross ~ hab.value + year, data = filter(df, buff == 90)),
    "mod120" = lm(tot.wt.cross ~ hab.value + year, data = filter(df, buff == 120)),
    "mod150" = lm(tot.wt.cross ~ hab.value + year, data = filter(df, buff == 150)),
    "mod180" = lm(tot.wt.cross ~ hab.value + year, data = filter(df, buff == 180)),
    "mod210" = lm(tot.wt.cross ~ hab.value + year, data = filter(df, buff == 210)),
    "mod240" = lm(tot.wt.cross ~ hab.value + year, data = filter(df, buff == 240)),
    "mod270" = lm(tot.wt.cross ~ hab.value + year, data = filter(df, buff == 270)),
    "mod300" = lm(tot.wt.cross ~ hab.value + year, data = filter(df, buff == 300))
  )
  
  scale_aic <- aictab(scale_mods, names(scale_mods)) %>% 
    data.frame() %>% 
    mutate(animal.id = zpuma,
           variable = zvarb)
  
  return(scale_aic)
}

all_puma_hab_scale_aic <- map2_df(puma_varbs$animal.id, puma_varbs$variable, fit_scale_mods)



all_puma_hab_scale_aic %>% 
  filter(variable == "mean.dev", !animal.id %in% hr_exclude_pumas, Delta_AICc <= 10) %>% 
  select(Modnames, Delta_AICc, animal.id) %>%
  arrange(Modnames, animal.id) %>% 
  pivot_wider(id_cols = c(animal.id), names_from = Modnames, values_from = Delta_AICc) %>% 
  view()

all_puma_hab_scale_aic %>% 
  filter(Delta_AICc <= 2, variable == "mean.dev") %>% 
  count(Modnames)

all_puma_hab_scale_aic %>%  filter(variable == "mean.dev", Modnames %in% c("mod90", "mod120")) %>% group_by(Modnames) %>% summarise(tot.delta = sum(Delta_AICc))

# mod120 is most good across all pumas for % development


all_puma_hab_scale_aic %>% 
  filter(variable == "mean.tre.shr", 
         !animal.id %in% hr_exclude_pumas#, 
         #         Delta_AICc <= 10
  ) %>% 
  select(Modnames, Delta_AICc, animal.id) %>%
  arrange(Modnames, animal.id) %>% 
  pivot_wider(id_cols = c(animal.id), names_from = Modnames, values_from = Delta_AICc) %>% 
  view()

all_puma_hab_scale_aic %>%  filter(variable == "mean.tre.shr", 
                                   #                                   Modnames %in% c("mod30", "mod210", "mod240", "mod270", "mod300"),
                                   !animal.id %in% hr_exclude_pumas) %>% 
  group_by(Modnames) %>% 
  summarise(tot.delta = sum(Delta_AICc)) %>% 
  ungroup() %>% 
  arrange(tot.delta)


all_puma_hab_scale_aic %>% 
  filter(Delta_AICc <= 2, variable == "mean.dev") %>% 
  count(Modnames)

# mod30 and mod300 are most good for tree/shrub cover





# finding best scale for un-summed crossings ----

ud_hab_wt_crossed_segs_longer <- all_hr_road_habitat %>%
  data.frame() %>%
  select(mean.dev, mean.tre.shr, seg.label, buff, year, animal.id) %>% 
  full_join(wt_road_crossed_segs %>% 
              data.frame() %>% 
              select(-contains("seg.length"), -geometry)) %>% 
  mutate(across(c(raw.crossing, weighted.crossing), ~replace_na(., 0)),
         label = ifelse(is.na(label), str_extract(seg.label, "^[^_]+(?=_)"), label)) %>% 
  filter(!is.na(seg.label)) %>% 
  pivot_longer(cols = c("mean.dev", "mean.tre.shr"), names_to = "variable", values_to = "hab.value")


puma_varbs <- ud_hab_wt_crossed_segs_longer %>% 
  distinct(animal.id, variable)


# first raw crossings ----
fit_scale_mods_raw <- function(zpuma, zvarb) {
  seg_filt <- ud_hab_wt_crossed_segs_longer %>% 
    filter(animal.id == zpuma, variable == zvarb, buff == 30) %>% 
    distinct(seg.label)
  
  df <- ud_hab_wt_crossed_segs_longer %>% 
    filter(animal.id == zpuma, variable == zvarb) %>% 
    right_join(seg_filt)
  
  scale_mods <- list(
    "mod30" = lm(raw.crossing ~ hab.value + year, data = filter(df, buff == 30)),
    "mod60" = lm(raw.crossing ~ hab.value + year, data = filter(df, buff == 60)),
    "mod90" = lm(raw.crossing ~ hab.value + year, data = filter(df, buff == 90)),
    "mod120" = lm(raw.crossing ~ hab.value + year, data = filter(df, buff == 120)),
    "mod150" = lm(raw.crossing ~ hab.value + year, data = filter(df, buff == 150)),
    "mod180" = lm(raw.crossing ~ hab.value + year, data = filter(df, buff == 180)),
    "mod210" = lm(raw.crossing ~ hab.value + year, data = filter(df, buff == 210)),
    "mod240" = lm(raw.crossing ~ hab.value + year, data = filter(df, buff == 240)),
    "mod270" = lm(raw.crossing ~ hab.value + year, data = filter(df, buff == 270)),
    "mod300" = lm(raw.crossing ~ hab.value + year, data = filter(df, buff == 300))
  )
  
  scale_aic <- aictab(scale_mods, names(scale_mods)) %>% 
    data.frame() %>% 
    mutate(animal.id = zpuma,
           variable = zvarb)
  
  return(scale_aic)
}

raw_hab_scale_aic <- map2_df(puma_varbs$animal.id, puma_varbs$variable, fit_scale_mods_raw)



raw_hab_scale_aic %>% 
  filter(variable == "mean.dev", !animal.id %in% hr_exclude_pumas, Delta_AICc <= 10) %>% 
  select(Modnames, Delta_AICc, animal.id) %>%
  arrange(Modnames, animal.id) %>% 
  pivot_wider(id_cols = c(animal.id), names_from = Modnames, values_from = Delta_AICc) %>% 
  view()

raw_hab_scale_aic %>% 
  filter(Delta_AICc <= 2, variable == "mean.dev") %>% 
  count(Modnames)

raw_hab_scale_aic %>%  filter(variable == "mean.dev", Modnames %in% c("mod90", "mod120")) %>% group_by(Modnames) %>% summarise(tot.delta = sum(Delta_AICc))

# mod120 is most good across all pumas for % development


all_puma_hab_scale_aic %>% 
  filter(variable == "mean.tre.shr", 
         !animal.id %in% hr_exclude_pumas#, 
         #         Delta_AICc <= 10
  ) %>% 
  select(Modnames, Delta_AICc, animal.id) %>%
  arrange(Modnames, animal.id) %>% 
  pivot_wider(id_cols = c(animal.id), names_from = Modnames, values_from = Delta_AICc) %>% 
  view()

all_puma_hab_scale_aic %>%  filter(variable == "mean.tre.shr", 
                                   #                                   Modnames %in% c("mod30", "mod210", "mod240", "mod270", "mod300"),
                                   !animal.id %in% hr_exclude_pumas) %>% 
  group_by(Modnames) %>% 
  summarise(tot.delta = sum(Delta_AICc)) %>% 
  ungroup() %>% 
  arrange(tot.delta)


all_puma_hab_scale_aic %>% 
  filter(Delta_AICc <= 2, variable == "mean.dev") %>% 
  count(Modnames)

# mod30 and mod300 are most good for tree/shrub cover

# then weighted crossings ----
fit_scale_mods_wt <- function(zpuma, zvarb) {
  seg_filt <- ud_hab_wt_crossed_segs_longer %>% 
    filter(animal.id == zpuma, variable == zvarb, buff == 30) %>% 
    distinct(seg.label)
  
  df <- ud_hab_wt_crossed_segs_longer %>% 
    filter(animal.id == zpuma, variable == zvarb) %>% 
    right_join(seg_filt)
  
  scale_mods <- list(
    "mod30" = lm(weighted.crossing ~ hab.value + year, data = filter(df, buff == 30)),
    "mod60" = lm(weighted.crossing ~ hab.value + year, data = filter(df, buff == 60)),
    "mod90" = lm(weighted.crossing ~ hab.value + year, data = filter(df, buff == 90)),
    "mod120" = lm(weighted.crossing ~ hab.value + year, data = filter(df, buff == 120)),
    "mod150" = lm(weighted.crossing ~ hab.value + year, data = filter(df, buff == 150)),
    "mod180" = lm(weighted.crossing ~ hab.value + year, data = filter(df, buff == 180)),
    "mod210" = lm(weighted.crossing ~ hab.value + year, data = filter(df, buff == 210)),
    "mod240" = lm(weighted.crossing ~ hab.value + year, data = filter(df, buff == 240)),
    "mod270" = lm(weighted.crossing ~ hab.value + year, data = filter(df, buff == 270)),
    "mod300" = lm(weighted.crossing ~ hab.value + year, data = filter(df, buff == 300))
  )
  
  scale_aic <- aictab(scale_mods, names(scale_mods)) %>% 
    data.frame() %>% 
    mutate(animal.id = zpuma,
           variable = zvarb)
  
  return(scale_aic)
}

wt_hab_scale_aic <- map2_df(puma_varbs$animal.id, puma_varbs$variable, fit_scale_mods_wt)



wt_hab_scale_aic %>% 
  filter(variable == "mean.dev", !animal.id %in% hr_exclude_pumas, Delta_AICc <= 10) %>% 
  select(Modnames, Delta_AICc, animal.id) %>%
  arrange(Modnames, animal.id) %>% 
  pivot_wider(id_cols = c(animal.id), names_from = Modnames, values_from = Delta_AICc) %>% 
  view()

wt_hab_scale_aic %>% 
  filter(Delta_AICc <= 2, variable == "mean.dev") %>% 
  count(Modnames)

wt_hab_scale_aic %>%  filter(variable == "mean.dev", Modnames %in% c("mod90", "mod120")) %>% group_by(Modnames) %>% summarise(tot.delta = sum(Delta_AICc))

# mod120 is most good across all pumas for % development


wt_hab_scale_aic %>% 
  filter(variable == "mean.tre.shr", 
         !animal.id %in% hr_exclude_pumas#, 
         #         Delta_AICc <= 10
  ) %>% 
  select(Modnames, Delta_AICc, animal.id) %>%
  arrange(Modnames, animal.id) %>% 
  pivot_wider(id_cols = c(animal.id), names_from = Modnames, values_from = Delta_AICc) %>% 
  view()

wt_hab_scale_aic %>%  filter(variable == "mean.tre.shr", 
                                   #                                   Modnames %in% c("mod30", "mod210", "mod240", "mod270", "mod300"),
                                   !animal.id %in% hr_exclude_pumas) %>% 
  group_by(Modnames) %>% 
  summarise(tot.delta = sum(Delta_AICc)) %>% 
  ungroup() %>% 
  arrange(tot.delta)


wt_hab_scale_aic %>% 
  filter(Delta_AICc <= 2, variable == "mean.dev") %>% 
  count(Modnames)

# mod30 and mod300 are most good for tree/shrub cover


# filter the best scales and save
habitat_varbs_scales <- ud_hab_wt_crossed_segs_longer %>% 
  filter((variable == "mean.dev" & buff == 120) | (variable == "mean.tre.shr" & buff %in% c(30, 300))) %>% 
  mutate(variable.buff = paste(str_replace(variable, "mean.", ""), buff, sep = ".")) %>% 
  pivot_wider(id_cols = c(animal.id, seg.label, year, tot.raw.cross, tot.wt.cross), names_from = variable.buff, values_from = hab.value)

saveRDS(habitat_varbs_scales, here("data/habitat_varbs_scales"))

habitat_varbs_scales <- readRDS(here("data/habitat_varbs_scales"))
