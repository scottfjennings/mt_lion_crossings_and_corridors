
library(tidyverse)
library(here)
library(sf)
library(ctmm)
library(AICcmodavg)

options(scipen = 999)

# read the road segment habitat values masked to each puma's home range
ud_rd_seg_habitats <- readRDS(here("data/ud_rd_seg_habitats")) %>% 
  rename(mean.dev = mead.dev)

# read the BBMM weighted road segment crossing densities
wt_road_crossed_segs <- readRDS(here("data/wt_road_crossed_segs"))






sum_wt_road_crossed_segs <- wt_road_crossed_segs %>% 
  data.frame() %>% 
  select(-geometry) %>% 
  #filter(!is.na(year)) %>% 
  #full_join(year_seg) %>% 
  #mutate(raw.crossing = replace_na(raw.crossing, 0),
  #       weighted.crossing = replace_na(weighted.crossing, 0)) %>% 
  mutate(animal.id = ifelse(animal.id == "P5*", "P5", animal.id)) %>% 
  group_by(year, animal.id, seg.label) %>% 
  summarise(tot.raw.cross = sum(raw.crossing),
            tot.wt.cross = sum(weighted.crossing)) %>% 
  ungroup() 



ud_hab_wt_crossed_segs <- ud_rd_seg_habitats %>%
  data.frame() %>%
  select(mean.dev, mean.tre.shr, seg.label, buff, year, animal.id) %>% 
  full_join(sum_wt_road_crossed_segs) %>% 
  mutate(across(c(tot.raw.cross, tot.wt.cross), ~replace_na(., 0))) %>% 
  filter(!is.na(seg.label))


ud_hab_wt_crossed_segs_longer <- ud_hab_wt_crossed_segs %>% 
  pivot_longer(cols = c("mean.dev", "mean.tre.shr"), names_to = "variable", values_to = "hab.value")

# selecting the best spatial scale for each predictor ----

puma_varbs <- ud_hab_wt_crossed_segs_longer %>% 
  distinct(animal.id, variable)

fit_scale_mods <- function(zpuma, zvarb) {
  seg_filt <- ud_hab_wt_crossed_segs_longer %>% 
    filter(animal.id == zpuma, variable == zvarb, buff == 30) %>% 
    distinct(seg.label)
  
  df <- ud_hab_wt_crossed_segs_longer %>% 
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

hr_exclude_pumas <- c("P36", #funky locations, in the ocean
                      "P14", # many points outside Sonoma/Napa
                      "P37", # not totally range resident and 100% HR covers much of sonoma 
                      "P19" # many points outside Sonoma/Napa
)

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

# mod120 is most good across all pumas


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

# mod30 and mod300 are most good








study_area_counties <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/data/study_area_counties")  %>% 
  filter(NAME %in% c("Napa", "Sonoma")) %>% 
  dplyr::select(NAME) %>% 
  st_transform(crs = 26910)


analysis_table <- readRDS(here("data/analysis_table")) %>% 
  mutate(animal.id = ifelse(animal.id == "P5*", "P5", animal.id)) %>% 
  dplyr::select(individual.local.identifier = animal.id, timestamp = datetime.local, location.long = longitude,
                location.lat = latitude) %>% 
  arrange(individual.local.identifier, timestamp) %>% 
  group_by(individual.local.identifier) %>% 
  mutate(zt = timestamp - lag(timestamp)) %>% 
  ungroup() %>% 
  filter(zt > 3600)

HR_UDS <- readRDS(here("model_objects/puma_hr_uds"))


kitty_plotter <- function(kitty) {
  zz <- as.sf(HR_UDS[[kitty]], DF = "PDF", level.UD = 0.999) %>% 
    st_transform(crs = 26910)
  
  yy <- analysis_table %>% 
    filter(individual.local.identifier == kitty) %>% 
    st_as_sf(x = .,
             coords = c("location.long", "location.lat"),
             crs = 4326)%>% 
    st_transform(crs = 26910)
  
  ggplot() +
    geom_sf(data = study_area_counties) +
    geom_sf(data = zz, fill = NA) +
    geom_sf(data = yy)
}

kitty_plotter("P13")


habitat_varbs_scales <- ud_hab_wt_crossed_segs_longer %>% 
  filter((variable == "mean.dev" & buff == 120) | (variable == "mean.tre.shr" & buff %in% c(30, 300))) %>% 
  mutate(variable.buff = paste(str_replace(variable, "mean.", ""), buff, sep = ".")) %>% 
  pivot_wider(id_cols = c(animal.id, seg.label, year, tot.raw.cross, tot.wt.cross), names_from = variable.buff, values_from = hab.value)



