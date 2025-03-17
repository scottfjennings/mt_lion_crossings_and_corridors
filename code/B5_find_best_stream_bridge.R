

library(tidyverse)
library(sf)
library(here)


# tally number of road/stream intersections per segment ----
# I created stream_road_Intersect and napa_sonoma_streams in ArcGIS pro by:

# 1. import the CARi stream layer from the Z drive and the final_cleaned_road layer and county boundaries from this project folder 

# 2. first use the county boundaries to trim the stream layer (maybe this not necessary for the intersection, but it made napa_sonoma_streams)
# napa_sonoma_streams <- st_read(here("data/shapefiles/napa_sonoma_streams.shp"))

# 3. get the intersection of napa_sonoma_streams and final_cleaned_road using the Pairwise Intersect tool and setting Output Type to Point

# did this for final_cleaned_road and 
# stream_road_Intersect <- st_read(here("data/shapefiles/stream_road_Intersect.shp"))
# napa_sonoma_rds_equal_segs - using this one b/c it includes seg.label
stream_segment_Intersect <- st_read(here("data/shapefiles/stream_segment_Intersect.shp"))


stream_segment_Intersect_df <- stream_segment_Intersect %>% 
  data.frame() %>% 
  select(name, orig_class, source_dat, clicklabel, leglabel, seg_lbl, -geometry) %>% 
  mutate(orig_class = str_replace(orig_class, "Stream/River: Hydrographic Category = ", ""))



# count(stream_segment_Intersect_df, orig_class, source_dat) %>% arrange(source_dat) %>% write.csv(here("data/stream_classification.csv"), row.names = FALSE)           
# then get chat to fill in definitions

# SQL query for these classifications to use in Arc on the original orig_class from stream_segment_Intersect.shp
# orig_class = 'FC' Or orig_class = 'C' Or orig_class = 'TC' Or orig_class = 'Canal/Ditch' Or orig_class = 'Connector' Or orig_class = 'PEMF' Or orig_class = 'PEMH' Or orig_class = 'PEMA' Or  orig_class = 'PEMC' Or orig_class = 'PSSC' Ororig_class = 'PSSA' Ororig_class = 'PFOA' Ororig_class = 'PFOC' Ororig_class = 'R2UBH' Or orig_class = 'R3UBH' Or orig_class = 'R4SBC' Or orig_class = 'FD' Or orig_class = 'SD' Or  orig_class = 'Artificial Path' Or orig_class LIKE '%Perennial%' Or orig_class LIKE '%Intermittent%'


keep_class = c("FC", "C", "TC", "TD", "PEMCx", "Canal/Ditch", "Connector", "PEMF", "PEMH", "PEMA",  
               "PEMC", "PSSC", "PSSA", "PFOA", "PFOC", "R2UBH", "R3UBH", "R4SBC",  
               "FD", "SD", "Artificial Path", "Perennial", "Intermittent")

stream_classifications <- read.csv(here("data/stream_classification_updated.csv")) %>% 
  mutate(keep_creek = orig_class %in% keep_class)

streams_per_segment <- stream_segment_Intersect_df %>% 
  full_join(stream_classifications) %>% 
  filter(keep_creek == TRUE) %>% 
  rename(seg.label = seg_lbl) %>% 
  group_by(seg.label) %>% 
  summarise(num.creek = n()) %>% 
  ungroup()

saveRDS(streams_per_segment, here("data/analysis_inputs/streams_per_segment"))



# tally number of bridges per segment ----
# final_cleaned_bridge_layer from A4_clean_bridges.R
final_cleaned_bridge_layer <- readRDS(here("data/final_cleaned_bridge_layer")) %>% 
  st_as_sf()

# can use a much smaller buffer since seg_midpoints was derived from napa_sonoma_rds_filtered
bridges_buff10 <- final_cleaned_bridge_layer %>% 
  st_buffer(., 10) 

napa_sonoma_rds_equal_segs <- readRDS(here("data/napa_sonoma_rds_equal_segs"))

napa_sonoma_rds_equal_segs_df <- napa_sonoma_rds_equal_segs %>%
  bind_rows() %>% 
  st_as_sf()


seg_bridges <- st_join(bridges_buff10, napa_sonoma_rds_equal_segs_df) %>% 
  #separate(seg.label, c("midpoint.label", "leftcity", "road.num", "seg.num"), sep = "_", remove = FALSE) %>% 
  rowwise() %>% # grepl in the next step doesn't work right without rowwise() but doesn't throw error
  mutate(correct.road = label.x == label.y)

filter(seg_bridges, correct.road == FALSE) %>% view()

bridges_per_segment <- seg_bridges %>% 
  filter(correct.road == TRUE) %>% 
  data.frame() %>% 
  select(-geometry) %>% 
  group_by(seg.label) %>% 
  summarise(num.bridge = n()) %>% 
  ungroup()

saveRDS(bridges_per_segment, here("data/bridges_per_segment"))


# compare the bridge vs stream layers for predicting number of crossings per segment

seg_crossing_sums <- readRDS(here("data/analysis_inputs/seg_crossing_sums_naive_roads_only"))


#
# for summed crossings; selecting the best spatial scale for each predictor ----
bridge_creek_crossings <- full_join(streams_per_segment, bridges_per_segment) %>% 
  full_join(seg_crossing_sums) %>% 
  mutate(across(c(seg.raw.crossing, seg.wt.crossing, num.creek, num.bridge), ~replace_na(., 0))) %>% 
  filter(!is.na(seg.label), !is.na(animal.id)) 

# create helper dfs for looping models
puma_varbs <- hab_seg_crossing_sums_longer %>% 
  distinct(animal.id, variable)


hab_seg_crossing_sums_longest <- hab_seg_crossing_sums_longer %>% 
  pivot_longer(cols = c("seg.raw.crossing", "seg.wt.crossing"), names_to = "which.cross", values_to = "cross.value")

puma_varbs_cross <- hab_seg_crossing_sums_longest %>% 
  distinct(animal.id, variable, which.cross)

# raw and weighted crossings in the same function ----

fit_scale_mods <- function(zpuma, zvarb, zcross) {
  seg_filt <- hab_seg_crossing_sums_longest %>% 
    filter(animal.id == zpuma, variable == zvarb, which.cross == zcross, buff == 30) %>% 
    distinct(seg.label)
  
  df <- hab_seg_crossing_sums_longest %>% 
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
) # 7 sec, 9

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
    summarise(tot.delta = sum(Delta_AICc),
              sd.delta = sd(Delta_AICc)) %>% 
    ungroup() %>% 
    arrange(tot.delta)
}

# "mean.dev" and "seg.raw.crossing"
all_aic_viewer("mean.dev", "seg.raw.crossing") %>% view()
all_aic_summer("mean.dev", "seg.raw.crossing")
# mod60 is most best for mean.dev across all lions


# "mean.dev" and "seg.wt.crossing"
all_aic_viewer("mean.dev", "seg.wt.crossing") %>% view()
all_aic_summer("mean.dev", "seg.wt.crossing")
# mod60 is most best across all lions


