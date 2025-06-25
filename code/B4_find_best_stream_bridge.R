

# test whether the cleaned bridge layer or the mostly raw stream X road intersection points are a better predictor of number of road crossings.



library(tidyverse)
library(sf)
library(here)
library(lme4)
library(AICcmodavg)

source(here("code/helper_data.R"))

# tally number of road/stream intersections per segment ----


napa_sonoma_rds_equal_segs_df <- readRDS(here("data/napa_sonoma_rds_equal_segs")) %>%
  bind_rows() %>% 
  st_as_sf()
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
  ungroup() %>% 
  full_join(napa_sonoma_rds_equal_segs_df %>% data.frame() %>% select(seg.label)) %>% 
  mutate(num.creek = replace_na(num.creek, 0))

saveRDS(streams_per_segment, here("data/analysis_inputs/streams_per_segment"))



# tally number of bridges per segment ----
# final_cleaned_bridge_layer from A4_clean_bridges.R
final_cleaned_bridge_layer <- readRDS(here("data/final_cleaned_bridge_layer")) %>% 
  st_as_sf()

# can use a much smaller buffer since seg_midpoints was derived from napa_sonoma_rds_filtered
bridges_buff10 <- final_cleaned_bridge_layer %>% 
  st_buffer(., 10) 



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
  ungroup() %>% 
  full_join(napa_sonoma_rds_equal_segs_df %>% data.frame() %>% select(seg.label)) %>% 
  mutate(num.bridge = replace_na(num.bridge, 0))

saveRDS(bridges_per_segment, here("data/analysis_inputs/bridges_per_segment"))


# compare the bridge vs stream layers for predicting number of crossings per segment ----


# read segment crossing values from A11_sum_segment_crossings.R
# this has just the lion X months that there is real data for
# this is crossings summed for each segmentXmonthXlion combination, only considering naive crossed roads
# the 0s ending to the file name indicates this has the uncrossed segments added back in
monthly_seg_crossings_naive_roads_only_0s <- readRDS(here("data/analysis_inputs/monthly_seg_crossings_naive_roads_only_0s")) %>% 
  data.frame() %>%  
  select(animal.id, year, month, seg.label, which.steps, monthly.seg.wt.crossing, monthly.seg.raw.crossing)


monthly_seg_crossings_naive_roads_only_0s <- monthly_seg_crossings_naive_roads_only_0s %>% 
  right_join(readRDS(here("data/full_lion_year_month_seg"))) %>% 
  right_join(readRDS(here("data/hr_segments_prop_in_developed"))) %>% 
  filter(prop.seg.in.dev50 == 0) %>% 
  data.frame() %>%  
  select(animal.id, year, month, seg.label, which.steps, monthly.seg.wt.crossing, monthly.seg.raw.crossing)




# streams_per_segment and bridges_per_segment have segments outside all home ranges so this join will have NA for animal.id, year, month
bridges_creeks <- full_join(readRDS(here("data/analysis_inputs/streams_per_segment")), 
                                    readRDS(here("data/analysis_inputs/bridges_per_segment")))

# any NA
bridges_creeks %>%
  filter(if_any(everything(), is.na)) %>% nrow()
# good 3/20/25


bridges_creeks_crossings <- bridges_creeks %>% 
  right_join(monthly_seg_crossings_naive_roads_only_0s)

filter(bridges_creeks_crossings, is.na(monthly.seg.raw.crossing)) %>% nrow()
# good June 2025

# save this for the main analysis
saveRDS(bridges_creeks_crossings, here("data/analysis_inputs/bridges_creeks_crossings"))



# some checking for number of records:
# should now be just 1 record for each segment, mt lion, year, month
count(bridges_creeks_crossings, seg.label, animal.id, year, month) %>% filter(n > 1) %>% nrow()
# if nrow = 0, good
count(bridges_creeks_crossings, animal.id, year, month) %>% nrow()
# should still be 299 as of 3/20/25


  # Fit models
bridges_creeks_mods <- list(
    "bridge" = lmer(monthly.seg.raw.crossing ~ num.bridge + offset(log(monthly.seg.wt.crossing + 0.0001)) + (1|animal.id), data = bridges_creeks_crossings, REML = FALSE),
    "creek" = lmer(monthly.seg.raw.crossing ~ num.creek + offset(log(monthly.seg.wt.crossing + 0.0001)) + (1|animal.id), data = bridges_creeks_crossings, REML = FALSE)
  )
  
aictab(bridges_creeks_mods, names(bridges_creeks_mods)) %>% 
    data.frame()



# stream X road intersection points is by far better supported predictor of # road crossings than the cleaned bridge layer (dAICc = 753.979)
# this may be due to the segments with bridges over non-streams
