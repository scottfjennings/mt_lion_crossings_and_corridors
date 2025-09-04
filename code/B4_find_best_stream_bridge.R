

# test whether the cleaned bridge layer or the mostly raw stream X road intersection points are a better predictor of number of road crossings.



library(tidyverse)
library(sf)
library(here)
library(lme4)
library(units)
library(AICcmodavg)

source(here("code/helper_data.R"))

# tally number of road/stream intersections per segment ----


all_analysis_segments <- readRDS(here("data/all_analysis_segments"))

# I created stream_road_Intersect and napa_sonoma_streams in ArcGIS pro by:

# 1. import the CARi stream layer from the Z drive and the final_cleaned_road layer and county boundaries from this project folder 

# 2. first use the county boundaries to trim the stream layer (maybe this not necessary for the intersection, but it made napa_sonoma_streams)
# napa_sonoma_streams <- st_read(here("data/shapefiles/napa_sonoma_streams.shp"))

# 3. get the intersection of napa_sonoma_streams and final_cleaned_road using the Pairwise Intersect tool and setting Output Type to Point

# did this for final_cleaned_road and 
# stream_road_Intersect <- st_read(here("data/shapefiles/stream_road_Intersect.shp"))
# napa_sonoma_rds_equal_segs - using this one b/c it includes seg.label
analysis_seg_riparian_intersection <- st_read(here("data/shapefiles/analysis_seg_riparian_intersection.shp"))


# 9/2/25 trying to do the stream road intersection here in R



analysis_seg_riparian_intersection_df <- analysis_seg_riparian_intersection %>% 
  data.frame() %>% 
  select(name, orig_class, clicklabel, leglabel, seg_lbl, year, -geometry) %>% 
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

analysis_seg_riparian_intersection_df_classification <- analysis_seg_riparian_intersection_df %>% 
  left_join(stream_classifications %>% distinct(orig_class, keep_creek))
# this shouldn't add any rows
nrow(analysis_seg_riparian_intersection_df_classification) == nrow(analysis_seg_riparian_intersection_df)


streams_per_segment <- analysis_seg_riparian_intersection_df_classification %>% 
  filter(keep_creek == TRUE) %>% 
  rename(seg.label = seg_lbl) %>% 
  group_by(seg.label, year) %>% 
  summarise(num.creek = n()) %>% 
  ungroup() %>% 
  full_join(all_analysis_segments %>% data.frame() %>% select(seg.label, year, seg.length, used.available)) %>% 
  mutate(num.creek = replace_na(num.creek, 0),
         seg.length.km = as.numeric(seg.length)/1000,
         num.creek.per.km = num.creek/seg.length.km,
         riparian.binary = as.numeric(num.creek > 0))

saveRDS(streams_per_segment, here("data/analysis_inputs/streams_per_segment"))



# tally number of bridges per segment ----
# final_cleaned_bridge_layer from A4_clean_bridges.R
final_cleaned_bridge_layer <- readRDS(here("data/final_cleaned_bridge_layer")) %>% 
  st_as_sf()

# can use a much smaller buffer since seg_midpoints was derived from napa_sonoma_rds_filtered
bridges_buff10 <- final_cleaned_bridge_layer %>% 
  st_buffer(., 10) 



analysis_segment_bridges <- st_join(bridges_buff10, all_analysis_segments) %>% 
  #separate(seg.label, c("midpoint.label", "leftcity", "road.num", "seg.num"), sep = "_", remove = FALSE) %>% 
  #rowwise() %>% # grepl in the next step doesn't work right without rowwise() but doesn't throw error
  mutate(correct.road = str_detect(seg.label, label)) %>% 
  filter(!is.na(seg.label))

filter(analysis_segment_bridges, correct.road == FALSE) %>% view()

bridges_per_segment <- analysis_segment_bridges %>% 
  filter(correct.road == TRUE) %>% 
  data.frame() %>% 
  select(-geometry) %>% 
  group_by(seg.label, year) %>% 
  summarise(num.bridge = n()) %>% 
  ungroup() %>% 
  full_join(all_analysis_segments %>% data.frame() %>% select(seg.label, year, seg.length, used.available)) %>% 
  mutate(num.bridge = replace_na(num.bridge, 0),
         seg.length.km = as.numeric(seg.length)/1000,
         num.bridge.per.km = num.bridge/seg.length.km,
         bridge.binary = as.numeric(num.bridge > 0))

saveRDS(bridges_per_segment, here("data/analysis_inputs/bridges_per_segment"))


# compare the bridge vs stream layers for predicting number of crossings per segment ----


# read segment crossing values from A10_combine_analysis_segments.R
# this has just the lion X months that there is real data for
# this is crossings summed for each segmentXmonthXlion combination, only considering naive crossed roads
# the 0s ending to the file name indicates this has the uncrossed segments added back in
all_analysis_segments_by_lion <- readRDS(here("data/all_analysis_segments_by_lion"))

summary(all_analysis_segments_by_lion)
all_analysis_segments_by_lion %>% 
  data.frame() %>% 
  count(used.available)


# streams_per_segment and bridges_per_segment have segments outside all home ranges so this join will have NA for animal.id, year, month
bridges_creeks <- full_join(readRDS(here("data/analysis_inputs/streams_per_segment")), 
                                    readRDS(here("data/analysis_inputs/bridges_per_segment"))) %>% 
  select(seg.label, year, riparian.binary, bridge.binary)

# any NA
bridges_creeks %>%
  filter(if_any(everything(), is.na)) %>% nrow()
# good 9/3/25


bridges_creeks_analysis_df <- bridges_creeks %>% 
  full_join(all_analysis_segments_by_lion) %>% 
  data.frame() %>% 
  select(seg.label, animal.id, used.available, bridge.binary, riparian.binary)

# shouldn't add more rows than are in all_analysis_segments_by_lion
nrow(all_analysis_segments_by_lion) == nrow(bridges_creeks_analysis_df)


# some checking for number of records:
# should now be just 1 record for each segment, mt lion, year, month
count(bridges_creeks_analysis_df, seg.label, animal.id, year) %>% filter(n > 1) %>% nrow()
# if nrow = 0, good

  # Fit models
bridges_creeks_mods <- list(
    "bridge" = glmer(used.available ~ bridge.binary + (1|animal.id), data = bridges_creeks_analysis_df, family = binomial(link = "logit")),
    "creek" = glmer(used.available ~ riparian.binary + (1|animal.id), data = bridges_creeks_analysis_df, family = binomial(link = "logit"))
  )
  
aictab(bridges_creeks_mods, names(bridges_creeks_mods)) %>% 
    data.frame()



# stream X road intersection points is by far better supported predictor of # road crossings than the cleaned bridge layer (dAICc = 446.2608)
# this may be due to the segments with bridges over non-streams
