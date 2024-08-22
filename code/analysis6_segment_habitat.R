

library(tidyverse)
library(here)
library(sf)
library(terra)
library(units)
library(ctmm)

options(scipen = 999)

#source(here("code/utilities.R"))


study_area_counties <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/data/study_area_counties")  %>% 
  filter(NAME %in% c("Napa", "Sonoma")) %>% 
  select(NAME) %>% 
  st_transform(crs = 26910)


step_years <- readRDS(here("data/crossing_clusters_gps_1step")) %>% 
  mutate(year = year(datetime.local)) %>% 
  distinct(crossing.step, year)



bbmm_road_slices <- readRDS(here("model_objects/crossed_bbmm_roads_1step"))  %>% 
  bind_rows(., .id = "crossing.step") %>% 
  full_join(step_years) #%>% 
  #group_by(crossing.step, label) %>% 
  #summarise() %>% 
  #ungroup() %>% 
  #mutate(full.crossed.length = st_length(.))

bbmm_road_slices_buff100 <- bbmm_road_slices %>% 
  full_join(step_years) %>% 
  st_buffer(., 100, endCapStyle = "FLAT")

ggplot() +
  geom_sf(data = bbmm_road_slices_buff100[1,]) +
  geom_sf(data = bbmm_road_slices[1,])


# roads
napa_sonoma_rds_equal_segs <- readRDS(here("data/napa_sonoma_rds_equal_segs")) %>% 
  bind_rows() %>% 
  rename("road.seg.length" = seg.length)

napa_sonoma_rds_equal_segs_buff100 <- st_buffer(napa_sonoma_rds_equal_segs, 100, endCapStyle = "FLAT")

rds_seg_lengths <- napa_sonoma_rds_equal_segs %>% 
  data.frame() %>% 
  select(seg.label, road.seg.length)

# habitat layers ----
# first, mask habitat layers to just sonoma and napa counties for faster extraction at road segment buffers ----

mask_habitat <- function(zyear) {
  zhab = rast(paste("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/Harvey_north bay habitat/RSF_Layers_", zyear, "_2024-03-19.TIF", sep = ""))
  
  zhab_counties <- mask(zhab, study_area_counties)

  writeRaster(zhab_counties, here(paste("data/sonoma_napa_usda_rap/hr_uds_usda_rap_", zyear, ".tif", sep = "")))
}

mask_habitat(2016)

map(seq(2017, 2023), mask_habitat)

ggplot() +
  tidyterra::geom_spatraster(data = zhab, aes(fill = Development))




# next, extract habitat values at multiple scales for each year ----
#' get_rd_habitat
#' 
#' extract tree and shrub cover values for a given year in polygons defined by a given buffer around road segments.
#'
#' @param zyear the desired year
#' @param zbuff the desired buffer around
#'
#' @return
#' @details
#' need to define zroad as the desired road layer before calling this function. 
#' 
#'
#' @examples
get_rd_habitat <- function(zyear, zbuff) { 
  
zhab <- rast(here(paste("data/sonoma_napa_usda_rap/sonoma_napa_usda_rap_", zyear, ".tif", sep = "")))
  
zroad_buff <- zroad %>% 
  #filter(year == zyear) %>% 
  st_buffer(., zbuff, endCapStyle = "FLAT")

  tre_ext <- extract(zhab, zroad_buff, layer = "TRE") %>% 
    mutate(zindex = row_number()) %>% 
    pivot_wider(id_cols = c(ID, zindex), names_from = layer, values_from = value)
                                            
  shr_ext <- extract(zhab, zroad_buff, layer = "SHR") %>% 
    mutate(zindex = row_number()) %>% 
    pivot_wider(id_cols = c(ID, zindex), names_from = layer, values_from = value)
  
  
  dev_ext <- extract(zhab, zroad_buff, layer = "Development")
  dev_ext <- dev_ext %>% 
    mutate(zindex = row_number()) %>% 
    pivot_wider(id_cols = c(ID, zindex), names_from = layer, values_from = value)
  
tre_shr_ext <- full_join(tre_ext, shr_ext) %>% 
  full_join(dev_ext) %>% 
  select(-zindex) %>% 
  mutate(tre.shr = TRE + SHR)

rds_buff_mean_tre_shr <- tre_shr_ext %>% 
  group_by(ID) %>% 
  summarise(mead.dev = mean(Development),
            mean.tre = mean(TRE),
            mean.shr = mean(SHR),
            mean.tre.shr = mean(tre.shr),
            num.cell = n()) %>% 
  ungroup()%>% 
  full_join(zroad_buff %>% mutate(ID = row_number())) %>% 
  st_as_sf() %>% 
  mutate(buff = zbuff,
         year = zyear)
}

zz <- get_rd_habitat(2016, 30)

years_buffs <- expand.grid(year = seq(2016, 2023),
                           buffs = seq(30, 300, by = 30))


system.time(
  all_rd_seg_habitats <- map2(years_buffs$year, years_buffs$buffs, get_rd_habitat, .progress = TRUE) %>% 
  list_rbind()
)

xyear <- rep(years_buffs$year, each = 2309)
all_rd_seg_habitats <- all_rd_seg_habitats %>% 
  bind_cols(year = xyear)

saveRDS(all_rd_seg_habitats, here("data/all_rd_seg_habitats"))


# filter road segment habitat rasters to each puma's home range ----


puma_years <- step_years %>% 
  mutate(puma = str_extract(crossing.step, "^[^_]+(?=_)"),
         puma = ifelse(puma == "P5*", "P5", puma)) %>% 
  ungroup() %>% 
  distinct(puma, year)

get_ud_rd_seg_habitat <- function(zpuma, zyear) {
hr_uds <- readRDS(here("model_objects/puma_hr_uds"))[[zpuma]] %>% 
  as.sf(., DF = "PDF", level.UD = 0.999) %>% 
  st_transform(crs = 26910) %>% 
  filter(str_detect(name, "est"))


all_rd_seg_habitats <- readRDS(here("data/all_rd_seg_habitats")) %>% 
  filter(year == zyear) %>% 
  st_as_sf()
  
ud_rd_seg_habitat <- st_intersection(hr_uds, all_rd_seg_habitats) %>% 
  mutate(animal.id = zpuma)
}

system.time(
ud_rd_seg_habitats <- map2_df(puma_years$puma, puma_years$year, get_ud_rd_seg_habitat)
)

saveRDS(ud_rd_seg_habitats, here("data/ud_rd_seg_habitats"))

ggplot() +
#  geom_sf(data = study_area_counties, fill = NA) +
  geom_sf(data = hr_uds, fill = NA) +
  geom_sf(data = ud_rd_seg_habitat, aes(color = mean.tre.shr))


# selecting the best spatial scale for each predictor ----


all_rd_seg_habitats <- readRDS(here("data/all_rd_seg_habitats")) %>% 
  data.frame() %>% 
  select(mean.dev = mead.dev, mean.tre.shr, label, seg.label, year, buff, road.seg.length, -geometry) 

#year_seg <- bbmm_slice_habitats %>% 
#  distinct(label, seg.label, year, road.seg.length)

wt_road_crossed_segs <- readRDS(here("data/wt_road_crossed_segs"))

sum_wt_road_crossed_segs <- wt_road_crossed_segs %>% 
  data.frame() %>% 
  select(-geometry) %>% 
  #filter(!is.na(year)) %>% 
  #full_join(year_seg) %>% 
  #mutate(raw.crossing = replace_na(raw.crossing, 0),
  #       weighted.crossing = replace_na(weighted.crossing, 0)) %>% 
  mutate(puma = ifelse(puma == "P5*", "P5", puma)) %>% 
  group_by(year, puma, seg.label) %>% 
  summarise(tot.raw.cross = sum(raw.crossing),
            tot.wt.cross = sum(weighted.crossing)) %>% 
  ungroup() 

pumas <- distinct(sum_wt_road_crossed_segs, puma)

all_rd_seg_habitats_sum_wt_cross <- all_rd_seg_habitats %>% 
  group_by(year, seg.label, buff) %>% 
  expand(puma = pumas$puma) %>% 
  ungroup() %>% 
  full_join(all_rd_seg_habitats) %>% 
  full_join(sum_wt_road_crossed_segs) %>% 
  mutate(tot.raw.cross = replace_na(tot.raw.cross, 0),
         tot.wt.cross = replace_na(tot.wt.cross, 0)) %>% 
  arrange(puma, seg.label, year, buff)




ggplot(sum_wt_road_crossed_segs) +
  geom_point(aes(x = tot.raw.cross, y = tot.wt.cross, color = as.character(year))) +
  stat_smooth(aes(x = tot.raw.cross, y = tot.wt.cross, color = as.character(year)), method = "lm")

sum_wt_cross_habitats <- full_join(sum_wt_road_crossed_segs, all_rd_seg_habitats_pumas) %>% 
  filter(!is.na(year), !is.na(buff)) %>% 
  arrange(seg.label, year, buff)

sum_wt_cross_habitats_longer <- sum_wt_cross_habitats %>% 
  expand(year, puma, seg.label, buff) %>% 
  full_join(sum_wt_cross_habitats)

  
  
fit_scale_mods <- function(zpred) {

  mod30 <- lm(tot.wt.cross ~ pred.value + year, data = filter(sum_wt_cross_habitats_longer, pred == zpred, buff == 30))
    
  
  
}








# map through the fixed length road segments ----

system.time(
  rd_habitat2015 <- get_rd_habitat(rast("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/Harvey_north bay habitat/RSF_Layers_2015_2024-03-19.TIF"), napa_sonoma_rds_equal_segs_buff100) %>% 
    mutate(year = 2015)
)
saveRDS(rd_habitat2015, here("data/rd_habitat/rd_habitat2015"))

system.time(
rd_habitat2016 <- get_rd_habitat(rast("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/Harvey_north bay habitat/RSF_Layers_2016_2024-03-19.TIF"), napa_sonoma_rds_equal_segs_buff100) %>% 
  mutate(year = 2016)
)
saveRDS(rd_habitat2016, here("data/rd_habitat/rd_habitat2016"))


system.time(
  rd_habitat2017 <- get_rd_habitat(rast("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/Harvey_north bay habitat/RSF_Layers_2017_2024-03-19.TIF"), napa_sonoma_rds_equal_segs_buff100) %>% 
    mutate(year = 2017)
)
saveRDS(rd_habitat2017, here("data/rd_habitat/rd_habitat2017"))


system.time(
  rd_habitat2018 <- get_rd_habitat(rast("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/Harvey_north bay habitat/RSF_Layers_2018_2024-03-19.TIF"), napa_sonoma_rds_equal_segs_buff100) %>% 
    mutate(year = 2018)
)
saveRDS(rd_habitat2018, here("data/rd_habitat/rd_habitat2018"))


system.time(
  rd_habitat2019 <- get_rd_habitat(rast("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/Harvey_north bay habitat/RSF_Layers_2019_2024-03-19.TIF"), napa_sonoma_rds_equal_segs_buff100) %>% 
    mutate(year = 2019)
)
saveRDS(rd_habitat2019, here("data/rd_habitat/rd_habitat2019"))


system.time(
  rd_habitat2020 <- get_rd_habitat(rast("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/Harvey_north bay habitat/RSF_Layers_2020_2024-03-19.TIF"), napa_sonoma_rds_equal_segs_buff100) %>% 
    mutate(year = 2020)
)
saveRDS(rd_habitat2020, here("data/rd_habitat/rd_habitat2020"))


system.time(
  rd_habitat2021 <- get_rd_habitat(rast("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/Harvey_north bay habitat/RSF_Layers_2021_2024-03-19.TIF"), napa_sonoma_rds_equal_segs_buff100) %>% 
    mutate(year = 2021)
)
saveRDS(rd_habitat2021, here("data/rd_habitat/rd_habitat2021"))


system.time(
  rd_habitat2022 <- get_rd_habitat(rast("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/Harvey_north bay habitat/RSF_Layers_2022_2024-03-19.TIF"), napa_sonoma_rds_equal_segs_buff100) %>% 
    mutate(year = 2022)
)
saveRDS(rd_habitat2022, here("data/rd_habitat/rd_habitat2022"))


system.time(
  rd_habitat2023 <- get_rd_habitat(rast("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/Harvey_north bay habitat/RSF_Layers_2023_2024-03-19.TIF"), napa_sonoma_rds_equal_segs_buff100) %>% 
    mutate(year = 2023)
)
saveRDS(rd_habitat2023, here("data/rd_habitat/rd_habitat2023"))


# map through the bbmm road slices ----

system.time(
  bbmm_slice_habitat2016 <- get_rd_habitat(rast("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/Harvey_north bay habitat/RSF_Layers_2016_2024-03-19.TIF"), filter(bbmm_road_slices_buff100, year == 2016)) %>% 
    mutate(year = 2016)
)
saveRDS(bbmm_slice_habitat2016, here("data/rd_habitat/bbmm_slice_habitat2016"))


system.time(
  bbmm_slice_habitat2017 <- get_rd_habitat(rast("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/Harvey_north bay habitat/RSF_Layers_2017_2024-03-19.TIF"), filter(bbmm_road_slices_buff100, year == 2017)) %>% 
    mutate(year = 2017)
)
saveRDS(bbmm_slice_habitat2017, here("data/rd_habitat/bbmm_slice_habitat2017"))


system.time(
  bbmm_slice_habitat2018 <- get_rd_habitat(rast("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/Harvey_north bay habitat/RSF_Layers_2018_2024-03-19.TIF"), filter(bbmm_road_slices_buff100, year == 2018)) %>% 
    mutate(year = 2018)
)
saveRDS(bbmm_slice_habitat2018, here("data/rd_habitat/bbmm_slice_habitat2018"))


system.time(
  bbmm_slice_habitat2019 <- get_rd_habitat(rast("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/Harvey_north bay habitat/RSF_Layers_2019_2024-03-19.TIF"), filter(bbmm_road_slices_buff100, year == 2019)) %>% 
    mutate(year = 2019)
)
saveRDS(bbmm_slice_habitat2019, here("data/rd_habitat/bbmm_slice_habitat2019"))


system.time(
  bbmm_slice_habitat2020 <- get_rd_habitat(rast("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/Harvey_north bay habitat/RSF_Layers_2020_2024-03-19.TIF"), filter(bbmm_road_slices_buff100, year == 2020)) %>% 
    mutate(year = 2020)
)
saveRDS(bbmm_slice_habitat2020, here("data/rd_habitat/bbmm_slice_habitat2020"))


system.time(
  bbmm_slice_habitat2021 <- get_rd_habitat(rast("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/Harvey_north bay habitat/RSF_Layers_2021_2024-03-19.TIF"), filter(bbmm_road_slices_buff100, year == 2021)) %>% 
    mutate(year = 2021)
)
saveRDS(bbmm_slice_habitat2021, here("data/rd_habitat/bbmm_slice_habitat2021"))


system.time(
  bbmm_slice_habitat2022 <- get_rd_habitat(rast("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/Harvey_north bay habitat/RSF_Layers_2022_2024-03-19.TIF"), filter(bbmm_road_slices_buff100, year == 2022)) %>% 
    mutate(year = 2022)
)
saveRDS(bbmm_slice_habitat2022, here("data/rd_habitat/bbmm_slice_habitat2022"))


system.time(
  bbmm_slice_habitat2023 <- get_rd_habitat(rast("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/Harvey_north bay habitat/RSF_Layers_2023_2024-03-19.TIF"), filter(bbmm_road_slices_buff100, year == 2023)) %>% 
    mutate(year = 2023)
)
saveRDS(bbmm_slice_habitat2023, here("data/rd_habitat/bbmm_slice_habitat2023"))


# visualizing road segment habitat ----

bbmm_slice_habitat2023 <- readRDS(here("data/rd_habitat/bbmm_slice_habitat2023")) %>% 
  left_join(rds_seg_lengths) %>% 
  mutate(crossed.prop = as.numeric(crossed.seg.length)/as.numeric(road.seg.length),
         wt.mean.tre.shr = mean.tre.shr * crossed.prop)


ggplot(bbmm_slice_habitat2016) +
  geom_density(aes(x = mean.tre.shr)) +
  geom_density(data = readRDS(here("data/rd_habitat/rd_habitat2016")), aes(x = mean.tre.shr), color = "red")


ggplot() +
  geom_sf(data = rds_buff100_mean_tre_shr, aes(color = mean.tre.shr))




# stream crossings. prob not using ----
# filter entire CARI stream layer to just Napa and Sonoma counties - RUN THIS IN AZURE 
st_layers("Z:/Libraries/Vector/CARI/CARI GISv0.2_SFEI2016_20171130/CARIv0.2.gdb")


cari <- st_read("Z:/Libraries/Vector/CARI/CARI GISv0.2_SFEI2016_20171130/CARIv0.2.gdb", layer = "CARIv0_2_streams_final") %>% 
  st_transform(crs = 26910)

study_area_cari <- st_intersection(study_area_counties, cari)


saveRDS(study_area_cari, here("data/study_area_cari"))

# intersect roads and streams 

study_area_cari <- readRDS(here("data/study_area_cari"))

study_area_cari <- study_area_cari %>% 
  mutate(Shape_Length = st_length(.),
         Shape_Length = as.numeric(Shape_Length),
         crick.group = ifelse(Shape_Length < 500, "crick", "creek"),
         keep.class = case_when(orig_datas == "National Wetland Inventory (NWI)" & 
                                  str_sub(orig_class, 1, 1) == "R" &
                                  str_sub(orig_class, 2, 2) %in% c(2, 3)  ~ "keep",
                                TRUE ~ NA))

xx <- study_area_cari %>%
  sf::st_touches(snap_radius = -1) 

yy <- xx %>% lengths()
  
study_area_cari <- study_area_cari %>% 
  mutate(num.touches = yy)

study_area_cari %>% 
  group_by(orig_datas) %>% 
  count(orig_class) %>% 
  view()

  ggplot() +
  geom_sf(data = study_area_cari, aes(color = as.factor(num.touches)))



ggplot() +
  geom_sf(data = filter(study_area_cari, Shape_Length > 150), aes(color = orig_datas))

