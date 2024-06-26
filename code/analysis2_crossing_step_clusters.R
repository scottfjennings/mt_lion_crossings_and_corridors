


library(tidyverse)
library(here)
library(sf)
library(move)
library(ctmm)
library(terra)

source(here("code/utilities.R"))

options(scipen = 999)


naive_crossings <- readRDS(here("data/naive_crossings_napa_sonoma_2hr"))

puma_steps <- readRDS(here("data/puma_steps")) %>% 
  mutate(step.dist.2hr = (step.dist/as.numeric(step.dur)) * 7200)
# crossing_steps <- readRDS(here("data/crossing_step_ids"))
crossing_steps <- distinct(naive_crossings, step.id)

# first need to ID the ~2 steps before and after each crossing


#' extract.with.context
#'
#' @param zstep 
#'
#' @return
#' @details
#' adapted from https://stackoverflow.com/questions/73142970/inspect-surroundings-area-neighbors-context-of-every-row-selected-by-a-filter-co
#' 
#'
#' @examples
extract.with.context <- function(zstep, num.steps) {
  
  match.idx  <- which(puma_steps$step.id == zstep)
  span       <- seq(from = (-1 * num.steps), to = num.steps)
  extend.idx <- c(outer(match.idx, span, `+`))
  extend.idx <- Filter(function(i) i > 0 & i <= nrow(puma_steps), extend.idx)
  extend.idx <- sort(unique(extend.idx))
  
  neighbor_steps = puma_steps[extend.idx, , drop = FALSE] %>% 
    mutate(crossing.step = zstep)
  return(neighbor_steps)
}



system.time(
  neighbor_steps_all <- map2_df(distinct(naive_crossings, step.id)$step.id, 2, extract.with.context), gcFirst = TRUE
)


# want all steps in a cluster to be close to 2 hours, currently using +- 10 min

ok_step_dur <- neighbor_steps_all %>% 
  dplyr::select(step.id, crossing.step, step.dur) %>% 
  mutate(ok.step.dur = between(as.numeric(step.dur), 6600, 7800)) %>% 
  group_by(crossing.step) %>% 
  mutate(all.step.dur.ok = all(ok.step.dur == TRUE)) %>% 
  ungroup()

ok_step_dur %>% 
  filter(all.step.dur.ok == TRUE) %>% 
  distinct(crossing.step) %>% nrow()

crossing_clusters <- ok_step_dur %>% 
  filter(all.step.dur.ok == TRUE) %>% 
  left_join(neighbor_steps_all) %>%
  group_by(crossing.step) %>% 
  mutate(cluster.step.num = row_number()) %>% 
  ungroup()

# first need to decompose each step back into simple GPS points

crossing_clusters_gps <- crossing_clusters %>%  
  data.frame() %>% 
  dplyr::select(crossing.step, step.id, cluster.step.num, easting, easting.end, northing, northing.end, datetime.local, datetime.local.end) %>% 
  mutate(across(c(easting, easting.end, northing, northing.end, datetime.local, datetime.local.end), ~as.character(.))) %>% 
  pivot_longer(cols = c(easting, easting.end, northing, northing.end, datetime.local, datetime.local.end)) %>%
  group_by(crossing.step) %>% 
  mutate(cluster.step.num = ifelse(cluster.step.num == max(cluster.step.num) & str_detect(name, "end"), paste(cluster.step.num, "b", sep = ""), cluster.step.num),
         name = ifelse(str_detect(cluster.step.num, "b"), str_replace(name, ".end", ""), name)) %>% 
  filter(!str_detect(name, ".end")) %>% 
  pivot_wider(id_cols = c(crossing.step, step.id, cluster.step.num), values_from = value, names_from = name) %>% 
  mutate(datetime.local = as.POSIXct(datetime.local),
         easting = as.numeric(easting),
         northing = as.numeric(northing)) %>% 
  dplyr::select(-cluster.step.num)

saveRDS(crossing_clusters_gps, here("data/crossing_clusters2_gps"))

