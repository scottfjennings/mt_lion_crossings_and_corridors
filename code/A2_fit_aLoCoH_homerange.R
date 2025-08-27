

library(tidyverse)
library(here)
library(sf)
library(parallel)

source(here("code/helper_data.R"))


# load, prep data ----

analysis_table <- readRDS(here("data/analysis_table"))  %>% 
  st_as_sf(coords = c("longitude", "latitude"),  # x = lon, y = lat
           crs = 4326,                            # WGS84 (GPS standard)
           remove = FALSE) %>% 
  st_transform(crs = 26910) %>% 
  rename("DATE" = datetime.local,
         "ID" = animal.id)


analysis_table %>% 
  data.frame() %>%  
  count(ID) %>% 
  arrange(n) %>% 
  filter(n < 9000) %>% 
  summarise(ids = paste(ID, collapse = '", "'))

# these lions have a lot of data because of many years tracked and are bogging the function. I tried P1 on AVD but killed it after 4.8 hours
# need to break up data into fewer year chunks
# use this to look at how many points per year to decide what chunks should be
analysis_table %>% 
  data.frame() %>% 
  filter(ID %in% c("P1", "P13", "P4")) %>% 
count(ID, year(DATE)) 


# NO RUN for testing only ----
data = analysis_table %>% 
  filter(ID == "P1", year(DATE) %in% c(2016, 2017, 2018))

date = "DATE"
id = "ID"
min_days = 30
min_fixes = 30
iso_levels = 0.95
fill_holes = TRUE 
min_neighbors = 2
n_cores = detectCores()- 2


# load a_LoCoH_HR function ----
source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/R_examples_resources/pseudo_packages/Dougherty aLoCoH/a_LoCoH_HR_SJ.R")
# p12_test <- a_LoCoH_HR(data = data)

# now a wrapper to run and save for each lion ---- 
# also logs how much time each lion takes to run

fit_save_aLoCoH <- function(zlion, row.cap = 6000) {
  # Start timer
  start_time <- Sys.time()
  
  # Run your analysis
  lion_df <- analysis_table %>%
    filter(ID == zlion)
  
  
  if (nrow(lion_df) <= row.cap) {
    
    # small enough, run directly
    lion_hr <- lion_df %>%
      a_LoCoH_HR(iso_levels = c(1))
    
  } else {
    
    # figure out how many chunks needed
    n_chunks <- ceiling(nrow(lion_df) / row.cap)
    
    # split into chunks of size <= row.cap
    lion_splits <- split(
      lion_df,
      cut(seq_len(nrow(lion_df)), breaks = n_chunks, labels = FALSE)
    )
    
    
    # run LoCoH on each subset with map()
    lion_chunks_hr <- map(lion_splits, ~ a_LoCoH_HR(.x, iso_levels = c(1)))
    
    
    # combine results
    lion_hr_all <- bind_rows(lion_chunks_hr)
    
    
    lion_hr <- lion_hr_all %>% 
      st_transform(crs = 26910) %>% 
      st_as_sf() %>% 
      group_by(iso_level) %>% 
      summarise(geometry = st_union(geometry), .groups = "drop") %>% 
      st_cast("MULTIPOLYGON") %>% 
      mutate(ID = zlion,
             Area = st_area(geometry)) %>% 
      st_transform(4326)
  
  }
  
  
  # End timer
  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  
  # Backup
  
  if (file.exists(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))) {
    readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>%
      saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs_backup"))
    
    # Save new version 
    readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>%
      bind_rows(lion_hr) %>%
      saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))
  } else {
    lion_hr %>%
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))
  }
  
  
  # Log timing
  log_entry <- data.frame(
    ID = zlion,
    num.fixes = nrow(lion_df),
    start_time = start_time,
    end_time = end_time,
    elapsed_seconds = elapsed,
    drive = here()
  )
  
  log_file <- here("model_objects/a_loCoH home ranges/a_loCoH_run_log.csv")
  
  if (file.exists(log_file)) {
    readr::write_csv(log_entry, log_file, append = TRUE)
  } else {
    readr::write_csv(log_entry, log_file)
  }
  
}

# fit_save_aLoCoH("P12")
all_lions <- distinct(analysis_table, ID) %>% pull(ID)
safe_all_lions_hr <- map(all_lions, safely(fit_save_aLoCoH))

# create shapefile ----

aLoCoH_hrs <- readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))

ggplot() +
  geom_sf(data = aLoCoH_hrs %>% filter(ID %in% c("P19", "P14", "P36") | ID %in% exclude_pumas), color = "gray", fill = NA, linewidth = 1) +
  geom_sf(data = aLoCoH_hrs %>% filter(!ID %in% c("P19", "P14", "P36"), !ID %in% exclude_pumas), aes(color = ID), fill = NA, linewidth = 1) +
  theme_bw()


ggplot() +
  geom_sf(data = aLoCoH_hrs, aes(color = ifelse((ID %in% c("P19", "P14", "P36") | !ID %in% exclude_pumas), "gray", ID)))


st_write(aLoCoH_hrs, here("data/shapefiles/aLoCoH_hrs.shp"), append = FALSE)

