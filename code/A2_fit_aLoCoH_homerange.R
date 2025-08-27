

library(tidyverse)
library(here)
library(sf)
library(parallel)




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

fit_save_aLoCoH <- function(zlion) {
  # Start timer
  start_time <- Sys.time()
  
  # Run your analysis
  lion_hr <- analysis_table %>%
    filter(ID == zlion) %>%
    a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))
  
  
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
    start_time = start_time,
    end_time = end_time,
    elapsed_seconds = elapsed
  )
  
  log_file <- here("model_objects/a_loCoH home ranges/a_loCoH_run_log.csv")
  
  if (file.exists(log_file)) {
    readr::write_csv(log_entry, log_file, append = TRUE)
  } else {
    readr::write_csv(log_entry, log_file)
  }
  
}

fit_save_aLoCoH("P12")
easy_lions = c("P34", "P44", "P6", "P12", "P41", "P5", "P37", "P19", "P14", "P2", "P9", "P24", "P30", "P11", "P26", "P25", "P36", "P39", "P31")

map(easy_lions, fit_save_aLoCoH)




aLoCoH_hrs <- readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))


#  P21   302 ----
# insufficient data

#  P34   343 ----
system.time(p34_hr <- analysis_table %>% filter(ID == "P34") %>% a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 25 sec. 28 on AVD

#  P44   850 ----
system.time(p44_hr <- analysis_table %>% filter(ID == "P44") %>% a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 38 sec. 66 on AVD

readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>%
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs_backup"))

readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>% 
  bind_rows(p44_hr) %>% 
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))


#   P6  1275 ----
system.time(p6_hr <- analysis_table %>% filter(ID == "P6") %>% a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 49 sec. 82 on AVD

readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>%
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs_backup"))

readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>% 
  bind_rows(p6_hr) %>% 
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))

#  P12  1541 ----
system.time(P12_hr <- analysis_table %>% filter(ID == "P12") %>% a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 63 sec. 97 on AVD

readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>%
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs_backup"))

readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>% 
  bind_rows(P12_hr) %>% 
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))


#  P41  1725 ----
system.time(P41_hr <- analysis_table %>% filter(ID == "P41") %>% a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 75 sec. 113 on AVD
readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>%
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs_backup"))

readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>% 
  bind_rows(P41_hr) %>% 
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))


#   P5  1922 ----
system.time(P5_hr <- analysis_table %>% filter(ID == "P5") %>% a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 84 sec. 126 on AVD
readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>%
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs_backup"))

readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>% 
  bind_rows(P5_hr) %>% 
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))

#  P37  2411 ----
system.time(P37_hr <- analysis_table %>% filter(ID == "P37") %>% a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 114 sec. 170 on AVD
readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>%
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs_backup"))

readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>% 
  bind_rows(P37_hr) %>% 
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))

#  P19  2435 ----
system.time(P19_hr <- analysis_table %>% filter(ID == "P19") %>% a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 106 sec. 166 on avd
readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>%
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs_backup"))

readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>% 
  bind_rows(P19_hr) %>% 
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))

# P14  2459 ----
system.time(P14_hr <- analysis_table %>% filter(ID == "P14") %>% a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 112 sec
readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>%
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs_backup"))

readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>% 
  bind_rows(P14_hr) %>% 
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))

#  P2  2466 ----
system.time(P2_hr <- analysis_table %>% filter(ID == "P2") %>% a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 98 sec
readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>%
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs_backup"))

readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>% 
  bind_rows(P2_hr) %>% 
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))

#  P9  2990 ----
system.time(P9_hr <- analysis_table %>% filter(ID == "P9") %>% a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 129 sec
readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>%
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs_backup"))

readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>% 
  bind_rows(P9_hr) %>% 
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))

# P24  3598 ----
system.time(P24_hr <- analysis_table %>% filter(ID == "P24") %>% a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 164 sec
readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>%
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs_backup"))

readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>% 
  bind_rows(P24_hr) %>% 
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))

# P30  4039 ----
system.time(P30_hr <- analysis_table %>% filter(ID == "P30") %>% a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 198 sec
readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>%
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs_backup"))

readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>% 
  bind_rows(P30_hr) %>% 
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))

# P11  4171 ----
system.time(p11_hr <- analysis_table %>% filter(ID == "P11") %>% a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 194
readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>%
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs_backup"))

readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>% 
  bind_rows(p11_hr) %>% 
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))

# P26  4191 ----
system.time(P26_hr <- analysis_table %>% filter(ID == "P26") %>% a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 184 sec
readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>%
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs_backup"))

readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>% 
  bind_rows(P26_hr) %>% 
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))

# P25  5178 ----
system.time(P25_hr <- analysis_table %>% filter(ID == "P25") %>% a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 284 sec
readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>%
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs_backup"))

readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>% 
  bind_rows(P25_hr) %>% 
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))

# P36  6117 ----
system.time(P36_hr <- analysis_table %>% filter(ID == "P36") %>% a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 322 sec
readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>%
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs_backup"))

readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>% 
  bind_rows(P36_hr) %>% 
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))

# P39  6199 ----
system.time(P39_hr <- analysis_table %>% filter(ID == "P39") %>% a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 318 sec
readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>%
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs_backup"))

readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>% 
  bind_rows(P39_hr) %>% 
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))

# P31  6420 ----
system.time(P31_hr <- analysis_table %>% filter(ID == "P31") %>% a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 456 sec

# P33  9023 ----
system.time(P33_hr <- analysis_table %>% filter(ID == "P33") %>% a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 611 sec

# P16 12345 ----
system.time(P16_hr <- analysis_table %>% filter(ID == "P16") %>% a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# crashes on local. 974 on avd


readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>% 
  bind_rows(P16_hr) %>% 
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))

# P1 16346 ----
system.time(P1_hr_16_18 <- analysis_table %>% 
              filter(ID == "P1", year(DATE) %in% c(2016, 2017, 2018)) %>% 
              a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 264. 561 on AVD

readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>% 
  bind_rows(P1_hr_16_18) %>% 
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))

# c(2019, 2021, 2022) crashed RStudio with 9937 points
system.time(P1_hr_19_21 <- analysis_table %>% 
              filter(ID == "P1", year(DATE) %in% c(2019, 2021)) %>% 
              a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 275. 486 on AVD

system.time(P1_hr_22 <- analysis_table %>% 
              filter(ID == "P1", year(DATE) %in% c(2022)) %>% 
              a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 

# 245 on AVD

ggplot() +
  geom_sf(data = P1_hr_16_18 %>% filter(iso_level == 0.99), color = "red", fill = NA) +
  geom_sf(data = P1_hr_19_21 %>% filter(iso_level == 0.99), color = "green", fill = NA) +
  geom_sf(data = P1_hr_22 %>% filter(iso_level == 0.99), color = "blue", fill = NA)

sf_use_s2(FALSE)
p1_hr_1 <- bind_rows(P1_hr_16_18, P1_hr_19_21, P1_hr_22) %>% 
  filter(iso_level == 1) %>% 
  st_transform(crs = 26910) %>% 
  st_union()  %>%
  st_as_sf()  %>%
  mutate(ID = "P1",
         Area = st_area(.),
         iso_level = 1) %>%
  rename(geometry = x)
  

ggplot() +
geom_sf(data = analysis_table %>% filter(ID == "P1"))+
  geom_sf(data = p1_hr, fill = NA)


st_write(p1_hr_1, here("data/shapefiles/p1_hr_alocoh_100.shp"), append = FALSE)




p1_hr_99 <- bind_rows(P1_hr_16_18, P1_hr_19_21, P1_hr_22) %>% 
  filter(iso_level == 0.99) %>% 
  st_transform(crs = 26910) %>% 
  st_union()  %>%
  st_as_sf()  %>%
  mutate(ID = "P1",
         Area = st_area(.),
         iso_level = 0.99) %>%
  rename(geometry = x)

st_write(p1_hr_99, here("data/shapefiles/p1_hr_alocoh_99.shp"), append = FALSE)


readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>% 
  bind_rows(p1_hr_1 %>%
              st_transform(4326), 
            p1_hr_99 %>%
              st_transform(4326)) %>% 
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))


# P13 19262 ----

p13_data <- analysis_table %>% 
  filter(ID == "P13")

nrow(p13_data)/6000

system.time(P13_hr_a <- p13_data[1:6000,] %>% 
              a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 460 on AVD

system.time(P13_hr_b <- p13_data[6001:12000,] %>% 
              a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 509 on AVD

system.time(P13_hr_c <- p13_data[12001:18000,] %>% 
              a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 424 on AVD

system.time(P13_hr_d <- p13_data[18001:nrow(p13_data),] %>% 
              a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 86 for 1262 points on AVD

p13_hr_all <- bind_rows(P13_hr_a, P13_hr_b, P13_hr_c, P13_hr_d)

p13_hr_1 <- p13_hr_all %>% 
  filter(iso_level == 1) %>% 
  st_transform(crs = 26910) %>% 
  st_union()  %>%
  st_as_sf()  %>%
  mutate(ID = "P13",
         Area = st_area(.),
         iso_level = 1) %>%
  rename(geometry = x) %>%
  st_transform(4326)

p13_hr_99 <- p13_hr_all %>% 
  filter(iso_level == 0.99) %>% 
  st_transform(crs = 26910) %>% 
  st_union()  %>%
  st_as_sf()  %>%
  mutate(ID = "P13",
         Area = st_area(.),
         iso_level = 1) %>%
  rename(geometry = x) %>%
  st_transform(4326)

readRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs")) %>% 
  bind_rows(p13_hr_1, p13_hr_99) %>% 
  saveRDS(here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))


# P4 22775 ----
system.time(P4_hr <- analysis_table %>% 
              filter(ID == "P34") %>% 
              a_LoCoH_HR(iso_levels = c(0.98, 0.99, 1))) 
# 198 sec



# combining as needed but might be taken care of above ----
aLoCoH_hrs <- bind_rows(# p34_hr, 
                        p44_hr, 
                        p6_hr, 
                        P12_hr, 
                        P41_hr, 
                        P5_hr, 
                        P37_hr, 
                        P19_hr, 
                        P14_hr, 
                        P2_hr, 
                        P9_hr, 
                        P24_hr, 
                        P30_hr, 
                        p11_hr, 
                        P26_hr, 
                        P25_hr, 
                        P36_hr, 
                        P39_hr, 
                        P31_hr, 
                        P33_hr#, 
#                        P16_hr, 
#                        P1_hr, 
#                        P13_hr, 
#                        P4_hr
)

saveRDS(aLoCoH_hrs, here("model_objects/a_loCoH home ranges/aLoCoH_hrs"))



ggplot() +
  geom_sf(data = analysis_table %>% filter(ID == "P11"), alpha = 0.2) +
  geom_sf(data = p11_hr2, aes(color = factor(iso_level)), fill = NA, linewidth = 1)




system.time(
  
  # Assume your data has a column called "ID" for animal id
  animal_hr <- analysis_table %>%
    group_by(ID) %>%                 # Group by animal
    nest() %>%                        # Nest each animal's data into a list column
    mutate(HR = map(data, ~ a_LoCoH_HR(.x, id = "ID", date = "DATE"))) %>%
    select(ID, HR)                    # Keep only ID and HR results
#  nest() creates a data column where each row contains all rows for that animal as a tibble.
  
 # map() runs your function on each animal's subset.

#HR will be a list-column containing the resulting sf objects for each animal.
  
)


