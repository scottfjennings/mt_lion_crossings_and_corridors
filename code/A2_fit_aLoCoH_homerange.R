

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

#' a_LoCoH_HR
#' 
#' original code from Kyle Dougherty and John Benson saved at C:\Users\scott.jennings\OneDrive - Audubon Canyon Ranch\R_examples_resources\pseudo_packages\Dougherty aLoCoH\a_LoCoH_HR_Function_updated.R
#'
#' @param data data frame for a single tracked animal. must be an sf object, projected in UTM
#' @param id the name of the animal id field in data
#' @param date the name of the date field in data
#' @param min_days minimum number of days an animal must be tracked to calculate a home range. default is 30
#' @param min_fixes minimum number of fixes recorded for a tracked animal to calculate a home range. default is 30 
#' @param iso_levels % levels for calculating UD isopleths. 
#' @param fill_holes logical. TRUE (default) fills interior holes in the calculated home range
#' @param min_neighbors minimum number of neighbors with the root point
#' @param n_cores number of cores to use for parallel processing
#'
#' @returns a set of sfc_POLYGONs, one for each iso_level. these are in lat lon, not projected UTM
#' 
#' @details
#' calculates adaptive Local Convex Hull following Getz et al 2007.
#' 
#' Follows Getz et al recommendation to use the largest distance between points in the dataset as the value of a
#' 
#'
#' @examples
a_LoCoH_HR <- function(data, id = "ID", date = "DATE", 
                       min_days = 30, min_fixes = 30, 
                       iso_levels = c(0.95, 1), fill_holes = TRUE, 
                       min_neighbors = 2, n_cores = detectCores()- 2) {
  
  # Summarise the number of days an individual was tracked and 
  # the number of fixes in the dataset
  data_summary = data %>%
    st_drop_geometry() %>%
    summarise(Days_Tracked = difftime(max(.[[date]], na.rm = TRUE), 
                                      min(.[[date]], na.rm = TRUE), 
                                      units = "days"),
              n_Fixes = n())
  
  # If the number of days or fixes is less than the user defined 
  # threshold (defaults to 30 days/fixes), do not estimate HR and 
  # return number of days/fixes with message
  if(data_summary$Days_Tracked < min_days | 
     data_summary$n_Fixes < min_fixes){
    
    capture.output(paste("Insufficient data to estimate home range:", 
                         unique(data[[id]]),
                         "n Days =",
                         data_summary$Days_Tracked, 
                         "n Fixes = ", data_summary$n_Fixes, sep = " "))
    
    } else{
    
    # Print start time for NN identification to console
    start_time = Sys.time()
    print(paste("Identifying Nearest Neighbors for", 
                nrow(data), "Points:", 
                unique(data[[id]]), Sys.time()))
    
    
    # Create a dataframe with the pairwise distance between 
    # each point in the telemetry data
    pairwise_distances = st_distance(data) %>%
      as_tibble(rownames = "Row_Number", 
                .name_repair = ~paste0("V", str_match_all(.x, "[0-9]+"))) %>%
      mutate(across(everything(), 
                    as.numeric))
    
    # Gets the maximum distance between any two points in 
    # the telemetry data. Will serve as the "a" parameter
    max_distance = pairwise_distances %>%
      select(-Row_Number) %>% max()
    
    # Set up parallelization:
    # If running on linux/mac, will utilize FORK clusters which are 
    # faster to initialize and more memory efficient
    if(Sys.info()[["sysname"]] %in% c("Linux", "Darwin")){
      
      cl <- makeCluster(n_cores, type = "FORK")
      
    }   else{
      # If running on windows, will utilize SOCKET clusters,
      # which require explicit exporting of any data/packages 
      # used in the function being run
      cl <- makeCluster(n_cores)
      clusterExport(cl, c("data", 
                          "pairwise_distances", 
                          "min_neighbors",
                          "max_distance"), 
                    envir = environment())
      clusterEvalQ(cl, {
        library(tidyverse)
        library(sf)
      })
    }
    
    # The function below will calculate the convex hull for each 
    # focal point (each row in the dataframe)
    hulls = parLapply(cl,
                1:nrow(data), 
                      function(row){
                        
                        # From the dataframe with the pairwise distances 
                        # for all points, select the row number and 
                        # column representing the distance to the focal point. 
                        nearest_neighbors = pairwise_distances %>%
                          select(Row_Number, Distance = paste0("V", row)) %>% 
                          # Arrange the dataframe by distance
                          arrange(Distance) %>% 
                          # Calculate the cumulative distance and filter 
                          # so that only rows where the cumulative distance
                          # is less than "a" (the maximum distance between any 
                          # two points in the dataset) are returned OR 
                          # to meet the specified minimum number of nearest neighbors
                          mutate(Cumulative_Distance = cumsum(Distance)) %>%
                          filter(Cumulative_Distance <= max_distance | 
                                   row_number() <= 1 + min_neighbors) %>%
                          # Get the row number representing each point identified 
                          # as a nearest neighbor
                          pull(Row_Number)
                        
                        # Using those points, create the convex hull 
                        # for the focal point and calculate the number
                        # of points contained within the hull and the 
                        # area of the hull
                        hull = data %>% 
                          filter(row_number() %in% nearest_neighbors) %>%
                          st_union() %>%
                          st_convex_hull() %>%
                          st_as_sf() %>%
                          mutate(n_points = length(nearest_neighbors),
                                 area = st_area(.))
                        
                      }) %>%
      # Bind hulls into a single dataframe and arrange by the 
      # number of points contained within the hull. If there is 
      # a tie in the number of points contained within hulls, 
      # they are arranged by area, from smallest to largest
      bind_rows() %>%
      arrange(desc(n_points), area)
    
    
    stopCluster(cl)
    
    print(paste("Hulls Created:", unique(data[[id]]), Sys.time()))
    
    # This function will repeat until the number of points contained 
    # within the union of the hulls is greater than the user defined
    # iso_level. For example, if iso_level = 0.95, it will break the loop 
    # when a percent of greater than 95% of points contained within the hull 
    # is reached, and return the largest union of hulls with < 95% of points 
    # contained
    Isopleths = lapply(iso_levels, 
                       function(iso_level){
                         
                         # If the iso_level is 1, return the 100% isopleth, 
                         # otherwise get desired isopleth below
                         if(iso_level == 1){
                           
                           Isopleth = hulls %>% 
                             st_union() %>%
                             st_as_sf() %>%
                             mutate(n_contained = lengths(st_intersects(., data)),
                                    percent = n_contained/nrow(data), 
                                    iso_level = iso_level)
                           
                         }else{
                           
                           # Rather than starting the repeat loop with the hull containing the
                           # the greatest number of points, this creates a sequence ranging from 
                           # 1 to the number of points in the dataset (also the number of hulls). 
                           # Using this sequence, calculate the percent of points contained within 
                           # each hull, then select an initial hull index value (the hull with the 
                           # largest number of points contained that is less than the use defined iso
                           # level).
                           initial_hull_numbers = unique(round(seq(1, nrow(data), length.out = 100)))
                           
                           hull_index = lapply(initial_hull_numbers,
                                               function(hull_index){ 
                                                 
                                                 hulls %>% 
                                                   head(hull_index) %>%
                                                   st_union() %>%
                                                   st_as_sf() %>% 
                                                   mutate(n_contained = lengths(st_intersects(., data)),
                                                          percent = n_contained/nrow(data), 
                                                          iso_level = iso_level) 
                                               }) %>%
                             bind_rows(.id = "Index") %>%
                             filter(percent < iso_level) %>%
                             filter(percent == max(percent)) %>%
                             filter(Index == max(Index)) %>%
                             pull(Index)
                           
                           hull_index = initial_hull_numbers[as.numeric(hull_index)]
                           
                           # From the initial hull index identified above, begin the repeat loop
                           
                           repeat{
                             
                             # Create isopleth and test if greater than or
                             # equal to the defined iso_level
                             test_isopleth = hulls %>%
                               head(hull_index) %>%
                               st_union() %>%
                               st_as_sf() %>%
                               mutate(n_contained = lengths(st_intersects(., data)),
                                      percent = n_contained/nrow(data),
                                      iso_level = iso_level)
                             
                             # Break the loop as described above
                             if(test_isopleth$percent >= iso_level){
                               break
                             }
                             
                             # If test_isopleth percent is not greater than iso_level
                             # set as new Isopleth and add 1 to hull_index to restart
                             # the loop
                             hull_index = hull_index + 1
                             
                             Isopleth = test_isopleth
                             
                           }
                           
                         }
                         
                         print(paste(iso_level, "Isopleth Identified:", unique(data[[id]]), Sys.time()))
                         return(Isopleth)
                         
                       })
    
    # If the fill hole argument is TRUE, the code below will extract the outer 
    # boundaries (1st element) of each polygon or multipolygon
    if(fill_holes == TRUE){
      
      Isopleths = Isopleths %>% 
        map_df(., function(isopleth){
          
          if(st_geometry_type(isopleth) == "MULTIPOLYGON"){
            isopleth %>% 
              mutate(x = isopleth[[1]] %>%
                       st_geometry() %>% 
                       unlist(recursive = FALSE) %>% 
                       map(., ~st_polygon(.x[1])) %>%
                     #  st_sfc(crs = unique(data$CRS)) %>% 
                       st_sfc(crs = st_crs(data)) %>% 
                       st_union())
          } else if(st_geometry_type(isopleth) == "POLYGON"){
            isopleth %>% 
              mutate(x = isopleth[[1]] %>%
                       st_geometry() %>%
                       unlist(recursive = FALSE) %>%
                       .[1] %>%
                       st_polygon() %>%
                     #  st_sfc(crs = unique(data$CRS)) %>%
                       st_sfc(crs = st_crs(data)) %>%
                       st_cast("MULTIPOLYGON"))
          }
          
        }) %>% 
        mutate(ID = unique(data[[id]]),
               Area = st_area(.)) %>%
        rename(geometry = x) %>%
        st_transform(4326)
      
    } else{
      
      Isopleths = Isopleths %>%
        map_df(., ~.x %>%
                 mutate(ID = unique(data[[id]]),
                        Area = st_area(.)) %>%
                 rename(geometry = x) %>%
                 st_transform(4326))
      
    }
    
    gc()
    time_difference = difftime(Sys.time(), start_time)
    print(time_difference)
    Isopleths
  }
}


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


