


library(tidyverse)
library(here)
library(lme4)
library(AICcmodavg)
library(merTools)
library(sf)



source(here("code/helper_data.R"))


# final data prep ----
puma_ok_bridges_per_seg <- readRDS(here("data/puma_ok_bridges_per_seg")) %>% 
  data.frame() %>% 
  dplyr::select(-geometry)

summed_crossing_analysis_table <- readRDS(here("data/habitat_varbs_scales")) %>% 
  left_join(readRDS(here("data/puma_ok_bridges_per_seg")) %>% 
              data.frame() %>% 
              dplyr::select(-geometry)) %>% 
  mutate(num.bridge = replace_na(num.bridge, 0)) %>% 
  left_join(readRDS(here("data/equal_length_classes")) %>% 
              dplyr::select(-road.seg.length)) %>% 
  filter(!animal.id %in% hr_exclude_pumas,
         !seg.label %in% p31_exclude_segments,
         !is.na(classes))


summed_crossing_analysis_table_long <- summed_crossing_analysis_table %>% 
  pivot_longer(cols = c(tot.raw.cross, tot.wt.cross), names_to = "cross.type", values_to = "cross.value")



# some raw visualization ----

summed_crossing_analysis_table_long %>% 
  filter(cross.type == "tot.raw.cross") %>% 
  ggplot() +
  geom_point(aes(y = cross.value, x = as.character(year)), alpha = 0.2) +
  stat_smooth(aes(y = cross.value, x = as.character(year))) +
  facet_wrap(~animal.id, scales = "free_y")
# not much year pattern


summed_crossing_analysis_table_long %>% 
  filter(cross.type == "tot.raw.cross") %>% 
  ggplot() +
  geom_point(aes(y = cross.value, x = dev.60, color = as.factor(year))) +
  stat_smooth(aes(y = cross.value, x = dev.60, color = as.factor(year)), method = "lm") +
  facet_wrap(~animal.id, scales = "free_y")


# modeling crossings with animal.id and year as random effects ----
fit_summed_crossing_mods <- function(zcross) {
  
  df <- summed_crossing_analysis_table_long %>% 
    filter(cross.type == zcross)
#"animal.id" "year" "tre.shr.30"  "dev.60"     "tre.shr.300" "num.bridge"  "classes"     "max.class  
  zmods <- list(
    cov30 = lmer(cross.value ~ tre.shr.30 + (1|year/animal.id), data = df, REML = FALSE),
    cov300 = lmer(cross.value ~ tre.shr.300 + (1|year/animal.id), data = df, REML = FALSE),
    dev60 = lmer(cross.value ~ dev.60 + (1|year/animal.id), data = df, REML = FALSE),
    bridge = lmer(cross.value ~ num.bridge + (1|year/animal.id), data = df, REML = FALSE),
    class = lmer(cross.value ~ max.class + (1|year/animal.id), data = df, REML = FALSE),
    # cover and number of bridges
    cov30_bridge = lmer(cross.value ~ tre.shr.30 + num.bridge + (1|year/animal.id), data = df, REML = FALSE),
    cov300_bridge = lmer(cross.value ~ tre.shr.300 + num.bridge + (1|year/animal.id), data = df, REML = FALSE),
    dev60_bridge = lmer(cross.value ~ dev.60 + num.bridge + (1|year/animal.id), data = df, REML = FALSE),
    
    cov30.bridge = lmer(cross.value ~ tre.shr.30 * num.bridge + (1|year/animal.id), data = df, REML = FALSE),
    cov300.bridge = lmer(cross.value ~ tre.shr.300 * num.bridge + (1|year/animal.id), data = df, REML = FALSE),
    dev60.bridge = lmer(cross.value ~ dev.60 * num.bridge + (1|year/animal.id), data = df, REML = FALSE),
    
    # cover and road class
    cov30_class = lmer(cross.value ~ tre.shr.30 + max.class + (1|year/animal.id), data = df, REML = FALSE),
    cov300_class = lmer(cross.value ~ tre.shr.300 + max.class + (1|year/animal.id), data = df, REML = FALSE),
    dev60_class = lmer(cross.value ~ dev.60 + max.class + (1|year/animal.id), data = df, REML = FALSE),
    
    cov30.class = lmer(cross.value ~ tre.shr.30 * max.class + (1|year/animal.id), data = df, REML = FALSE),
    cov300.class = lmer(cross.value ~ tre.shr.300 * max.class + (1|year/animal.id), data = df, REML = FALSE),
    dev60.class = lmer(cross.value ~ dev.60 * max.class + (1|year/animal.id), data = df, REML = FALSE),
    
    intercept = lmer(cross.value ~ 1  + (1|year/animal.id), data = df, REML = FALSE)
    
  )
  
  zmods$aic = aictab(zmods, names(zmods))
  
  return(zmods)
  
}




raw_crossings_mods <- fit_summed_crossing_mods("tot.raw.cross")

#
# modeling crossings with just animal.id as random effect ----

fit_summed_crossing_mods <- function(zcross) {
  
  df <- summed_crossing_analysis_table_long %>% 
    filter(cross.type == zcross)
  #"animal.id" "year" "tre.shr.30"  "dev.60"     "tre.shr.300" "num.bridge"  "classes"     "max.class  
  zmods <- list(
    cov30 = lmer(cross.value ~ tre.shr.30 + (1|animal.id), data = df, REML = FALSE),
    cov300 = lmer(cross.value ~ tre.shr.300 + (1|animal.id), data = df, REML = FALSE),
    dev60 = lmer(cross.value ~ dev.60 + (1|animal.id), data = df, REML = FALSE),
    bridge = lmer(cross.value ~ num.bridge + (1|animal.id), data = df, REML = FALSE),
    class = lmer(cross.value ~ max.class + (1|animal.id), data = df, REML = FALSE),
    # cover and number of bridges
    cov30_bridge = lmer(cross.value ~ tre.shr.30 + num.bridge + (1|animal.id), data = df, REML = FALSE),
    cov300_bridge = lmer(cross.value ~ tre.shr.300 + num.bridge + (1|animal.id), data = df, REML = FALSE),
    dev60_bridge = lmer(cross.value ~ dev.60 + num.bridge + (1|animal.id), data = df, REML = FALSE),
    
    cov30.bridge = lmer(cross.value ~ tre.shr.30 * num.bridge + (1|animal.id), data = df, REML = FALSE),
    cov300.bridge = lmer(cross.value ~ tre.shr.300 * num.bridge + (1|animal.id), data = df, REML = FALSE),
    dev60.bridge = lmer(cross.value ~ dev.60 * num.bridge + (1|animal.id), data = df, REML = FALSE),
    
    # cover and road class
    cov30_class = lmer(cross.value ~ tre.shr.30 + max.class + (1|animal.id), data = df, REML = FALSE),
    cov300_class = lmer(cross.value ~ tre.shr.300 + max.class + (1|animal.id), data = df, REML = FALSE),
    dev60_class = lmer(cross.value ~ dev.60 + max.class + (1|animal.id), data = df, REML = FALSE),
    
    cov30.class = lmer(cross.value ~ tre.shr.30 * max.class + (1|animal.id), data = df, REML = FALSE),
    cov300.class = lmer(cross.value ~ tre.shr.300 * max.class + (1|animal.id), data = df, REML = FALSE),
    dev60.class = lmer(cross.value ~ dev.60 * max.class + (1|animal.id), data = df, REML = FALSE),
    
    intercept = lmer(cross.value ~ 1  + (1|animal.id), data = df, REML = FALSE)
    
  )
  
  zmods$aic = aictab(zmods, names(zmods))
  
  return(zmods)
  
}


# fit for raw crossings as response

raw_crossings_mods <- fit_summed_crossing_mods("tot.raw.cross")


znewdat = expand.grid(dev.60 = seq(0, 1, by = 0.1),
                      num.bridge = seq(0, max(summed_crossing_analysis_table_long$num.bridge))
                      #, animal.id = distinct(summed_crossing_analysis_table_long, animal.id)$animal.id
                      ) 



zpred_raw = predict(raw_crossings_mods$dev60.bridge, znewdat, se.fit = TRUE, type = "response", re.form = NA) %>% 
  data.frame() %>% 
  bind_cols(znewdat) %>% 
  mutate(lwr = fit - (1.96 * se.fit),
         upr = fit + (1.96 * se.fit),
         dev.60 = dev.60 * 100)

zpred_raw %>% 
  ggplot() +
  geom_line(aes(y = fit, x = dev.60, color = as.character(num.bridge))) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, x = dev.60, fill = as.character(num.bridge)), alpha = 0.3) +
  #facet_wrap(~num.bridge) +
  labs(x = "% developed area within 60m of road",
       y = "# crossings",
       title = "Mean number of road crossings\nper year per 1300m road segment",
       color = "# bridges per\nroad segment",
       fill = "# bridges per\nroad segment") +
  theme_bw()


# fit with weighted crossings as response
wt_crossings_mods <- fit_summed_crossing_mods("tot.wt.cross")

zpred_wt = predict(wt_crossings_mods$dev60.bridge, znewdat, se.fit = TRUE, type = "response", re.form = NA) %>% 
  data.frame() %>% 
  bind_cols(znewdat) %>% 
  mutate(lwr = fit - (1.96 * se.fit),
         upr = fit + (1.96 * se.fit),
         dev.60 = dev.60 * 100)

zpred_wt %>% 
  ggplot() +
  geom_line(aes(y = fit, x = dev.60, color = as.character(num.bridge))) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, x = dev.60, fill = as.character(num.bridge)), alpha = 0.3) +
  #facet_wrap(~num.bridge) +
  labs(x = "% developed area within 60m of road",
       y = "# crossings",
       title = "Mean number of weighted road crossings\nper year per 1300m road segment",
       color = "# bridges per\nroad segment",
       fill = "# bridges per\nroad segment") +
  theme_bw()




