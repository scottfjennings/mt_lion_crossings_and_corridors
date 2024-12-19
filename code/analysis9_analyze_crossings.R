


library(tidyverse)
library(here)
library(lme4)
library(AICcmodavg)
library(MuMIn)
library(readxl)
#library(merTools)
#library(sf)



source(here("code/helper_data.R"))

puma_sexes <- read_xlsx("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/data/COLLAR DATE INFO_31Dec2023.xlsx")

names(puma_sexes) <- tolower(names(puma_sexes))

puma_sexes <- puma_sexes %>%
  rename("animal.id" = `animal id`) %>% 
  distinct(animal.id, sex)

# final data prep ----
# not including varbs for traffic conditions for TWS/CADFW mt lion working group
summed_crossing_analysis_table <- readRDS(here("data/habitat_varbs_scales")) %>% 
  left_join(readRDS(here("data/puma_ok_bridges_per_seg")) ) %>% 
  mutate(num.bridge = replace_na(num.bridge, 0)) %>% 
#  left_join(readRDS(here("data/equal_length_classes")) %>% 
#             dplyr::select(-road.seg.length)) %>% 
  filter(!animal.id %in% hr_exclude_pumas,
         !seg.label %in% p31_exclude_segments#,
         #!is.na(classes)
         ) %>% 
  left_join(puma_sexes)


# summed_crossing_analysis_table_long <- summed_crossing_analysis_table %>% 
#  pivot_longer(cols = c(tot.raw.cross, tot.wt.cross), names_to = "cross.type", values_to = "cross.value")



# some raw visualization ----

summed_crossing_analysis_table %>%
  ggplot() +
  geom_point(aes(y = tot.raw.cross, x = as.character(year), color = sex), alpha = 0.2) +
  stat_smooth(aes(y = tot.raw.cross, x = as.character(year), color = sex)) +
  facet_wrap(~animal.id, scales = "free_y")
# not much year pattern


summed_crossing_analysis_table_long %>% 
  filter(cross.type == "tot.raw.cross") %>% 
  ggplot() +
  geom_point(aes(y = cross.value, x = dev.60, color = as.factor(year))) +
  stat_smooth(aes(y = cross.value, x = dev.60, color = as.factor(year)), method = "lm") +
  facet_wrap(~animal.id, scales = "free_y")

# fit models ----


# compare lmer vs glmer ----

dev300.bridge.lmer = lmer(tot.raw.cross ~ dev.300 * num.bridge + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE)
dev300.bridge.glmer = glmer(tot.raw.cross ~ dev.300 * num.bridge + (1|animal.id), data = summed_crossing_analysis_table, family = "poisson")
aictab(list(dev300.bridge.glmer, dev300.bridge.lmer))

# full candidate sets ----
# dev.300 is best for tot.raw.cross
# dev.60 is best for tot.wt.cross
# currently using tot.raw.cross so will need to change model structures and names in the functions below if using tot.wt.cross instead
# modeling crossings with animal.id and year as random effects ----
fit_summed_crossing_mods_pumayr <- function(zcross) {
  
  df <- summed_crossing_analysis_table_long %>% 
    filter(cross.type == zcross)
#"animal.id" "year" "tre.shr.30"  "dev.60"     "tre.shr.300" "num.bridge"  "classes"     "max.class  
  zmods <- list(
    cov30 = lmer(cross.value ~ tre.shr.30 + (1|year/animal.id), data = df, REML = FALSE),
    cov300 = lmer(cross.value ~ tre.shr.300 + (1|year/animal.id), data = df, REML = FALSE),
    dev300 = lmer(cross.value ~ dev.300 + (1|year/animal.id), data = df, REML = FALSE),
    bridge = lmer(cross.value ~ num.bridge + (1|year/animal.id), data = df, REML = FALSE),
    # cover and number of bridges
    cov30_bridge = lmer(cross.value ~ tre.shr.30 + num.bridge + (1|year/animal.id), data = df, REML = FALSE),
    cov300_bridge = lmer(cross.value ~ tre.shr.300 + num.bridge + (1|year/animal.id), data = df, REML = FALSE),
    dev300_bridge = lmer(cross.value ~ dev.300 + num.bridge + (1|year/animal.id), data = df, REML = FALSE),
    
    cov30.bridge = lmer(cross.value ~ tre.shr.30 * num.bridge + (1|year/animal.id), data = df, REML = FALSE),
    cov300.bridge = lmer(cross.value ~ tre.shr.300 * num.bridge + (1|year/animal.id), data = df, REML = FALSE),
    dev300.bridge = lmer(cross.value ~ dev.300 * num.bridge + (1|year/animal.id), data = df, REML = FALSE),
    
    intercept = lmer(cross.value ~ 1  + (1|year/animal.id), data = df, REML = FALSE)
    
  )
  
  zmods$aic = aictab(zmods, names(zmods))
  
  return(zmods)
  
}



system.time(
raw_crossings_mods_pumayr <- fit_summed_crossing_mods_pumayr("tot.raw.cross")
)

# getting singular warning for most/all models
# and year resid variance is small, so removing year random effect

#
# modeling raw crossings with just animal.id as random effect ----

fit_raw_crossing_mods_puma <- function(zcross) {
  
  zmods <- list(
    cov30 = lmer(tot.raw.cross ~ tre.shr.30 + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    cov300 = lmer(tot.raw.cross ~ tre.shr.300 + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    dev300 = lmer(tot.raw.cross ~ dev.300 + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    bridge = lmer(tot.raw.cross ~ num.bridge + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    # cover and number of bridges
    cov30_bridge = lmer(tot.raw.cross ~ tre.shr.30 + num.bridge + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    cov300_bridge = lmer(tot.raw.cross ~ tre.shr.300 + num.bridge + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    dev300_bridge = lmer(tot.raw.cross ~ dev.300 + num.bridge + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    
    cov30.bridge = lmer(tot.raw.cross ~ tre.shr.30 * num.bridge + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    cov300.bridge = lmer(tot.raw.cross ~ tre.shr.300 * num.bridge + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    dev300.bridge = lmer(tot.raw.cross ~ dev.300 * num.bridge + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    
    intercept = lmer(tot.raw.cross ~ 1  + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE)
    
  )
  
  zmods$aic = aictab(zmods, names(zmods))
  
  return(zmods)
  
}
# fit for raw crossings as response

system.time(
  raw_crossings_mods_puma <- fit_raw_crossing_mods_puma("tot.raw.cross")
)



# modeling weighted crossings with just animal.id as random effect ----

fit_wt_crossing_mods_puma <- function() {
#  60m is thge best scale for % development for weighted crossings
  zmods <- list(
    cov30 = lmer(tot.wt.cross ~ tre.shr.30 + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    cov300 = lmer(tot.wt.cross ~ tre.shr.300 + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    dev60 = lmer(tot.wt.cross ~ dev.60 + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    bridge = lmer(tot.wt.cross ~ num.bridge + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    sex = lmer(tot.wt.cross ~ sex + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    # 2 way additive - not combining cov30 and cov300, cov30 and dev60, or cov300 and dev60
    cov30_bridge = lmer(tot.wt.cross ~ tre.shr.30 + num.bridge + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    cov300_bridge = lmer(tot.wt.cross ~ tre.shr.300 + num.bridge + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    dev60_bridge = lmer(tot.wt.cross ~ dev.60 + num.bridge + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    #
    cov30_sex = lmer(tot.wt.cross ~ tre.shr.30 + sex + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    cov300_sex = lmer(tot.wt.cross ~ tre.shr.300 + sex + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    dev60_sex = lmer(tot.wt.cross ~ dev.60 + sex + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    #
    bridge_sex = lmer(tot.wt.cross ~ num.bridge + sex + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    
    # 3 way additive
    cov30_sex_bridge = lmer(tot.wt.cross ~ tre.shr.30 + sex + num.bridge + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    cov300_sex_bridge = lmer(tot.wt.cross ~ tre.shr.300 + sex + num.bridge + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    dev60_sex_bridge = lmer(tot.wt.cross ~ dev.60 + sex + num.bridge + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    
    
    # 2 way interactions
    cov30.bridge = lmer(tot.wt.cross ~ tre.shr.30 * num.bridge + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    cov300.bridge = lmer(tot.wt.cross ~ tre.shr.300 * num.bridge + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    dev60.bridge = lmer(tot.wt.cross ~ dev.60 * num.bridge + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    #
    cov30.sex = lmer(tot.wt.cross ~ tre.shr.30 * sex + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    cov300.sex = lmer(tot.wt.cross ~ tre.shr.300 * sex + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    dev60.sex = lmer(tot.wt.cross ~ dev.60 * sex + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    #
    
    bridge.sex = lmer(tot.wt.cross ~ num.bridge * sex + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    
    # 3 way interactions
    cov30.sex.bridge = lmer(tot.wt.cross ~ tre.shr.30 * sex * num.bridge + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    cov300.sex.bridge = lmer(tot.wt.cross ~ tre.shr.300 * sex * num.bridge + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    dev60.sex.bridge = lmer(tot.wt.cross ~ dev.60 * sex * num.bridge + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE),
    
    # null
    intercept = lmer(tot.wt.cross ~ 1  + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE)
    
  )
  
  zmods$aic = aictab(zmods, names(zmods))
  
  return(zmods)
  
}


# fit for weighted crossings as response

system.time(
  wt_crossings_mods_puma <- fit_wt_crossing_mods_puma()
)

wt_crossings_mods_puma$aic

saveRDS(wt_crossings_mods_puma, here("model_objects/wt_crossings_mods_puma"))


wt_crossings_mods_puma <- readRDS(here("model_objects/wt_crossings_mods_puma"))

best_mod <- wt_crossings_mods_puma$dev60.sex.bridge


r.squaredGLMM(best_mod)



znewdat = expand.grid(sex = c("F", "M"),
                      dev.60 = seq(0, 1, by = 0.1),
                      num.bridge = seq(0, 3)
                      #, animal.id = distinct(summed_crossing_analysis_table_long, animal.id)$animal.id
                      ) 


zpred_wt = predict(wt_crossings_mods_puma$dev60.sex.bridge, znewdat, se.fit = TRUE, type = "response", re.form = NA) %>% 
  data.frame() %>% 
  bind_cols(znewdat) %>% 
  mutate(lwr = fit - (1.96 * se.fit),
         upr = fit + (1.96 * se.fit),
         dev.60 = dev.60 * 100)

zpred_wt %>% 
  ggplot() +
  geom_line(aes(y = fit, x = dev.60, color = as.character(num.bridge))) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, x = dev.60, fill = as.character(num.bridge)), alpha = 0.3) +
  facet_wrap(~sex) +
  labs(x = "% developed area within 60m of road",
       y = "# crossings",
       title = "Mean number of road crossings per mt lion per year per 1300m road segment",
       color = "# bridges per\nroad segment",
       fill = "# bridges per\nroad segment") +
  theme_bw() + 
  guides(colour = guide_legend(reverse=T),
         fill = guide_legend(reverse=T))

ggsave(here("figures/wt_mod_plot_3bridge.png"), width = 7, height = 5)

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




