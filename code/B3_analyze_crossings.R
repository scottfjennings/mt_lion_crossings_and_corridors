


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
all_hr_road_habitat_df <- readRDS(here("data/analysis_inputs/all_hr_road_habitat_df")) %>% 
  filter(!animal.id %in% hr_exclude_pumas,
         !seg.label %in% p31_exclude_segments)

best_hab <- full_join(all_hr_road_habitat_df %>% 
                        filter(buff == 60) %>% 
                        select(seg.label, animal.id, year, dev60 = mean.dev),
                      all_hr_road_habitat_df %>% 
                        filter(buff == 300) %>% 
                        select(seg.label, animal.id, year, treshr300 = mean.tre.shr))


crossing_analysis_table <- readRDS(here("data/analysis_inputs/seg_crossing_sums_naive_roads_only")) %>% 
  filter(!animal.id %in% hr_exclude_pumas) %>% 
  left_join(puma_sexes) %>% 
  left_join(best_hab) %>% 
  left_join(readRDS(here("data/analysis_inputs/streams_per_segment"))) %>% 
  filter(!animal.id %in% hr_exclude_pumas,
         !seg.label %in% p31_exclude_segments,
         !(is.na(dev60) | is.na(treshr300))) # still a few lingering segments along the coast with NA habitat values
  

# any NA
crossing_analysis_table %>%
  filter(if_any(everything(), is.na)) %>% nrow()
# good 3/20/25

# some raw visualization ----

crossing_analysis_table %>%
  ggplot() +
  geom_point(aes(y = seg.raw.crossing, x = as.character(year), color = sex), alpha = 0.2) +
  stat_smooth(aes(y = seg.raw.crossing, x = as.character(year), color = sex)) +
  facet_wrap(~animal.id, scales = "free_y")
# not much year pattern


# fit models ----


# compare lmer vs glmer ----

dev60.bridge.lmer = lmer(seg.raw.crossing ~ dev60 * num.creek + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE)
dev60.bridge.glmer = glmer(seg.raw.crossing ~ dev60 * num.creek + (1|animal.id), data = summed_crossing_analysis_table, family = "poisson")
aictab(list(dev60.bridge.glmer, dev60.bridge.lmer))

# full candidate sets ----
# dev.60 is best for seg.raw.crossing
# dev.60 is best for seg.wt.crossing
# currently using seg.raw.crossing so will need to change model structures and names in the functions below if using seg.wt.crossing instead
# modeling crossings with animal.id and year as random effects ----
fit_summed_crossing_mods_pumayr <- function(df) {
  
#"animal.id" "year" "tre.shr.30"  "dev.60"     "tre.shr.300" "num.creek"  "classes"     "max.class  
  zmods <- list(
    cov30 = lmer(cross.value ~ tre.shr.30 + (1|year/animal.id), data = df, REML = FALSE),
    cov300 = lmer(cross.value ~ tre.shr.300 + (1|year/animal.id), data = df, REML = FALSE),
    dev60 = lmer(cross.value ~ dev.60 + (1|year/animal.id), data = df, REML = FALSE),
    bridge = lmer(cross.value ~ num.creek + (1|year/animal.id), data = df, REML = FALSE),
    # cover and number of bridges
    cov30_bridge = lmer(cross.value ~ tre.shr.30 + num.creek + (1|year/animal.id), data = df, REML = FALSE),
    cov300_bridge = lmer(cross.value ~ tre.shr.300 + num.creek + (1|year/animal.id), data = df, REML = FALSE),
    dev60_bridge = lmer(cross.value ~ dev.60 + num.creek + (1|year/animal.id), data = df, REML = FALSE),
    
    cov30.bridge = lmer(cross.value ~ tre.shr.30 * num.creek + (1|year/animal.id), data = df, REML = FALSE),
    cov300.bridge = lmer(cross.value ~ tre.shr.300 * num.creek + (1|year/animal.id), data = df, REML = FALSE),
    dev60.bridge = lmer(cross.value ~ dev.60 * num.creek + (1|year/animal.id), data = df, REML = FALSE),
    
    intercept = lmer(cross.value ~ 1  + (1|year/animal.id), data = df, REML = FALSE)
    
  )
  
  zmods$aic = aictab(zmods, names(zmods))
  
  return(zmods)
  
}



system.time(
raw_crossings_mods_pumayr <- summed_crossing_analysis_table %>% 
  rename(cross.value = seg.raw.crossing) %>% 
  fit_summed_crossing_mods_pumayr()
)

# getting singular warning for most/all models
# and year resid variance is small, so removing year random effect

#
# modeling raw crossings with just animal.id as random effect ----

fit_raw_crossing_mods_puma <- function(df) {
  
  zmods <- list(
    cov30 = lmer(seg.raw.crossing ~ tre.shr.30 + (1|animal.id), data = df, REML = FALSE),
    cov300 = lmer(seg.raw.crossing ~ tre.shr.300 + (1|animal.id), data = df, REML = FALSE),
    dev60 = lmer(seg.raw.crossing ~ dev.60 + (1|animal.id), data = df, REML = FALSE),
    bridge = lmer(seg.raw.crossing ~ num.creek + (1|animal.id), data = df, REML = FALSE),
    # cover and number of bridges
    cov30_bridge = lmer(seg.raw.crossing ~ tre.shr.30 + num.creek + (1|animal.id), data = df, REML = FALSE),
    cov300_bridge = lmer(seg.raw.crossing ~ tre.shr.300 + num.creek + (1|animal.id), data = df, REML = FALSE),
    dev60_bridge = lmer(seg.raw.crossing ~ dev.60 + num.creek + (1|animal.id), data = df, REML = FALSE),
    
    cov30.bridge = lmer(seg.raw.crossing ~ tre.shr.30 * num.creek + (1|animal.id), data = df, REML = FALSE),
    cov300.bridge = lmer(seg.raw.crossing ~ tre.shr.300 * num.creek + (1|animal.id), data = df, REML = FALSE),
    dev60.bridge = lmer(seg.raw.crossing ~ dev.60 * num.creek + (1|animal.id), data = df, REML = FALSE),
    
    intercept = lmer(seg.raw.crossing ~ 1  + (1|animal.id), data = df, REML = FALSE)
    
  )
  
  zmods$aic = aictab(zmods, names(zmods))
  
  return(zmods)
  
}
# fit for raw crossings as response

system.time(
  raw_crossings_mods_puma <- fit_raw_crossing_mods_puma(summed_crossing_analysis_table)
)



# modeling weighted crossings with just animal.id as random effect ----

fit_wt_crossing_mods_puma <- function(df) {
#  60m is thge best scale for % development for weighted crossings
  zmods <- list(
    cov30 = lmer(seg.wt.crossing ~ tre.shr.30 + (1|animal.id), data = df, REML = FALSE),
    cov300 = lmer(seg.wt.crossing ~ tre.shr.300 + (1|animal.id), data = df, REML = FALSE),
    dev60 = lmer(seg.wt.crossing ~ dev.60 + (1|animal.id), data = df, REML = FALSE),
    bridge = lmer(seg.wt.crossing ~ num.creek + (1|animal.id), data = df, REML = FALSE),
    class = lmer(seg.wt.crossing ~ class + (1|animal.id), data = df, REML = FALSE),
    sex = lmer(seg.wt.crossing ~ sex + (1|animal.id), data = df, REML = FALSE),
    # 2 way additive - not combining cov30 and cov300, cov30 and dev60, or cov300 and dev60
    cov30_bridge = lmer(seg.wt.crossing ~ tre.shr.30 + num.creek + (1|animal.id), data = df, REML = FALSE),
    cov300_bridge = lmer(seg.wt.crossing ~ tre.shr.300 + num.creek + (1|animal.id), data = df, REML = FALSE),
    dev60_bridge = lmer(seg.wt.crossing ~ dev.60 + num.creek + (1|animal.id), data = df, REML = FALSE),
    #
    cov30_sex = lmer(seg.wt.crossing ~ tre.shr.30 + sex + (1|animal.id), data = df, REML = FALSE),
    cov300_sex = lmer(seg.wt.crossing ~ tre.shr.300 + sex + (1|animal.id), data = df, REML = FALSE),
    dev60_sex = lmer(seg.wt.crossing ~ dev.60 + sex + (1|animal.id), data = df, REML = FALSE),
    #
    cov30_class = lmer(seg.wt.crossing ~ tre.shr.30 + class + (1|animal.id), data = df, REML = FALSE),
    cov300_class = lmer(seg.wt.crossing ~ tre.shr.300 + class + (1|animal.id), data = df, REML = FALSE),
    dev60_class = lmer(seg.wt.crossing ~ dev.60 + class + (1|animal.id), data = df, REML = FALSE),
    #
    bridge_sex = lmer(seg.wt.crossing ~ num.creek + sex + (1|animal.id), data = df, REML = FALSE),
    class_sex = lmer(seg.wt.crossing ~ class + sex + (1|animal.id), data = df, REML = FALSE),
    class_bridge = lmer(seg.wt.crossing ~ class + num.creek + (1|animal.id), data = df, REML = FALSE),
    
    # 3 way additive
    cov30_sex_bridge = lmer(seg.wt.crossing ~ tre.shr.30 + sex + num.creek + (1|animal.id), data = df, REML = FALSE),
    cov300_sex_bridge = lmer(seg.wt.crossing ~ tre.shr.300 + sex + num.creek + (1|animal.id), data = df, REML = FALSE),
    dev60_sex_bridge = lmer(seg.wt.crossing ~ dev.60 + sex + num.creek + (1|animal.id), data = df, REML = FALSE),
    #
    cov30_sex_class = lmer(seg.wt.crossing ~ tre.shr.30 + sex + class + (1|animal.id), data = df, REML = FALSE),
    cov300_sex_class = lmer(seg.wt.crossing ~ tre.shr.300 + sex + class + (1|animal.id), data = df, REML = FALSE),
    dev60_sex_class = lmer(seg.wt.crossing ~ dev.60 + sex + class + (1|animal.id), data = df, REML = FALSE),
    
    
    # 2 way interactions
    cov30.bridge = lmer(seg.wt.crossing ~ tre.shr.30 * num.creek + (1|animal.id), data = df, REML = FALSE),
    cov300.bridge = lmer(seg.wt.crossing ~ tre.shr.300 * num.creek + (1|animal.id), data = df, REML = FALSE),
    dev60.bridge = lmer(seg.wt.crossing ~ dev.60 * num.creek + (1|animal.id), data = df, REML = FALSE),
    #
    cov30.sex = lmer(seg.wt.crossing ~ tre.shr.30 * sex + (1|animal.id), data = df, REML = FALSE),
    cov300.sex = lmer(seg.wt.crossing ~ tre.shr.300 * sex + (1|animal.id), data = df, REML = FALSE),
    dev60.sex = lmer(seg.wt.crossing ~ dev.60 * sex + (1|animal.id), data = df, REML = FALSE),
    #
    cov30.class = lmer(seg.wt.crossing ~ tre.shr.30 * class + (1|animal.id), data = df, REML = FALSE),
    cov300.class = lmer(seg.wt.crossing ~ tre.shr.300 * class + (1|animal.id), data = df, REML = FALSE),
    dev60.class = lmer(seg.wt.crossing ~ dev.60 * class + (1|animal.id), data = df, REML = FALSE),
    #
    bridge.sex = lmer(seg.wt.crossing ~ num.creek * sex + (1|animal.id), data = df, REML = FALSE),
    bridge.class = lmer(seg.wt.crossing ~ num.creek * class + (1|animal.id), data = df, REML = FALSE),
    class.sex = lmer(seg.wt.crossing ~ class * sex + (1|animal.id), data = df, REML = FALSE),
    
    # 3 way interactions
    cov30.sex.bridge = lmer(seg.wt.crossing ~ tre.shr.30 * sex * num.creek + (1|animal.id), data = df, REML = FALSE),
    cov300.sex.bridge = lmer(seg.wt.crossing ~ tre.shr.300 * sex * num.creek + (1|animal.id), data = df, REML = FALSE),
    dev60.sex.bridge = lmer(seg.wt.crossing ~ dev.60 * sex * num.creek + (1|animal.id), data = df, REML = FALSE),
    #
    cov30.sex.class = lmer(seg.wt.crossing ~ tre.shr.30 * sex * class + (1|animal.id), data = df, REML = FALSE),
    cov300.sex.class = lmer(seg.wt.crossing ~ tre.shr.300 * sex * class + (1|animal.id), data = df, REML = FALSE),
    dev60.sex.class = lmer(seg.wt.crossing ~ dev.60 * sex * class + (1|animal.id), data = df, REML = FALSE),
    #
    cov30.class.bridge = lmer(seg.wt.crossing ~ tre.shr.30 * class * num.creek + (1|animal.id), data = df, REML = FALSE),
    cov300.class.bridge = lmer(seg.wt.crossing ~ tre.shr.300 * class * num.creek + (1|animal.id), data = df, REML = FALSE),
    dev60.class.bridge = lmer(seg.wt.crossing ~ dev.60 * class * num.creek + (1|animal.id), data = df, REML = FALSE),
    #
    # null
    intercept = lmer(seg.wt.crossing ~ 1  + (1|animal.id), data = df, REML = FALSE)
    
  )
  
  zmods$aic = aictab(zmods, names(zmods))
  
  return(zmods)
  
}


# fit for weighted crossings as response

system.time(
  wt_crossings_mods_puma <- fit_wt_crossing_mods_puma(summed_crossing_analysis_table)
)

best_aic <- wt_crossings_mods_puma$aic

saveRDS(wt_crossings_mods_puma, here("model_objects/wt_crossings_mods_puma"))


wt_crossings_mods_puma <- readRDS(here("model_objects/wt_crossings_mods_puma"))

best_mod <- wt_crossings_mods_puma$dev60.sex.class

sec_best_mod <- wt_crossings_mods_puma$dev60.class.bridge

r.squaredGLMM(best_mod)



znewdat = expand.grid(sex = c("F", "M"),
                      dev.60 = seq(0, 1, by = 0.1),
                      num.creek = seq(0, 3),
                      class = c("Local", "Collector", "Arterial", "Highway", "Freeway")
                      #, animal.id = distinct(summed_crossing_analysis_table_long, animal.id)$animal.id
                      ) 


zpred_wt = predict(wt_crossings_mods_puma$dev60.sex.class, znewdat, se.fit = TRUE, re.form = NA) %>% 
  data.frame() %>% 
  bind_cols(znewdat) %>% 
  mutate(lwr = fit - (1.96 * se.fit),
         upr = fit + (1.96 * se.fit),
         dev.60 = dev.60 * 100,
         sex = ifelse(sex == "F", "Female", "Male"),
         class = factor(class, levels = c("Local", "Collector", "Arterial", "Highway", "Freeway")))

zpred_wt %>% 
  ggplot() +
  geom_line(aes(y = fit, x = dev.60, color = class), linewidth = 3) +
  #geom_ribbon(aes(ymin = lwr, ymax = upr, x = dev.60, fill = class), alpha = 0.3) +
  facet_wrap(~sex) +
  labs(x = "% developed area within 60m of road",
       y = "# crossings",
       #title = "Mean number of road crossings per mt lion per year per 1300m road segment",
       color = "",
       fill = "") +
  theme_bw() + 
  guides(colour = guide_legend(reverse=T))+
  theme(
    axis.title = element_text(size = 16),        # Increase axis title size
    axis.text = element_text(size = 14),         # Increase axis text size
    strip.text = element_text(size = 14),        # Increase facet label size
    legend.text = element_text(size = 14),       # Increase legend text size
    legend.title = element_text(size = 16)       # Increase legend title size
  )

ggsave(here("figures/dev60.sex.class_600dpi.png"), dpi = 600, width = 8.5)

# fit with weighted crossings as response
wt_crossings_mods <- fit_summed_crossing_mods("seg.wt.crossing")

zpred_wt = predict(wt_crossings_mods$dev60.bridge, znewdat, se.fit = TRUE, type = "response", re.form = NA) %>% 
  data.frame() %>% 
  bind_cols(znewdat) %>% 
  mutate(lwr = fit - (1.96 * se.fit),
         upr = fit + (1.96 * se.fit),
         dev.60 = dev.60 * 100)

zpred_wt %>% 
  ggplot() +
  geom_line(aes(y = fit, x = dev.60, color = as.character(num.creek))) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, x = dev.60, fill = as.character(num.creek)), alpha = 0.3) +
  #facet_wrap(~num.creek) +
  labs(x = "% developed area within 60m of road",
       y = "# crossings",
       title = "Mean number of weighted road crossings\nper year per 1300m road segment",
       color = "# bridges per\nroad segment",
       fill = "# bridges per\nroad segment") +
  theme_bw()




