




library(lme4)
library(tidyverse)
library(here)



round2_crossing_models <- readRDS(here("model_objects/round2_crossing_models"))

analysis_table_complete <- readRDS(here("data/analysis_table_complete"))



analysis_table_scaled <- analysis_table_complete %>% 
  mutate(cohesion = scale(cohesion)[,1],
         impervious = scale(impervious)[,1],
         tre.shr = scale(tre.shr)[,1])


model1 <- round2_crossing_models$cohesion2_treshr2_impervious2_patchrd_bridgebinary_sex.class


model1_coef <- fixef(model1) %>% 
  data.frame()%>% rename(coef = 1) %>% 
  rownames_to_column("varb")


exp(
  1 * -1.36197357 + #(Intercept)
    0 * 0.88652337 + #poly(cohesion, 2, raw = TRUE)1
    0 * 0.25932724 + #poly(cohesion, 2, raw = TRUE)2
    0 * -0.98636838 + #poly(tre.shr, 2, raw = TRUE)1
    0 * 0.15780868 + #poly(tre.shr, 2, raw = TRUE)2
    0 * 0.24163886 + #poly(impervious, 2, raw = TRUE)1
    0 * -0.10034629 + #poly(impervious, 2, raw = TRUE)2
    0 * 0.76337037 + #patchrdTRUE
    0 * 1.18097134 + #bridge.binary
    1 * -2.14426149 + #sexM
    1 * -0.19726895 + #classCollector
    1 * -0.21034499 + #classArterial
    1 * -0.99959263 + #classHighway
    1 * 0.09811267 + #sexM:classCollector
    1 * 0.17110058 + #sexM:classArterial
    1 * 0.76067214 #sexM:classHighway
)


exp(1 * -1.36197357 + #(Intercept)
  1 * 1.18097134)




#female local vs highway
female_local <- exp(
  1 * -1.36197357 #(Intercept)
)

female_collector <-  exp(
    1 * -1.36197357 + #(Intercept)
      1 * -0.19726895 #classCollector
  ) 


female_arterial <- exp(
  1 * -1.36197357 + #(Intercept)
    1 * -0.21034499 #classArterial
)

female_highway <- exp(
    1 * -1.36197357 + #(Intercept)
      1 * -0.99959263 #classHighway
  ) 

male_local <- exp(
  1 * -1.36197357 + #(Intercept)
    1 * -2.14426149 #sexM
)

male_collector <- exp(
  1 * -1.36197357 + #(Intercept)
    1 * -2.14426149 + #sexM
    1 * -0.19726895 #classCollector
)

male_arterial <- exp(
  1 * -1.36197357 + #(Intercept)
    1 * -2.14426149 + #sexM
    1 * -0.21034499 + #classArterial
    1 * 0.17110058 #sexM:classArterial
)

male_highway <- exp(
  1 * -1.36197357 + #(Intercept)
    1 * -2.14426149 + #sexM
    1 * -0.99959263 + #classHighway
    1 * 0.76067214 #sexM:classHighway
)

# woody cover
woody.0 <- exp(
  1 * -1.36197357 + #(Intercept)
    0 * -0.98636838 + #poly(tre.shr, 2, raw = TRUE)1
    0 * 0.15780868 #poly(tre.shr, 2, raw = TRUE)2
)

woody.1 <- exp(
  1 * -1.36197357 + #(Intercept)
    1 * -0.98636838 + #poly(tre.shr, 2, raw = TRUE)1
    1 * 0.15780868 #poly(tre.shr, 2, raw = TRUE)2
)

woody.n1 <- exp(
  1 * -1.36197357 + #(Intercept)
    -1 * -0.98636838 + #poly(tre.shr, 2, raw = TRUE)1
    1 * 0.15780868 #poly(tre.shr, 2, raw = TRUE)2
)

# cohesion
cohesion.0 <- exp(
  1 * -1.36197357 + #(Intercept)
    0 * 0.88652337 + #poly(cohesion, 2, raw = TRUE)1
    0 * 0.25932724 #poly(cohesion, 2, raw = TRUE)2
)

cohesion.1 <- exp(
  1 * -1.36197357 + #(Intercept)
    1 * 0.88652337 + #poly(cohesion, 2, raw = TRUE)1
    1 * 0.25932724 #poly(cohesion, 2, raw = TRUE)2
)

cohesion.n1 <- exp(
  1 * -1.36197357 + #(Intercept)
    -1 * 0.88652337 + #poly(cohesion, 2, raw = TRUE)1
    1 * 0.25932724 #poly(cohesion, 2, raw = TRUE)2
)


sex_class_rss_crossings <- data.frame(
f.collector.local = female_collector/female_local,
f.arterial.local = female_arterial/female_local,
f.highway.local = female_highway/female_local,
m.collector.local = male_collector/male_local,
m.arterial.local = male_arterial/male_local,
m.highway.local = male_highway/male_local,
bridge = exp(1.18097134),
woody.patch = exp(0.76337037),
woody.cover.0.1 = woody.0/woody.1,
woody.cover.n1.0 = woody.n1/woody.0,
cohesion.1.0 = cohesion.1/cohesion.0,
cohesion.0.n1 = cohesion.0/cohesion.n1
) %>% 
  pivot_longer(cols = everything(), names_to = "scenario", values_to = "rss.crossing") %>% 
  mutate(rss.crossing = round(rss.crossing, 2))



# 


newdat <- expand.grid(sex = c("F", "M"),
                               cohesion = c(-1, 0, 1),
                               patchrd = c(FALSE, TRUE),
                               tre.shr = c(-1, 0, 1),
                               impervious = c(-1, 0, 1),
                               bridge.binary = c(0, 1),
                               class = c("Local", "Collector", "Arterial", "Highway"))

best_model_pred <- predict(model1, newdat,  re.form = NA, type = "link") %>% 
  data.frame() %>% 
  rename(predicted = 1) %>% 
  bind_cols(newdat) %>% 
  mutate(ss = exp(predicted))


filter(best_model_pred, sex == "F",cohesion == 0, patchrd == FALSE, tre.shr == -1, impervious == 0, bridge.binary == 0, class == "Local") %>% pull(ss)/
  filter(best_model_pred, sex == "F",cohesion == 0, patchrd == FALSE, tre.shr == 0, impervious == 0, bridge.binary == 0, class == "Local") %>% pull(ss)


filter(best_model_pred, sex == "F",cohesion == 0, patchrd == FALSE, tre.shr == 0, impervious == 0, bridge.binary == 0, class == "Local") %>% pull(ss)/
  filter(best_model_pred, sex == "F",cohesion == 0, patchrd == FALSE, tre.shr == 1, impervious == 0, bridge.binary == 0, class == "Local") %>% pull(ss)
