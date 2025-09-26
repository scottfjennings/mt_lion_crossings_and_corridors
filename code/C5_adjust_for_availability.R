


# adjusting estimated preference for how available each habitat is following 
# Fieberg, J., J. Signer, Brian Smith, and T. Avgar. 2021. A ‘How to’ guide for interpreting parameters in habitat-selection analyses. Journal of Animal Ecology 90:1027–1043.
# code is adapted from Appendix A


library(lme4)
library(tidyverse)
library(here)

#crossing_models <- readRDS(here("model_objects/crossing_models"))
#best_model <- crossing_models$cohesion_patchrd_tre.shr_impervious_bridge.binary_class_sex_age

round2_crossing_models <- readRDS(here("model_objects/round2_crossing_models"))
best_model <- round2_crossing_models$cohesion2_patchrd_tre.shr2_impervious2_bridge.binary_class_sex_age


analysis_table <- readRDS(here("data/analysis_table_complete"))



analysis_table_complete <- analysis_table %>% 
  mutate(cohesion = scale(cohesion)[,1],
         impervious = scale(impervious)[,1],
         tre.shr = scale(tre.shr)[,1])






# Filter to include only available locations
available_locs_predicted_crossing_prob <- filter(analysis_table_complete, used.available == 0)
# predict from the model across all available locations
available_locs_predicted_crossing_prob$predicted.crossing.prob <- exp(predict(best_model, available_locs_predicted_crossing_prob))

saveRDS(available_locs_predicted_crossing_prob, here("model_objects/available_locs_predicted_crossing_prob"))


#  then sum predicted crossing probability over various combinations of predictor variables to derive the relative integrated probabilities of interest
# likelihood of females crossing relative to males
with(available_locs_predicted_crossing_prob, sum(wx[sex == "F"]) / sum(wx[sex == "M"]))
# likelihood of adults crossing relative to sub-adults
with(available_locs_predicted_crossing_prob, sum(wx[age == "adult"]) / sum(wx[age == "sub-adult"]))


# likelihood of crossing different road classes, relative to Local

with(available_locs_predicted_crossing_prob, sum(wx[class == "Collector"]) / sum(wx[class == "Local"]))
with(available_locs_predicted_crossing_prob, sum(wx[class == "Arterial"]) / sum(wx[class == "Local"]))
with(available_locs_predicted_crossing_prob, sum(wx[class == "Highway"]) / sum(wx[class == "Local"]))

class_integrated_probs <- available_locs_predicted_crossing_prob %>% 
  group_by(class, used.available) %>% 
  summarise(integrated.prob = sum(wx)) %>% 
  ungroup()

class_integrated_probs <- class_integrated_probs %>% 
  mutate(ref.integrated.prob = filter(class_integrated_probs, class == "Local") %>% pull(integrated.prob),
         integrated.relative.use = integrated.prob/ref.integrated.prob)



# likelihood of females crossing different road classes relative to males
with(available_locs_predicted_crossing_prob, sum(wx[sex == "F" & class == "Local"]) / sum(wx[sex == "M" & class == "Local"]))
with(available_locs_predicted_crossing_prob, sum(wx[sex == "F" & class == "Collector"]) / sum(wx[sex == "M" & class == "Collector"]))
with(available_locs_predicted_crossing_prob, sum(wx[sex == "F" & class == "Arterial"]) / sum(wx[sex == "M" & class == "Arterial"]))
with(available_locs_predicted_crossing_prob, sum(wx[sex == "F" & class == "Highway"]) / sum(wx[sex == "M" & class == "Highway"]))

sex_class_integrated_probs <- available_locs_predicted_crossing_prob %>% 
  group_by(sex, class, used.available) %>% 
  summarise(integrated.prob = sum(wx)) %>% 
  ungroup()

sex_class_integrated_probs <- sex_class_integrated_probs %>% 
  mutate(ref.integrated.prob = ifelse(sex == "F", filter(sex_class_integrated_probs, sex == "F", class == "Local") %>% pull(integrated.prob), filter(sex_class_integrated_probs, sex == "M", class == "Local") %>% pull(integrated.prob)),
    ref.integrated.prob.f = filter(sex_class_integrated_probs, sex == "F", class == "Local") %>% pull(integrated.prob),
    integrated.relative.use = integrated.prob/ref.integrated.prob)

sex_class_integrated_probs %>% 
  filter(class != "Local") %>% 
  ggplot(aes(x = class, y = integrated.relative.use, color = sex, group = sex)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_linerange(aes(ymin = 1, ymax = integrated.relative.use), linewidth = 2, position = position_dodge(width = 0.25)) +
  #geom_point(position = position_dodge(width = 0.25), size = 4) +
  theme_bw()
