


library(tidyverse)
library(here)
library(spaMM)

options(scipen = 999)

source(here("code/utilities.R"))
source(here("code/helper_data.R"))




coarsener_test_mods <- readRDS(here("model_objects/lions_combined/coarsener_test_mods"))




compare_coarsener_coefs <- full_join(fixef(coarsener_test_mods$coarsener300) %>% data.frame() %>% rename_with(~"c300") %>% rownames_to_column("varb") %>% mutate(across(everything(), ~as.character(.))),
                                     fixef(coarsener_test_mods$coarsener500) %>% data.frame() %>% rename_with(~"c500") %>% rownames_to_column("varb") %>% mutate(across(everything(), ~as.character(.)))) %>% 
  full_join(fixef(coarsener_test_mods$coarsener1000) %>% data.frame() %>% rename_with(~"c1000") %>% rownames_to_column("varb") %>% mutate(across(everything(), ~as.character(.)))) %>% 
  full_join(fixef(coarsener_test_mods$coarsener1500) %>% data.frame() %>% rename_with(~"c1500") %>% rownames_to_column("varb") %>% mutate(across(everything(), ~as.character(.)))) %>% 
  full_join(fixef(coarsener_test_mods$coarsener2000) %>% data.frame() %>% rename_with(~"c2000") %>% rownames_to_column("varb") %>% mutate(across(everything(), ~as.character(.)))) %>% 
  full_join(fixef(coarsener_test_mods$coarsener2500) %>% data.frame() %>% rename_with(~"c2500") %>% rownames_to_column("varb") %>% mutate(across(everything(), ~as.character(.)))) %>% 
  full_join(fixef(coarsener_test_mods$coarsener3000) %>% data.frame() %>% rename_with(~"c3000") %>% rownames_to_column("varb") %>% mutate(across(everything(), ~as.character(.))))


get_betas <- function(zmod, name) {
  zsumm <- summary(zmod)
  zbeta_tab <- zsumm$beta_table %>%
    as.data.frame() %>%
    rownames_to_column("variable")  %>%
    mutate(coarsener = name)
  return(zbeta_tab)
}

# Apply over names
coarsener_beta_table <- imap_dfr(coarsener_test_mods, get_betas)

names(coarsener_beta_table) <- str_replace_all(names(coarsener_beta_table), " ", "")
names(coarsener_beta_table) <- str_replace_all(names(coarsener_beta_table), "-", ".")

coarsener_beta_table_summary <- coarsener_beta_table%>%
  filter(!coarsener %in% c("coarsener300", "coarsener500")) %>% 
  mutate(Estimate = as.numeric(Estimate)) %>% 
  group_by(variable) %>% 
  mutate(mean.Estimate = mean(Estimate),
         coarsener = str_replace(coarsener, "coarsener", ""),
         scaled.est = 100 * ((Estimate - mean.Estimate)/mean.Estimate),
         scaled.se = 100 * (Cond.SE/abs(mean.Estimate)),
         sd.est = sd(Estimate),
         cv.est = sd.est/abs(mean.Estimate),
         min.est = min(Estimate),
         max.est = max(Estimate),
         est.range = max(Estimate) - min(Estimate),
         scaled.est.range = max(scaled.est) - min(scaled.est),
         coarsener = as.numeric(coarsener),
         coarsener = coarsener/1000,
         coarsener = as.factor(coarsener)
         ) %>%
  ungroup() %>% 
  mutate(variable = str_replace_all(variable, "_", " + "),
         variable = str_replace_all(variable, "cohesion.200.25", "Cohesion"),
         variable = str_replace_all(variable, "mean.tre.shr.30", "% Woody"),
         variable = str_replace_all(variable, "patch.touches.roadTRUE", "Road intersects woody"),
         variable = str_replace_all(variable, "mean.dev.60", "% Development"),
         variable = str_replace_all(variable, "num.creek.bin", "# riparian crossings"),
         variable = str_replace_all(variable, "class", "Road Class-"))

saveRDS(coarsener_beta_table_summary, here("model_objects/lions_combined/coarsener_beta_table_summary"))


position_dodge_val <- position_dodge(width = 0.5)

coarsener_beta_table_summary %>%
  mutate(variable = fct_reorder(variable, -scaled.est.range)) %>%
  ggplot(aes(y = variable, x = scaled.est, color = coarsener)) +
  geom_point(position = position_dodge_val) +
  geom_errorbar(aes(xmin = scaled.est - scaled.se, xmax = scaled.est + scaled.se), width = 0.1, position = position_dodge_val) +
  theme_bw() +
  labs(color = "Coordinate\naggregation\nscale (km)",
       y = "",
       x = "% deviation in estimated coefficient from the mean")

ggsave(here("figures/aggregation_scale_coef_sensitivity.png"))






coarsener_beta_table_summary %>% 
  mutate(variable = str_replace_all(variable, "_", " + "),
         variable = str_replace_all(variable, "\\(|\\)", ""),
         variable = str_replace_all(variable, "cohesion.200.25", "Cohesion"),
         variable = str_replace_all(variable, "mean.tre.shr.30", "% Woody"),
         variable = str_replace_all(variable, "patch.touches.roadTRUE", "Road intersects woody"),
         variable = str_replace_all(variable, "mean.dev.60", "% Development"),
         variable = str_replace_all(variable, "num.creek.bin", "# riparian crossings"),
         variable = str_replace_all(variable, "class", "Road Class-")) %>%
  mutate(variable = fct_reorder(variable, -est.range),
         zfacet = ifelse(variable == "Intercept", "Intercept", "Other fixed effects")) %>%
  ggplot(aes(y = variable, x = Estimate, color = coarsener)) +
  geom_point(position = position_dodge_val) +
  geom_errorbar(aes(xmin = Estimate - Cond.SE, xmax = Estimate + Cond.SE), width = 0.1, position = position_dodge_val) +
  theme_bw() +
  labs(color = "Coordinate\naggregation\nscale (km)",
       y = "",
       x = "Estimated coefficients") + 
  facet_wrap(~zfacet, ncol = 1, scales = "free")

