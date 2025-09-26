




library(tidyverse)
library(here)
library(lme4)
library(AICcmodavg)
library(MuMIn)
library(purrr)



# from C1_analysis_table_model_structure
analysis_table_complete <- readRDS(here("data/analysis_table_complete"))


analysis_table_complete %>% 
  ungroup() %>% 
  ggplot() +
  geom_bar(aes(x = class, fill =  as.factor(used.available))) +
  facet_wrap(~sex)


analysis_table_complete %>% 
  ungroup() %>% 
  count(class, sex, used.available) %>% 
  mutate(used.available = ifelse(used.available == 0, "available", "used")) %>% 
  pivot_wider(id_cols = c(class, sex), names_from = used.available, values_from = n) %>% 
  mutate(raw.cross.prob = used/available)


analysis_table_complete %>% 
  ungroup() %>% 
  ggplot() +
  stat_density(aes(x = tre.shr, color = as.factor(used.available)), fill = NA)

analysis_table_complete %>% 
  ungroup() %>% 
  ggplot() +
  stat_density(aes(x = cohesion, color = as.factor(used.available)), fill = NA)

analysis_table_complete %>% 
  ungroup() %>% 
  ggplot() +
  stat_density(aes(x = impervious, color = as.factor(used.available)), fill = NA)

analysis_table_complete %>% 
  filter(impervious > 50) %>% 
  count(used.available)


analysis_table_scaled <- analysis_table_complete %>% 
  mutate(cohesion = scale(cohesion)[,1],
         impervious = scale(impervious)[,1],
         tre.shr = scale(tre.shr)[,1])







#' fit_glmer_mods
#' 
#' general model fitting function. this should be good for exploratory and hypothesis testing models. fits a lme4::glmer with offset for segment proportion inside each BBMM UD and a random effect for animal.id.
#' 
#' 
#' @param zformulas named list of fixed effects model structures
#' @param zfamily model family to specify the error distribution and link function
#'
#' @returns list of model objects, with names matching the names of zformulas, and an aic table "$aic" for all model structures in zformulas
#'
#' @examples
fit_glmer_mods <- function(zformulas) {
  
  zmods <- lapply(zformulas, function(fix.stru) {
    formula <- as.formula(paste("used.available ~ ", fix.stru, " + (1|animal.id)"))
    glmer(formula, data = analysis_table_scaled, family = binomial(link = "logit"))
  })
  
  zmods$aic <- aictab(zmods, names(zmods))
  
  return(zmods)
}




# initial model formulas  ----
base_model_formulas <- list(cohesion_treshr_impervious_patchrd_bridgebinary_class = "cohesion + tre.shr + impervious + patchrd + bridge.binary + class",
                            
                            cohesion2_treshr_impervious_patchrd_bridgebinary_class = "poly(cohesion, 2) + tre.shr + impervious + patchrd + bridge.binary + class",
                            cohesion_treshr2_impervious_patchrd_bridgebinary_class =  "cohesion + poly(tre.shr, 2) + impervious + patchrd + bridge.binary + class",
                            cohesion_treshr_impervious2_patchrd_bridgebinary_class =  "cohesion + tre.shr + poly(impervious, 2) + patchrd + bridge.binary + class",
                            
                            cohesion2_treshr2_impervious_patchrd_bridgebinary_class =  "poly(cohesion, 2) + poly(tre.shr, 2) + impervious + patchrd + bridge.binary + class",
                            cohesion_treshr2_impervious2_patchrd_bridgebinary_class =  "cohesion + poly(tre.shr, 2) + poly(impervious, 2) + patchrd + bridge.binary + class",
                            cohesion2_treshr_impervious2_patchrd_bridgebinary_class =  "poly(cohesion, 2) + tre.shr + poly(impervious, 2) + patchrd + bridge.binary + class",
                            
                            cohesion2_treshr2_impervious2_patchrd_bridgebinary_class =  "poly(cohesion, 2) + poly(tre.shr, 2) + poly(impervious, 2) + patchrd + bridge.binary + class",
                            
                            intercept = "1")



system.time(
  base_crossing_models <- fit_glmer_mods(base_model_formulas)
) 
# 113 for base_model_formulas

saveRDS(base_crossing_models, here("model_objects/base_crossing_models"))

base_crossing_models$aic         


#Model selection based on AICc:

#                                                                 K     AICc Delta_AICc AICcWt Cum.Wt       LL
#cohesion2_treshr2_impervious2_patchrd_bridgebinary_class         13 16820.53       0.00      1      1 -8397.26
#cohesion2_treshr_impervious2_patchrd_bridgebinary_class          12 16842.72      22.19      0      1 -8409.35
#cohesion2_treshr2_impervious_patchrd_bridgebinary_class          12 16854.86      34.33      0      1 -8415.42
#cohesion_treshr2_impervious2_patchrd_bridgebinary_class          12 16867.95      47.41      0      1 -8421.96
#cohesion2_treshr_impervious_patchrd_bridgebinary_class           11 16877.52      56.99      0      1 -8427.75
#cohesion_treshr_impervious2_patchrd_bridgebinary_class           11 16877.98      57.45      0      1 -8427.98
#cohesion_treshr2_impervious_patchrd_bridgebinary_class           11 16895.54      75.00      0      1 -8436.76
#cohesion_treshr_impervious_patchrd_bridgebinary_class            10 16906.66      86.13      0      1 -8443.32
#intercept                                                         2 17919.36    1098.83      0      1 -8957.68



# substantial support for the model with quadratic effect of all continuous


# next testing for sex and age interactions with all variables in best from above


sex_age_model_formulas <- list(cohesion2_treshr2_impervious2_patchrd_bridgebinary_class =  "poly(cohesion, 2) + poly(tre.shr, 2) + poly(impervious, 2) + patchrd + bridge.binary + class",
                               
                               sex.cohesion2_treshr2_impervious2_patchrd_bridgebinary_class =  "sex * poly(cohesion, 2) + poly(tre.shr, 2) + poly(impervious, 2) + patchrd + bridge.binary + class",
                               cohesion2_sex.treshr2_impervious2_patchrd_bridgebinary_class =  "poly(cohesion, 2) + sex * poly(tre.shr, 2) + poly(impervious, 2) + patchrd + bridge.binary + class",
                               cohesion2_treshr2_sex.impervious2_patchrd_bridgebinary_class =  "poly(cohesion, 2) + poly(tre.shr, 2) + sex * poly(impervious, 2) + patchrd + bridge.binary + class",
                               cohesion2_treshr2_impervious2_sex.patchrd_bridgebinary_class =  "poly(cohesion, 2) + poly(tre.shr, 2) + poly(impervious, 2) + sex * patchrd + bridge.binary + class",
                               cohesion2_treshr2_impervious2_patchrd_sex.bridgebinary_class =  "poly(cohesion, 2) + poly(tre.shr, 2) + poly(impervious, 2) + patchrd + sex * bridge.binary + class",
                               cohesion2_treshr2_impervious2_patchrd_bridgebinary_sex.class =  "poly(cohesion, 2) + poly(tre.shr, 2) + poly(impervious, 2) + patchrd + bridge.binary + sex * class",
                               
                               age.cohesion2_treshr2_impervious2_patchrd_bridgebinary_class =  "age * poly(cohesion, 2) + poly(tre.shr, 2) + poly(impervious, 2) + patchrd + bridge.binary + class",
                               cohesion2_age.treshr2_impervious2_patchrd_bridgebinary_class =  "poly(cohesion, 2) + age * poly(tre.shr, 2) + poly(impervious, 2) + patchrd + bridge.binary + class",
                               cohesion2_treshr2_age.impervious2_patchrd_bridgebinary_class =  "poly(cohesion, 2) + poly(tre.shr, 2) + age * poly(impervious, 2) + patchrd + bridge.binary + class",
                               cohesion2_treshr2_impervious2_age.patchrd_bridgebinary_class =  "poly(cohesion, 2) + poly(tre.shr, 2) + poly(impervious, 2) + age * patchrd + bridge.binary + class",
                               cohesion2_treshr2_impervious2_patchrd_age.bridgebinary_class =  "poly(cohesion, 2) + poly(tre.shr, 2) + poly(impervious, 2) + patchrd + age * bridge.binary + class",
                               cohesion2_treshr2_impervious2_patchrd_bridgebinary_age.class =  "poly(cohesion, 2) + poly(tre.shr, 2) + poly(impervious, 2) + patchrd + bridge.binary + age * class"
                               
)


system.time(
  sex_age_crossing_models <- fit_glmer_mods(sex_age_model_formulas)
) #355 

saveRDS(sex_age_crossing_models, here("model_objects/sex_age_crossing_models"))


sex_age_crossing_models$aic

#Model selection based on AICc:

#                                                             K     AICc Delta_AICc AICcWt Cum.Wt       LL
#cohesion2_treshr2_impervious2_patchrd_bridgebinary_sex.class 17 16799.58       0.00   0.52   0.52 -8382.77
#cohesion2_treshr2_sex.impervious2_patchrd_bridgebinary_class 16 16800.32       0.74   0.36   0.88 -8384.14
#cohesion2_treshr2_impervious2_patchrd_sex.bridgebinary_class 15 16804.08       4.49   0.05   0.93 -8387.02
#cohesion2_treshr2_impervious2_sex.patchrd_bridgebinary_class 15 16805.21       5.63   0.03   0.97 -8387.59
#sex.cohesion2_treshr2_impervious2_patchrd_bridgebinary_class 16 16806.04       6.45   0.02   0.99 -8387.00
#age.cohesion2_treshr2_impervious2_patchrd_bridgebinary_class 16 16807.87       8.29   0.01   0.99 -8387.92
#cohesion2_sex.treshr2_impervious2_patchrd_bridgebinary_class 16 16808.99       9.41   0.00   1.00 -8388.48
#cohesion2_treshr2_age.impervious2_patchrd_bridgebinary_class 16 16813.04      13.45   0.00   1.00 -8390.50
#cohesion2_treshr2_impervious2_age.patchrd_bridgebinary_class 15 16815.58      16.00   0.00   1.00 -8392.78
#cohesion2_age.treshr2_impervious2_patchrd_bridgebinary_class 16 16818.51      18.93   0.00   1.00 -8393.24
#cohesion2_treshr2_impervious2_patchrd_bridgebinary_class     13 16820.53      20.95   0.00   1.00 -8397.26
#cohesion2_treshr2_impervious2_patchrd_age.bridgebinary_class 15 16820.69      21.11   0.00   1.00 -8395.33
#cohesion2_treshr2_impervious2_patchrd_bridgebinary_age.class 17 16823.92      24.34   0.00   1.00 -8394.94



summary(sex_age_crossing_models$cohesion2_treshr2_impervious2_patchrd_bridgebinary_sex.class)

