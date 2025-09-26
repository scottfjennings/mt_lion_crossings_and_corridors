



library(tidyverse)
library(here)
library(lme4)
library(DHARMa)
library(car)
library(influence.ME)


crossing_models <- readRDS(here("model_objects/crossing_models"))

best_model <- crossing_models$cohesion_patchrd_tre.shr_impervious_bridge.binary_class_sex_age

round2_crossing_models <- readRDS(here("model_objects/round2_crossing_models"))

best_model <- round2_crossing_models$cohesion2_patchrd_tre.shr2_impervious2_bridge.binary_class_sex_age

# 1. Convergence & singularity ---------------------------------
summary(best_model)         # check warnings, coefficient estimates, random effects
isSingular(best_model)      # TRUE if random effects are redundant

# 2. Residual diagnostics (DHARMa) -----------------------------
sim_res <- simulateResiduals(best_model, n = 1000)  # simulate residuals
plot(sim_res)                                     # overall fit
testDispersion(sim_res)                           # overdispersion test
testZeroInflation(sim_res)                        # zero-inflation test
testUniformity(sim_res)                           # uniformity (general fit)



# fitting model with observation level random effect

analysis_table_complete <- readRDS(here("data/analysis_table_complete"))

analysis_table_scaled <- analysis_table_complete %>% 
  mutate(cohesion = scale(cohesion)[,1],
         impervious = scale(impervious)[,1],
         tre.shr = scale(tre.shr)[,1])


# create an observation-level factor (one level per row)
analysis_table_scaled$obsID <- factor(seq_len(nrow(analysis_table_scaled)))

# quick way to add OLRE to whatever formula best_model already uses
best_model_olre <- update(best_model, . ~ . + (1 | obsID))

summary(best_model_olre)


sim_res_olre <- simulateResiduals(best_model_olre, n = 1000)  # simulate residuals
plot(sim_res_olre)                                     # overall fit
testDispersion(sim_res_olre)                           # overdispersion test
testZeroInflation(sim_res_olre)                        # zero-inflation test
testUniformity(sim_res_olre)                           # uniformity (general fit)


# fitting best model with betabinomial in glmmTMB
library(glmmTMB)

best_model_bb <- glmmTMB(
  used.available ~ cohesion + patchrd + tre.shr + impervious + bridge.binary + class + sex + age + (1 | animal.id),
  data = analysis_table_scaled,
  family = betabinomial(link = "logit")
)

summary(best_model_bb)


sim_res_bb <- simulateResiduals(best_model_bb, n = 1000)  # simulate residuals
plot(sim_res_bb)                                     # overall fit
testDispersion(sim_res_bb)                           # overdispersion test
testZeroInflation(sim_res_bb)                        # zero-inflation test
testUniformity(sim_res_bb)                           # uniformity (general fit)



# 3. Overdispersion check (alternative quick test) -------------
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq, ratio=prat, rdf=rdf, p=pval)
}
overdisp_fun(best_model)

# 4. Multicollinearity among predictors ------------------------
# (use fixed-effect part only, random effects not included in VIF calc)
fixed_form <- formula(best_model, fixed.only = TRUE)
lm_equiv <- lm(fixed_form, data = best_model@frame)  
vif(lm_equiv)   # rule of thumb: >5–10 = potential problem

# 5. Influence diagnostics (group-level) -----------------------
infl <- influence(best_model, group="animal.id")  
# Cook's distances per animal
cooks <- cooks.distance(infl)
plot(cooks, type="h", ylab="Cook's distance", main="Influence by animal ID")

# 6. Random effects inspection ---------------------------------
re <- ranef(best_model, condVar=TRUE)  # BLUPs with conditional variances
dotplot(re)                            # visualize variation among animals

# 7. Optional: check fitted vs observed ------------------------
plotResiduals(sim_res, form = best_model@frame$cohesion)  # example for one predictor
