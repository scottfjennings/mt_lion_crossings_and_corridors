

# bootstrapping confidence intervals for coefficients and model predictions

# uses parallel processing so much quicker to run on AVD (32 cores) than my laptop (16 cores)

# - `future.seed = TRUE` ensures parallel-safe, reproducible random numbers.
# - `tryCatch()` prevents a single failed refit from stopping the workflow.
# - Adjust `nsim` and `n_cores` depending on your machine.
# - `re.form = NA` gives population-level predictions (ignores random effects).



library(lme4)
library(tidyverse)
library(here)
library(future.apply)



sex_age_crossing_models <-readRDS(here("model_objects/sex_age_crossing_models"))
#best_model <- crossing_models$cohesion_patchrd_tre.shr_impervious_bridge.binary_class_sex_age


analysis_table <- readRDS(here("data/analysis_table_complete"))



analysis_table_complete <- analysis_table %>% 
  mutate(cohesion = scale(cohesion)[,1],
         impervious = scale(impervious)[,1],
         tre.shr = scale(tre.shr)[,1])


# Setup for bootstrapping ----

# Number of bootstrap iterations
nsim <- 1000  # increase to 1000+ if your computer allows

# Number of parallel workers
n_cores <- parallel::detectCores() - 1
plan(multisession, workers = n_cores)  # works on Windows/macOS/Linux

# Optional: reproducible results
set.seed(1234)


#  Bootstrap for fixed-effect coefficients ----


bootstrap_coef_fun <- function(i) {
  # simulate response
  y_sim <- simulate(best_model, nsim = 1)[[1]]
  
  # create simulated dataset
  data_sim <- analysis_table_complete %>% 
    select(-used.available)
  data_sim[["used.available"]] <- y_sim
  
  # refit model safely
  tryCatch({
    fit <- update(best_model, data = data_sim)
    fixef(fit)
  }, error = function(e) rep(NA, length(fixef(best_model))))
}

# Run parallel bootstrap for coefficients

best_model = sex_age_crossing_models$cohesion2_treshr2_impervious2_patchrd_bridgebinary_sex.class # 2859 to run bootstrap_coef_fun
best_model = sex_age_crossing_models$cohesion2_treshr2_sex.impervious2_patchrd_bridgebinary_class # 2535 to run bootstrap_coef_fun


system.time(
boot_coefs <- future_lapply(1:nsim, bootstrap_coef_fun, future.seed = TRUE)
) #1303 for nsim = 500. 800 on AVD
# 5157 for nsim = 2000
#1292 for nsim = 1000 on avd for full round 1 model

#saveRDS(boot_coefs, here("model_objects/best_mod_boot_coefs_2000sim"))

# Convert to matrix and remove failed iterations
boot_matrix <- do.call(rbind, boot_coefs)
boot_matrix <- boot_matrix[complete.cases(boot_matrix), ]

# Compute 95% CI
boot_ci_coef <- t(apply(boot_matrix, 2, quantile, probs = c(0.025, 0.5, 0.975)))
colnames(boot_ci_coef) <- c("lower", "median", "upper")
rownames(boot_ci_coef) <- names(fixef(best_model))

boot_ci_coef <- data.frame(boot_ci_coef) %>% 
  rownames_to_column("term")

saveRDS(boot_ci_coef, here("model_objects/boot_coef_ci_1000sim_cohesion2_treshr2_sex-impervious2_patchrd_bridgebinary_class"))
boot_ci_coef <- readRDS(here("model_objects/best_mod_boot_ci_coef_2000sim"))

# checking fixed-effect CI against CI from 1.96 * SE ----

coefs <- fixef(best_model)
ses <- sqrt(diag(vcov(best_model)))
  
fixed_effects_df <- tibble(
    term = names(coefs),
    estimate = coefs,
    std_error = ses
  )%>% 
  mutate(lower = estimate - (1.96 * std_error),
         upper = estimate + (1.96 * std_error)) %>% 
  mutate(which.ci = "1.96 * SE") 


boot_ci_coef <- boot_ci_coef %>% 
  data.frame() %>% 
  rownames_to_column("term") %>% 
  mutate(which.ci = "bootstrapped") %>% 
  rename(estimate = median)


ci_compare <- bind_rows(fixed_effects_df, boot_ci_coef) %>% 
  mutate(term = str_replace_all(term, "poly\\(", ""),
         term = str_replace_all(term, ", 2\\)1", ""),
         term = str_replace_all(term, ", 2\\)", ""),
         term = str_replace_all(term, "TRUE", ""),
         term = str_replace(term, "cohesion", "Cohesion"),
         term = str_replace(term, "cohesion", "Cohesion"),
         term = str_replace(term, "patchrd", "CrossWoody"),
         term = str_replace(term, "tre.shr", "% Woody"),
         term = str_replace(term, "impervious", "% Impervious"),
         term = str_replace(term, "bridge.binary", "Bridge/culvert"),
         term = str_replace(term, "class", "Class-"),
         term = str_replace(term, "sexM", "Sex-Male"),
         term = str_replace(term, "agesub-adult", "Age-Sub adult"),
         term = str_replace_all(term, "2", "²")
  ) %>% 
  filter(term != "(Intercept)")




ci_compare %>% 
  ggplot() +
  geom_point(aes(x = term, y = estimate, color = which.ci), position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(x = term, ymin = lower, ymax = upper, color = which.ci), position = position_dodge(width = 0.2), width = 0.2) +
  geom_hline(yintercept = 0)

# bootstrapped CI are slightly broader but don't change overlap 0 status or other interpretation


# Bootstrap for predicted probabilities----
# define bootstrapping function

bootstrap_pred_fun <- function(i, best_model, newdat) {
  # Simulate new response
  y_sim <- simulate(best_model, nsim = 1)[[1]]
  
  # Copy model data and replace response
  data_sim <- analysis_table_complete %>% 
    select(-used.available)
  
  data_sim[["used.available"]] <- y_sim
  
  # Refit model directly (no update())
  fit <- tryCatch({
    glmer(
      #formula = formula(best_model),
      formula =  used.available ~ poly(cohesion, 2) + patchrd + poly(tre.shr, 2) + poly(impervious, 2) + bridge.binary + class + sex + age + (1|animal.id),
      data    = data_sim,
      family  = binomial(link = "logit")#,
      #control = glmerControl(optimizer = "bobyqa")
    )
  }, error = function(e) NULL)
  
  # If refit fails, return NA row
  if (is.null(fit)) {
    return(rep(NA, nrow(newdat)))
  }
  
  # Get predictions on response scale
  predict(fit, newdata = newdat, re.form = NA, type = "response")
}




# pull scaling attributes from the data the models were fitted to
cohesion_center <- attr(scale(analysis_table$cohesion), "scaled:center")
cohesion_scale  <- attr(scale(analysis_table$cohesion), "scaled:scale")

tre.shr_center <- attr(scale(analysis_table$tre.shr), "scaled:center")
tre.shr_scale  <- attr(scale(analysis_table$tre.shr), "scaled:scale")

impervious_center <- attr(scale(analysis_table$impervious), "scaled:center")
impervious_scale  <- attr(scale(analysis_table$impervious), "scaled:scale")

# Build newdata with scaled predictors (for the model)
# this newdat has full combos of all predictors and works for bootstrap_pred_fun on the AVD with 31 cores
newdat = expand.grid(sex = c("F", "M"),
                             age = c("adult", "sub-adult"),
                             patchrd = c(TRUE, FALSE),
                             bridge.binary = c(0, 1),
                             class = c("Local", "Collector", "Arterial", "Highway"),
                             year = 2020,
                             cohesion.orig = c(mean(analysis_table$cohesion), c(quantile(analysis_table$cohesion, c(0, .2, .4, .6, .8, 1)))),
                             tre.shr.orig = c(mean(analysis_table$tre.shr), c(quantile(analysis_table$tre.shr, c(0, .2, .4, .6, .8, 1)))),
                             impervious.orig = c(mean(analysis_table$impervious), c(quantile(analysis_table$impervious, c(0, .2, .4, .6, .8, 1))))) %>% 
  mutate(cohesion = (cohesion.orig - cohesion_center) / cohesion_scale,
         tre.shr = (tre.shr.orig - tre.shr_center) / tre.shr_scale,
         impervious = (impervious.orig - impervious_center) / impervious_scale)



# define best_model
#best_model = crossing_models$cohesion_patchrd_tre.shr_impervious_bridge.binary_class_sex_age
#best_model = crossing_models$cohesion_patchrd_tre.shr_bridge.binary_class_sex_age
#best_model = round2_crossing_models$cohesion2_patchrd_tre.shr2_impervious2_bridge.binary_class_sex_age
#best_model = round2_crossing_models$cohesion_patchrd_tre.shr_impervious_bridge.binary_class_sex
#best_model = round2_crossing_models$cohesion_patchrd_tre.shr_impervious_bridge.binary_class_age


best_model = sex_age_crossing_models$cohesion2_treshr2_impervious2_patchrd_bridgebinary_sex.class # 2265, 2112 to run bootstrap_pred_fun
best_model = sex_age_crossing_models$cohesion2_treshr2_sex.impervious2_patchrd_bridgebinary_class # 2148 to run bootstrap_pred_fun

# Run parallel bootstrap for predictions
system.time(
  boot_preds <- future_lapply(
    1:nsim,
    bootstrap_pred_fun,
    best_model = best_model,
    newdat = newdat,
    future.seed = TRUE,
    future.packages = c("lme4")
  )
  ) #132 for nsim = 20
# 485 for nsim = 200
# 1111 for nsim = 500
# 2078 for nsim = 1000

# Convert to matrix and remove failed iterations
boot_pred_matrix <- do.call(rbind, boot_preds)
boot_pred_matrix <- boot_pred_matrix[complete.cases(boot_pred_matrix), ]

# Compute 95% CI for each row of newdat
pred_ci <- t(apply(boot_pred_matrix, 2, quantile, probs = c(0.025, 0.5, 0.975)))
colnames(pred_ci) <- c("lower", "median", "upper")
rownames(pred_ci) <- 1:nrow(newdat)

# predict from the original model for comparison
mod_pred <- predict(best_model, newdat, re.form = NA, type = "response") %>% 
  data.frame() %>% 
  rename("prediction" = 1)

pred_ci <- data.frame(pred_ci) %>% 
  bind_cols(newdat) %>% 
  bind_cols(mod_pred)


saveRDS(pred_ci, here("model_objects/boot_pred_ci_1000sim_cohesion2_treshr2_impervious2_patchrd_bridgebinary_sex-class"))

# compare

test_ci <- bind_rows(readRDS(here("model_objects/best_mod_factors_pred_ci_1000")) %>% 
                       mutate(which.ci = "bootstrap 1000 sim"),
                     readRDS(here("model_objects/best_mod_factors_pred_ci_500")) %>% 
                       mutate(which.ci = "bootstrap 500 sim"),
                     readRDS(here("model_objects/best_mod_factors_pred_ci_200")) %>% 
                       mutate(which.ci = "bootstrap 200 sim"),
                     readRDS(here("model_objects/best_mod_factors_pred_ci_20")) %>% 
                       mutate(which.ci = "bootstrap 20 sim"),
                     readRDS(here("model_objects/best_mod_factors_predictions_se")) %>% 
                       mutate(which.ci = "1.96 * SE") %>% 
                       rename(lower = resp.lwr,
                              upper = resp.upr,
                              median = resp.fit)) %>% 
  mutate(which.ci = factor(which.ci, levels = c("1.96 * SE", "bootstrap 1000 sim", "bootstrap 500 sim", "bootstrap 200 sim", "bootstrap 20 sim")))

# nsim = 500 


# sex and age
test_ci %>%
  filter(age == "adult", patchrd == 0, bridge.binary == 0, class == "Local") %>% 
  ggplot() +
  geom_point(aes(y = median, x = sex, color = which.ci), size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower, ymax = upper, x = sex, color = which.ci), linewidth = 1, position = position_dodge(width = 0.5), width = 0.25) +
  labs(x = "Sex",
       y = "Relative probability of crossing",
       title = "Relative probability of road crossing for male and female\nand adult and sub-adult mountain lions",
       color = "Age",
       fill = "") +
  theme_bw() 



