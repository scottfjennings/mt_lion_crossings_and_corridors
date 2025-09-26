





library(tidyverse)
library(here)
library(gstat)
library(sp)
library(spaMM)
library(MuMIn)
library(AICcmodavg)
library(lme4)
library(pROC)


# load data ----
source(here("code/helper_data.R"))


seg_midpoints <- readRDS(here("data/seg_midpoints"))

# Extract coordinates from the sf geometry
coords <- seg_midpoints %>%
  mutate(coord = sf::st_coordinates(geometry)) %>%
  # st_coordinates returns a matrix with X and Y columns
  mutate(
    x = coord[, "X"],
    y = coord[, "Y"]
  ) %>%
  select(-coord, -geometry)

seg_regions <- readRDS(here("data/hr_segments_prop_in_developed")) %>% 
  data.frame() %>% 
  select(-geometry, -seg.length)   %>% 
  filter(animal.id %in% analysis_pumas,
         !animal.id %in% few_crossings_pumas,
         !animal.id %in% hr_exclude_pumas,
         !seg.label %in% p31_exclude_segments) %>% 
  mutate(region = ifelse(animal.id %in% c("P31", "P39"), 1, 2)) %>% 
  distinct(seg.label, region)

# from C1_analysis_table_model_structure
main_analysis_table <- readRDS(here("data/analysis_inputs/combined_lions_analysis_table")) %>%
  mutate(across(c(cohesion.200.25, mean.tre.shr.30, mean.dev.60), ~ as.numeric(scale(.x)))
         , class = ifelse(class %in% c("Highway", "Arterial"), "Major", as.character(class))
         , num.creek.bin = pmin(num.creek, 3) # still treating this as a continuous predictor b/c we believe, given our segment length, that the relative importance of adding 1 more creek intersection is minimal beyond 4 creeks (e.g. creek intersections more than every 300m probably not important)
         ) %>% 
  left_join(coords) %>% 
  left_join(seg_regions)



# need to load exploration_model_formulas from C1_
# initial modelling attempt, glm logistic regression  ----

#' fit_glm_logreg
#' 
#' general model fitting function. this should be good for exploratory and hypothesis testing models. fits a logistic regression via glm with offset for segment proportion inside each BBMM UD and 
#' 
#' 
#' @param zformulas named list of fixed effects model structures
#' 
#' 
#' @returns list of model objects, with names matching the names of zformulas, and an aic table "$aic" for all model structures in zformulas
#'
#' @examples
fit_logreg_mods <- function(zformulas) {
  
  zmods <- lapply(zformulas, function(fix.stru) {
    formula <- as.formula(paste("bin.crossing ~ ", fix.stru))
    glm(formula, 
        data = main_analysis_table, 
        family = binomial,
        weights = num.lion.months)
  })
  
  zmods$aic <- aictab(zmods, names(zmods))
  
  return(zmods)
}


system.time(
  exploratory_mods_logreg <- fit_logreg_mods(exploration_model_formulas)
) # 1.2

exploratory_mods_logreg$aic

saveRDS(exploratory_mods_logreg, here("model_objects/lions_combined/exploratory_mods_logreg"))
exploratory_mods_logreg <- readRDS(here("model_objects/lions_combined/exploratory_mods_logreg"))
exploratory_mods_logreg$aic


# cohesion_patchrd_woody_dev_creek_class  is best. next best dAIC = 584
   

summary(exploratory_mods_logreg$cohesion_patchrd_woody_dev_creek_class)

plot(exploratory_mods_logreg$cohesion_patchrd_woody_dev_creek_class)

# spatial patterns? ----


# Example: check variogram for first model in your list
mod <- exploratory_mods_logreg$cohesion_patchrd_woody_dev_creek_class

# Extract residuals: deviance residuals are common for GLMs
residuals_df <- data.frame(
  residuals = residuals(mod, type = "deviance"),
  x = main_analysis_table$x,
  y = main_analysis_table$y
)

# Convert to SpatialPointsDataFrame for gstat
coordinates(residuals_df) <- ~ x + y

# Compute empirical variogram of residuals
vgm_res <- variogram(residuals ~ 1, residuals_df)

# Plot variogram
plot(vgm_res, main = "Empirical Variogram of GLM Residuals")

# poisson regression ----
#' fit_pois_mods
#' 
#' general model fitting function. this should be good for exploratory and hypothesis testing models. fits a poisson regression via glm. negative binomial theta (overdisp perm) was estimated as huge so poisson better 
#' 
#' 
#' @param zformulas named list of fixed effects model structures
#' 
#' 
#' @returns list of model objects, with names matching the names of zformulas, and an aic table "$aic" for all model structures in zformulas
#'
#' @examples
fit_pois_mods <- function(zformulas) {
  
  zmods <- lapply(zformulas, function(fix.stru) {
    formula <- as.formula(paste("num.lions.crossing ~ ", fix.stru))
    glm(formula, 
        data = main_analysis_table, 
        weights = num.lion.months,
        family = poisson)
  })
  
  zmods$aic <- aictab(zmods, names(zmods))
  
  return(zmods)
}



system.time(
  exploratory_mods_pois <- fit_pois_mods(exploration_model_formulas)
) # 1.2

exploratory_mods_pois$aic
# cohesion_patchrd_woody_dev_creek_class is best, next best dAICc = 18

saveRDS(exploratory_mods_pois, here("model_objects/lions_combined/exploratory_mods_pois"))


summary(exploratory_mods_pois$cohesion_patchrd_woody_dev_creek_class)

plot(exploratory_mods_pois$cohesion_patchrd_woody_dev_creek_class)

# Example: check variogram for first model in your list


# Extract residuals: deviance residuals are common for GLMs
residuals_df <- data.frame(
  residuals = residuals(exploratory_mods_pois$cohesion_patchrd_woody_dev_creek_class, type = "deviance"),
  x = main_analysis_table$x,
  y = main_analysis_table$y
)

# Convert to SpatialPointsDataFrame for gstat
coordinates(residuals_df) <- ~ x + y

# Compute empirical variogram of residuals
vgm_res <- variogram(residuals ~ 1, residuals_df)

# Plot variogram
plot(vgm_res, main = "Empirical Variogram of GLM Residuals")


# negative binomial regression ----
#' fit_negbin_mods
#' 
#' general model fitting function. this should be good for exploratory and hypothesis testing models. fits a poisson regression via glm. negative binomial theta (overdisp perm) was estimated as huge so poisson better 
#' 
#' 
#' @param zformulas named list of fixed effects model structures
#' 
#' 
#' @returns list of model objects, with names matching the names of zformulas, and an aic table "$aic" for all model structures in zformulas
#'
#' @examples
fit_negbin_mods <- function(zformulas) {
  
  zmods <- lapply(zformulas, function(fix.stru) {
    formula <- as.formula(paste("num.lions.crossing ~ ", fix.stru))
    MASS::glm.nb(formula, 
        data = main_analysis_table, 
        weights = num.lion.months,
        control = glm.control(maxit = 100))
  })
  
  zmods$aic <- aictab(zmods, names(zmods))
  
  return(zmods)
}



system.time(
  exploratory_mods_negbin <- fit_negbin_mods(exploration_model_formulas)
) # 1.2

exploratory_mods_pois$aic
# cohesion_patchrd_woody_dev_creek_class is best, next best dAICc = 18

saveRDS(exploratory_mods_pois, here("model_objects/lions_combined/exploratory_mods_pois"))


summary(exploratory_mods_pois$cohesion_patchrd_woody_dev_creek_class)

plot(exploratory_mods_pois$cohesion_patchrd_woody_dev_creek_class)

# Example: check variogram for first model in your list


# Extract residuals: deviance residuals are common for GLMs
residuals_df <- data.frame(
  residuals = residuals(exploratory_mods_pois$cohesion_patchrd_woody_dev_creek_class, type = "deviance"),
  x = main_analysis_table$x,
  y = main_analysis_table$y
)

# Convert to SpatialPointsDataFrame for gstat
coordinates(residuals_df) <- ~ x + y

# Compute empirical variogram of residuals
vgm_res <- variogram(residuals ~ 1, residuals_df)

# Plot variogram
plot(vgm_res, main = "Empirical Variogram of GLM Residuals")


# logistic regression with spatial autocorrelation for segments crossed or not each year ----

# coarsen the coordinates to speed processing time
coarsener = 1000

main_analysis_table <- main_analysis_table %>% 
  mutate(coarse_x = (floor(x / coarsener) + 0.5) * coarsener,
         coarse_y = (floor(y / coarsener) + 0.5) * coarsener)



# set for parallel processing
spaMM.options(nb_cores = parallel::detectCores())
spaMM.options(example_maxtime = 1000)


table(main_analysis_table$bin.crossing, main_analysis_table$class)




# so going to refit the whole candidate set:
fit_spatial_logreg_mods <- function(zformulas) {
  
  zmods <- lapply(zformulas, function(fix.stru) {
    formula_str <- paste0("bin.crossing ~ ", fix.stru, 
                          " + Matern(1 | region + coarse_x + coarse_y)")
    formula <- as.formula(formula_str)
    
    fitme(
      formula = formula,
      data = main_analysis_table,
      family = binomial(),
      prior.weights = main_analysis_table$num.lion.months,
      control.HLfit = list(algebra = "spprec",
                           max.iter.mean = 2000)  
    )
  })
  
  names(zmods) <- zformulas
  
  return(zmods)
}


global_formula <- list(cohesion_patchrd_woody_dev_creek_class = "cohesion.200.25 + patch.touches.road + mean.tre.shr.30 + mean.dev.60 + num.creek.bin + class")


system.time(
  global_logreg_sp1km <- fit_spatial_logreg_mods(global_formula)
) # 211



system.time(
  exploratory_mods_logreg_sp <- fit_spatial_logreg_mods(exploration_model_formulas)
) #

names(exploratory_mods_logreg_sp) <- str_replace_all(names(exploratory_mods_logreg_sp), " \\+ ", "_")
names(exploratory_mods_logreg_sp) <- str_replace_all(names(exploratory_mods_logreg_sp), "cohesion.200.25", "cohesion")
names(exploratory_mods_logreg_sp) <- str_replace_all(names(exploratory_mods_logreg_sp), "mean.dev.60", "dev")
names(exploratory_mods_logreg_sp) <- str_replace_all(names(exploratory_mods_logreg_sp), "mean.tre.shr.300|mean.tre.shr.120|mean.tre.shr.30", "woody")
names(exploratory_mods_logreg_sp) <- str_replace_all(names(exploratory_mods_logreg_sp), "num.creek.bin", "creek")
names(exploratory_mods_logreg_sp) <- str_replace_all(names(exploratory_mods_logreg_sp), "patch.touches.road", "patchrd")



saveRDS(exploratory_mods_logreg_sp, here("model_objects/lions_combined/exploratory_mods_logreg_sp"))

exploratory_mods_logreg_sp <- readRDS(here("model_objects/lions_combined/exploratory_mods_logreg_sp"))

# set coarsener = 1000 above and refit 5 best models to test sensitivity of model selection to coarsener.

top5_formulas <- exploration_model_formulas[c("cohesion_patchrd_woody_dev_creek_class", 
                                              "cohesion_patchrd_woody_dev_class", 
                                              "cohesion_patchrd_dev_creek_class", 
                                              "cohesion_patchrd_dev_class", 
                                              "cohesion_patchrd_dev_creek")]

system.time(
  exploratory_mods_logreg_sp1000_top5 <- fit_spatial_logreg_mods(top5_formulas)
) # 13646.74 



names(exploratory_mods_logreg_sp1000_top5) <- str_replace_all(names(exploratory_mods_logreg_sp1000_top5), " \\+ ", "_")
names(exploratory_mods_logreg_sp1000_top5) <- str_replace_all(names(exploratory_mods_logreg_sp1000_top5), "cohesion.200.25", "cohesion")
names(exploratory_mods_logreg_sp1000_top5) <- str_replace_all(names(exploratory_mods_logreg_sp1000_top5), "mean.dev.60", "dev")
names(exploratory_mods_logreg_sp1000_top5) <- str_replace_all(names(exploratory_mods_logreg_sp1000_top5), "mean.tre.shr.300|mean.tre.shr.120|mean.tre.shr.30", "woody")
names(exploratory_mods_logreg_sp1000_top5) <- str_replace_all(names(exploratory_mods_logreg_sp1000_top5), "num.creek.bin", "creek")
names(exploratory_mods_logreg_sp1000_top5) <- str_replace_all(names(exploratory_mods_logreg_sp1000_top5), "patch.touches.road", "patchrd")


saveRDS(exploratory_mods_logreg_sp1000_top5, here("model_objects/lions_combined/exploratory_mods_logreg_sp1000_top5"))

exploratory_mods_logreg_sp1000_top5 <- readRDS(here("model_objects/lions_combined/exploratory_mods_logreg_sp1000_top5"))




# need a custom function to get AIC table from spaMM models ----

get_marg_aic <- function(zmod) {
  aic_tbl <- zmod %>%
    AIC() %>%
    data.frame() %>%
    rename_with(~"value") %>%
    tibble::rownames_to_column("criterion") %>%
    mutate(criterion = trimws(criterion))
  
  marg_aic <- aic_tbl %>%
    filter(criterion == "marginal AIC:") %>%
    mutate(
      logLik = as.numeric(logLik(zmod)),
      k_fixed = length(fixef(zmod))    )
  
  return(marg_aic)
}

# run for the full model set with 2000m coarsener
system.time(
logreg_sp_aic <- map_df(
  exploratory_mods_logreg_sp,
  get_marg_aic,
  .id = "model_name"
)
) # 28

logreg_sp_aic <- logreg_sp_aic %>% 
  mutate(deltaAIC = value - min(logreg_sp_aic$value),
         AIC_weight = exp(-0.5 * deltaAIC) / sum(exp(-0.5 * deltaAIC)),
         across(c(value, deltaAIC, AIC_weight, logLik), ~round(., 3))) %>% 
  select(model_name, k_fixed, "marginal AIC" = value, deltaAIC, AIC_weight, logLik) %>% 
  arrange(deltaAIC)

saveRDS(logreg_sp_aic, here("model_objects/lions_combined/exploratory_mods_logreg_sp_aic"))

logreg_sp_aic <- readRDS(here("model_objects/lions_combined/exploratory_mods_logreg_sp_aic"))


summary(exploratory_mods_logreg_sp$cohesion_patchrd_woody_dev_creek_class)

plot(exploratory_mods_logreg_sp$`patch.touches.road + mean.dev.60 + num.creek.bin + class`)
# looks ok except Q-Q a little suspect




# run for the reduced model set with 1000m coarsener
system.time(
  logreg_sp_aic1000_top5 <- map_df(
    exploratory_mods_logreg_sp1000_top5,
    get_marg_aic,
    .id = "model_name"
  )
) # 28

logreg_sp_aic1000_top5 <- logreg_sp_aic1000_top5 %>% 
  mutate(deltaAIC = value - min(logreg_sp_aic1000_top5$value),
         AIC_weight = exp(-0.5 * deltaAIC) / sum(exp(-0.5 * deltaAIC)),
         across(c(value, deltaAIC, AIC_weight, logLik), ~round(., 3))) %>% 
  select(model_name, k_fixed, "marginal AIC" = value, deltaAIC, AIC_weight, logLik) %>% 
  arrange(deltaAIC)

saveRDS(logreg_sp_aic1000_top5, here("model_objects/lions_combined/logreg_sp_aic1000_top5"))




# refit best model structure in spaMM but without SAC for direct AIC comparison to models with SAC ----
system.time(
  cohesion_patchrd_woody_dev_creek_class <- fitme(
  formula = bin.crossing ~ cohesion.200.25 + patch.touches.road + mean.tre.shr.30 +
    mean.dev.60 + num.creek.bin + class,
  data = main_analysis_table,
  family = binomial(),
  prior.weights = main_analysis_table$num.lion.months
)
) # 0.13
AIC(cohesion_patchrd_woody_dev_creek_class)

# cross validation and ROC of best spatial logistic regression model ----


best_formula <- bin.crossing ~ cohesion.200.25 + patch.touches.road + mean.tre.shr.30 +
  mean.dev.60 + num.creek.bin + class +
  Matern(1 | region + coarse_x + coarse_y)

set.seed(123)
n_folds <- 10
folds <- sample(rep(1:n_folds, length.out = nrow(main_analysis_table)))

# Storage for per-fold results
cv_results <- data.frame(fold = 1:n_folds, logLik = NA, AUC = NA)

# Storage for ROC plot data
roc_plot_data <- data.frame()

# Storage for overall predictions and truths
all_preds <- numeric(0)
all_truths <- numeric(0)

system.time(
for (k in 1:n_folds) {
  train_data <- main_analysis_table[folds != k, ]
  test_data <- main_analysis_table[folds == k, ]
  
  # Fit the model on training fold
  model <- fitme(
    best_formula,
    data = train_data,
    family = binomial(),
    prior.weights = train_data$num.lion.months,
    control.HLfit = list(algebra = "spprec", max.iter.mean = 2000)
  )
  
  # Predict probabilities on test fold
  pred_probs <- predict(model, newdata = test_data, type = "response")
  true_vals <- test_data$bin.crossing
  
  # Calculate log-likelihood for fold
  cv_results$logLik[k] <- sum(dbinom(true_vals, size = 1, prob = pred_probs, log = TRUE))
  
  # Calculate and store AUC for fold
  roc_fold <- roc(true_vals, pred_probs)
  cv_results$AUC[k] <- auc(roc_fold)
  
  # Store predictions and truths for overall ROC
  all_preds <- c(all_preds, pred_probs)
  all_truths <- c(all_truths, true_vals)
  
  # Store ROC points for fold to plot ROC curves by fold later
  fold_roc_df <- data.frame(
    specificity = rev(roc_fold$specificities),  # reverse for plotting
    sensitivity = rev(roc_fold$sensitivities),
    fold = paste0("Fold ", k)
  )
  roc_plot_data <- rbind(roc_plot_data, fold_roc_df)
}
)

logreg_sp_best_model_diagnostics <- list(cv_results = cv_results,
                                         all_truths = all_truths, 
                                         all_preds = all_preds,
                                         roc_plot_data = roc_plot_data)

saveRDS(logreg_sp_best_model_diagnostics, here("model_objects/lions_combined/logreg_sp_best_model_diagnostics"))

logreg_sp_best_model_diagnostics <- readRDS(here("model_objects/lions_combined/logreg_sp_best_model_diagnostics"))


# Print summary of CV results (logLik and AUC per fold)
print(logreg_sp_best_model_diagnostics$cv_results)
cat("Mean AUC across folds:", mean(logreg_sp_best_model_diagnostics$cv_results$AUC), "\n")
cat("Median AUC across folds:", median(logreg_sp_best_model_diagnostics$cv_results$AUC), "\n")
cat("AUC range:", range(logreg_sp_best_model_diagnostics$cv_results$AUC), "\n")

# Overall ROC from pooled predictions across folds
overall_roc <- roc(logreg_sp_best_model_diagnostics$all_truths, logreg_sp_best_model_diagnostics$all_preds)

# Plot per-fold ROC curves
ggplot(logreg_sp_best_model_diagnostics$roc_plot_data, aes(x = 1 - specificity, y = sensitivity, color = fold)) +
  geom_line(size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "ROC Curves by Cross-Validation Fold",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    color = "Fold"
  ) +
  theme_minimal()

# Plot overall ROC curve
plot(overall_roc, col = "#2c7bb6", lwd = 3, main = "Overall ROC Curve - 10-fold CV")
abline(a = 0, b = 1, lty = 2, col = "gray")
text(0.6, 0.2, paste("AUC =", round(auc(overall_roc), 3)), cex = 1.2)


# poisson regression with spatial autocorr for number of individual lions crossing each segment each year ----


table(main_analysis_table$num.lions, main_analysis_table$class)


fit_spatial_pois_mods <- function(zformulas) {
  
  zmods <- lapply(zformulas, function(fix.stru) {
    formula_str <- paste0("num.lions.crossing ~ ", fix.stru, 
                          " + Matern(1 | region + coarse_x + coarse_y)")
    formula <- as.formula(formula_str)
    
    fitme(
      formula = formula,
      data = main_analysis_table,
      family = poisson(),
      prior.weights = main_analysis_table$num.lion.months,
      control.HLfit = list(algebra = "spprec")  # or "spcorr"
    )
  })
  
  # Name the list so aictab can work
  names(zmods) <- zformulas
  
  # Calculate AICc table
  #zmods$aic <- aictab(zmods, modnames = names(zmods), second.ord = TRUE)
  
  return(zmods)
}


# patchrd_woody_creek_class was best for non spatial poisson regression
system.time(
  exploratory_mods_pois_sp <- fit_spatial_pois_mods(exploration_model_formulas$cohesion_patchrd_woody_dev_creek_class)
) # 11.2


summary(exploratory_mods_pois_sp$`cohesion.200.25 + patch.touches.road + mean.tre.shr.30 + mean.dev.60 + num.creek.bin + class`)
plot(exploratory_mods_pois_sp$`cohesion.200.25 + patch.touches.road + mean.tre.shr.30 + mean.dev.60 + num.creek.bin + class`)

sum(residuals(exploratory_mods_pois_sp$`cohesion.200.25 + patch.touches.road + mean.tre.shr.30 + mean.dev.60 + num.creek.bin + class`, type = "pearson")^2) / df.residual(exploratory_mods_pois_sp$`cohesion.200.25 + patch.touches.road + mean.tre.shr.30 + mean.dev.60 + num.creek.bin + class`)

# poisson model not very good. probably mostly due to overdispersion

saveRDS(exploratory_mods_pois_sp[1], here("model_objects/lions_combined/best_mod_pois_sp"))


# cross validation of best spatial poisson regression model ----


best_formula_pois <- num.lions.crossing ~ cohesion.200.25 + patch.touches.road + mean.tre.shr.30 +
  mean.dev.60 + num.creek.bin + class +
  Matern(1 | region + coarse_x + coarse_y)

n_folds <- 5
set.seed(123)
folds <- sample(rep(1:n_folds, length.out = nrow(main_analysis_table)))

cv_results_pois <- data.frame(fold = 1:n_folds, logLik = NA, RMSE = NA)
system.time(
for (k in 1:n_folds) {
  train_data_pois <- main_analysis_table[folds != k, ]
  test_data_pois  <- main_analysis_table[folds == k, ]
  
  model_pois <- fitme(
    best_formula_pois,
    data = train_data_pois,
    family = poisson(),
    prior.weights = train_data_pois$num.lion.months,
    control.HLfit = list(algebra = "spprec")
  )
  
  predicted_pois <- predict(model_pois, newdata = test_data_pois, type = "response")
  observed_pois  <- test_data_pois$num.lions.crossing  # or whatever your count variable is
  
  cv_results_pois$logLik[k] <- sum(dpois(observed_pois, lambda = predicted_pois, log = TRUE))
  cv_results_pois$RMSE[k]   <- sqrt(mean((observed_pois - predicted_pois)^2))
}
)
summary(cv_results_pois)



# testing sensitivity to coarsener value ----



# set for parallel processing
spaMM.options(nb_cores = parallel::detectCores())
spaMM.options(example_maxtime = 1000)


table(main_analysis_table$bin.crossing, main_analysis_table$class)





# patchrd_dev_creek_class was best from non spatial logistic regression by >500 AIC so not fitting any other models
test_coasrener <- function(zcoarsener) {
  
  main_analysis_table <- main_analysis_table %>% 
    mutate(coarse_x = (floor(x / zcoarsener) + 0.5) * zcoarsener,
           coarse_y = (floor(y / zcoarsener) + 0.5) * zcoarsener)
  
  
  
  cohesion_patchrd_woody_dev_creek_class_sp <- fitme(
    formula = as.formula(bin.crossing ~ cohesion.200.25 + patch.touches.road + mean.tre.shr.30 +
                           mean.dev.60 + num.creek.bin + class + 
                           Matern(1 | region + coarse_x + coarse_y)),
    data = main_analysis_table,
    family = binomial(),
    prior.weights = main_analysis_table$num.lion.months,
    control.HLfit = list(algebra = "spprec",
                         allow_sparse = TRUE,   # Will stop if sparse algebra cannot be used
                         max.iter.mean = 2000)
  )
  
  return(cohesion_patchrd_woody_dev_creek_class_sp)
  
}

system.time(
coarsener2000 <- test_coasrener(2000)
)

system.time(
  coarsener1500 <- test_coasrener(1500)
)

system.time(
  coarsener1000 <- test_coasrener(1000)
) # 483.57 

system.time(
  coarsener2500 <- test_coasrener(2500)
)

system.time(
  coarsener3000 <- test_coasrener(3000)
)


system.time(
  coarsener500 <- test_coasrener(500)
) # 1758.98 

system.time(
  coarsener300 <- test_coasrener(300)
) # 19582.43 


coarsener_test_mods <- list(coarsener1000 = coarsener1000,
                            coarsener1500 = coarsener1500,
                            coarsener2000 = coarsener2000,
                            coarsener2500 = coarsener2500,
                            coarsener3000 = coarsener3000)

coarsener_test_mods$coarsener500 = coarsener500
coarsener_test_mods$coarsener300 = coarsener300

saveRDS(coarsener_test_mods, here("model_objects/lions_combined/coarsener_test_mods"))

coarsener_test_mods <- readRDS(here("model_objects/lions_combined/coarsener_test_mods"))


# run for the full model set with 2000m coarsener
system.time(
  coarsener_test_aic <- map_df(
    coarsener_test_mods,
    get_marg_aic,
    .id = "model_name"
  )
) # 28

coarsener_test_aic <- coarsener_test_aic %>% 
  mutate(deltaAIC = value - min(coarsener_test_aic$value),
         AIC_weight = exp(-0.5 * deltaAIC) / sum(exp(-0.5 * deltaAIC)),
         across(c(value, deltaAIC, AIC_weight, logLik), ~round(., 3))) %>% 
  select(model_name, k_fixed, "marginal AIC" = value, deltaAIC, AIC_weight, logLik) %>% 
  arrange(deltaAIC)




compare_coarsener_coefs <- full_join(fixef(coarsener_test_mods$coarsener1000) %>% data.frame() %>% rename_with(~"c1000") %>% rownames_to_column("varb") %>% mutate(across(everything(), ~as.character(.))),
           fixef(coarsener_test_mods$coarsener1500) %>% data.frame() %>% rename_with(~"c1500") %>% rownames_to_column("varb") %>% mutate(across(everything(), ~as.character(.)))) %>% 
  full_join(fixef(coarsener_test_mods$coarsener2000) %>% data.frame() %>% rename_with(~"c2000") %>% rownames_to_column("varb") %>% mutate(across(everything(), ~as.character(.)))) %>% 
  full_join(fixef(coarsener_test_mods$coarsener2500) %>% data.frame() %>% rename_with(~"c2500") %>% rownames_to_column("varb") %>% mutate(across(everything(), ~as.character(.)))) %>% 
  full_join(fixef(coarsener_test_mods$coarsener3000) %>% data.frame() %>% rename_with(~"c3000") %>% rownames_to_column("varb") %>% mutate(across(everything(), ~as.character(.))))

compare_coarsener_coefs_summary <- compare_coarsener_coefs %>% 
  pivot_longer(cols = starts_with("c"), values_to = "coef", names_to = "coarsener") %>% 
  mutate(coef = as.numeric(coef)) %>% 
  group_by(varb) %>% 
  summarise(mean.coef = mean(coef),
            max.coef = max(coef),
            min.coef = min(coef),
            range.coef = max.coef - min.coef,
            scale.range.coef = range.coef/abs(mean.coef)) %>% 
  arrange(scale.range.coef)


# retrying 5 best ranked models with no coarsener ----

# best
system.time(
cohesion_patchrd_woody_dev_creek_class_sp_nocoarsener <- fitme(
  formula = as.formula(bin.crossing ~ cohesion.200.25 + patch.touches.road + mean.tre.shr.30 + mean.dev.60 + num.creek.bin + class + 
                         Matern(1 | region + x + y)),
  data = main_analysis_table,
  family = binomial(),
  prior.weights = main_analysis_table$num.lion.months,
  control.HLfit = list(algebra = "spprec",
                       allow_sparse = TRUE,   # Will stop if sparse algebra cannot be used
                       max.iter.mean = 2000))
) # 29997.09 --- 8.3 hours


saveRDS(cohesion_patchrd_woody_dev_creek_class_sp_nocoarsener, here("model_objects/lions_combined/cohesion_patchrd_woody_dev_creek_class_sp_nocoarsener"))

# 2nd
system.time(
  cohesion_patchrd_woody_dev_class_sp_nocoarsener <- fitme(
    formula = as.formula(bin.crossing ~ cohesion.200.25 + patch.touches.road + mean.tre.shr.30 +
                           mean.dev.60 + class + 
                           Matern(1 | region + x + y)),
    data = main_analysis_table,
    family = binomial(),
    prior.weights = main_analysis_table$num.lion.months,
    control.HLfit = list(algebra = "spprec",
                         allow_sparse = TRUE,   # Will stop if sparse algebra cannot be used
                         max.iter.mean = 2000))
) # 36689   10 hours

saveRDS(cohesion_patchrd_woody_dev_class_sp_nocoarsener, here("model_objects/lions_combined/cohesion_patchrd_woody_dev_class_sp_nocoarsener"))


# 3rd
system.time(
  cohesion_patchrd_dev_creek_class_sp_nocoarsener <- fitme(
    formula = as.formula(bin.crossing ~ cohesion.200.25 + patch.touches.road  + mean.dev.60 + num.creek.bin + class + 
                           Matern(1 | region + x + y)),
    data = main_analysis_table,
    family = binomial(),
    prior.weights = main_analysis_table$num.lion.months,
    control.HLfit = list(algebra = "spprec",
                         allow_sparse = TRUE,   # Will stop if sparse algebra cannot be used
                         max.iter.mean = 2000))
) # 32042.99 


saveRDS(cohesion_patchrd_dev_creek_class_sp_nocoarsener, here("model_objects/lions_combined/cohesion_patchrd_dev_creek_class_sp_nocoarsener"))



# 4th - not actually run
system.time(
  cohesion_patchrd_dev_class_sp_nocoarsener <- fitme(
    formula = as.formula(bin.crossing ~ cohesion.200.25 + patch.touches.road  + mean.dev.60 + class + 
                           Matern(1 | region + x + y)),
    data = main_analysis_table,
    family = binomial(),
    prior.weights = main_analysis_table$num.lion.months,
    control.HLfit = list(algebra = "spprec",
                         allow_sparse = TRUE,   # Will stop if sparse algebra cannot be used
                         max.iter.mean = 2000))
) # 

saveRDS(cohesion_patchrd_dev_class_sp_nocoarsener, here("model_objects/lions_combined/cohesion_patchrd_dev_class_sp_nocoarsener"))


# 5th - not actually run
system.time(
  cohesion_patchrd_dev_creek_sp_nocoarsener <- fitme(
    formula = as.formula(bin.crossing ~ cohesion.200.25 + patch.touches.road + mean.dev.60 + num.creek.bin + 
                           Matern(1 | region + x + y)),
    data = main_analysis_table,
    family = binomial(),
    prior.weights = main_analysis_table$num.lion.months,
    control.HLfit = list(algebra = "spprec",
                         allow_sparse = TRUE,   # Will stop if sparse algebra cannot be used
                         max.iter.mean = 2000))
) 

saveRDS(cohesion_patchrd_dev_creek_sp_nocoarsener, here("model_objects/lions_combined/cohesion_patchrd_dev_creek_sp_nocoarsener"))



