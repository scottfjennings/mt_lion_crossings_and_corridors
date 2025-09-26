


# must run all of B3_analysis_table_model_structure.R before this script to load crossing_analysis_table and set model structures

library(glmmTMB)

# find best 0 infl structure

find_best_zi_stru <- function(zcross) {
  
  
  
zi_models <- list(
  zi_intercept = glmmTMB(as.formula(paste(zcross, "~ dev.60 * num.creek * sex + (1|animal.id)")), 
                ziformula = ~1,
                family = ziGamma(link = "log"), data = crossing_analysis_table),
  
  zi_dev60 = glmmTMB(as.formula(paste(zcross, "~ dev.60 * num.creek * sex + (1|animal.id)")), 
                ziformula = ~dev.60,
                family = ziGamma(link = "log"), data = crossing_analysis_table),
  
  
  zi_creek = glmmTMB(as.formula(paste(zcross, "~ dev.60 * num.creek * sex + (1|animal.id)")), 
                     ziformula = ~num.creek,
                     family = ziGamma(link = "log"), data = crossing_analysis_table),
  
  zi_dev60_creek = glmmTMB(as.formula(paste(zcross, "~ dev.60 * num.creek * sex + (1|animal.id)")), 
                           ziformula = ~dev.60 + num.creek,
                           family = ziGamma(link = "log"), data = crossing_analysis_table),
  
  zi_dev60.creek = glmmTMB(as.formula(paste(zcross, "~ dev.60 * num.creek * sex + (1|animal.id)")), 
                           ziformula = ~dev.60 * num.creek,
                           family = ziGamma(link = "log"), data = crossing_analysis_table)
)


zi_models$aic <- aictab(zi_models, names(zi_models))

return(zi_models)
}

zi_stru_models <- find_best_zi_stru("seg.wt.crossing")

zi_stru_models$aic

##
fit_wt_crossing_mods_puma_0infl <- function(zcross, zformulas) {
  zi_formula <- ~ dev.60 * num.creek  # zero-inflation part for all models
  
  zmods <- lapply(zformulas, function(rhs) {
    formula <- as.formula(paste(zcross, "~", rhs, "+ (1|animal.id)"))
    glmmTMB(formula, ziformula = zi_formula,
            family = ziGamma(link = "log"), data = crossing_analysis_table)
  })
  
  zmods$aic <- aictab(zmods, names(zmods))
  
  return(zmods)
}

system.time(
  wt_crossings_mods_puma_0infl_class2 <- fit_wt_crossing_mods_puma_0infl("seg.wt.crossing", model_formulas_1step)
)

wt_crossings_mods_puma_0infl_class2$aic


plot(
  fitted(wt_crossings_mods_puma_0infl_class2$dev60.creek.class), residuals(wt_crossings_mods_puma_0infl_class2$dev60.creek.class),
  xlab = "Fitted Values",
  ylab = "Residuals",
  main = "Residuals vs Fitted Values",
  pch = 20,
  col = "steelblue"
)
abline(h = 0, lty = 2, col = "red")


######################

saveRDS(wt_crossings_mods_puma_0infl_class2, here("model_objects/wt_crossings_mods_puma_0infl_class2"))

wt_crossings_mods_puma_0infl_class2 <- readRDS(here("model_objects/wt_crossings_mods_puma_0infl_class2"))


best_mod <- wt_crossings_mods_puma_0infl$dev60.creek.class

sec_best_mod <- wt_crossings_mods_puma$dev60.class.bridge

# model validation ----
# r-squared

r.squaredGLMM(best_mod)

# cross validation
cross_validate_best_model_0infl <- function(zpuma, zcross) {
  
  
  zi_formula <- ~ dev.60 * num.creek  # zero-inflation part for all models
  
  
  train_data <- crossing_analysis_table %>% filter(animal.id != zpuma)
  test_data  <- crossing_analysis_table %>% filter(animal.id == zpuma)
  
  dev60.creek.class = glmmTMB(as.formula(paste(zcross, "~ dev.60 * num.creek * class + (1|animal.id)")), ziformula = zi_formula,
                              family = ziGamma(link = "log"), data = crossing_analysis_table)
  
  preds = data.frame(preds = predict(dev60.creek.class, newdata = test_data, allow.new.levels = TRUE, type = "response"))
  results = bind_cols(test_data, preds) %>% 
    mutate(rmse = sqrt(mean((preds - seg.wt.crossing)^2)),
           left.out = zpuma)
  
  return(results)
}

zz <- cross_validate_best_model_0infl("P1", "seg.wt.crossing")

cv_results_0infl <- map2_df(distinct(crossing_analysis_table, animal.id)$animal.id, rep("seg.wt.crossing", nrow(distinct(crossing_analysis_table, animal.id))), cross_validate_best_model_0infl)

mean(cv_results_0infl$rmse)


filter(cv_results_0infl, preds == 0) %>% nrow()



ggplot(cv_results_0infl) +
  geom_point(aes(x = seg.wt.crossing, y = preds)) +
  geom_abline() +
  facet_wrap(~left.out, scales = "free")


