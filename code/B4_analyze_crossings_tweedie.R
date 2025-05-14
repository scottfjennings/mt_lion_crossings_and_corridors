


# must run all of B3_analysis_table_model_structure.R before this script to load crossing_analysis_table and set model structures


##

fit_wt_crossing_mods_puma_tweedie <- function(zcross) {

  
  base_formula <- function(rhs) {
    as.formula(paste(zcross, "~", rhs, "+ (1|animal.id)"))
  }
  
  zmods <- list(
    # single predictor
    treshr300 = glmmTMB(base_formula("tre.shr.300"),
                        family = tweedie(link = "log"), data = crossing_analysis_table),
    dev60 = glmmTMB(base_formula("dev.60"),
                    family = tweedie(link = "log"), data = crossing_analysis_table),
    creek = glmmTMB(base_formula("num.creek"),
                    family = tweedie(link = "log"), data = crossing_analysis_table),
    class = glmmTMB(base_formula("class"),
                    family = tweedie(link = "log"), data = crossing_analysis_table),
    sex = glmmTMB(base_formula("sex"),
                  family = tweedie(link = "log"), data = crossing_analysis_table),
    
    # 2-way additive
    treshr300_creek = glmmTMB(base_formula("tre.shr.300 + num.creek"),
                              family = tweedie(link = "log"), data = crossing_analysis_table),
    dev60_creek = glmmTMB(base_formula("dev.60 + num.creek"),
                          family = tweedie(link = "log"), data = crossing_analysis_table),
    treshr300_sex = glmmTMB(base_formula("tre.shr.300 + sex"),
                            family = tweedie(link = "log"), data = crossing_analysis_table),
    dev60_sex = glmmTMB(base_formula("dev.60 + sex"),
                        family = tweedie(link = "log"), data = crossing_analysis_table),
    treshr300_class = glmmTMB(base_formula("tre.shr.300 + class"),
                              family = tweedie(link = "log"), data = crossing_analysis_table),
    dev60_class = glmmTMB(base_formula("dev.60 + class"),
                          family = tweedie(link = "log"), data = crossing_analysis_table),
    creek_sex = glmmTMB(base_formula("num.creek + sex"),
                        family = tweedie(link = "log"), data = crossing_analysis_table),
    class_sex = glmmTMB(base_formula("class + sex"),
                        family = tweedie(link = "log"), data = crossing_analysis_table),
    class_creek = glmmTMB(base_formula("class + num.creek"),
                          family = tweedie(link = "log"), data = crossing_analysis_table),
    
    # 3-way additive
    treshr300_creek_class = glmmTMB(base_formula("tre.shr.300 + num.creek + class"),
                                    family = tweedie(link = "log"), data = crossing_analysis_table),
    treshr300_creek_sex = glmmTMB(base_formula("tre.shr.300 + num.creek + sex"),
                                  family = tweedie(link = "log"), data = crossing_analysis_table),
    treshr300_class_sex = glmmTMB(base_formula("tre.shr.300 + class + sex"),
                                  family = tweedie(link = "log"), data = crossing_analysis_table),
    dev60_creek_class = glmmTMB(base_formula("dev.60 + num.creek + class"),
                                family = tweedie(link = "log"), data = crossing_analysis_table),
    dev60_creek_sex = glmmTMB(base_formula("dev.60 + num.creek + sex"),
                              family = tweedie(link = "log"), data = crossing_analysis_table),
    dev60_class_sex = glmmTMB(base_formula("dev.60 + class + sex"),
                              family = tweedie(link = "log"), data = crossing_analysis_table),
    creek_class_sex = glmmTMB(base_formula("num.creek + class + sex"),
                              family = tweedie(link = "log"), data = crossing_analysis_table),
    
    # 2-way interactions
    treshr300.creek = glmmTMB(base_formula("tre.shr.300 * num.creek"),
                              family = tweedie(link = "log"), data = crossing_analysis_table),
    dev60.creek = glmmTMB(base_formula("dev.60 * num.creek"),
                          family = tweedie(link = "log"), data = crossing_analysis_table),
    treshr300.sex = glmmTMB(base_formula("tre.shr.300 * sex"),
                            family = tweedie(link = "log"), data = crossing_analysis_table),
    dev60.sex = glmmTMB(base_formula("dev.60 * sex"),
                        family = tweedie(link = "log"), data = crossing_analysis_table),
    treshr300.class = glmmTMB(base_formula("tre.shr.300 * class"),
                              family = tweedie(link = "log"), data = crossing_analysis_table),
    dev60.class = glmmTMB(base_formula("dev.60 * class"),
                          family = tweedie(link = "log"), data = crossing_analysis_table),
    creek.sex = glmmTMB(base_formula("num.creek * sex"),
                        family = tweedie(link = "log"), data = crossing_analysis_table),
    creek.class = glmmTMB(base_formula("num.creek * class"),
                          family = tweedie(link = "log"), data = crossing_analysis_table),
    class.sex = glmmTMB(base_formula("class * sex"),
                        family = tweedie(link = "log"), data = crossing_analysis_table),
    
    # 3-way interactions
    treshr300.creek.class = glmmTMB(base_formula("tre.shr.300 * num.creek * class"),
                                    family = tweedie(link = "log"), data = crossing_analysis_table),
    treshr300.creek.sex = glmmTMB(base_formula("tre.shr.300 * num.creek * sex"),
                                  family = tweedie(link = "log"), data = crossing_analysis_table),
    treshr300.class.sex = glmmTMB(base_formula("tre.shr.300 * class * sex"),
                                  family = tweedie(link = "log"), data = crossing_analysis_table),
    dev60.creek.class = glmmTMB(base_formula("dev.60 * num.creek * class"),
                                family = tweedie(link = "log"), data = crossing_analysis_table),
    dev60.creek.sex = glmmTMB(base_formula("dev.60 * num.creek * sex"),
                              family = tweedie(link = "log"), data = crossing_analysis_table),
    dev60.class.sex = glmmTMB(base_formula("dev.60 * class * sex"),
                              family = tweedie(link = "log"), data = crossing_analysis_table),
    creek.class.sex = glmmTMB(base_formula("num.creek * class * sex"),
                              family = tweedie(link = "log"), data = crossing_analysis_table),
    
    # null model
    intercept = glmmTMB(base_formula("1"),
                        family = tweedie(link = "log"), data = crossing_analysis_table)
  )
  
  zmods$aic <- aictab(zmods, names(zmods))
  
  return(zmods)
}


# fit for weighted crossings as response

system.time(
  wt_crossings_mods_puma_tweedie <- fit_wt_crossing_mods_puma_tweedie("seg.wt.crossing")
)

wt_crossings_mods_puma_tweedie$aic

saveRDS(wt_crossings_mods_puma, here("model_objects/wt_crossings_mods_puma"))


best_mod <- wt_crossings_mods_puma_tweedie$dev60.creek.class

sec_best_mod <- wt_crossings_mods_puma$dev60.class.bridge

# model validation ----
# r-squared

r.squaredGLMM(best_mod)

# cross validation
cross_validate_best_model_tweedie <- function(zpuma, zcross) {
  
  train_data <- crossing_analysis_table %>% filter(animal.id != zpuma)
  test_data  <- crossing_analysis_table %>% filter(animal.id == zpuma)
  
  dev60.creek.class = glmmTMB(as.formula(paste(zcross, "~ dev.60 * num.creek * class + (1|animal.id)")),
                              family = tweedie(link = "log"), data = crossing_analysis_table)
  
  preds = data.frame(preds = predict(dev60.creek.class, newdata = test_data, allow.new.levels = TRUE, type = "response"))
  results = bind_cols(test_data, preds) %>% 
    mutate(rmse = sqrt(mean((preds - seg.wt.crossing)^2)),
           left.out = zpuma)
  
  return(results)
}

cv_results_tweedie <- map2_df(distinct(crossing_analysis_table, animal.id)$animal.id, rep("seg.wt.crossing", nrow(distinct(crossing_analysis_table, animal.id))), cross_validate_best_model_tweedie)

mean(cv_results_tweedie$rmse)




ggplot(cv_results_tweedie) +
  geom_point(aes(x = seg.wt.crossing, y = preds)) +
  geom_abline() +
  stat_smooth(aes(x = seg.wt.crossing, y = preds), color = "red", method = "lm") +
  facet_wrap(~left.out, scales = "free")
