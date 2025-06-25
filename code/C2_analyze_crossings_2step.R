


library(tidyverse)
library(here)
library(lme4)
library(AICcmodavg)
library(MuMIn)
library(readxl)
library(glmmTMB)
#library(merTools)
#library(sf)



source(here("code/helper_data.R"))


# from C1_analysis_table_model_structure
main_analysis_table <- readRDS(here("data/analysis_inputs/annual_analysis_table")) %>%
  mutate(across(c(cohesion.200.50, mean.tre.shr.210, mean.dev.300), ~ as.numeric(scale(.x))),
         binned.annual.seg.raw.crossing = ifelse(annual.seg.raw.crossing > 0, 1, annual.seg.raw.crossing),
         prop.year = num.months/12)





### step 1 model crossed vs not crossed with binomial GLMM ----



# fitting negative binomial with glmmTMB which should be faster than fitting with lme4
#' fit_crossing_mods_yes_no
#' 
#' General model fitting function using glmmTMB. Useful for exploratory and hypothesis-testing models.
#' Fits a negative binomial model with an offset and a random effect for animal.id.
#' 
#' @param zformulas Named list of fixed effect model structures
#' 
#' @return List of model objects, with names matching zformulas, and an AIC table "$aic"
#' 
#' @examples
fit_crossing_mods_yes_no <- function(zformulas, zlink = "logit") {
  
  zmods <- lapply(zformulas, function(fix.stru) {
    formula <- as.formula(
      paste(
        "binned.annual.seg.raw.crossing ~", 
        fix.stru, 
        "+ prop.year + offset(log(annual.seg.wt.crossing + 0.0001)) + (1|animal.id)"
      )
    )
    
    glmmTMB::glmmTMB(
      formula = formula,
      family = binomial(link = zlink),
      data = main_analysis_table,
      ziformula = ~ cohesion.200.50 * patch.touches.road  # or include predictors for zero-inflation
    )
  })
  
  zmods$aic <- aictab(zmods, base = TRUE)
  
  return(zmods)
}



# fit for weighted crossings as response

system.time(
  crossings_mods_yes_no_logit <- fit_crossing_mods_yes_no(exploration_model_formulas, "logit")
) # 231. 560 if ziformula

crossings_mods_yes_no_logit$aic
# dev90.creek.class best with all AICcWt

plot(
  fitted(crossings_mods_yes_no_logit$cohesion_dev_creek_class_sex), residuals(crossings_mods_yes_no_logit$cohesion_dev_creek_class_sex, type = "pearson"),
  xlab = "Fitted Values",
  ylab = "Residuals",
  main = "Residuals vs Fitted Values for best model for binomial crossed/not crossed",
  pch = 20,
  col = "steelblue"
)
abline(h = 0, lty = 2, col = "red")


library(DHARMa)
sim_res <- simulateResiduals(fittedModel = yes_no_crossings_mods_logit_class2$dev90.creek.sex)
plot(sim_res)

outliers <- which(sim_res$scaledResiduals > 0.9 | sim_res$scaledResiduals < 0.1)
zz <- crossing_analysis_table[outliers, ]  # inspect rows


# trying cloglog link which may be better for 0-infl logistic regression (Zuur 2009 chpt 10)
system.time(
  yes_no_crossings_mods_cloglog_class2 <- fit_crossing_mods_yes_no("cloglog")
) # 623

yes_no_crossings_mods_cloglog_class2$aic


aictab(list(yes_no_crossings_mods_logit$dev90.creek.class, yes_no_crossings_mods_cloglog$dev90.creek.class), c("logit", "cloglog"))
aictab(list(yes_no_crossings_mods_logit_class2$dev90.creek.sex, yes_no_crossings_mods_cloglog_class2$dev90.creek.sex), c("logit", "cloglog"))

# logit better by 20 aic

saveRDS(yes_no_crossings_mods_logit, here("model_objects/yes_no_crossings_mods_logit"))


###

wt_crossings_mods_puma <- readRDS(here("model_objects/wt_crossings_mods_puma"))

best_mod <- wt_crossings_mods_puma$dev60.creek.class

sec_best_mod <- wt_crossings_mods_puma$dev60.class.bridge

# cross validation
cross_validate_best_yes_no_model <- function(zpuma) {

    train_data <- crossing_analysis_table %>% filter(animal.id != zpuma)
    test_data  <- crossing_analysis_table %>% filter(animal.id == zpuma)
    
    
    dev90.creek.class.logit     = glmmTMB(crossed.bin ~ dev.90 * num.creek.bin * class + (1|animal.id), data = train_data, family = binomial(link = "logit"))
    preds.logit = data.frame(preds = predict(dev90.creek.class.logit, newdata = test_data, allow.new.levels = TRUE, type = "response")) %>% 
      mutate(link = "logit")
    
    dev90.creek.class.cloglog     = glmmTMB(crossed.bin ~ dev.90 * num.creek.bin * class + (1|animal.id), data = train_data, family = binomial(link = "cloglog"))
    preds.cloglog = data.frame(preds = predict(dev90.creek.class.cloglog, newdata = test_data, allow.new.levels = TRUE, type = "response")) %>% 
      mutate(link = "cloglog")
    
    results = bind_rows(bind_cols(test_data, preds.logit),
                        bind_cols(test_data, preds.cloglog)) %>% 
      mutate(rmse = sqrt(mean((preds - crossed.bin)^2)),
             left.out = zpuma)
    
  return(results)
}

cv_results_yes_no <- map_df(distinct(crossing_analysis_table, animal.id)$animal.id, cross_validate_best_yes_no_model)

cv_results_yes_no %>% 
  group_by(link) %>% 
  summarise(meanRMSE = mean(rmse))


  ggplot() +
  geom_violin(data = cv_results_yes_no, aes(x = as.factor(crossed.bin), y = preds)) +
    facet_wrap(~link)

# logit and clog-log links both have similar cross-validation mean RMSE (similar out of sample prediction), but the logit link has far better within-sample model fit (dAICc for cloglog = 20). Going to stick with the logit link for interpretation because we're interested in both providing a good description of observed crossings and predicting unobserved crossings.
  
  
  
### now number of crossings for crossed segments ----

  
  
  fit_crossing_mods_crossed_only <- function(zcross, family_choice = "gamma") {
    # Define the family based on user input
    fam <- switch(family_choice,
                  gamma = Gamma(link = "log"),
                  lognormal = gaussian(link = "log"),
                  stop("Invalid family. Use 'gamma' or 'lognormal'."))
    
    df <- crossing_analysis_table %>% 
      filter(seg.wt.crossing > 0)
    
    model_formulas <- list(
      treshr60 = "tre.shr.60",
      dev30 = "dev.30",
      creek = "num.creek",
      class = "class",
      sex = "sex",
      
      # 2 way additive
      treshr60_creek = "tre.shr.60 + num.creek",
      dev30_creek = "dev.30 + num.creek",
      treshr60_sex = "tre.shr.60 + sex",
      dev30_sex = "dev.30 + sex",
      treshr60_class = "tre.shr.60 + class",
      dev30_class = "dev.30 + class",
      creek_sex = "num.creek + sex",
      class_sex = "class + sex",
      class_creek = "class + num.creek",
      
      # 3 way additive
      treshr60_creek_class = "tre.shr.60 + num.creek + class",
      treshr60_creek_sex = "tre.shr.60 + num.creek + sex",
      treshr60_class_sex = "tre.shr.60 + class + sex",
      dev30_creek_class = "dev.30 + num.creek + class",
      dev30_creek_sex = "dev.30 + num.creek + sex",
      dev30_class_sex = "dev.30 + class + sex",
      creek_class_sex = "num.creek + class + sex",
      
      # 2 way interactions
      treshr60.creek = "tre.shr.60 * num.creek",
      dev30.creek = "dev.30 * num.creek",
      treshr60.sex = "tre.shr.60 * sex",
      dev30.sex = "dev.30 * sex",
      treshr60.class = "tre.shr.60 * class",
      dev30.class = "dev.30 * class",
      creek.sex = "num.creek * sex",
      #creek.class = "num.creek * class",
      class.sex = "class * sex",
      
      # 3 way interactions
      #treshr60.creek.class = "tre.shr.60 * num.creek * class",
      treshr60.creek.sex = "tre.shr.60 * num.creek * sex",
      treshr60.class.sex = "tre.shr.60 * class * sex",
      #dev30.creek.class = "dev.30 * num.creek * class",
      dev30.creek.sex = "dev.30 * num.creek * sex",
      dev30.class.sex = "dev.30 * class * sex",
      #creek.class.sex = "num.creek * class * sex",
      
      # Null
      intercept = "1"
    )
    
    zmods <- lapply(model_formulas, function(rhs) {
      formula <- as.formula(paste(zcross, "~", rhs, "+ (1|animal.id)"))
      glmmTMB(formula, data = df, family = fam)
    })
    
    zmods$aic <- aictab(zmods, names(zmods))
    
    return(zmods)
  }
  
  # fit for weighted crossings as response
  
  system.time(
    wt_crossings_mods_crossed_only_gamma <- fit_crossing_mods_crossed_only("seg.wt.crossing", "gamma")
  ) # 24
  
  wt_crossings_mods_crossed_only_gamma$aic
  
  
  
  system.time(
    wt_crossings_mods_crossed_only_lognormal <- fit_crossing_mods_crossed_only("seg.wt.crossing", "lognormal")
  ) # 25
  
  wt_crossings_mods_crossed_only_lognormal$aic
  
  aictab(list(wt_crossings_mods_crossed_only_gamma$dev30.creek.class, wt_crossings_mods_crossed_only_lognormal$dev30.creek.class), c("gamma", "lognormal"))
  
  saveRDS(wt_crossings_mods_crossed_only_gamma, here("model_objects/wt_crossings_mods_crossed_only_gamma"))
  
  
  wt_crossings_mods_crossed_only_gamma <- readRDS(here("model_objects/wt_crossings_mods_crossed_only_gamma"))
  
  
  best_plot_df <- data.frame(Fitted = fitted(wt_crossings_mods_crossed_only_gamma$dev30.creek.class), 
                             Residuals = resid(wt_crossings_mods_crossed_only_gamma$dev30.creek.class))
 
   ggplot(best_plot_df, aes(x = Fitted, y = Residuals)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    theme_minimal()
  
  # model validation ----
  # r-squared
  
  r.squaredGLMM(best_mod)
  
  # cross validation
  cross_validate_crossed_only_model <- function(zpuma) {
    
    df <- crossing_analysis_table %>% 
      filter(seg.wt.crossing > 0)
    
    train_data <- df %>% filter(animal.id != zpuma)
    test_data  <- df %>% filter(animal.id == zpuma)
    
    dev30.creek.class.gamma = glmmTMB(seg.wt.crossing ~ dev.30 * num.creek * class + (1|animal.id), data = train_data, family = Gamma(link = "log"))
    dev30.creek.class.lognormal = glmmTMB(seg.wt.crossing ~ dev.30 * num.creek * class + (1|animal.id), data = train_data, family = gaussian(link = "log"))
      
    preds.gamma = data.frame(preds = predict(dev30.creek.class.gamma, newdata = test_data, allow.new.levels = TRUE, type = "response"),
                             link = "gamma")
    preds.lognormal = data.frame(preds = predict(dev30.creek.class.lognormal, newdata = test_data, allow.new.levels = TRUE, type = "response"),
                                 link = "lognormal")
    
    results = bind_rows(bind_cols(test_data, preds.gamma),
                        bind_cols(test_data, preds.lognormal)) %>% 
      mutate(rmse = sqrt(mean((preds - crossed.bin)^2)),
             left.out = zpuma)    
    return(results)
  }
  
  cv_results_crossed_only <- map_df(distinct(crossing_analysis_table, animal.id)$animal.id, cross_validate_crossed_only_model)
  
  cv_results_crossed_only %>% 
    group_by(link) %>% 
    summarise(meanRMSE = mean(rmse))
  
  
  ggplot() +
    geom_point(data = cv_results_crossed_only, aes(x = seg.wt.crossing, y = preds), alpha = 0.2) +
    stat_smooth(data = cv_results_crossed_only, aes(x = seg.wt.crossing, y = preds), color = "red", method = "lm") +
    geom_abline() +
    ylim(0, 10) +
    facet_wrap(~link)
  

# similar to crossed/not crossed, the gamma link is far better supported by AIC (better within group model fit) and has similar out of sample prediction as the lognormal link. so will use the models fit with the gamma link for inference  
    

# plotting best model estimates
  # Get observed range of dev.60 for each class
  dev30_ranges <- crossing_analysis_table %>% 
    filter(seg.wt.crossing > 0) %>%
    group_by(class) %>%
    summarise(min_dev30 = min(dev.30, na.rm = TRUE),
              max_dev30 = max(dev.30, na.rm = TRUE)) %>%
    ungroup()
  
  newdat <- crossing_analysis_table %>%
    filter(seg.wt.crossing > 0) %>%
    group_by(class) %>%
    summarise(min_dev30 = min(dev.30, na.rm = TRUE),
              max_dev30 = max(dev.30, na.rm = TRUE)) %>% 
    mutate(dev.30 = map2(min_dev30, max_dev30, ~ seq(from = .x, to = .y, by = 0.1))) %>%
    select(class, dev.30) %>%
    unnest(dev.30) %>%
    crossing(num.creek = 0:3)
  

zpred_wt = predict(wt_crossings_mods_crossed_only_gamma$dev30.creek.class, newdat, se.fit = TRUE, re.form = NA, type = "response") %>% 
  data.frame() %>% 
  bind_cols(newdat) %>% 
  mutate(lwr = fit - (1.96 * se.fit),
         upr = fit + (1.96 * se.fit),
         dev.30 = dev.30 * 100,
         class = factor(class, levels = c("Local", "Collector", "Arterial", "Highway")))

zpred_wt %>% 
  ggplot() +
  geom_line(aes(y = fit, x = dev.30, color = as.character(num.creek)), linewidth = 1) +
  #geom_ribbon(aes(ymin = lwr, ymax = upr, x = dev.60, fill = class), alpha = 0.3) +
  facet_wrap(~class) +
  labs(x = "% developed area within 30m of road",
       y = "# crossings",
       title = "Mean number of road crossings per mt lion per month\nin each 888m road segment, by road type",
       color = "Number of\ncreek/river\nintersections",
       fill = "") +
  theme_bw() + 
  guides(colour = guide_legend(reverse=T))+
  theme(
    #title = element_text(size = 14),
    #axis.title = element_text(size = 14),        # Increase axis title size
    #axis.text = element_text(size = 12),         # Increase axis text size
    #strip.text = element_text(size = 12),        # Increase facet label size
    #legend.text = element_text(size = 12),       # Increase legend text size
    #legend.title = element_text(size = 14),      # Increase legend title size
    legend.position = c(0.85, 0.3)      
  )

cggsave(here("figures/dev60.creek.class_600dpi.png"), dpi = 600, width = 6, height = 4)

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




