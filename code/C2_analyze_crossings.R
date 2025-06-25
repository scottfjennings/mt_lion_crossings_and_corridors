


library(tidyverse)
library(here)
library(sf)
library(lme4)
library(AICcmodavg)
library(MuMIn)
library(readxl)
library(glmmTMB)


source(here("code/helper_data.R"))


# from C1_analysis_table_model_structure
main_analysis_table <- readRDS(here("data/analysis_inputs/annual_analysis_table")) %>%
  mutate(across(c(cohesion.200.50, mean.tre.shr.210, mean.dev.300), ~ as.numeric(scale(.x))),
         binned.annual.seg.raw.crossing = ifelse(annual.seg.raw.crossing > 0, 1, annual.seg.raw.crossing),
         prop.year = num.months/12,
         class = ifelse(class == "Local", as.character(class), "Non-local"),
         num.creek.bin = pmin(num.creek, 1)) # still treating this as a continuous predictor b/c we believe, given our segment length, that the relative importance of adding 1 more creek intersection is minimal beyond 4 creeks (e.g. creek intersections more than every 300m probably not important)



# main_analysis_table <- main_analysis_table %>% 
#  filter(monthly.seg.raw.crossing < 10)


# need to load exploration_model_formulas from C1_
# initial modelling attempt, glmm with Poisson distribution  ----

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
fit_glmer_mods <- function(zformulas, zfamily = "poisson") {
  
  zmods <- lapply(zformulas, function(fix.stru) {
    formula <- as.formula(paste("monthly.seg.raw.crossing ~ ", fix.stru, "+ offset(log(monthly.seg.wt.crossing + 0.0001)) + (1|animal.id)"))
    glmer(formula, data = main_analysis_table, family = zfamily)
  })
  
  zmods$aic <- aictab(zmods, names(zmods))
  
  return(zmods)
}



system.time(
  exploratory_mods_glmer <- fit_glmer_mods(exploration_model_formulas, "poisson")
) # 589, 570


exploratory_mods_glmer$aic         

summary(exploratory_mods_glmer$cohesion_dev_creek_sex)


# Plot residuals vs fitted
plot(
  fitted(exploratory_mods_glmer$cohesion_dev_creek_sex), resid(exploratory_mods_glmer$cohesion_dev_creek_sex),
  xlab = "Fitted Values",
  ylab = "Residuals",
  main = "Residuals vs Fitted Values best glmer model",
  pch = 20,
  col = "steelblue"
)
abline(h = 0, lty = 2, col = "red")

# the best glmm poisson model is still under predicting segments with few crossings and overpredicting segments with a lot of crossings
# zero inflation is still probably causing problems

# fitting negative binomial with glmmTMB which should be faster than fitting with lme4
#' fit_glmmTMB_nb_mods
#' 
#' General model fitting function using glmmTMB. Useful for exploratory and hypothesis-testing models.
#' Fits a negative binomial model with an offset and a random effect for animal.id.
#' 
#' @param zformulas Named list of fixed effect model structures
#' 
#' @return List of model objects, with names matching zformulas, and an AIC table "$aic"
#' 
#' @examples
fit_glmmTMB_nb_mods <- function(zformulas) {
  
  zmods <- lapply(zformulas, function(fix.stru) {
    formula <- as.formula(
      paste(
        "monthly.seg.raw.crossing ~", 
        fix.stru, 
        "+ offset(log(monthly.seg.wt.crossing + 0.0001)) + (1|animal.id)"
      )
    )
    
    glmmTMB::glmmTMB(
      formula = formula,
      family = glmmTMB::nbinom2(),
      data = main_analysis_table
    )
  })
  
  zmods$aic <- aictab(zmods, base = TRUE)
  
  return(zmods)
}


system.time(
  exploratory_mods_glmmtmb_nb <- fit_glmmTMB_nb_mods(exploration_model_formulas[1])
) # 


exploratory_mods$aic         

summary(exploratory_mods$cohesion_dev_creek_sex)


# Plot residuals vs fitted
plot(
  fitted(exploratory_mods_glmer$cohesion_dev_creek_sex), resid(exploratory_mods_glmer$cohesion_dev_creek_sex),
  xlab = "Fitted Values",
  ylab = "Residuals",
  main = "Residuals vs Fitted Values best glmer model",
  pch = 20,
  col = "steelblue"
)
abline(h = 0, lty = 2, col = "red")



# fitting glmmTMB mods which allow more flexible handling of zero inflation, overdispersion ----


#' fit_glmmTMB_mods
#' 
#' general model fitting function. this should be good for exploratory and hypothesis testing models. fits glmmTMB::glmmTMB models with offset for segment proportion inside each BBMM UD and a random effect for animal.id. allows specifying formula to model the zero-inflated process
#' 
#' 
#' @param zformulas named list of fixed effects model structures
#' @param zfamily model family to specify the error distribution and link function
#'
#' @returns list of model objects, with names matching the names of zformulas, and an aic table "$aic" for all model structures in zformulas
#'
#' @examples
fit_glmmtmb_mods <- function(zresponse, zformulas, zfamily, ziform) {
  # Ensure zfamily is interpreted correctly
  if (is.character(zfamily)) {
    zfamily <- match.fun(zfamily)()  # e.g., "nbinom2" → nbinom2()
  }
  
  form_names <- names(zformulas)
  
  zmods <- lapply(zformulas, function(fix.stru) {
    formula <- as.formula(paste(
      zresponse, "~", fix.stru,
      "+ prop.year + offset(log(annual.seg.wt.crossing + 0.0001)) + (1|animal.id)"
    ))
    
    glmmTMB::glmmTMB(
      formula,
      data = main_analysis_table,
      family = zfamily,
      ziformula = as.formula(ziform)
    )
  })
  
  names(zmods) <- form_names
  zmods$aic <- AICcmodavg::aictab(zmods, form_names)
  return(zmods)
}






system.time(
  exploratory_mods_crosscount <- fit_glmmtmb_mods("annual.seg.raw.crossing", exploration_model_formulas, zfamily = "nbinom2", ziform = "~ 0")
) # 


exploratory_mods_crosscount$aic         

summary(exploratory_mods_glmmtmb$cohesion_dev_creek_sex)


# Plot residuals vs fitted
plot(
  fitted(exploratory_mods_glmmtmb$cohesion_dev_creek_sex), resid(exploratory_mods_glmmtmb$cohesion_dev_creek_sex),
  xlab = "Fitted Values",
  ylab = "Residuals",
  main = "Residuals vs Fitted Values best glmmTMB model",
  pch = 20,
  col = "steelblue"
)
abline(h = 0, lty = 2, col = "red")





system.time(
  exploratory_mods_crossbin <- fit_glmmtmb_mods("binned.annual.seg.raw.crossing", exploration_model_formulas, zfamily = binomial(link = "logit"), ziform = "~ 1")
) # 


exploratory_mods_crossbin$aic


summary(exploratory_mods_crossbin$dev_sex)


# Plot residuals vs fitted
plot(
  fitted(exploratory_mods_crossbin$dev_sex), resid(exploratory_mods_crossbin$dev_sex),
  xlab = "Fitted Values",
  ylab = "Residuals",
  main = "Residuals vs Fitted Values best glmmTMB model",
  pch = 20,
  col = "steelblue"
)
abline(h = 0, lty = 2, col = "red")










# model validation ----
# r-squared

r.squaredGLMM(best_mod)

# cross validation
cross_validate_best_model <- function(zpuma, zcross) {

    train_data <- crossing_analysis_table %>% filter(animal.id != zpuma)
    test_data  <- crossing_analysis_table %>% filter(animal.id == zpuma)
    
    
    dev60.creek.class = lmer(as.formula(paste(zcross, "~ dev.60 * num.creek * class + (1|animal.id)")), data = train_data, REML = FALSE)
    
      preds = data.frame(preds = predict(dev60.creek.class, newdata = test_data, allow.new.levels = TRUE))
      results = bind_cols(test_data, preds) %>% 
        mutate(rmse = sqrt(mean((preds - seg.wt.crossing)^2)),
               left.out = zpuma)

  return(results)
}

cv_results <- map2_df(distinct(crossing_analysis_table, animal.id)$animal.id, rep("seg.wt.crossing", nrow(distinct(crossing_analysis_table, animal.id))), cross_validate_best_model)


mean(cv_results$rmse)

  ggplot() +
  geom_point(data = cv_results, aes(x = actuals, y = preds)) +
    geom_abline()

  
ggplot(cv_results) +
  geom_point(aes(x = seg.wt.crossing, y = preds)) +
  geom_abline() +
  facet_wrap(~left.out, scales = "free")
  

cv_results$dev60_bin <- cut(cv_results$dev.60, breaks = 4)  # or choose a different number of bins

ggplot(cv_results) +
  geom_point(aes(x = seg.wt.crossing, y = preds)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~dev60_bin)


# plotting best model estimates
znewdat = expand.grid(sex = c("F", "M"),
                      dev.60 = seq(0, 1, by = 0.1),
                      num.creek = seq(0, 3),
                      class = c("Local", "Collector", "Arterial", "Highway", "Freeway")
                      #, animal.id = distinct(summed_crossing_analysis_table_long, animal.id)$animal.id
                      ) 


zpred_wt = predict(best_mod, znewdat, se.fit = TRUE, re.form = NA) %>% 
  data.frame() %>% 
  bind_cols(znewdat) %>% 
  mutate(lwr = fit - (1.96 * se.fit),
         upr = fit + (1.96 * se.fit),
         dev.60 = dev.60 * 100,
         sex = ifelse(sex == "F", "Female", "Male"),
         class = factor(class, levels = c("Local", "Collector", "Arterial", "Highway", "Freeway")))

zpred_wt %>% 
  filter(sex == "Female") %>% 
  ggplot() +
  geom_line(aes(y = fit, x = dev.60, color = as.character(num.creek)), linewidth = 1) +
  #geom_ribbon(aes(ymin = lwr, ymax = upr, x = dev.60, fill = class), alpha = 0.3) +
  facet_wrap(~class) +
  labs(x = "% developed area within 60m of road",
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
    legend.position = c(0.85, 0.2)      
  )

ggsave(here("figures/dev60.creek.class_600dpi.png"), dpi = 600, width = 6, height = 4)

#######

# fitting hypotheis testing models ----

hypothesis_model_formulas <- list(
  
  cohesion.patchrd = "cohesion.200.50 * patch.touches.road", # cohesion importance is dependent on the woody vegetation patch touching the segment
  cohesion.treshr = "cohesion.200.50 * mean.tre.shr.210", # cohesion importance is dependent on woody vegetation % cover
  cohesion.treshr = "cohesion.200.50 * I(mean.tre.shr.210^2)", # cohesion importance is dependent on woody vegetation % cover but the relationship is not linear
  
)
