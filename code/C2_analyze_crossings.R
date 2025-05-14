


# must run the data loading and model structure portions of B3_analysis_table_model_structure.R before this script to load crossing_analysis_table and set model structures



# fit models ----


# compare lmer vs glmer ----

dev60.bridge.lmer = lmer(seg.raw.crossing ~ dev60 * num.creek + (1|animal.id), data = summed_crossing_analysis_table, REML = FALSE)
dev60.bridge.glmer = glmer(seg.raw.crossing ~ dev60 * num.creek + (1|animal.id), data = summed_crossing_analysis_table, family = "poisson")
aictab(list(dev60.bridge.glmer, dev60.bridge.lmer))

# full candidate sets ----

fit_wt_crossing_mods_puma <- function(zcross, zformulas) {
  
zmods <- lapply(zformulas, function(rhs) {
  formula <- as.formula(paste(zcross, "~", rhs, "+ (1|animal.id)"))
  lmer(formula, data = crossing_analysis_table, REML = FALSE)
})

zmods$aic <- aictab(zmods, names(zmods))

return(zmods)
}

system.time(
  wt_crossings_mods_class2 <- fit_wt_crossing_mods_puma("seg.wt.crossing", model_formulas_1step)
)

# Plot residuals vs fitted
plot(
  fitted(wt_crossings_mods_class2$dev60.creek.sex), resid(wt_crossings_mods_class2$dev60.creek.sex),
  xlab = "Fitted Values",
  ylab = "Residuals",
  main = "Residuals vs Fitted Values",
  pch = 20,
  col = "steelblue"
)
abline(h = 0, lty = 2, col = "red")

# modeling weighted crossings with just animal.id as random intercept ----

saveRDS(wt_crossings_mods_class2, here("model_objects/wt_crossings_mods_class2"))



wt_crossings_mods_puma <- readRDS(here("model_objects/wt_crossings_mods_puma"))

best_mod <- wt_crossings_mods_puma$dev60.creek.class

sec_best_mod <- wt_crossings_mods_puma$dev60.class.bridge

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




