

library(tidyverse)
library(here)
library(sf)
library(spaMM)

options(scipen = 999)

source(here("code/utilities.R"))
source(here("code/helper_data.R"))


exploratory_mods_logreg_sp <- readRDS(here("model_objects/lions_combined/exploratory_mods_logreg_sp"))

best_sp <- exploratory_mods_logreg_sp$"cohesion.200.25 + patch.touches.road + mean.tre.shr.30 + mean.dev.60 + num.creek.bin + class"

best_ranef <- ranef(best_sp)$`Matern(1 | region + coarse_x + coarse_y)` %>% 
  data.frame()


# Fixed effects
fixef_df <- as.data.frame(summary(best_sp)$beta_table)
fixef_df <- fixef_df %>%
  mutate(variable = rownames(.),
         effect_type = "Fixed effect",
         lower = Estimate - `Cond. SE`,
         upper = Estimate + `Cond. SE`) %>% 
  mutate(variable = str_replace_all(variable, "_", " + "),
         variable = str_replace_all(variable, "cohesion.200.25", "Cohesion"),
         variable = str_replace_all(variable, "mean.tre.shr.30", "% Woody"),
         variable = str_replace_all(variable, "patch.touches.roadTRUE", "Road intersects woody"),
         variable = str_replace_all(variable, "mean.dev.60", "% Development"),
         variable = str_replace_all(variable, "num.creek.bin", "# riparian crossings"),
         variable = str_replace_all(variable, "class", "Road Class-")) 



spatial_ranef_vals <- best_ranef$.
spatial_est <- sd(spatial_ranef_vals)
spatial_ci <- quantile(spatial_ranef_vals, probs = c(0.025, 0.975))


spatial_df <- data.frame(
  variable = "Spatial random field",
  Estimate = spatial_est,
  lower = spatial_ci[1],
  upper = spatial_ci[2],
  effect_type = "Spatial random effect"
)

# Combine
plot_df <- bind_rows(
  fixef_df,
  spatial_df
)


plot_df %>% 
  filter(variable != "(Intercept)") %>% 
  mutate(variable = fct_reorder(variable, Estimate)) %>%
  ggplot() +
  geom_point(aes(y = variable, x = Estimate)) +
  geom_errorbar(aes(y = variable, xmin = lower, xmax = upper), width = 0.1) +
  theme_bw() +
  labs(y = "",
       x = "Coefficient or Random Effect SD (logit scale)") +
  geom_vline(xintercept = 0)


# variance explained by fixed and fixed + random effects ----

X <- model.matrix(best_sp)
eta_fixed <- as.vector(X %*% fixef(best_sp))
var_fixed <- var(eta_fixed)

best_summary <- summary(best_sp)
best_summary_lambda <- best_summary$lambda_table

var_random <- best_summary_lambda$Var.# this is lambda from the spaMM output

var_residual <- (pi^2) / 3  # ≈ 3.29

R2_marginal <- var_fixed / (var_fixed + var_random + var_residual)
R2_conditional <- (var_fixed + var_random) / (var_fixed + var_random + var_residual)

