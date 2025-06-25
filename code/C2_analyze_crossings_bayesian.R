



library(tidyverse)
library(here)
library(sf)
library(brms)


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

# from C1_analysis_table_model_structure
# still treating num.creek.bin as a continuous predictor b/c we believe, given our segment length, that the relative importance of adding 1 more creek intersection is minimal beyond 4 creeks (e.g. creek intersections more than every 300m probably not important)
main_analysis_table <- readRDS(here("data/analysis_inputs/annual_analysis_table")) %>%
  left_join(coords) %>% 
  mutate(across(c(cohesion.200.50, mean.tre.shr.210, mean.dev.300), ~ as.numeric(scale(.x))),
         binned.annual.seg.raw.crossing = ifelse(annual.seg.raw.crossing > 0, 1, annual.seg.raw.crossing),
         prop.year = num.months/12,
         class = ifelse(class == "Local", as.character(class), "Non-local"),
         num.creek.bin = pmin(num.creek, 3),
         region = ifelse(animal.id %in% c("P31", "P39"), "West", "East")) 


# Convert character to factor
main_analysis_table <- main_analysis_table %>%
  mutate(
    patch.touches.road = factor(patch.touches.road, levels = c(FALSE, TRUE)),
    class = factor(class),
    sex = factor(sex),
    prop.year.c = prop.year - mean(prop.year, na.rm = TRUE)
  )




system.time(
test_model <- brm(
  annual.seg.raw.crossing ~ num.creek.bin + (1|animal.id),
  data = main_analysis_table,
  family = zero_inflated_negbinomial(link = "log", link_zi = "logit"),
  chains = 4, cores = 4, iter = 4000, warmup = 1000,
  control = list(adapt_delta = 0.99),
  refresh = 400
)
) # >1 hour for 1 model!


summary(test_model)
bayesplot::mcmc_trace(as.mcmc(test_model))
brms::check_hmc_diagnostics(test_model)





# Your predictors:
core_vars <- c("cohesion.200.50"
               #, "patch.touches.road"
               #, "mean.tre.shr.210"
               , "mean.dev.300"
               , "num.creek.bin"
               , "class"
               , "sex"
               , "region")

# Build the formula as a string:
formula_str <- paste(
  "annual.seg.raw.crossing ~",
  paste(core_vars, collapse = " + "),
  "+ prop.year + (1 | animal.id) + offset(log(annual.seg.wt.crossing + 0.0001))"
)

# Convert to formula object
bf_formula <- bf(as.formula(formula_str))

# Set weakly regularizing priors for fixed effects (betas)
priors <- c(
  set_prior("normal(0, 1)", class = "b"),           # slopes for fixed effects
  set_prior("normal(0, 5)", class = "Intercept"),   # intercept prior can be wider
  set_prior("student_t(3, 0, 10)", class = "sd"),   # random effect SDs
  set_prior("gamma(0.01, 0.01)", class = "shape")#,  # shape for negbin
#  set_prior("beta(1,1)", class = "zi")               # zero-inflation prior, uniform on [0,1]
)

# Fit the model
system.time(
fit <- brm(
  formula = bf_formula,
  data = main_analysis_table,
  family = negbinomial(link = "log"),
  prior = priors,
  chains = 4, cores = 4, iter = 4000, warmup = 1000,
  control = list(adapt_delta = 0.999),
  refresh = 200,
  save_pars = save_pars(all = TRUE)
)
)



# Model formula with spatial GP term
formula_str_sp <- paste(
  "annual.seg.raw.crossing ~",
  paste(core_vars, collapse = " + "),
  "+ prop.year + (1 | animal.id) + offset(log(annual.seg.wt.crossing + 0.0001)) + gp(x, y, k = 5, by = region)"
)

# Convert to formula object
bf_formula_sp <- bf(as.formula(formula_str_sp))

# Fit the model with zero-inflated negative binomial family
system.time(
fit_spatial <- brm(
  formula = bf_formula_sp,
  data = main_analysis_table,
  family = negbinomial(link = "log"),
  prior = priors,
  chains = 4, cores = 4, iter = 4000, warmup = 1000,
  control = list(adapt_delta = 0.9995),
  refresh = 200,
  save_pars = save_pars(all = TRUE)
)
)





# To check posterior correlations of fixed effects:
library(bayesplot)
mcmc_pairs(as.matrix(fit), pars = paste0("b_", core_vars))
