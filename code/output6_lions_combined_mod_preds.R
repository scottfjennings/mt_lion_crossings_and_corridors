


library(tidyverse)
library(here)
library(spaMM)
library(patchwork)




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
  mutate(region = ifelse(animal.id %in% c("P31", "P39"), 1, 2)) %>% # region 1 is west of 101, 2 is east of 101
  distinct(seg.label, region)



coarsener = 2000

main_analysis_table <- readRDS(here("data/analysis_inputs/combined_lions_analysis_table")) %>% 
  mutate(class = ifelse(class %in% c("Highway", "Arterial"), "Major", as.character(class))
         , num.creek.bin = pmin(num.creek, 3)) %>% 
  left_join(coords) %>% 
  left_join(seg_regions) %>% 
  mutate(coarse_x = (floor(x / coarsener) + 0.5) * coarsener,
         coarse_y = (floor(y / coarsener) + 0.5) * coarsener) 

best_logreg_sp <- readRDS(here("model_objects/lions_combined/exploratory_mods_logreg_sp"))$"cohesion.200.25 + patch.touches.road + mean.tre.shr.30 + mean.dev.60 + num.creek.bin + class"


# prep newdata to get predictions on the scale of the original predictors ----
# Extract means and sds from original data for each scaled predictor
means <- sapply(main_analysis_table[c("cohesion.200.25", "mean.tre.shr.30", "mean.dev.60")], mean)
sds   <- sapply(main_analysis_table[c("cohesion.200.25", "mean.tre.shr.30", "mean.dev.60")], sd)


# For illustration, say we want to plot predicted probability vs cohesion.200.25
# holding other variables at their mean or typical values:


newdat_coarse_coords2 <- main_analysis_table %>% 
  filter(region == 2) %>% 
  summarise(med.coarse.x = median(coarse_x),
            med.coarse.y = median(coarse_y)) %>% 
  ungroup()

newdata <- crossing(orig.cohesion.200.25 = seq(0, 100, length.out = 11),
                    orig.mean.tre.shr.30 = seq(0, 100, length.out = 11),
                    orig.mean.dev.60 = seq(0, 100, length.out = 11),
                    class = distinct(main_analysis_table, class)$class,
                    patch.touches.road = distinct(main_analysis_table, patch.touches.road)$patch.touches.road,
                    num.creek.bin = distinct(main_analysis_table, num.creek.bin)$num.creek.bin) %>% 
  mutate(region = 2,
         coarse_x = newdat_coarse_coords2$med.coarse.x,
         coarse_y = newdat_coarse_coords2$med.coarse.y) %>% 
  mutate(cohesion.200.25 = (orig.cohesion.200.25 - means["cohesion.200.25"]) / sds["cohesion.200.25"],
         mean.tre.shr.30 = (orig.mean.tre.shr.30 - means["mean.tre.shr.30"]) / sds["mean.tre.shr.30"],
         mean.dev.60 = (orig.mean.dev.60 - means["mean.dev.60"]) / sds["mean.dev.60"])





# Bootstrap predictions with CIs ----
#preds <- predict(best_mod_logreg_sp[[1]], newdata = newdata, type = "link", interval = "confidence", re.form = NA)
# not calculating CI

# manual bootstrapping for CI
boot_predict_fixed <- function(data, newdata, formula_fixed, B = 200) {
  preds_matrix <- matrix(NA, nrow = nrow(newdata), ncol = B)
  
  for (i in seq_len(B)) {
    boot_idx <- sample(nrow(data), replace = TRUE)
    boot_data <- data[boot_idx, ]
    
    # Assign weights to a column named 'weights' (this is key!)
    
    fit <- fitme(
      formula = formula_fixed,
      family = binomial(),
      data = boot_data,
      prior.weights = boot_data$num.lion.months,  # refers to the 'weights' column in boot_data
      method = "ML",
      control.HLfit = list(algebra = "spprec",
                           max.iter.mean = 2000)
    )
    
    # Marginal (population-level) prediction: exclude random/spatial effects
    preds_matrix[, i] <- predict(fit, newdata = newdata, type = "response", re.form = NULL)
  }
  
  tibble(
    fit = rowMeans(preds_matrix),
    lower = apply(preds_matrix, 1, quantile, 0.025),
    upper = apply(preds_matrix, 1, quantile, 0.975)
  )
}




system.time(
  boot_results <- boot_predict_fixed(
    data = main_analysis_table,
    newdata = newdata,
    formula_fixed = bin.crossing ~ cohesion.200.25 + patch.touches.road +
      mean.tre.shr.30 + mean.dev.60 +
      num.creek.bin + class + Matern(1 | region + coarse_x + coarse_y),
    B = 1000
  )
) # ~21 hours


saveRDS(boot_results, here("model_objects/lions_combined/boot_results_best_logreg_sp"))


# Plot bootstrapped predictions ----


boot_results <- readRDS(here("model_objects/lions_combined/boot_results_best_logreg_sp"))

pred_plot_df <- bind_cols(newdata, boot_results) %>% 
  mutate(class = factor(class, levels = c("Local", "Collector", "Major")))


# Create plots without y-axis labels
p1 <- pred_plot_df %>% 
  filter(class == "Local", patch.touches.road == TRUE, num.creek.bin == 1, 
         orig.mean.tre.shr.30 == 50, orig.mean.dev.60 == 50) %>% 
  ggplot(aes(x = orig.cohesion.200.25, y = fit)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  labs(x = "Cohesion") +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
  theme_bw() + theme(axis.title.y = element_blank())

p2 <- pred_plot_df %>% 
  filter(class == "Local", patch.touches.road == TRUE, num.creek.bin == 1, 
         orig.cohesion.200.25 == 50, orig.mean.dev.60 == 50) %>% 
  ggplot(aes(x = orig.mean.tre.shr.30, y = fit)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  labs(x = "% Woody") +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
  theme_bw() + theme(axis.title.y = element_blank())

p3 <- pred_plot_df %>% 
  filter(class == "Local", patch.touches.road == TRUE, num.creek.bin == 1, 
         orig.cohesion.200.25 == 50, orig.mean.tre.shr.30 == 50) %>% 
  ggplot(aes(x = orig.mean.dev.60, y = fit)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.3) +
  labs(x = "% Development") +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
  theme_bw() + theme(axis.title.y = element_blank())

p4 <- pred_plot_df %>% 
  filter(class == "Local", orig.cohesion.200.25 == 50, orig.mean.tre.shr.30 == 50, 
         orig.mean.dev.60 == 50, patch.touches.road == TRUE) %>% 
  ggplot(aes(x = num.creek.bin, y = fit)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.3) +
  labs(x = "Number of riparian crossings") +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
  theme_bw() + theme(axis.title.y = element_blank())

p5 <- pred_plot_df %>% 
  filter(class == "Local", orig.cohesion.200.25 == 50, orig.mean.tre.shr.30 == 50, 
         orig.mean.dev.60 == 50, num.creek.bin == 1) %>% 
  ggplot(aes(x = as.character(patch.touches.road), y = fit, group = num.creek.bin)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), linewidth = 1, width = .2) +
  labs(x = "Road intersects woody") +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
  theme_bw() + theme(axis.title.y = element_blank())

p6 <- pred_plot_df %>% 
  filter(patch.touches.road == TRUE, orig.cohesion.200.25 == 50, orig.mean.tre.shr.30 == 50, 
         orig.mean.dev.60 == 50, num.creek.bin == 1) %>% 
  ggplot(aes(x = class, y = fit)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), linewidth = 1, width = .2) +
  labs(x = "Road class") +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
  theme_bw() + theme(axis.title.y = element_blank())


# combine with patchwork (cowplot totally failed when trying to export)
six_panel <- (p1 + p2) / (p3 + p4) / (p5 + p6) +
  plot_layout(ncol = 1, byrow = TRUE)

#  Step 2: Create a rotated y-axis label as a grob
y_axis_label <- grid::textGrob(
  "Predicted crossing probability", 
  rot = 90, gp = grid::gpar(fontsize = 12, fontface = "plain")
)

# Step 3: Use patchwork's layout to place y label on left
final_plot <- wrap_elements(full = y_axis_label) + six_panel +
  plot_layout(widths = c(0.05, 1)) +
  plot_annotation(
    #title = "Marginal effects for each fixed effect in the best-supported model",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.margin = margin(10, 10, 10, 10)
    )
  )

ggsave(
  filename = here("figures/lions_combined_best_mod_marginal_effects.png"),
  plot = final_plot,
  width = 9,
  height = 8
)





pred_plot_df %>% 
  filter(class == "Local", orig.cohesion.200.25 == 50, orig.mean.tre.shr.30 == 50, 
         orig.mean.dev.60 == 50, patch.touches.road == TRUE)

pred_plot_df %>% 
  filter(class == "Local", orig.cohesion.200.25 == 50, orig.mean.tre.shr.30 == 50, 
         orig.mean.dev.60 == 50, num.creek.bin == 0)

pred_plot_df %>% 
  filter(class == "Local", orig.cohesion.200.25 == 50, orig.mean.tre.shr.30 == 50, orig.mean.dev.60 == 50) %>% 
  filter((fit == min(fit) | fit == max(fit)))
