

# based on this chat
# https://chatgpt.com/share/684329f7-6084-8002-8770-76e84cc5ead0


library(tidyverse)
library(here)

monthly_seg_crossings_naive_roads_only_0s <- readRDS(here("data/analysis_inputs/monthly_seg_crossings_naive_roads_only_0s")) %>% 
  select()


# Replace this with your actual data frame name if needed
df_transformed <- monthly_seg_crossings_naive_roads_only_0s %>%
  mutate(
    log_eps_1e6 = log(monthly.seg.wt.crossing + 1e-6),
    log_eps_1e4 = log(monthly.seg.wt.crossing + 1e-4),
    log_eps_1e3 = log(monthly.seg.wt.crossing + 1e-3),
    log_eps_1e2 = log(monthly.seg.wt.crossing + 1e-2)
  ) %>%
  pivot_longer(cols = starts_with("log_eps_"),
               names_to = "epsilon",
               values_to = "log_transformed") %>%
  mutate(epsilon = recode(epsilon,
                          log_eps_1e6 = "ε = 1e-6",
                          log_eps_1e4 = "ε = 1e-4",
                          log_eps_1e3 = "ε = 1e-3",
                          log_eps_1e2 = "ε = 1e-2"))

# Plot
ggplot(df_transformed, aes(x = log_transformed, color = epsilon)) +
  geom_density(size = 1) +
  labs(title = "Effect of Different ε on log(monthly.seg.wt.crossing + ε)",
       x = "Transformed Value",
       y = "Density",
       color = "Epsilon (ε)") +
  theme_minimal(base_size = 14)
