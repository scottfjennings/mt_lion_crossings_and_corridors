

library(tidyverse)
library(here)


dep <- read.csv(here("data/depredation/depredation.csv"))

dep %>% 
  filter(year >= 2015) %>% 
  group_by(issued_taken) %>% 
  summarise(tot.taken = sum(take))
