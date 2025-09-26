


library(tidyverse)
library(here)



analysis_table_complete <- readRDS(here("data/analysis_table_complete"))


# sex ----
analysis_table_complete %>% 
  count(sex)

analysis_table_complete %>% 
  count(sex, used.available) %>% 
  pivot_wider(id_cols = sex, names_from = used.available, values_from = n)

ggplot(analysis_table_complete) +
  geom_bar(aes(fill = as.factor(used.available), x = sex))


# seems like females are crossing more

# age ----
analysis_table_complete %>% 
  count(age)

analysis_table_complete %>% 
  count(age, used.available) %>% 
  pivot_wider(id_cols = age, names_from = used.available, values_from = n)

ggplot(analysis_table_complete) +
  geom_bar(aes(fill = as.factor(used.available), x = age))

# seems like adults are crossing more

# patchrd ----

analysis_table_complete %>% 
  count(patchrd)

analysis_table_complete %>% 
  count(patchrd, used.available) %>% 
  pivot_wider(id_cols = patchrd, names_from = used.available, values_from = n)


ggplot(analysis_table_complete) +
  geom_bar(aes(fill = as.factor(used.available), x = patchrd))

ggplot(analysis_table_complete) +
  geom_bar(aes(x = as.factor(used.available), fill = patchrd))

# riparian.binary ----

analysis_table_complete %>% 
  count(riparian.binary)

analysis_table_complete %>% 
  count(riparian.binary, used.available) %>% 
  pivot_wider(id_cols = riparian.binary, names_from = used.available, values_from = n)


ggplot(analysis_table_complete) +
  geom_bar(aes(fill = as.factor(used.available), x = as.factor(riparian.binary)))

ggplot(analysis_table_complete) +
  geom_bar(aes(x = as.factor(used.available), fill = as.factor(riparian.binary)))


# class ----
analysis_table_complete %>% 
  count(class)

analysis_table_complete %>% 
  count(class, used.available) %>% 
  pivot_wider(id_cols = class, names_from = used.available, values_from = n)


ggplot(analysis_table_complete) +
  geom_bar(aes(fill = as.factor(used.available), x = as.factor(class)))

ggplot(analysis_table_complete) +
  geom_bar(aes(x = as.factor(used.available), fill = as.factor(class)))



# continuous predictors ----

# cohesion ----

ggplot(analysis_table_complete) +
  geom_density(aes(x = cohesion, color = as.factor(used.available)))

ggplot(analysis_table_complete) +
  geom_violin(aes(x = as.factor(used.available), y = cohesion))

# big spike in availability of high cohesion segments


# tre.shr ----


ggplot(analysis_table_complete) +
  geom_density(aes(x = tre.shr, color = as.factor(used.available)))

ggplot(analysis_table_complete) +
  geom_violin(aes(x = as.factor(used.available), y = tre.shr))


# impervious ----



ggplot(analysis_table_complete) +
  geom_density(aes(x = impervious, color = as.factor(used.available)))

ggplot(analysis_table_complete) +
  geom_violin(aes(x = as.factor(used.available), y = impervious))



# all predictors show some relationship with used.available so there isn't any strong justification for leaving any out of modelling completely

