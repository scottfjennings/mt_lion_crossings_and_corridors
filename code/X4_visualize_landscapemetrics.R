library(terra)
library(landscapemetrics)

# Create a raster with equal resolution (required for lsm_p_circle)
r <- rast(nrows = 100, ncols = 100, xmin = 0, xmax = 100, ymin = 0, ymax = 100)
values(r) <- 0

# Assign unique patch IDs
r[30:40, 30:40] <- 1  # Square patch
r[10:15, 70:95] <- 2  # Corridor-like patch
r[70:72, 20:22] <- 3  # Small patch 1
r[75:77, 25:27] <- 4  # Small patch 2
r[80:82, 30:32] <- 5  # Small patch 3

# Convert to categorical/factor
r_factor <- as.factor(r)

# Plot raster with patch IDs
plot(r_factor, col = c("lightgray", terrain.colors(5)), main = "Labeled Patch Landscape")


# Compute patch-level metrics
frac <- lsm_p_frac(r_factor) %>% 
  rename(frac = value) %>% 
  select(class, id, frac)
shape <- lsm_p_shape(r_factor) %>% 
  rename(shape = value) %>% 
  select(class, id, shape)
para <- lsm_p_para(r_factor) %>% 
  rename(para = value) %>% 
  select(class, id, para)
circle <- lsm_p_circle(r_factor) %>% 
  rename(circle = value) %>% 
  select(class, id, circle)

# Combine metrics by patch ID
metrics <- full_join(frac, shape) %>% 
  full_join(para) %>% 
  full_join(circle)

metrics
