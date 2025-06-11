


######################################################

library(landscapemetrics)
library(raster)
library(tidyverse)
library(gridExtra)

# Function to generate fragmented corridor landscape
generate_fragmented_corridor <- function(size = 20, frag_level = 0) {
  mat <- matrix(0, nrow = size, ncol = size)
  corridor_rows <- floor(size * 0.4):ceiling(size * 0.6)
  mat[corridor_rows, ] <- 1  # base corridor
  
  # Add fragmentation by randomly turning 1s to 0s in the corridor
  if (frag_level > 0) {
    corridor_cells <- which(mat == 1)
    n_frag <- round(length(corridor_cells) * frag_level)
    fragged_indices <- sample(corridor_cells, n_frag)
    mat[fragged_indices] <- 0
  }
  
  mat <- mat[nrow(mat):1, ]  # flip for raster orientation
  raster(mat)
}

# Fragmentation levels to simulate
frag_levels <- seq(0, 0.9, by = 0.2)

# Generate landscapes and calculate cohesion
plots <- list()

for (frag in frag_levels) {
  r <- generate_fragmented_corridor(frag_level = frag)
  
  # Calculate class-level cohesion (class = 1)
  cohesion_val <- tryCatch({
    round(
      lsm_c_cohesion(r) %>%
        filter(class == 1) %>%
        pull(value),
      2
    )
  }, error = function(e) NA)
  
  # Plot
  df <- as.data.frame(r, xy = TRUE)
  names(df)[3] <- "value"
  
  p <- ggplot(df, aes(x = x, y = y, fill = factor(value))) +
    geom_raster() +
    coord_equal() +
    scale_fill_manual(values = c("white", "forestgreen")) +
    labs(
      title = paste0("Fragmentation of mini-corridor: ", frag),
      subtitle = paste0("Cohesion: ", ifelse(is.na(cohesion_val), "NA", cohesion_val))
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "none")
  
  plots[[as.character(frag)]] <- p
}

# Show plots in grid
grid.arrange(grobs = plots, ncol = 3)







library(landscapemetrics)
library(raster)
library(ggplot2)
library(dplyr)
library(gridExtra)

# Function to generate fragmented corridor landscape
generate_fragmented_corridor <- function(size = 20, frag_level = 0) {
  mat <- matrix(0, nrow = size, ncol = size)
  corridor_rows <- floor(size * 0.4):ceiling(size * 0.6)
  mat[corridor_rows, ] <- 1  # base corridor
  
  # Add fragmentation by randomly turning 1s to 0s in the corridor
  if (frag_level > 0) {
    corridor_cells <- which(mat == 1)
    n_frag <- round(length(corridor_cells) * frag_level)
    fragged_indices <- sample(corridor_cells, n_frag)
    mat[fragged_indices] <- 0
  }
  
  mat <- mat[nrow(mat):1, ]  # flip for raster orientation
  raster(mat)
}

# Fragmentation levels and replicates
frag_levels <- seq(0, 0.9, by = 0.2)
n_reps <- 4

plots <- list()
plot_idx <- 1

# Generate and plot 4 replicates per frag level
for (frag in frag_levels) {
  for (rep in 1:n_reps) {
    r <- generate_fragmented_corridor(frag_level = frag)
    
    # Calculate class-level cohesion (class = 1)
    cohesion_val <- tryCatch({
      round(
        lsm_c_cohesion(r) %>%
          filter(class == 1) %>%
          pull(value),
        2
      )
    }, error = function(e) NA)
    
    # Raster to dataframe for plotting
    df <- as.data.frame(r, xy = TRUE)
    names(df)[3] <- "value"
    
    # Build plot
    p <- ggplot(df, aes(x = x, y = y, fill = factor(value))) +
      geom_raster() +
      coord_equal() +
      scale_fill_manual(values = c("white", "forestgreen")) +
      labs(
        title = paste0("Frag: ", frag, ", Rep: ", rep),
        subtitle = paste0("Cohesion: ", ifelse(is.na(cohesion_val), "NA", cohesion_val))
      ) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none")
    
    plots[[plot_idx]] <- p
    plot_idx <- plot_idx + 1
  }
}

# Show all plots in grid (e.g. 5 frag levels x 4 reps = 20)
grid.arrange(grobs = plots, ncol = 4)


