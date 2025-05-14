library(sf)
library(spdep)

# Assuming 'lines_sf' is your sf object with LINESTRINGs

# Step 1: Compute midpoints
lines_sf$midpoint <- st_line_sample(lines_sf$geometry, sample = 0.5) |> st_cast("POINT")

# Step 2: Extract coordinates from midpoints
coords <- st_coordinates(lines_sf$midpoint)

# Step 3: Use k-nearest neighbors (e.g., k = 4)
knn_nb <- knearneigh(coords, k = 4)
nb <- knn2nb(knn_nb)

# Step 4: Plot to check neighborhood structure
plot(nb, coords, pch = 20)

# Step 5: Convert to BUGS-compatible lists
nb_list <- nb2listw(nb, style = "B", zero.policy = TRUE)
bugs_structure <- nb2WB(nb)

# These are used in BUGS:
adj <- bugs_structure$adj        # Adjacency list
weights <- bugs_structure$weights  # Typically set all to 1
num <- bugs_structure$num        # Number of neighbors per area
