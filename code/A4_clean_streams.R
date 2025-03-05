

library(tidyverse)
library(sf)
library(here)


# I created stream_road_Intersect and napa_sonoma_streams in ArcGIS pro by:
# 1. import the CARi stream layer from the Z drive and the final_cleaned_road layer and county boundaries from this project folder 
# 2. first use the county boundaries to trim the stream layer (maybe this not necessary for the intersection, but it made napa_sonoma_streams)
# 3. get the intersection of napa_sonoma_streams and final_cleaned_road

napa_sonoma_streams <- st_read(here("data/shapefiles/napa_sonoma_streams.shp"))

stream_road_Intersect <- st_read(here("data/shapefiles/stream_road_Intersect.shp"))
