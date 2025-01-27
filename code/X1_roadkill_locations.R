
# which roads did mortality happen on?

mort <- read.csv("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/other_research/mt_lion_data_work/data/North Bay Puma Roadkill Data.csv")

mort_sf <- mort %>%
  dplyr::select(Source, FID, Lion.ID, lat, long, date_death) %>% 
  st_as_sf(x = .,
           coords = c("long", "lat"),
           crs = "+proj=longlat +datum=WGS84") %>% 
  st_transform(., crs = 26910)

st_write(mort_sf, "data/puma_mortalities.shp")

buff_roads <- napa_sonoma_rds_utm %>% 
  st_buffer(., 30)

mort_road_points <- st_intersection(mort_sf, buff_roads)

mort_roads <- mort_road_points %>% 
  data.frame() %>% 
  dplyr::select(label, leftcity, class, FID) %>% 
  left_join(napa_sonoma_rds_utm) %>% 
  st_as_sf()

ggplot() + 
  geom_sf(data = napa_sonoma_rds_utm, color = "gray") + 
  geom_sf(data = mort_roads, aes(color = as.character(FID))) +
  geom_sf(data = mort_sf, aes(color = as.character(FID)))

