

library(tidyverse)
library(here)
library(readxl)
library(sf)

options(scipen = 999)
source(here("code/utilities.R"))
source(here("code/helper_data.R"))

# load and prep sonoma county road layer ----

napa_rds <- read_sf(dsn= "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_county/Road_Centerlines/road_centerlines_roadcenterlines_calc.shp") %>% 
  dplyr::select(objectid, fullname, munileft, speedlimit, lanes, surface, roadclass, geometry, owned_by) %>% 
  rename("label" = fullname,
         "class" = roadclass,
         "leftcity" = munileft,
         "pubpriv" = owned_by) %>% 
#  rename_all(., ~tolower(.)) %>% 
  mutate(county = "Napa",
         leftcity = str_to_title(leftcity))


sonoma_rds <- read_sf(dsn= "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/sonoma_county/Streets/TRA_STREET_PUB.shp") %>% 
  dplyr::select(OBJECTID, Label, Class, PubPriv, LeftCity, SurfaceTyp, geometry) %>%
  rename("surface" = SurfaceTyp) %>% 
  rename_all(., ~tolower(.)) %>% 
  mutate(county = "Sonoma")

napa_sonoma_rds <- bind_rows(napa_rds, sonoma_rds) %>% 
  mutate(class = ifelse(str_detect(class, "Arterial"), "Arterial", class),
         class = ifelse(str_detect(class, "Collector"), "Collector", class),
         class = ifelse(str_detect(class, "Ramp|Interchange"), "Ramp/Interchange", class))

napa_sonoma_rds %>% 
  st_transform(crs = 26910) %>% 
  saveRDS("data/napa_sonoma_rds")

napa_sonoma_rds %>%  
  st_write(., "data/napa_sonoma_rds.shp", append = FALSE)


pub_priv <- napa_sonoma_rds %>% 
  data.frame() %>% 
  select(-geometry) %>% 
  distinct(label, leftcity, pubpriv) %>% 
  mutate(pubpriv = ifelse(pubpriv == "Private", pubpriv, "Public")) %>% 
  group_by(label, leftcity) %>% 
  mutate(pub.n.priv = any(pubpriv == "Public") & any(pubpriv == "Private")) %>% 
  arrange(label, leftcity)



# road class info here: https://www.fhwa.dot.gov/planning/processes/statewide/related/highway_functional_classifications/section01.cfm#Toc329359418
# and here: https://safety.fhwa.dot.gov/speedmgt/data_facts/docs/rd_func_class_1_42.pdf


# filtering ----
# exclude_roads.csv is a list of objects to filter out that was created by manually vieing the road layers in ArcGIS
# 
# trimmed roundabouts to be only a single path connecting the roads
# exclude all roads with 
# " Ct" in the name (courts)
# " Cir" in the name (circles)
# " Way" in the name
# data/exclude_roads.csv has all the Ct, Cir, and Way objects
# data/exclude_roads2.csv has only the manually selected objects - as of 10/8/24 this is the one to use

exclude_roads <- read.csv(here("data/exclude_roads2.csv"))

#exclude_roads <- exclude_roads %>% 
#  filter(!str_detect(label, " Way| Ct| Cir"))

#write.csv(exclude_roads, here("data/exclude_roads2.csv"))

napa_sonoma_rds_filtered <- napa_sonoma_rds %>%
  mutate(label = str_to_title(label),
         label = str_replace(label, "State Highway", "Hwy"),,
         pubpriv = case_when(objectid == "3654" & label == "Pleasant Hill Rd" ~ "Public",
                             objectid == "48432" & label == "Trenton Rd" ~ "Public",
                             objectid %in% c("49605", "49605", "55054") & label == "Barnett Valley Rd" ~ "Public",
                             objectid %in% c("25154", "25157", "25159", "26122", "41230") & label == "Channel Dr" ~ "Public",
                             objectid %in% c("41279") & label == "Brush Creek Rd" ~ "Public",
                             objectid %in% c("25768", "25762", "25761", "39798") & label == "Montecito Ave" ~ "Public",
                             objectid %in% c("15181", "14457") & label == "Franz Valley Rd" ~ "Public",
                             objectid %in% c("41720") & label == "Franz Valley School Rd" ~ "Public",
                             objectid %in% c("52674") & label == "Lichau Rd" ~ "Public",
                             objectid %in% c("28917") & label == "Ludwig Ave" ~ "Public",
                             objectid %in% c("36298") & label == "Pine Flat Rd" ~ "Public",
                             objectid %in% c("54055", "13669", "13670", "17107", "41620", "43731", "54056") & label == "5th St E" ~ "Public",
                             objectid %in% c("11757") & label == "Bailhache Ave" ~ "Public",
                             objectid %in% c("50327") & label == "Cavedale Rd" ~ "Public",
                             objectid %in% c("24940") & label == "Chanate Rd" ~ "Public",
                             objectid %in% c("50518") & label == "Coleman Valley Rd" ~ "Public",
                             objectid %in% c("13637", "54060") & label == "Denmark St" ~ "Public",
                             objectid %in% c("1333169", "54060") & label == "Grant St" ~ "Public",
                             objectid %in% c("15871") & label == "Jensen Ln" ~ "Public",
                             objectid %in% c("12558") & label == "Lovell Valley Rd" ~ "Public",
                             objectid %in% c("32024") & label == "Martin Ave" ~ "Public",
                             objectid %in% c("12258") & label == "Napa Rd" ~ "Public",
                             objectid %in% c("51524") & label == "Sharp Rd" ~ "Public",
                             objectid %in% c("1337358") & label == "Soscol Ferry Rd" ~ "Public",
                             objectid %in% c("1335883", "1337334", "1337615", "1337871", "1337872") & label == "Spring Mountain Rd" ~ "Public",
                             objectid %in% c("54059") & label == "5th St E" ~ "Public",
                             TRUE ~ pubpriv),
         surface = case_when(objectid %in% c("1335217", "1339630") & label == "Mount Veeder Rd" ~ "Paved",
                             objectid %in% c("1337482") & label == "4th Ave" ~ "Paved",
                             objectid %in% c("1332266") & label == "Soda Canyon Rd" ~ "Paved",
                             objectid %in% c("1340521", "1332959") & label == "Washington St" ~ "Paved",
                             TRUE ~ surface),
         label = case_when(objectid %in% c("1335392", "1341767", "1341768") & label == "State Highway 37" ~ "Hwy 37",
                           objectid %in% c(41622, 17501) ~ "Todd Rd",
                           objectid %in% c(54017) & label == "Rohnert Park Expy E" ~ "Rohnert Park Expy",
                           objectid %in% c(55073) ~ "W Dry Creek Rd",
                           TRUE ~ label),
         leftcity = case_when(leftcity == "Nap" ~ "Napa",
                              leftcity == "St Helena" ~ "St. Helena",
                              objectid == "1333813" & label == "Silverado Trl" ~ "Napa", # from Unincorportated
                              objectid %in% c("1332955", "1332956", "1332982", "1333867") & label == "Hwy 29" ~ "Napa", # from Yountville
                              objectid %in% c("1334999") & label == "Atlas Peak Rd" ~ "Napa", # from Unincorportated
                              objectid %in% c("1334126") & label == "Capell Valley Rd" ~ "Napa",
                              objectid %in% c("1335098", "1341970") & label == "Chiles Pope Valley Rd" ~ "St. Helena",
                              objectid %in% c("22209", "23194_", "23196", "23208", "23209", "24397", "24549", "24551", "26900", "26901", "26902", "26903", "26904", "28249", "28251", "42024", "42232", "51517") & leftcity == "Fulton" ~ "Santa Rosa",
                              objectid %in% c("34931", "34932", "35856") & leftcity == "Cazadero" ~ "Jenner",
                              objectid %in% c("14711", "14735", "14743", "14746") & leftcity == "Healdsburg" ~ "Calistoga",
                              objectid %in% c("1331046", "1335168") & leftcity == "Unicorporated" ~ "Napa",
                              objectid %in% c("9128", "9129", "9178", "9179", "9589", "51306", "41560", "7842") & leftcity == "Santa Rosa" ~ "Windsor",
                              objectid %in% c("1334087") & leftcity == "NA" ~ "Napa",
                              objectid %in% c("6647") & leftcity == "Rohnert Park" ~ "Santa Rosa",
                              objectid %in% c("1333550", "1335116", "1337476", "1337477", "1337478", "1340303", "1340305") & leftcity == "Pope Valley" ~ "St. Helena",
                              objectid %in% c("40717", "40718", "40719") & leftcity == "Santa Rosa" ~ "Rohnert Park",
                              objectid %in% c("1331703", "1341731", "1341732") & leftcity == "Unicorporated" ~ "Napa",
                              objectid %in% c("17127", "17132", "33028", "33146", "33924", "39805", "42340") & leftcity == "Rohnert Park" ~ "Santa Rosa",
                              objectid %in% c("1337037", "1341967", "1341968") & is.na(leftcity) ~ "Napa",
                              objectid %in% c("1337104", "1341969") & is.na(leftcity) ~ "Napa",
                              objectid %in% c("1335182", "1335184") & leftcity == "Unicorporated" ~ "Napa",
                              objectid %in% c("1333126", "1341747", "1341748", "1341749") & leftcity == "Unicorporated" ~ "Calistoga",
                              objectid %in% c("37548") & leftcity == "Geyserville" ~ "Annapolis",
                              objectid %in% c("1332260") & leftcity == "Napa" ~ "Yountville",
                              objectid %in% c("9904", "10887", "48414", "48415", "48416", "48417") & leftcity == "Forestville" ~ "Healdsburg",
                              objectid %in% c("11341", "11356", "27314") & leftcity ==  "Penngrove" ~ "Rohnert Park",
                              objectid %in% c("1341755", "1341756", "1341757", "1341758") & leftcity == "Unicorporated" ~ "Napa",
                              objectid %in% c("1333025", "1334127", "1334138", "1334652", "1334654", "1336202", "1341906", "1341908", "1341950", "1341951", "1341956", "1341957", "1341958", "1341959", "1341960") & leftcity == "Unincorporated" ~ "Napa",
                              objectid %in% c("340076", "1341711") & leftcity == "Unicorporated" ~ "Calistoga",
                              objectid %in% c("33774", "53043", "53046") & leftcity == "Rohnert Park" ~ "Cotati",
                              objectid %in% c("39264") & leftcity == "Healdsburg" ~ "Forestville",
                              objectid %in% c("23194") & leftcity == "Fulton" ~ "Santa Rosa",
                              objectid %in% c("1334196") & leftcity == "Unicorporated" ~ "Napa",
                              objectid %in% c("2667", "2668", "10867", "10876", "11804") & leftcity == "Graton" ~ "Sebastopol",
                              objectid %in% c("1333526", "1333527") & leftcity == "Unicorporated" ~ "St. Helena",
                              objectid %in% c("1335032", "1335034") & leftcity == "Unincorporated" ~ "Calistoga",
                              objectid %in% c("1334131", "1341713") & leftcity == "Unicorporated" ~ "Calistoga",
                              objectid %in% c("47871") & leftcity == "Rohnert Park" ~ "Santa Rosa",
                              objectid %in% c("38384") & leftcity == "Santa Rosa" ~ "Windsor",
                              objectid %in% c("1334074", "1341751", "1341752", "1341753") & leftcity == "Unicorporated" ~ "Napa",
                              objectid %in% c("1334087") & is.na(leftcity) ~ "Napa",
                              objectid %in% c("53195") & leftcity == "Rohnert Park" ~ "Santa Rosa",
                              objectid %in% c("1338130") & leftcity == "Out Of County" ~ "Calistoga",
                              objectid %in% c("7880", "7887", "7890", "7891", "7892", "8601", "14606", "14607", "14608", "37841", "37842") & leftcity == "Windsor" ~ "Santa Rosa",
                              objectid %in% c("23048", "23203", "23204", "23206", "23207", "24395", "24396", "24399", "38401", "42025", "51534", "53210") & leftcity == "Fulton" ~ "Santa Rosa",
                              objectid %in% c("10864") & leftcity == "Graton" ~ "Sebastopol",
                              objectid %in% c("1337037", "1341967", "1341968") & is.na(leftcity) ~ "Napa",
                              objectid %in% c("18624", "18625") & leftcity == "Santa Rosa" ~ "Glen Ellen",
                              objectid %in% c("1337869", "1341580") & leftcity == "Unincorporated" ~ "St. Helena",
                              objectid %in% c("1332262", "1332264") & leftcity == "Unicorporated" ~ "Pope Valley",
                              objectid %in% c("1335204") & leftcity == "Unicorporated" ~ "Napa",
                              objectid %in% c("1337093") & leftcity == "Unincorporated" ~ "St. Helena",
                              objectid %in% c("1340076") & leftcity == "Unicorporated" ~ "Calistoga",
                              objectid %in% c("1339688", "1341971") & is.na(leftcity) ~ "Napa",
                              objectid %in% c("1337872") & leftcity == "Unincorporated" ~ "St. Helena",
                              TRUE ~ leftcity)
         ) %>% 
  filter(class %in% keep_road_classes, # just the main roads
         !label %in% exclude_labels, # exclude duplicate highway centerlines with direction indicated in label 
         !objectid %in% exclude_roads$objectid # exclude manually IDed duplicate centerlines, loops, and other problem segments
         ) %>% 
  filter(surface %in% c("Paved", "PAVED")) %>%  # excluding unpaved and private roads; these mostly show up in the Local class
  filter(pubpriv != "Private") %>% 
  mutate(label.city = paste(label, leftcity, sep = "_"))

napa_sonoma_rds_filtered %>% 
  ggplot() +
  geom_bar(aes(x = class))

napa_sonoma_rds_filtered %>%  
  st_write(., "data/napa_sonoma_rds_filtered2.shp", append = FALSE)



zz <- full_join(read_xlsx(here("data/known_split_rds.xlsx"), sheet = "Sheet1") %>% 
                  select(label_city, notes, checked1),
                read_xlsx(here("data/known_split_rds.xlsx"), sheet = "Sheet2") %>% 
                  select(label_city) %>% 
                  mutate(round2 = TRUE)) %>% 
  full_join(read.csv(here("data/known_split_rds_check.csv"))) %>% 
  full_join(read_xlsx(here("data/known_split_rds.xlsx"), sheet = "Sheet3") %>% 
              select(label_city) %>% 
              mutate(round3 = TRUE)) %>% 
  full_join(read.csv(here("data/known_split_rds_check2.csv"))) %>% 
  full_join(read_xlsx(here("data/known_split_rds.xlsx"), sheet = "Sheet4") %>% 
              select(label_city) %>% 
              mutate(round4 = TRUE))

zz %>% 
  filter(checked2 == "fixed" & round3 == TRUE) %>%
  mutate(checked3 = "") %>% 
  write.csv(here("data/known_split_rds_check2.csv"), row.names = FALSE)


##############  below here probably not needed as of 10/8/24 #############
#Projection transformation
# the data come projected in NAD83 / California zone 2 (ftUS) https://epsg.io/2226
st_crs(napa_sonoma_rds_filtered)
# convert to utm zone 10N
napa_sonoma_rds_utm = st_transform(napa_sonoma_rds_filtered, crs = 26910)


napa_sonoma_rds_utm %>%
  data.frame() %>% 
  count(county, class) %>% 
  pivot_wider(id_cols = class, names_from = county, values_from = n) %>% 
  view()


saveRDS(napa_sonoma_rds_utm, here("data/napa_sonoma_rds_utm"))

napa_sonoma_rds_utm %>%  
  dplyr::select(-objectid) %>% 
  st_write(., "data/napa_sonoma_rds_utm.shp", append = FALSE)


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




napa_sonoma_rds_utm <- readRDS(here("data/napa_sonoma_rds_utm"))





# create a shapefile

  st_write(napa_sonoma_rds_utm, "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_sonoma_main_roads/napa_sonoma_rds_utm_filtered.shp", append = FALSE)

road %>% 
  filter(is.na(label)) %>% 
  st_write("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/napa_sonoma_main_roads/na_roads.shp", append = FALSE)
  
# ggplot(sonoma_rds_utm) + geom_sf(aes(color = Class))

dropped_roads <- sonoma_rds %>% 
  filter(!Class %in% keep_road_classes) %>% 
  st_as_sf(x = .,
           coords = c("Long", "Lat"),
           crs = "+proj=longlat +datum=WGS84")

#Projection transformation
dropped_rds_utm = st_transform(dropped_roads, crs = 26910) %>% 
  select(OBJECTID, StreetID, geometry, Class, Label)



ggplot() +
  geom_sf(data = filter(study_area_counties, NAME %in% c("Sonoma", "Napa"))) +
  geom_sf(data = filter(napa_sonoma_rds_utm, !class %in% keep_road_classes), color = "gray") +
  geom_sf(data = filter(napa_sonoma_rds_utm, class %in% keep_road_classes), aes(color = class), linewidth = 1) +
  coord_sf(datum = st_crs(26910)) +
  theme_bw()

