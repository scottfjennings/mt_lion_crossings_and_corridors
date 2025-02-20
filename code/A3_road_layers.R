

library(tidyverse)
library(here)
library(readxl)
library(sf)

options(scipen = 999)
source(here("code/utilities.R"))
source(here("code/helper_data.R"))

# Downloaded road layers from:  
#  https://gis-sonomacounty.hub.arcgis.com/datasets/sonomacounty::streets-2/about  
# and  
#  https://gisdata.countyofnapa.org/datasets/napacounty::road-centerlines-1/about  


# road class info here: https://www.fhwa.dot.gov/planning/processes/statewide/related/highway_functional_classifications/section01.cfm#Toc329359418
# and here: https://safety.fhwa.dot.gov/speedmgt/data_facts/docs/rd_func_class_1_42.pdf

# ArcGIS steps use this project:
# C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/general_data_sources/roads/roads.aprx


# 1. load and prep sonoma county road layer ----

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


# 2. combine the 2 counties ----
napa_sonoma_rds <- bind_rows(napa_rds, sonoma_rds) %>% 
  mutate(class = ifelse(str_detect(class, "Arterial"), "Arterial", class),
         class = ifelse(str_detect(class, "Collector"), "Collector", class),
         class = ifelse(str_detect(class, "Ramp|Interchange"), "Ramp/Interchange", class))

napa_sonoma_rds %>% 
  st_transform(crs = 26910) %>% 
  saveRDS("data/napa_sonoma_rds")

# These files have multiple segments of varying length for each road. These segments needed to be combined so that the roads could then be split into equal length segments for the analysis. After several attempts to combine in R, I switched to ArcGIS Pro where the combining/cleaning process was much more efficient. 


# Export to shapefile, then open this in ArcGIS to manually view road objects and ID objects to filter out and other cleaning needed (e.g. fix road names, public/private designation).   

napa_sonoma_rds %>%  
  st_write(., "data/shapefiles/napa_sonoma_rds.shp", append = FALSE)

# 3. OPTIONAL. initial manual check road objects in ArcGIS ----
# generally don't need to do this a second time
# Import combined road shapefile into ArcGIS Pro for more data cleaning. This mostly involved manually panning around the whole study area and seeing where there were problem objects.  
# + trimmed roundabouts to be only a single path connecting the roads  
# + exclude duplicate highway centerlines with direction indicated in label - Merge Divided Roads tool in ArcGIS Pro would do this too   
# + exclude duplicate centerlines, loops, and other problem segments  

# I copied the attributes for these road objects into data/manual_edit_data/exclude_roads2.csv so that I had a record of which segments were filtered out and so I could do the filtering programatically in R. An original version of this, data/manual_edit_data/exclude_roads.csv, also has all the Ct, Cir, and Way objects filtered out (Courts, Circles, and Ways), but I subsequently (on 10/8/24) decided this was too strict a filter. on Jan 6, 2025 exclude_roads.csv was moved to data/manual_edit_data/temp_archive20250106 for eventual deletion

# 4. filtering bad road objects ----
# back in R, bulk remove the objects that were IDed as bad in ArcGIS and here in R

# 4.1. ArcGIS IDed objects
# data/manual_edit_data/exclude_roads2.csv has only the manually selected objects - as of 10/8/24 this is the one to use

exclude_roads <- read.csv(here("data/manual_edit_data/exclude_roads2.csv"))

napa_sonoma_rds_filtered <- napa_sonoma_rds %>%
  mutate(label = str_to_title(label),
         label = str_replace(label, "State Highway", "Hwy")
  ) %>% 
  filter(class %in% keep_road_classes, # just the main roads
         !label %in% exclude_labels, # exclude duplicate highway centerlines with direction indicated in label 
         !objectid %in% exclude_roads$objectid # exclude manually IDed duplicate centerlines, loops, and other problem segments
  )

# 4.2. R IDed objects
# there were also several objects that seemed mis-classified as private when they seemed to be public (i.e. in a long stretch of segments classified as Public there would be 1 segment classified as Private)
#### ----  START OPTIONAL ----####
# this helper object helps ID roads that had segments classified as Public and Private, 
pub_priv <- napa_sonoma_rds %>% 
  data.frame() %>% 
  select(-geometry) %>% 
  distinct(label, leftcity, pubpriv) %>% 
  mutate(pubpriv = ifelse(pubpriv == "Private", pubpriv, "Public")) %>% 
  group_by(label, leftcity) %>% 
  mutate(pub.n.priv = any(pubpriv == "Public") & any(pubpriv == "Private")) %>% 
  arrange(label, leftcity)
#### ----  END OPTIONAL ----####

# and based on viewing those in table and mapped format, and additional checking in ArcGIS, I came up with the following corrections  
napa_sonoma_rds_filtered <- napa_sonoma_rds_filtered %>%
  # fixing public vs private
  mutate(pubpriv = case_when(objectid == "3654" & label == "Pleasant Hill Rd" ~ "Public",
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
                             objectid %in% c("1335631") & label == "Factory Stores Dr" ~ "Public",
                             objectid %in% c("12558") & label == "Lovall Valley Rd" ~ "Public",
                             objectid %in% c("41175") & label == "Moraga Dr" ~ "Public",
                             objectid %in% c("46077", "10843") & label == "Robinson Rd" ~ "Public",
                             objectid %in% c("1337443", "1337444") & label == "Theresa Ave" ~ "Public",
                             objectid %in% c("20970", "25339") & label == "A St" ~ "Public",
                             objectid %in% c("52670") & label == "Eucalyptus Ave" ~ "Public",
                             objectid %in% c("1335561") & label == "Factory Stores Dr" ~ "Public",
                             objectid %in% c("1338129", "50014") & label == "Lovall Valley Rd" ~ "Public",
                             objectid %in% c("41683") & label == "River Rd" ~ "Public",
                             TRUE ~ pubpriv)) %>% 
         # fixing surface to paved based on viewing aerial imagry in ArcGIS
         mutate(surface = case_when(objectid %in% c("1335217", "1339630") & label == "Mount Veeder Rd" ~ "Paved",
                             objectid %in% c("1337482") & label == "4th Ave" ~ "Paved",
                             objectid %in% c("1332266") & label == "Soda Canyon Rd" ~ "Paved",
                             objectid %in% c("1340521", "1332959") & label == "Washington St" ~ "Paved",
                             TRUE ~ surface)) %>% 
         # standardizing road names
         mutate(label = case_when(objectid %in% c("1335392", "1341767", "1341768") & label == "State Highway 37" ~ "Hwy 37",
                           objectid %in% c("41622", "17501") ~ "Todd Rd",
                           objectid %in% c("54017") & label == "Rohnert Park Expy E" ~ "Rohnert Park Expy",
                           objectid %in% c("55073") ~ "W Dry Creek Rd",
                           objectid %in% c("39256") ~ "Frankel Ln",
                           objectid %in% c("51240") ~ "Golden Ridge Ave",
                           objectid %in% c("6414", "6416", "6418", "6426", "6427") ~ "Hwy 116", # from Main St in Guerneville
                           objectid %in% c("41683") ~ "Hwy 116", # from River Rd in Guerneville
                           objectid %in% c("11518", "48508", "11519", "54528", "11452", "11455") ~ "Hwy 116", # from Front St in Forestville
                           label == "Main St" & leftcity == "St Helena" ~ "Hwy 29",
                           label == "Lincoln Ave" & leftcity == "Calistoga" ~ "Hwy 29",
                           objectid %in% c("1341879") & label == "Syar Way" ~ "Kaiser Rd", # small segment of roundabout
                           objectid %in% c("46844") ~ "Meadow Dr",
                           objectid %in% c("46923") & label == "Morgan St" ~ "Benton St", # connecting Benton and splitting Morgan. 2 other parts of roundabout are dropped with exclude_roads2.csv
                           objectid %in% c("55041") & label == "Decanter Cir" ~ "Oak Way", # connecting Benton and splitting 	Decanter Cir. 2 other parts of roundabout are dropped with exclude_roads2.csv
                           objectid %in% c("49584") & label == "Parkcreek Dr" ~ "Burt St", 
                           objectid %in% c("49585") & label == "Burt St" ~ "Parkcreek Dr", 
                           objectid %in% c("46853") & leftcity == "Sonoma" ~ "Flint Ct", 
                           objectid %in% c("1338099") & leftcity == "Napa" ~ "W Imola Ave", 
                           objectid %in% c("32402") & label == "Casa Grande Rd" ~ "Ely Blvd S", 
                           objectid %in% c("8095", "8096", "8099", "8100", "8101", "8103", "8104", "8105", "8107", "8233", "8235", "21027", "21155", "28527", "49189", "49392", "49421", "52342", "52345", "54030", "54031", "54032", "54602") & label == "Sebastopol Rd" ~ "Hwy 12", # continuous with Hwy 12 from Santa Rosa
                           label == "Lakeville Hwy" ~ "Hwy 116",
                           label %in% c("Hwy 116 N", "Hwy 116 S") ~ "Hwy 116",
                           label %in% c("Hwy 12 E", "Hwy 12 W") ~ "Hwy 12",
                           label %in% c("Hwy 101 N", "Hwy 101 S") ~ "Hwy 101",
                           TRUE ~ label)) %>% 
         # fixing city names
         mutate(leftcity = case_when(leftcity == "Nap" ~ "Napa",
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
                              objectid %in% c("1334653") & leftcity == "Nap" ~ "Napa",
                              objectid %in% c("1338126") & leftcity == "American Canyon" ~ "Napa",
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
                              objectid %in% c("1334074", "1341751", "1341752", "1341753", "1341683") & leftcity == "Unicorporated" ~ "Napa",
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
                              objectid %in% c("1338129") & leftcity == "Out Of County" ~ "Sonoma",
                              objectid %in% c("1336810", "1338408", "1341684") & leftcity == "Unicorporated" ~ "Napa",
                              objectid %in% c("6528", "6527", "6526", "6521", "6520", "6519", "6522", "6522", "6524", "6525", "6523") & leftcity == "Occidental" ~ "Jenner",
                              objectid %in% c("1330406", "1338015") & leftcity == "Unincorporated" ~ "Napa",
                              TRUE ~ leftcity)
         ) %>% 
  filter(surface %in% c("Paved", "PAVED")) %>%  # excluding unpaved and private roads; these mostly show up in the Local class
  filter(pubpriv != "Private") %>% 
  mutate(label.city = paste(label, leftcity, sep = "_"))


# save as RDS and shapefile

napa_sonoma_rds_filtered %>%  
  st_write(., "data/shapefiles/napa_sonoma_rds_filtered.shp", append = FALSE)

saveRDS(napa_sonoma_rds_filtered, here("data/napa_sonoma_rds_filtered"))


# 5. combine objects into a single object for each named road in each city ----
# Import that back to ArcGIS Pro, use the Dissolve tool to combine segments into a single object for each named road. 
# Settings for the Dissolve tool:
#  Dissolve Field = label and leftcity
#  Statistics Fields = objectid with Statistic Type = Concatenate  
#  Concatenation Separator = _
#  Create multipart features = unchecked
#  Unsplit lines = checked

# this only combines objects that are touching

#  Then used the Frequency tool to count how many times each named road showed up in each city. Anywhere this number was >1, I investigated further to see if there were further problem segments that needed to be added to data/exclude_roads2.csv or fixed with code in 4.2 above. I focused only on multipart roads here where the total length was >1 km since I am likely ultimately going to filter out all roads <1km. This step still left multiple objects per named road in each city; these are mostly legitimate, where the actual road has a break, but there are still several unresolved broken roads or lollipops that should be connected.


# I iterated through steps 3, 4, and 5 multiple times until I had a relatively tidy merged road layer.


# then export a shapefile using the Feature Class To Shapefile tool

# and read back in to R for some further checking
napa_sonoma_rds_filtered_Dissolve <- st_read("data/shapefiles/napa_sonoma_rds_fil_Dissolve.shp")

# adding unique identifier for multiple dissolved roads with the same name in the same city
napa_sonoma_rds_filtered_Dissolve <- napa_sonoma_rds_filtered_Dissolve %>%
  group_by(label, leftcity) %>% 
  mutate(city.road.num = row_number()) %>% 
  ungroup() %>% 
  mutate(label = ifelse(is.na(label), paste(leftcity, "road", city.road.num, sep = "_"), label)) %>% 
  arrange(label, leftcity)


# review cases where there are still multiple objects with the same road name in the same city
# This is an iterative process, created by checking objects in multiple_roads_to_check.shp in arcgis 

# manual_edit_data/problem_split_roads.csv has roads that are still split but seem like they shouldn't be. these are the objects that I may want to find a way to join.
problem_split_roads <- read.csv(here("data/manual_edit_data/problem_split_roads.csv"))

# manual_edit_data/ok_split_roads.csv and manual_edit_data/ok_split_roads2.csv have roads that are legitimately split
ok_split_roads <- read.csv(here("data/manual_edit_data/ok_split_roads.csv"))
ok_split_roads2 <- read.csv(here("data/manual_edit_data/ok_split_roads2.csv"))

multiple_roads_to_check <- napa_sonoma_rds_filtered_Dissolve %>% 
  group_by(label, leftcity) %>% 
  mutate(num.roads = max(city.road.num))%>% 
  ungroup() %>% 
  filter(num.roads > 1, !str_detect(label, "_road_")) %>% 
  filter(!CONCATENAT %in% problem_split_roads$CONCATENAT) %>% 
  filter(!CONCATENAT %in% ok_split_roads$CONCATENAT) %>% 
  filter(!CONCATENAT %in% ok_split_roads2$CONCATE)

# as of 1/27/25 there are just 1 record in this filtered multiple_roads_to_check:
# label            leftcity CONCATENAT
# Lovall Valley Rd Sonoma   12373_12374_12376_12377_18788_49911
# this one is ok and should be in manual_edit_data/ok_split_roads2.csv but the other object is in manual_edit_data/problem_split_roads.csv

st_write(multiple_roads_to_check, "data/shapefiles/multiple_roads_to_check.shp", append = FALSE)


# with all multiple roads resolved, the road layer should be clean. 
# data/shapefiles/napa_sonoma_rds_fil_Dissolve.shp is the object to move forward for subsequent analysis steps
# removing fields CONCATENAT and Shape_Leng, resaving with a meaningful name

final_cleaned_road_layer <- napa_sonoma_rds_filtered_Dissolve %>% 
  select(-CONCATENAT, -Shape_Leng)

st_write(final_cleaned_road_layer, here("data/shapefiles/final_cleaned_road_layer.shp"), append = FALSE)

saveRDS(final_cleaned_road_layer, here("data/final_cleaned_road_layer"))

