# Analysis of GPS collar data for mountain lions in the north San Francisco Bay, CA



## Description of data objects created and saved  
* data/analysis_table
  + created by prep_data1_gps_data.R
  + basic table of GPS data with 
  + fields = c(animal.id, collar.id, datetime.local, date.local, latitude, longitude, altitude, dop, sex, collar, age)
  
* data/puma_steps
  + created by prep_data1_gps_data.R
  + analysis_table converted to "step" format with start and end coordinates
  + fields = c(animal.id, collar.id, datetime.local, easting, northing, datetime.local.end, easting.end, northing.end, step.dur, step.dist, step.id)

