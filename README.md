# Analysis of GPS collar data for mountain lions in the north San Francisco Bay, CA



## Description of data objects created and saved  

throughout these objects, step.id or  is generally the main "key" field and is a concatenation of animal.id, collar.id, and a sequential number for each step 

### objects in data/
* analysis_table
  + created by prep_data1_gps_data.R
  + basic table of GPS data with 
  + fields = c(animal.id, collar.id, datetime.local, date.local, latitude, longitude, altitude, dop, sex, collar, age)
  
* puma_steps
  + created by prep_data1_gps_data.R
  + analysis_table converted to "step" format with start and end coordinates
  + fields = c(animal.id, collar.id, datetime.local, easting, northing, datetime.local.end, easting.end, northing.end, step.dur, step.dist, step.id)
  
* napa_sonoma_rds_utm
  + created by prep_data2_road_layers.R
  + combined napa and sonoma county roads projected in UTM 10N
  + fields = c(objectid, label, speedlimit, lanes, surface, class, shape_leng, geometry, county, pubpriv)

  
* naive_crossings_napa_sonoma_2hr
  + created by analysis1_naive_road_crossing.R
  + road sections that were crossed crossed as a special feature
  + geometry field represents the road segment (not the mt lion step), projected in UTM 10N
  + fields = c(objectid, label, speedlimit, lanes, surface, class, shape_leng, county, pubpriv, geometry, step.id)  
  
* crossing_clusters2_gps  
  + created by analysis2_naive_road_crossing.R
  + clusters of GPS points around road crossing steps, including 2 steps (3 points) each side of the crossing step. in fix format (not step format) with just a single GPS fix contained in each record
  + flat data frame, no geometry field, but coords as UTM 10N
  + fields = c(crossing.step, step.id, easting, northing, datetime.local) step.id is the fix for the start of each step, and crossing.step is the step.id for the step that crossed the road (should be the middle step of the cluster). there should only be 1 record per step.id but 6 records per crossing.step 
    

* crossing_clusters3_gps  
 + same as data/crossing_clusters2_gps but with 3 steps (4 points) on each side of the crossing step for a total of 8 records per crossing step.  
 
### objects in model_objects/
 * all_clusters2_bbmm  
  + created by analysis3_fit_BBMM.R  
  + list with a Brownian Bridge Movement Model object for each cluster of GPS points around road crossing steps in data/crossing_clusters2_gps.   
  + each element of list is the output from BBMM::brownian.bridge  
  + fields = c(Brownian motion variance, x, y, probability)  
  
* all_bbmm_ud  
  + created by analysis4_probabilistic_crossings.R  
  + list with each element being a data frame representing a Utilization Distribution at a certain level for each element in model_objects/all_clusters2_bbmm  
  + each element is named for the corresponding crossing step  
  + has fields c(Brownian motion variance, x, y, probability, level.lab); level.lab is the probability level of the UD (currently using default 90%); x, y are coordinates projected in UTM 10N  
  + fields = c(Brownian motion variance, x, y, probability. level.lab)  
  
* all_ud_rast  
  + created by analysis4_probabilistic_crossings.R  
  + list with each element being a SpatVector [created by terra::rast() then terra::as.polygons] object for the corresponding Utilization Distribution from all_bbmm_ud  
  + each element is named for the corresponding crossing step  
  + projected in UTM 10N  
  
* all_step_boxes  
  + created by analysis4_probabilistic_crossings.R  
  + list with each element being a box centered at the crossing step, with the side parallel to the step as long as the step and the other side = 3x the step length.  
  + geometry is sfc_POLYGON    
  + each element is named for the corresponding crossing step  
  + projected in UTM 10N  
  
* all_ud_trim_to_step     
  + created by analysis4_probabilistic_crossings.R  
  + list with each element being a the intersection of the UD from all_ud_rast trimmed and the crossing step box from all_step_boxes  
  + geometry is sfc_POLYGON  
  + each element is named for the corresponding crossing step  
  + projected in UTM 10N  
  + names = c(X, geometry)
  
* all_bbmm_road_slices    
  + created by analysis4_probabilistic_crossings.R  
  + list with each element representing all the road segments in the trimmed UD from all_ud_trim_to_step. may contain multiple rows per named road, for each segment in the original road layers  
  + geometry is sfc_LINESTRING  
  + each element is named for the corresponding crossing step  
  + projected in UTM 10N   
  + names = c(objectid, label, speedlimit, lanes, surface, class, shape_leng, county, pubpriv, X, geometry)  

* all_bbmm_road_slices_continuous  
  + created by analysis4_probabilistic_crossings.R  
  + list with each element representing the road segments from all_bbmm_road_slices but with touching road segments combined into the same object  
  + geometry is sfc_LINESTRING  
  + each element is named for the corresponding crossing step  
  + projected in UTM 10N    
  + names = c(label, section, geometry); section is a helper field IDing each resulting road segment    

* bbmm_crossing_steps  
  + created by analysis4_probabilistic_crossings.R  
  + list with each element being the road segments from all_bbmm_road_slices_continuous that are crossed by the straight line step. should be a single row for each named road  
  + geometry is sfc_LINESTRING  
  + each element is named for the corresponding crossing step  
  + projected in UTM 10N  
  + names = c(label, section, geometry); section is a helper field IDing each resulting road segment    
  
  
  