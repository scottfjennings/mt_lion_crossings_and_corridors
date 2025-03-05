# Analysis of GPS collar data for mountain lions in the north San Francisco Bay, CA

## Overall workflow - see annotation in each code file for more detail  

code files are grouped by letter:
A... generally data prep and management (but has some preliminary analysis components)
B... generally the main analysis (but has some additional data management)
C... generally output figures, tables, etc to report results and data summaries

Workflow should proceed through code fiel groups in alphabetical order, and within each group in numerical order

### A1_gps_data.R  
clean and prepare the mt lion GPS collar data  

### A2_fit_ctmm_homerange.R  
calculate a home range for each mt lion  
these home ranges are used at several points in subsequent data cleaning to filter habitat layers, etc to speed computing times
  
### A3_road_layers.R  
combine road layers for Sonoma and Napa counties and initial basic cleaning  
this step involves substantial work in ArcGIS, see script for detailed notes on that



    
### prep_data4_equal_length_road_segments.R  
split the combined 

### prep_data5_clean_bridges.R  
  
### analysis1_naive_road_crossing.R
### analysis10_crossing_autocorr.R
### analysis2_crossing_step_clusters.R
### analysis3_fit_BBMM.R
### analysis4_probabilistic_crossings.R
### analysis5_segment_crossing_densities.R
### analysis6_segment_habitat.R
### analysis7_segment_road_chars.R
### analysis8_find_best_hab_scale.R
### analysis9_analyze_crossings.R
### output1_explore_naive_crossings.docx
### output1_explore_naive_crossings.Rmd
### output1_explore_naive_crossings_files
### output2_basic_data_summary.R
### output3_basic_crossing_density.html
### output3_basic_crossing_density.Rmd
### output4_data_vis_maps.R
### output5_summarize_depredations.R


## Other code files  
### road_cliping_broken_dreams.R
### utilities.R"
### helper_data.R
  
  
  
  
  
  
  
  
  
  

## Description of data objects created and saved  

throughout these objects, step.id or  is generally the main "key" field and is a concatenation of animal.id, collar.id, and a sequential number for each step 

### objects in data/
* analysis_table
  + created by prep_data1_gps_data.R
  + basic table of GPS data with 
  + fields = c(animal.id, collar.id, datetime.local, date.local, latitude, longitude, altitude, dop, sex, collar, age)
  %>% %>% %>% %>% %>% 
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
  
  
  
  


