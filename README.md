# Analysis of GPS collar data for mountain lions in the north San Francisco Bay, CA

## Overall workflow - see annotation in each code file for more detail  

### prep_data1_gps_data.R  
clean and prepare the mt lion GPS collar data  

### prep_data2_fit_ctmm_homerange.R  
calculate a home range for eachmt lion

### prep_data3_road_layers.R  
combine road layers for Sonoma and Napa counties and initial basic cleaning  

1. Downloaded road layers from:  
  + https://gis-sonomacounty.hub.arcgis.com/datasets/sonomacounty::streets-2/about  
  and  
  + https://gisdata.countyofnapa.org/datasets/napacounty::road-centerlines-1/about  
  
2. Export to shapefile. These files have multiple segments of varying length for each road. These segments needed to be combined so that the roads could then be split into equal length segments for the analysis. After several attempts to combine in R, I switched to ArcGIS Pro where the combining/cleaning process was much more efficient.   

3. Import combined road shapefile into ArcGIG Pro for more data cleaning. This mostly involved manually panning around the whole study area and seeing where there were problem objects.  
  + trimmed roundabouts to be only a single path connecting the roads  
  + exclude duplicate highway centerlines with direction indicated in label  
  + exclude duplicate centerlines, loops, and other problem segments  
  
I copied the attributes for these road objects into data/exclude_roads2.csv so that I had a record of which segments were filtered out and so I could do the filtering programatically in R. An original version of this, data/exclude_roads.csv, also has all the Ct, Cir, and Way objects filtered out (Courts, Circles, and Ways), but I subsequently decided this was too strict a filter.  

4. Still in ArcGIS Pro, use the Dissolve tool to combine segments into a single object for each named road. 
Settings for the Dissolve tool:
Dissolve Field = label
Statistics Fields = objectid and leftcity, both with Statistic Type = Concatenate; Concatenation Separator = _
Create multipart features = unchecked
Unsplit lines = checked

Then used the Frequency tool to count how many times each named road showed up in each city. Anywhere this number was >1, I investigated further to see if there were further problem segments that needed to be added to data/exclude_roads2.csv. I focused only on multip part roads here where the total length was >1 km since I am likely ultimately going to filter out all roads <1km.  


I iterated through steps 3 and 4 multiple times until I had a relatively tidy merged road layer.
  
    
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
  
  
  
  


