

keep_road_classes = c("Collector", 
                      "Freeway", 
                      "Highway", 
                      "Arterial"#,
                      #"Ramp/Interchange"
)


exclude_labels <- c("Hwy 101 N", "Hwy 12 E")




hr_exclude_pumas <- c("P36", #funky locations, in the ocean
                      "P14", # many points outside Sonoma/Napa
                      "P37", # not totally range resident and 100% HR covers much of sonoma 
                      "P19" # many points outside Sonoma/Napa
)