

keep_road_classes = c("Collector" 
                      , "Freeway" 
                      , "Highway" 
                      , "Arterial"
                      , "Local"
                      , "Access Road"
                      , "Other"
                      #"Ramp/Interchange"
)


exclude_labels <- c("Hwy 101 N", "Hwy 12 E")

exclude_pumas <- c("P10"  # only collared 2 weeks
                   , "P15" # only collared 1 week
                   , "P17" # very little data so likely will break logistic regression models
                   , "P20" # only collared 1 week
                   , "P27" # daughter of P16, both died at the same time while P27 still dependent
                   , "P32" # moved to Hopland, Q thinks data after move are good, but that's outside our study area
)


hr_exclude_pumas <- c("P36", #funky locations, in the ocean
                      "P14", # many points outside Sonoma/Napa
                      "P37", # not totally range resident and 100% HR covers much of sonoma 
                      "P19" # many points outside Sonoma/Napa
)


p31_exclude_segments <- c("Hwy 1_16", "Hwy 1_17", "Hwy 1_18", "Hwy 1_19", "Hwy 1_20", "Hwy 1_21", "Hwy 1_22", "Hwy 1_23")