

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


analysis_pumas <- c("P1", "P11", "P12", "P13", "P16", "P2", "P24", "P25", "P26", "P31", "P33", "P34", "P39", "P4", "P41", "P44", "P5", "P6", "P9")


# from Harvey, J. A., Q. Martins, and J. F. Benson. 2024. Impact of Sublethal Injuries on Mountain Lion Predation and Spatial Ecology. Canadian Wildlife Biology & Management 13.
puma_injuries <- data.frame(puma = c("P1", "P4"),
                            injury.date = c("2022-01-01", "2022-09-01"))
