

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

exclude_pumas <- c("P10"  # only collared 2 weeks, 196 fixes
                   , "P15" # only collared 1 week, 30 fixes
                   , "P17" # only 36 fixes. very little data so likely will break logistic regression models
                   , "P20" # only collared 1 week, 183 fixes
                   , "P27" # daughter of P16, both died at the same time while P27 still dependent
                   , "P32" # moved to Hopland, Q thinks data after move are good, but that's outside our study area
                   , "P34" # only collared for 41 days
)


aLoCoH_exclude_lions <- c("P19" # dispersing; many points outside Sonoma/Napa
                          )

hr_exclude_pumas <- c("P36", #funky locations, in the ocean
                      "P14", # many points outside Sonoma/Napa
                      "P37", # not totally range resident and 100% HR covers much of sonoma 
                      "P19" # many points outside Sonoma/Napa
)


p31_exclude_segments <- c("Hwy 1_Jenner_16", "Hwy 1_Jenner_17", "Hwy 1_Jenner_18", "Hwy 1_Jenner_19", "Hwy 1_Jenner_20", "Hwy 1_Jenner_21", "Hwy 1_Jenner_22", "Hwy 1_Jenner_23")


analysis_pumas <- c("P1", "P11", "P12", "P13", "P16", "P2", "P24", "P25", "P26", "P31", "P33", "P34", "P39", "P4", "P41", "P44", "P5", "P6", "P9")

few_crossings_pumas <- c("P11", # 77
                         "P12", # 19
                         "P24", # 6
                         "P25", # 5
                         "P26", # 44
                         "P34", # 17
                         "P44" # 11
                         )

# from Harvey, J. A., Q. Martins, and J. F. Benson. 2024. Impact of Sublethal Injuries on Mountain Lion Predation and Spatial Ecology. Canadian Wildlife Biology & Management 13.
puma_injuries <- data.frame(puma = c("P1", "P4"),
                            injury.date = c("2022-01-01", "2022-09-01"))
