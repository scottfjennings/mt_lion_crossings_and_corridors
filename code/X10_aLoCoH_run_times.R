txt <- "
P34, 343, 25
P44, 850, 38
P6, 1275, 49
P12, 1541, 63
P41, 1725, 75
P5, 1922, 84
P37, 2411, 114
P19, 2435, 106
P14, 2459, 112
P2, 2466, 98
P9, 2990, 129
P24, 3598, 164
P30, 4039, 198
P11, 4171, 194
P26, 4191, 184
P25, 5178, 284
P36, 6117, 322
P39, 6199, 318
P31, 6420, 456
P33, 9023, 611
"


aLoCoH_process_times <- read.table(text = txt, sep = ",", 
                 col.names = c("ID","num.points","process.time"),
                 strip.white = TRUE)

ggplot(aLoCoH_process_times) +
  geom_line(aes(x = num.points, y = process.time))
