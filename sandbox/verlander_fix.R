## address issue #3

data(verlander, package = "tigerstats")
View(verlander)
library(dplyr)
verlander <-
  verlander %>% 
  arrange(season, gamedate, pitches)
save(verlander, file = "data/verlander.rda")
