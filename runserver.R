# install.packages("plumber")
library("plumber")
r<-plumb("~/Documents/Projects/fantasy-hockey/standings.R")
r$run(port=8000)