# install.packages("plumber")
library("plumber")
r<-plumb("~/Documents/Projects/fantasy-hockey/scrape.R")
r$run(port=8000)