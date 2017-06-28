# Project Blind Haste
# Michael Schulte-Mecklenbeck, Emanuel de Bellis
# generate map for measurement locations
# blank slate
  rm(list = ls())

# set working directory %UPDATE%
  setwd("BlindHaste/R")

# add packages
#install.packages('devtools')
  library(devtools)
#devtools::install_github("dkahle/ggmap")
  library(ggmap)
# load location data
  locations <- read.csv(file="../data/locations/longlat.csv", sep=';', stringsAsFactors = FALSE)

# simple overlay of measure points
  qmap('ZurichMap', zoom=12, color="bw", maptype="roadmap") + 
    geom_point(data=locations, aes(x = lon, y = lat, colour = "red")) + 
    theme(legend.position="none")
# export tiff
  ggsave(file="S1_Fig.png",height=9,width=12,dpi=300)
