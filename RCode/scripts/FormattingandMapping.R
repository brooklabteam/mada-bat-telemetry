############################################
# Formatting spatial data & basic mapping in ggmap
#############################################

rm(list=ls())

# set wd
setwd("/Users/sophiahorigan/Documents/GitHub/mada-bat-telemetry/RCode")

# load data
points.df <- read.csv('telem_111423.csv', header = TRUE)

