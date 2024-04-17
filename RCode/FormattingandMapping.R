############################################
# Formatting spatial data & basic mapping in ggmap
#############################################

rm(list=ls())

# set wd
setwd("/Users/sophiahorigan/Documents/GitHub/mada-bat-telemetry/RCode")

# load data
points.df <- read.csv('telem_111423.csv', header = TRUE)

test.df$day <- NA
for(i in 1:length(test.df$Time)){
  if(test.df[i,]$Time > "07:00:00 AM" & test.df[i,]$Time < "01:00:00 PM"){
    test.df$day = 1
  }
  else{
    test.df$day = 0
  }
}

# To format for spatial analyses, we need to remove NA values from the coordinate columns
points.df <- points.df[!is.na(points.df$location.lat) & !is.na(points.df$location.long),]

# The dataframe should only have 3 columns (x, y, and an identifier) 
# for calculating trajectories and home ranges.
points.sp <- points.df[, c("individual.local.identifier", "location.long", "location.lat")]

# Turn into a spatial points dataframe (class: SpatialPointsDataFrame)
library(sp)
coordinates(points.sp) <- c("location.long", "location.lat")

# Examine the structure of our SpatialPointsDataFrame
str(points.sp)

# Set coordinate system & projection
crs_wgs84 <- CRS(SRS_string = "EPSG:4326") # WGS 84 has EPSG code 4326
class(crs_wgs84)
slot(points.sp, "proj4string") <- crs_wgs84

plot(points.sp, col = as.factor(points.sp@data$individual.local.identifier), pch = 16)

# Transform the point object (points.sp)
points.sp.geo <- spTransform(points.sp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "))

# Plot study site using ggmap
library(ggmap)

## Satellite imagery
register_google(key = "AIzaSyCwa0OOmg7nRXgOrBZgBxgvmdn1h_bIO7g")
## The location argument can take a vector with latitude and longitude, or a character string. 
mybasemap <- get_map(location = c(lon = mean(points.sp@coords[,1]) , 
                                  lat = mean(points.sp@coords[,2])), 
                     source = "google", zoom = 9, maptype = 'satellite')
ggmap(mybasemap)

# Turn the spatial data frame of points into just a dataframe for plotting in ggmap
points.geo <- data.frame(id = points.sp.geo@data$individual.local.identifier, # add individual identifier
                         points.sp.geo@coords) # Add coordinates

# Plot imagery + points + paths
mymap.paths <-ggmap(mybasemap) + 
  geom_point(data = points.geo, aes(x = coords.x1, y = coords.x2, colour = as.factor(id))) +
  geom_path(data = points.geo, aes(x = coords.x1, y = coords.x2, colour = as.factor(id))) +
  theme(legend.position = c(0.15, 0.80)) +
  labs(x = "Longitude", y = "Latitude") 
  #scale_colour_manual(name = "Animal number",
   #                   values = c("black", "red", "green", "yellow", "purple", "blue", "pink", "orange"))

mymap.paths + labs(linetype='Bat ID')
mymap.paths
# if 'warning message, removed rows containing missing values', adjust zoom above