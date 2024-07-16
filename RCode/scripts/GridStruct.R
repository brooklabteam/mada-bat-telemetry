###########################
## GRID STRUCTURE
###########################

# Created by: Sophia Horigan
# Contact: shorigan@uchicago.edu
# Last updated: June 18, 2024

# This script works with the known Eidolon roosts in the Mangoro river valley
# from the Natural History of Madagascar to determine some key grid structure
# parameters

rm(list=ls())

library(lubridate)
library(sp)
library(ggmap)
library(adehabitatHR)
library(scales)
library(maptools)
library(ggplot2)
library(ggmap)
library(dplyr)
library(amt)
library(geosphere)

#--------------------------------------------------------------------------------------------
## SET WD
homedir <- "/Users/sophiahorigan/Documents/GitHub/Bat-disease-metapop/bat-disease-metapop/Models/WIP/"
setwd(homedir)

#--------------------------------------------------------------------------------------------
## LOAD DATA
eidolon_roosts.df <- read.csv(paste0(homedir,"Input/Eidolon_roosts.csv"), header = TRUE)

#--------------------------------------------------------------------------------------------
## FORMAT FOR SPATIAL ANALYSIS

# To format for spatial analyses, we need to remove NA values from the coordinate columns
points.df <- eidolon_roosts.df[!is.na(eidolon_roosts.df$location.lat) & !is.na(eidolon_roosts.df$location.long),]

# Subset to relevant columns only
points.sp <- points.df[, c("Roost.ID", "location.long", "location.lat")]

# Turn into a spatial points dataframe (class: SpatialPointsDataFrame)
coordinates(points.sp) <- c("location.long", "location.lat")

# Examine the structure of our SpatialPointsDataFrame
str(points.sp)

# Set coordinate system & projection
crs_wgs84 <- CRS(SRS_string = "EPSG:4326") # WGS 84 has EPSG code 4326
class(crs_wgs84)
slot(points.sp, "proj4string") <- crs_wgs84

#--------------------------------------------------------------------------------------------
## GENERATE BASIC PLOT

plot(points.sp, pch = 16)

writeSpatialShape(points.sp, "eidolon_roosts.shp")

# Transform the point object (points.sp)
points.sp.geo <- spTransform(points.sp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "))

## Satellite imagery
register_google(key = "AIzaSyCwa0OOmg7nRXgOrBZgBxgvmdn1h_bIO7g")
## The location argument can take a vector with latitude and longitude, or a character string. 
mybasemap <- get_map(location = c(lon = mean(points.sp@coords[,1]) , 
                                  lat = mean(points.sp@coords[,2])), 
                     source = "google", zoom = 5, maptype = 'satellite')
ggmap(mybasemap)

# Turn the spatial data frame of points into just a dataframe for plotting in ggmap
points.geo <- data.frame(id = points.sp.geo@data$Roost.ID, # add individual identifier
                         points.sp.geo@coords, # Add coordinates
                         Region = points.sp.geo@data$Province) # add region

# Plot imagery + points + paths
mymap.roosts <-ggmap(mybasemap) + 
  geom_point(data = points.geo, aes(x = coords.x1, y = coords.x2, colour = Region), size = 1.5) +
  theme(legend.position = c(0.15, 0.80)) +
  labs(x = "Longitude", y = "Latitude") 

mymap.roosts

#--------------------------------------------------------------------------------------------
## EXPORT 

#--------------------------------------------------------------------------------------------
## AVERAGE DISTANCE BETWEEN ROOSTS
points.xy <- points.df[, c("location.long", "location.lat")]

### ALL
point.mat <- matrix(c(points.xy$location.long, points.xy$location.lat), ncol = 2)
point.mat
geospatial_dist <- distm(point.mat, fun = distGeo)   
print(geospatial_dist)
mean(geospatial_dist)
# 434726.7

### BY REGION
# Antananarivo
antananarivo.xy <- points.df[points.df$Province == 'Antananarivo', c("location.long", "location.lat")]
antananarivo.mat <- matrix(c(antananarivo.xy$location.long, antananarivo.xy$location.lat), ncol = 2)
geospatial_dist_antananarivo <- distm(antananarivo.mat, fun = distGeo)   
print(geospatial_dist_antananarivo)
mean(geospatial_dist_antananarivo)
# 73920.83

# Antsiranana
antsiranana.xy <- points.df[points.df$Province == 'Antsiranana', c("location.long", "location.lat")]
antsiranana.mat <- matrix(c(antsiranana.xy$location.long,antsiranana.xy$location.lat), ncol = 2)
geospatial_dist_antsiranana <- distm(antsiranana.mat, fun = distGeo)   
print(geospatial_dist_antsiranana)
mean(geospatial_dist_antsiranana)
# 24098.78

# Fianarantsoa
fianarantsoa.xy <- points.df[points.df$Province == 'Fianarantsoa', c("location.long", "location.lat")]
fianarantsoa.mat <- matrix(c(fianarantsoa.xy$location.long, fianarantsoa.xy$location.lat), ncol = 2)
geospatial_dist_fianarantsoa <- distm(fianarantsoa.mat, fun = distGeo)   
print(geospatial_dist_fianarantsoa)
mean(geospatial_dist_fianarantsoa)
# 106956.3

# Mahajunga
mahajunga.xy <- points.df[points.df$Province == 'Mahajunga', c("location.long", "location.lat")]
mahajunga.mat <- matrix(c(mahajunga.xy$location.long, mahajunga.xy$location.lat), ncol = 2)
geospatial_dist_mahajunga <- distm(mahajunga.mat, fun = distGeo)   
print(geospatial_dist_mahajunga)
mean(geospatial_dist_mahajunga)
# 160128.7

# Toamasina
toamasina.xy <- points.df[points.df$Province == 'Toamasina', c("location.long", "location.lat")]
toamasina.mat <- matrix(c(toamasina.xy$location.long, toamasina.xy$location.lat), ncol = 2)
geospatial_dist_toamasina <- distm(toamasina.mat, fun = distGeo)   
print(geospatial_dist_toamasina)
mean(geospatial_dist_toamasina)
# 6587.811

# Toliara
toliara.xy <- points.df[points.df$Province == 'Toliara', c("location.long", "location.lat")]
toliara.mat <- matrix(c(toliara.xy$location.long, toliara.xy$location.lat), ncol = 2)
geospatial_dist_toliara <- distm(toliara.mat, fun = distGeo)   
print(geospatial_dist_toliara)
mean(geospatial_dist_toliara)
# 187963.7

# average between all provinces
all.prov <- c(mean(geospatial_dist_antananarivo), mean(geospatial_dist_antsiranana), mean(geospatial_dist_fianarantsoa), mean(geospatial_dist_mahajunga), mean(geospatial_dist_toamasina), mean(geospatial_dist_toliara))
print(mean(all.prov))
# 93276.01




