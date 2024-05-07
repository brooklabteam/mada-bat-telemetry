# Creating minimum convex polygons from telemetry data
rm(list=ls())

# set wd
setwd("/Users/sophiahorigan/Documents/GitHub/mada-bat-telemetry/RCode")

# load data
points.df <- read.csv('telem_111423.csv', header = TRUE)

# To format for spatial analyses, we need to remove NA values from the coordinate columns
points.df <- points.df[!is.na(points.df$Latitude) & !is.na(points.df$Longitude),]

# Only include three columns (id, x, and y coordinates) for making MCP's
points.sp <- points.df[, c("Bat.ID.", "Longitude", "Latitude")]

# Turn into a spatial points dataframe (class: SpatialPointsDataFrame)
library(sp)
coordinates(points.sp) <- c("Longitude", "Latitude")

# Set coordinate system & projection
crs_wgs84 <- CRS(SRS_string = "EPSG:4326") # WGS 84 has EPSG code 4326
class(crs_wgs84)
slot(points.sp, "proj4string") <- crs_wgs84

library(adehabitatHR) # Load library

# Calculate MCPs for each turtle
points.df.mcp <- mcp(points.sp, percent = 100)

# Examine output
points.df.mcp

# Plot
library(scales) # Helps make polygons partly transparent using the alpha argument below
plot(points.sp, col = as.factor(points.sp@data$individual.local.identifier), pch = 16)
plot(points.df.mcp, col = alpha(1:7, 0.5), add = TRUE)

# Calculate the MCP by including 50 to 100 percent of points
hrs <- mcp.area(points.sp, percent = seq(50, 100, by = 5))

hrs # examine dataframe

# Transform the point and MCP objects. 
points.spgeo <- spTransform(points.sp, CRS("+proj=longlat"))
points.mcpgeo <- spTransform(points.df.mcp, CRS("+proj=longlat"))

library(maptools)
library(ggplot2)
# Download tiles using ggmap
library(ggmap)
# Google tiles (requires a key first)

register_google(key = "AIzaSyCwa0OOmg7nRXgOrBZgBxgvmdn1h_bIO7g")
mybasemap <- get_map(location = c(lon = mean(points.spgeo@coords[,1]), 
                                  lat = mean(points.spgeo@coords[,2])), 
                     source = "google", 
                     zoom = 9,
                     maptype = 'satellite')


# Turn the spatial data frame of points into just a dataframe for plotting in ggmap
points.geo <- data.frame(points.spgeo@coords, 
                          id = points.spgeo@data$individual.local.identifier. )

mymap.hr <- ggmap(mybasemap) + 
  geom_polygon(data = fortify(points.mcpgeo),  
               # Polygon layer needs to be "fortified" to add geometry to the dataframe
               aes(long, lat, colour = id, fill = id),
               alpha = 0.3) + # alpha sets the transparency
  geom_point(data = points.geo, 
             aes(x = coords.x1, y = coords.x2, colour = as.character(id)))  +
  theme(legend.position = c(0.15, 0.80)) +
  labs(x = "Longitude", y = "Latitude") 
  #scale_fill_manual(name = "Bat ID", 
   #                 values = c("red", "blue", "purple", "green", "orange", "black", "pink", "yellow"),
    #                breaks = c("ANA139", "ANA140", "ANA154", "MARO113", "MARO114", "MARO115", "TSI092", "TSI093")) +
  #scale_colour_manual(name = "Bat ID", 
   #                   values = c("red", "blue", "purple", "green", "orange", "black", "pink", "yellow"),
    #                  breaks = c("ANA139", "ANA140", "ANA154", "MARO113", "MARO114", "MARO115", "TSI092", "TSI093")) 
mymap.hr
