# Home range estimates with kernels

rm(list=ls())

# set wd
setwd("/Users/sophiahorigan/Documents/GitHub/mada-bat-telemetry/RCode")

# load data
points.df <- read.csv('metapop_telem_data.csv', header = TRUE)

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


library(adehabitatHR)
kernel.ref <- kernelUD(points.sp, h = "href")  # href = the reference bandwidth
image(kernel.ref) # plot
kernel.ref[[1]]@h # The smoothing factor is stored for each animal in the "h" slot

kernel.lscv <- kernelUD(points.sp, h = "LSCV") # Least square cross validation smoothing 
image(kernel.lscv) # plot

plotLSCV(kernel.lscv) # Look for a dip

points.kernel.poly.95 <- getverticeshr(kernel.ref, percent = 95) 
points.kernel.poly.50 <- getverticeshr(kernel.ref, percent = 50) 
print(points.kernel.poly)  # returns the area of each polygon

plot(points.kernel.poly.50, col = as.factor(points.kernel.poly.50@data$id))
plot(points.kernel.poly.95)
plot(points.kernel.poly.)
plot(points.sp, add = TRUE, col = as.factor(points.kernel.poly.95@data$id), pch = 21)


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





