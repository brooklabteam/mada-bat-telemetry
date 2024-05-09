
#################################
## ALL ANALYSIS TELEM 
################################
# Created by: Sophia Horigan
# Contact: shorigan@uchicago.edu
# Last updated: May 9, 2024

# This script takes in MoveBank telemetry downloads, cleans and preps, then generates figures
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

#--------------------------------------------------------------------------------------------
## SET WD
homedir <- "/Users/shorigan/Documents/GitHub/mada-bat-telemetry/RCode"
setwd(homedir)

#--------------------------------------------------------------------------------------------
## LOAD DATA
points.df <- read.csv(paste0(homedir,"/input/Movebank-AllTags-AllSensorTypes-06072024.csv"), header = TRUE)

#--------------------------------------------------------------------------------------------
## BASIC STATS

# cleanup
# convert timestamp column 
points.df$timestamp <- ymd_hms(points.df$timestamp)
# change time zone to Madagascar
points.df$timestamp <- with_tz(points.df$timestamp, "Africa/Addis_Ababa")
# add site column
points.df <- points.df %>% 
  mutate(site = case_when(grepl("MAN", individual.local.identifier) ~ 'Ambositra - P',
                          grepl("ANA", individual.local.identifier) ~ 'Analambotaka - P',
                          grepl("TSI", individual.local.identifier) ~ 'Marotsipohy - P',
                          grepl("MARO", individual.local.identifier) ~ 'Marovitsika - P',
                          grepl("HAR", individual.local.identifier) ~ 'Nosy Hara - P',
                          grepl("KEL", individual.local.identifier) ~ 'Angavokely - E',
                          grepl("LOR", individual.local.identifier) ~ 'Ambositra - E',
                          grepl("NAT", individual.local.identifier) ~ 'Mangroves - P',
                          grepl("VHL", individual.local.identifier) ~ 'Vahialava - P',
                          grepl("KEL", individual.local.identifier) ~ 'Angavokely - E',
                          grepl("WAY", individual.local.identifier) ~ 'Ankarana - E'))

table(points.df$individual.local.identifier)
# drop any that have less than 5 data points (right now just 1)
points.df <- points.df[!(points.df$individual.local.identifier %in% "LOR002"),]


# DAY VS NIGHT
dusk_hour = 18 # made up 
dawn_hour = 5

points.df <- points.df %>%
                mutate(daytime = ifelse(hour(timestamp) < dusk_hour & hour(timestamp) > dawn_hour, "day", "night"))
points.df %>% 
  group_by(individual.taxon.canonical.name)  %>%
    count(daytime)

daytime.df <- points.df[, c("individual.local.identifier", "site", "daytime", "location.long", "location.lat")]

# by individual
ggplot(data = daytime.df, aes(location.long, location.lat)) + 
  geom_point(aes(colour = daytime)) +
  facet_wrap(individual.local.identifier ~ ., scales = 'free')

# by site
ggplot(data = daytime.df, aes(location.long, location.lat)) + 
  geom_point(aes(colour = daytime)) +
  facet_wrap(site ~ ., scales = 'free')
  
# color by site shape by individual
ggplot(data = daytime.df, aes(location.long, location.lat)) + 
  geom_point(aes(colour = daytime), shape = as.factor(daytime.df$individual.local.identifier)) +
  facet_wrap(site ~ ., scales = 'free')


# SEASON
May = 5 # from Andrianiaina et al 2022
October = 10

points.df <- points.df %>%
                mutate(season = ifelse(month(timestamp) < May | month(timestamp) > October, "wet", "dry"))

points.df %>%  # both species
  count(season)

points.df %>%  # by species
  group_by(individual.taxon.canonical.name)  %>%
    count(season)

season.df <- points.df[, c("individual.local.identifier", "site", "season", "location.long", "location.lat")]

# by individual bat
ggplot(data = season.df, aes(location.long, location.lat)) + 
  geom_point(aes(colour=season)) +
  facet_wrap(individual.local.identifier ~ ., scales = 'free')

# group by site 
ggplot(data = season.df, aes(location.long, location.lat)) + 
  geom_point(aes(colour=season)) +
  facet_wrap(site ~ ., scales = 'free')

# group by site shape by individual
# could larger daytime movement indicate a more disturbed roost??????!
ggplot(data = season.df, aes(location.long, location.lat)) + 
  geom_point(aes(colour = season), shape = as.factor(season.df$individual.local.identifier)) +
  facet_wrap(site ~ ., scales = 'free')

# MONTH
points.df %>%
  count(month(timestamp)) # both species

points.df %>%
  group_by(individual.taxon.canonical.name)  %>%
  count(month(timestamp)) # by species

# TAG ACTIVITY
# make df with just tag id, start and end of deployment
# interval of tag activity
tag_interval <- start %--% end # NEEDS TO BE FIXED TO BE FOR EACH TAG ID
# duration of tag activity
tag_duration <- as.duration(points.df$tag_interval)
# period of tag activity
tag_period <- as.period(time.interval)

# ROOST VS COMMUTING VS FORAGING
points.df <- points.df %>%
  mutate(season = ifelse(month(timestamp) < May | month(timestamp) > October, "wet", "dry"))

# distance from previous point
points.df <- points.df %>%
  mutate(loc_diff = ifelse(month(timestamp) < May | month(timestamp) > October, "wet", "dry"))

# close to roost?
points.df <- points.df %>%
  mutate(loc_diff = ifelse(month(timestamp) < May | month(timestamp) > October, "wet", "dry"))

points.df <- points.df %>% 
                group_by(individual.local.identifier) %>%
                    mutate(activity = case_when((daytime == 'day') ~ 'roosting',
                                                (loc_diff < 5 & daytime == 'night') ~ 'foraging', # less than 5 m apart for foraging?
                                                TRUE ~ 'commuting')) # everything else

#--------------------------------------------------------------------------------------------
## FORMAT FOR SPATIAL ANALYSIS

# To format for spatial analyses, we need to remove NA values from the coordinate columns
points.df <- points.df[!is.na(points.df$location.lat) & !is.na(points.df$location.long),]

# The dataframe should only have 3 columns (x, y, and an identifier) for home ranges
points.sp <- points.df[, c("individual.local.identifier", "location.long", "location.lat")]

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

plot(points.sp, col = as.factor(points.sp@data$individual.local.identifier), pch = 16)

# Transform the point object (points.sp)
points.sp.geo <- spTransform(points.sp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "))

## Satellite imagery
register_google(key = "AIzaSyCwa0OOmg7nRXgOrBZgBxgvmdn1h_bIO7g")
## The location argument can take a vector with latitude and longitude, or a character string. 
mybasemap <- get_map(location = c(lon = mean(points.sp@coords[,1]) , 
                                  lat = mean(points.sp@coords[,2])), 
                     source = "google", zoom = 6, maptype = 'satellite')
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

#--------------------------------------------------------------------------------------------
## MCP HOME RANGES

# Calculate MCPs for each bat
points.df.mcp <- mcp(points.sp, percent = 95) # after Epstein et al

# Examine output
points.df.mcp

# Plot
plot(points.sp, col = as.factor(points.sp@data$individual.local.identifier), pch = 16)
plot(points.df.mcp, col = alpha(1:7, 0.5), add = TRUE)

# Calculate the MCP by including 50 to 100 percent of points
hrs <- mcp.area(points.sp, percent = seq(50, 100, by = 5))
hrs # examine dataframe

# Transform the point and MCP objects. 
points.spgeo <- spTransform(points.sp, CRS("+proj=longlat"))
points.mcpgeo <- spTransform(points.df.mcp, CRS("+proj=longlat"))

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

# by month
# by season
# annual


#--------------------------------------------------------------------------------------------
## FIGURE 2 - KERNAL DENSITIES

kernel.ref <- kernelUD(points.sp, h = "href")  # href = the reference bandwidth
image(kernel.ref) # plot
kernel.ref[[1]]@h # The smoothing factor is stored for each animal in the "h" slot

kernel.lscv <- kernelUD(points.sp, h = "LSCV") # Least square cross validation smoothing 
image(kernel.lscv) # plot

plotLSCV(kernel.lscv) # Look for a dip

points.kernel.poly.95 <- getverticeshr(kernel.ref, percent = 95) 
points.kernel.poly.50 <- getverticeshr(kernel.ref, percent = 50) 
print(points.kernel.poly.50)  # returns the area of each polygon

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

# by month
# by season
# annual

#--------------------------------------------------------------------------------------------
## FIGURE 3 - MOST LIKELY DISTANCE MOVED ??

# by month
# by season
# annual

#--------------------------------------------------------------------------------------------
## FIGURE 4 - PROB INTERMINGLING ??

# by month
# by season
# annual






