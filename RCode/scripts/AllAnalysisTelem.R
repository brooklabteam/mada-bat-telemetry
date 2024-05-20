
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
library(amt)

#--------------------------------------------------------------------------------------------
## SET WD
homedir <- "/Users/sophiahorigan/Documents/GitHub/mada-bat-telemetry/RCode"
setwd(homedir)

#--------------------------------------------------------------------------------------------
## LOAD DATA
points.df <- read.csv(paste0(homedir,"/input/Movebank-AllTags-AllSensorTypes-06072024.csv"), header = TRUE)

#--------------------------------------------------------------------------------------------
## CLEAN DATA

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


#--------------------------------------------------------------------------------------------
## DAY VS NIGHT
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
# could larger daytime movement indicate a more disturbed roost??????!
ggplot(data = daytime.df, aes(location.long, location.lat)) + 
  geom_point(aes(colour = daytime), shape = as.factor(daytime.df$individual.local.identifier)) +
  facet_wrap(site ~ ., scales = 'free')

#--------------------------------------------------------------------------------------------
## SEASON
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
ggplot(data = season.df, aes(location.long, location.lat)) + 
  geom_point(aes(colour = season), shape = as.factor(season.df$individual.local.identifier)) +
  facet_wrap(site ~ ., scales = 'free')

#--------------------------------------------------------------------------------------------
## MONTH
points.df %>%
  count(month(timestamp)) # both species

points.df %>%
  group_by(individual.taxon.canonical.name)  %>%
  count(month(timestamp)) # by species

#--------------------------------------------------------------------------------------------
## TAG ACTIVITY

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
  geom_point(data = points.geo, aes(x = coords.x1, y = coords.x2, colour = as.factor(id)), size = 1) +
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
## KERNAL DENSITIES

# calculate kernal densities
kernel.ref <- kernelUD(points.sp, h = "href")
image(kernel.ref)

# get volume
kernel.vud <- getvolumeUD(kernel.ref)

# get contour
levels <- c(25, 50, 75, 95, 100)
list <- vector(mode = "list", length = length(kernel.vud))

for (i in 1:length(kernel.vud)){
  list[[i]] <- as.image.SpatialGridDataFrame(kernel.vud[[i]])
}

# plot
par(mfrow = c(6,4))

for (i in 1:length(kernel.vud)){
  plot(kernel.vud[[i]])
  contour(list[[i]], add = TRUE, levels = levels)
}

# calculate home range area
# grid size issues
homerange <- kernel.area(kernel.ref, percent = seq(50, 95, by=5))
plot(homerange)


#--------------------------------------------------------------------------------------------
## HOME RANGE OVERLAP

# remove duplicate points
test <- points.df %>% distinct(location.lat, location.lat, .keep_all = TRUE)

# Make an amt `track` object with our sample data set
points_track <- test %>%
  # Remove NA x and y rows
  filter(!is.na(location.lat), !is.na(location.long)) %>%
  # Make track with coordinates, date-time, id
  make_track(location.long, location.lat, timestamp, id = individual.local.identifier,
             # Make sure to specify coordinate reference system (CRS)
             crs = "EPSG:4326") %>%
  # Use nest() to allow us to deal with multiple animals (5 in sample set)
  # Each animal's track is stored in a tibble (table) nested within the data column
  nest(data = -"id") %>%
  arrange(id)

# Examine object. It is a tibble with a row for each individual. 
#The value of 'data' contains the nested track_xyt object for each individual
points_track

# Examine one individual's track
head(points_track$data[1])

# Add MCP list-column to track using `map`
points_track <- points_track %>% 
  mutate(mcp = map(data, function(x) 
    # levels are for which proportions (1.0 = 100%)
    x %>% hr_mcp(levels = c(1.0)))) 

# Each id's MCP is stored in the list column "mcp"
# Check by plotting the first one
plot(points_track$mcp[[1]])

# Calculate overlap between T002 (row 2) and T004 (row 4)
# labels are from ID in track object 
# overlap is fraction of overlap between two home ranges
hr_overlap(points_track$mcp[[2]], 
           points_track$mcp[[4]], 
           labels = points_track$id[c(2,4)],
           type = "hr")

# Calculate overlap between all
points_hr_overlap <- hr_overlap(points_track$mcp,
                                labels = points_track$id, 
                                which = "all", 
                                # alternative which = "consecutive",
                                # "one_to_all"
                                conditional = FALSE)

head(points_hr_overlap)


# Make a track of the data that is not nested.
points_track_nonest <- points.df %>%
  dplyr::select(location.lat, location.long, timestamp, individual.local.identifier) %>%
  make_track(location.lat, location.long, timestamp, id = individual.local.identifier)

points_track_nonest <- points.df %>%
  # Remove NA x and y rows
  make_track(location.long, location.lat, timestamp, id = individual.local.identifier,
             # Make sure to specify coordinate reference system (CRS)
             crs = "EPSG:4326")


# Make a base raster for whole study. Without this common raster, 
# each Kernel Density Estimator will likely use different pixel sizes and 
# output of hr_overlap will be blank.
base_trast <- make_trast(points_track_nonest,
                         res = 50)

# Calculate the kernel density estimator for two turtles at 95%
hr_245091 <- hr_kde(points_track$data[2][[1]], trast = base_trast, levels = 0.95)
hr_245093 <- hr_kde(points_track$data[4][[1]], trast = base_trast, levels = 0.95)

hr_overlap(hr_245091, hr_245093, type = "vi", conditional = FALSE)



#--------------------------------------------------------------------------------------------
## FIGURE 3 - MOST LIKELY DISTANCE MOVED ??

# by month
# by season
# annual






#--------------------------------------------------------------------------------------------
## FUCKING AROUND
## Satellite imagery
register_google(key = "AIzaSyCwa0OOmg7nRXgOrBZgBxgvmdn1h_bIO7g")
## The location argument can take a vector with latitude and longitude, or a character string. 
mybasemap <- get_map(location = c(lon = mean(points.sp@coords[,1]) , 
                                  lat = mean(points.sp@coords[,2])), 
                     source = "google", zoom = 6, maptype = 'satellite')
ggmap(mybasemap, extent="normal") + 
  geom_density2d(data = points.df, aes(x=location.long, y=location.lat, group=individual.local.identifier, colour=individual.local.identifier))

ggmap(mybasemap, extent="normal") + 
  geom_density2d(data = points.df, aes(x=location.long, y=location.lat, group=site, colour=site))









