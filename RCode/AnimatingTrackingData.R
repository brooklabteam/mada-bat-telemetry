# Animating tracking data

rm(list=ls())

# set wd
setwd("/Users/sophiahorigan/Documents/GitHub/mada-bat-telemetry/RCode")

# load data
points.df <- read.csv('metapop_telem_data.csv', header = TRUE)

# First time only, you will need to install any missing packages
# install.packages("missingpackagename")
library(sp)
library(gganimate)
library(ggmap)
library(dplyr)
library(magrittr)
library(gifski)

# Make sure points are in order (in case they weren't before)
points.df <- points.df %>%
  arrange(individual.local.identifier, timestamp) %>% # arrange by animal, and ascending date
  filter(!is.na(location.long), !is.na(location.lat)) # remove NA's

# Make spatial
points.sp <- points.df
coordinates(points.sp) <- c("location.long", "location.lat")

# Set coordinate system & projection
crs_wgs84 <- CRS(SRS_string = "EPSG:4326") # WGS 84 has EPSG code 4326
class(crs_wgs84)
slot(points.sp, "proj4string") <- crs_wgs84

# Make back into dataframe (but include date for our animation)
# ggmap and gganimate use dataframes for plotting
points.geo <- as.data.frame(points.sp@coords)
points.geo$id <- points.sp@data$individual.local.identifier # add individual identifier
points.geo$date <- as.POSIXct(points.sp@data$timestamp, format = "%m/%d/%y %H:%M") # Important! the variable for revealing in the animation must be
# either integer, numberic, POSIXct, Date, difftime, or orhms. Here I made sure it is a date.

# Download tiles using ggmap
library(ggmap)
# Google tiles (requires a key first)

register_google(key = "AIzaSyCwa0OOmg7nRXgOrBZgBxgvmdn1h_bIO7g")
mybasemap <- get_map(location = c(lon = mean(points.sp@coords[,1]), 
                                  lat = mean(points.sp@coords[,2])), 
                     source = "google", 
                     zoom = 9,
                     maptype = 'satellite')


# Plot static imagery + points + paths
mymap.paths <-ggmap(mybasemap) + 
  geom_point(data = points.geo, aes(x = location.long, y = location.lat, colour = as.character(id))) +
  geom_path(data = points.geo, aes(x = location.long, y = location.lat, colour = as.character(id), group = as.character(id))) +
  theme(legend.position = c(0.15, 0.80)) +
  labs(x = "Longitude", y = "Latitude") +
  #scale_colour_manual(name = "Bat ID",
                      # Adjust the number of values for how many animals you have
   #                   values = c("red", "blue", "purple", "green", "orange", "black", "pink"), 
                      # Enough breaks for every animal in the data set
    #                  breaks = unique(as.character(points.geo$id))) + 

# Static plot
mymap.paths

# Update plot to animate. I used 'transition_reveal' so that the path builds from the beginning to the end. Use 'transition_states' to show only one point at a time
path.animate.plot <- mymap.paths +
  transition_reveal(along = date) +
  labs(title = 'Date: {frame_along}')  # Add a label on top to say what date each frame is

# To display the animation, use `animate`.
# When using your own data, adjust frames per second (fps) to be as fast or slow as you like.
# Be patient at this stage! It will eventually render in your plotting window
animate(path.animate.plot,
        fps = 5, renderer = gifski_renderer())

# Save as gif. This may be a large file, depending on your data set! 
anim_save(path.animate.plot,
          file = "animatedpaths.gif")
