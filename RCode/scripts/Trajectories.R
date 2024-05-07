# Creating trajectories from telemetry data
rm(list=ls())

# set wd
setwd("/Users/sophiahorigan/Documents/GitHub/mada-bat-telemetry/RCode")

# load data
points.df <- read.csv('metapop_telem_data.csv', header = TRUE)

# Examine the structure of the data. Note presence of some NA's
str(points.df)

plot(points.df$Latitude~points.df$Longitude, 
     col = as.factor(points.df$Bat.ID.), 
     pch = 16)

# Load library
library(adehabitatLT)  
# subset to relevant data
points.df.sub <- points.df[, c("Bat.ID.", "Longitude", "Latitude", "Loc..date")]

# remove duplicate points (multiple pings of the same data)
library(dplyr)
points.df.sub <- distinct(points.df.sub)

# We need to make sure that date is correctly formatted, and that there is an ID column
points.df.ltraj <- as.ltraj(xy = points.df.sub[,c("Longitude", "Latitude")], 
                          date =  as.POSIXct(points.df.sub$Loc..date, format = '%m-%d-%Y %H:%M:%S'), 
                         id = points.df.sub$Bat.ID.)

plot(points.df.ltraj) # Plots each animal's points with a path connecting them

points.df.ltraj  # data.ltraj is a list

# Each element of the list is a dataframe for one individual
head(points.df.ltraj[[1]])  # The first six locations of the first animal

# Create a dataframe to hold all of the contents of bltu.paths with a column for id. 
# Put first element into the dataframe
total.path.df <- data.frame(points.df.ltraj[[1]], id = attr(points.df.ltraj[[1]], "id"))
# Use a 'for' loop to fill the larger dataframe with the rest of the trajectories.
for(i in 2:length(points.df.ltraj)) {
  total.path.df <- rbind(total.path.df, 
                         data.frame(points.df.ltraj[[i]], id = attr(points.df.ltraj[[i]], "id")))
}

# Calculate distance travelled per day and add it to the dataframe
total.path.df$distperday <- total.path.df$dist / (total.path.df$dt/60/60/24)

# Aggregate to show mean distance per day for each turtle
path.summary <- aggregate(distperday~id, data = total.path.df, FUN = mean)
path.summary$sd <- aggregate(distperday~id, data = total.path.df, FUN = sd)$distperday

# Look at summmary dataframe
path.summary

# Make a graph to visualize data using ggplot
library(ggplot2)
# Create limits used for error bars in graph
limits <- aes(ymax = path.summary$distperday + path.summary$sd, 
              ymin = path.summary$distperday - path.summary$sd)

# Make plot. Choose the dataframe (data) and aesthetics (aes; for the x and y)
path.plot <- ggplot(data = path.summary, aes(x = id, y = distperday, colour = id)) + 
  geom_point(size = 3) + # add points
  geom_errorbar(limits, width = 0.2) + # adds error bars
  labs(x = "Animal number", 
       y = "Mean distance travelled per day (m)" ) + # Axis labels
  theme_classic() + # Make plot black and white with no background grid
  theme(legend.position = "none")
path.plot # call plot
