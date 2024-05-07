library(plotKML)
library(ggplot2)
library(ggmap)
library(maps)
library(sf)
library(OpenStreetMap)

telem <- as.data.frame(telemtest)

points = st_as_sf(telem, coords = c("lat", "lon"), crs = 4326)

plot(st_geometry(points), pch=16, col="navy")

