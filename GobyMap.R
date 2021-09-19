sites <- read.csv("GobySites.csv")
sites$Longitude <- -sites$Longitude

library(dplyr)
library(latticeExtra)
library(mapview)
library(raster)
library(rasterVis)
library(RColorBrewer)
library(reshape2)
library(rgeos)
library(RgoogleMaps)
library(sf)
library(sp)
library(leaflet)
library(htmlwidgets)
library(googledrive)
library(googlesheets4)
library(ggplot2)
library(ggmap)
qmplot(Longitude, Latitude, data=sites, colour = I('red'), size = I(3), darken = .3)

library(tibble)
library(sf)
epsg <- 4326
df <- data_frame(sites)
l <- lapply(unique(df$Site), function(x){
  df <- df[df$Site == x,]
  df  <-  st_as_sf(df, coords = c('Longitude', 'Latitude'), crs = epsg)
})
elk <- data_frame(sites[8,])
l_elk <- lapply(unique(elk$Site), function(x){
  elk <- elk[elk$Site == x,]
  elk  <-  st_as_sf(elk, coords = c('Longitude', 'Latitude'), crs = epsg)
})
mapview(l)

m <- mapview(l,
             method = "ngb", 
             na.color = rgb(0, 0, 255, max = 255, alpha = 0), #get rid of color
             query.type = "click", #CLICK ON A PLACE TO KNOW WHICH CELL YOU ARE IN
             trim = TRUE,
             legend = FALSE, #no need for legend
             map.types =  "Esri.WorldImagery",#,#"OpenStreetMap",#"Esri.WorldImagery",#, # CHANGE TO "Esri.WorldImagery" IF YOU WANT
             alpha.regions = 0,
             lwd=2,
             color="pink") #get rid of color
elk <- mapview(l_elk,
             method = "ngb", 
             na.color = rgb(0, 0, 255, max = 255, alpha = 0), #get rid of color
             query.type = "click", #CLICK ON A PLACE TO KNOW WHICH CELL YOU ARE IN
             trim = TRUE,
             legend = FALSE, #no need for legend
             map.types =  "Esri.WorldImagery",#,#"OpenStreetMap",#"Esri.WorldImagery",#, # CHANGE TO "Esri.WorldImagery" IF YOU WANT
             alpha.regions = 0,
             lwd=2,
             color="red") 
