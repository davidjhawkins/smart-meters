## try WPD open data from 
# https://github.com/wpd-data/smart-meters/blob/main/Lab1%20-%20Smart%20Meter%20Heatmap.ipynb
library(data.table)
library("rgdal")
library("maptools")
library(tmap)
library(leaflet)
library(tidyverse)
library(plotly)
library(shiny)
library(ggthemes)
library(leaflet)
library(sf)
library(htmltools)
library(fasttime)
library(jsonlite)

##
setwd("~/OneDrive/R analysis/NGRID/WPD_open_data/smart-meters")

#####  download WPD open data and convet to data table
url <- 'https://connecteddata.westernpower.co.uk/api/3/action/datastore_search?resource_id=5e531ff5-02ff-48d7-8512-6e603fb569b4&limit=300000'  
wpdd <- fromJSON(url, flatten = TRUE)
wpd1 <- wpdd[["result"]][["records"]]
wpd1 <- as.data.table(wpd1)

## calculate percentage smart meters
wpd1$pct_smart <- 100 * (wpd1$SMETS1 + wpd1$SMETS2) / (wpd1$SMETS1 + wpd1$SMETS2 + wpd1$NON_SMART)

#####  graph some data (prefer boxplots to pie charts)
boxplot(wpd1[,c(8:10)])
boxplot(wpd1[,c(8:10)], outline = F, col="green")

## load wpd geo-polygons
wpe <- st_read("https://connecteddata.westernpower.co.uk/dataset/d8e73301-e26a-4b5f-8eb5-e9a2f1b61338/resource/c0bc2a2d-deda-4dc8-8317-0ec8a24af677/download/east-midlands-primary.gpkg")
wpw <- st_read("https://connecteddata.westernpower.co.uk/dataset/d8e73301-e26a-4b5f-8eb5-e9a2f1b61338/resource/547db41e-fb44-423f-9c71-98266b666c38/download/south-wales-primary.gpkg")
wps <- st_read("https://connecteddata.westernpower.co.uk/dataset/d8e73301-e26a-4b5f-8eb5-e9a2f1b61338/resource/fe4c3f49-7de9-4ebf-bb10-413c50d59f83/download/south-west-primary.gpkg")
wpm <- st_read("https://connecteddata.westernpower.co.uk/dataset/d8e73301-e26a-4b5f-8eb5-e9a2f1b61338/resource/83c22640-3a54-415f-b253-e51bee3bd146/download/west-midlands-primary.gpkg")
wpdall <- rbind(wpe,wpw,wps,wpm)

## load WPD 33kV overhead line data (downloaded from WPD earlier)
wpd33 <- st_read("WPD33ohl.kml")

## add NGrid 400 and 275 overhead networks - open data
## note that these data use a different coordinate reference system
n400 <- st_read("ohl400.kml")
n275 <- st_read("ohl275.kml")

## use TMAP to plot the data
## NB: lot of geo-data, this takes some time
tmap_mode("view")
tm_shape(wpd33) +
  tm_lines(col = "green",
           alpha=0.2,
           lwd=1.5,
           id="NAME") +
  tm_shape(n400) +
  tm_lines(col = "blue",
           alpha=1,
           lwd=1.5,
           id="NAME") +
  tm_shape(n275) +
  tm_lines(col = "red",
           alpha=1,
           lwd=1.5,
           id="NAME") +
  tm_basemap(server = "Esri.WorldStreetMap")

 #####  just plot the WPD SW region data
tmap_mode("view")
tm_shape(wps) +
  tm_polygons(col = "blue",
              alpha=0.2,
              lwd=0.7,
              id="NAME") +
  tm_basemap(server = "Esri.WorldStreetMap")

### plot all 4x WPD regions plus National Grid overhead 
tm_shape(wps) +
  tm_polygons(col = "blue",
              alpha=0.2,
              lwd=0.7,
              id="NAME") +
  tm_shape(wpw)+
  tm_polygons(col="green",
              alpha=0.2,
              lwd=0.7,
              id="NAME") +
  tm_shape(wpe)+
  tm_polygons(col="orange",
              alpha=0.2,
              lwd=0.7,
              id="NAME") +
  tm_shape(wpm)+
  tm_polygons(col="purple",
              alpha=0.2,
              lwd=0.7,
              id="NAME") +
tm_shape(n400) +
  tm_lines(col = "blue",
           alpha=1,
           lwd=1.5,
           id="NAME") +
  tm_shape(n275) +
  tm_lines(col = "red",
           alpha=1,
           lwd=1.5,
           id="NAME") +
  tm_basemap(server = "Esri.WorldStreetMap")

## first need to 'align' the names so that the data sets can be merged
names(wpdall)
wn<- names(wpd1)
wn[7] <- "PRIM_NRID"
names(wpd1)  <- wn
wpdall <- left_join(wpdall, wpd1, by=c("PRIM_NRID"))
## calculate percentage smart meters in this new dataset
wpdall$pct_smart <- 100 * (wpdall$SMETS1 + wpdall$SMETS2) / (wpdall$SMETS1 + wpdall$SMETS2 + wpdall$NON_SMART)
### remove duplicate columns - there are other/easier ways to do this
wpdall$GSP_NRID <- NULL
wpdall$BSP_NRID <- NULL

## check it still plots
tmap_mode("view")
tm_shape(wpdall) +
  tm_polygons(col = "green",
              alpha=0.2,
              lwd=0.7,
              id="NAME") +
  tm_shape(n400) +
  tm_lines(col = "blue",
           alpha=1,
           lwd=1.5,
           id="NAME") +
  tm_shape(n275) +
  tm_lines(col = "red",
           alpha=1,
           lwd=1.5,
           id="NAME") +
  tm_basemap(server = "Esri.WorldStreetMap")
### all looks good, so far


#####  reduce the data burden - extract the Nat Grid geo-data where it overlaps with WPD's region
## align coord-ref-systems CRS
wpda_crs <- st_crs(wpdall)
wpd400 <- st_transform(n400, crs=wpda_crs)
wpd275 <- st_transform(n275, crs=wpda_crs)


# limit 275 and 400 to the WPD bounding box - NB: this is a rectangle
wpdabb <- st_bbox(wpdall)
bbx <- st_as_sfc(wpdabb)
wpd400 <- st_intersection(wpd400, bbx)
wpd275 <- st_intersection(wpd275, bbx)
## repeat the above TMAP plot to check limits
tm_shape(wpdall) +
  tm_polygons(col = "green",
              alpha=0.2,
              lwd=0.7,
              id="NAME") +
  tm_shape(wpd400) +
  tm_lines(col = "blue",
           alpha=1,
           lwd=2.5,
           id="NAME") +
  tm_shape(wpd275) +
  tm_lines(col = "red",
           alpha=1,
           lwd=2.5,
           id="NAME") +
  tm_shape(wpd33) +
  tm_lines(col = "green",
           alpha=1,
           lwd=2.5,
           id="NAME") +
  tm_basemap(server = "Esri.WorldStreetMap")

## try a closer overlap
wpd400 <- st_intersection(wpdall, wpd400)
wpd275 <- st_intersection(wpdall, wpd275)
wpd400 <- wpd400[,c(14:16)]
wpd275 <- wpd275[,c(14:16)]

## ...and plot (takes ~ 1 minute on my laptop)
## NB: 33kV 'goes underneath" higher voltages here
tmap_mode("view")
tm_shape(wpdall) +
  tm_polygons(col = "green",
              alpha=0.2,
              lwd=0.7,
              id="NAME") +
  tm_shape(wpd33) +
  tm_lines(col = "green",
           alpha=1,
           lwd=1.5,
           id="NAME") +
  tm_shape(wpd400) +
  tm_lines(col = "blue",
           alpha=1,
           lwd=2.5,
           id="NAME") +
  tm_shape(wpd275) +
  tm_lines(col = "red",
           alpha=1,
           lwd=2.5,
           id="NAME") +
  tm_basemap(server = "Esri.WorldStreetMap")

### END
###
