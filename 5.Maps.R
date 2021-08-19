### Load libraries ----

library(ggplot2)
library(ggthemes)
library(cowplot)
library(randomForest)
library(sp)
library(rgdal)
library(raster)
library(caTools)
library(reshape2)
library(tidyr)
library(car)
library(lattice)
library(dplyr)
library(raster)
library(rasterVis)
library(zoo)
library(sf)
library(fields)
library(ROCR)
library(caret)
library(VSURF)
library(geoR)
library(gstat)
#library(elsa)
#install.packages("corrplot")
library(corrplot)
library(broman)
library(mapview)
library(tmap)
library(mapdata)
library(leaflet)

# Clear memory ----
rm(list=ls())

### Set directories ----
w.dir<- dirname(rstudioapi::getActiveDocumentContext()$path)
d.dir <- paste(w.dir, "data", sep='/')
s.dir <- paste(w.dir, "spatial_data", sep='/')
p.dir <- paste(w.dir, "plots", sep='/')
o.dir <- paste(w.dir, "outputs", sep='/')

fine.fwd <- raster(paste(o.dir, "GBpred-Fine-BOSS-fwd.tif", sep ='/'))
fine.dwd <- raster(paste(o.dir, "GBpred-Fine-BOSS.tif", sep ='/'))

coarse.fwd <- raster(paste(o.dir, "GBpred-coarese-BOSS-fwd.tif", sep ='/'))
coarse.fwd <- raster(paste(o.dir, "GBpred-coarese-BOSS-dwd.tif", sep ='/'))



## Plot FINE DOWNWARD ----

pred <- raster(paste(o.dir, "GBpred-Fine-BOSS.tif", sep ='/'))

raster::plot(pred)
#e <- drawExtent()
e <- raster::extent( 115.0376, 115.6031  , -33.61737 , -33.33799 )
testx <- raster::crop(pred, e)
raster::plot(pred)

# basic plot using lattice --
# https://pjbartlein.github.io/REarthSysSci/rasterVis01.html
# https://stat.ethz.ch/pipermail/r-sig-geo/2013-March/017893.html

#pick colors --
sg <- brocolors("crayons")["Jungle Green"] # "#78dbe2"
sg <- brocolors("crayons")["Forest Green"] # "#78dbe2"
sg <- brocolors("crayons")["Fern"] # "#78dbe2"
alg <-  brocolors("crayons")["Raw Umber"] # "#1dacd6" 
sand <-  brocolors("crayons")["Unmellow Yellow"] # "#f75394"

# read gb cmr
#gb <- readOGR(dsn="C:/Users/00093391/Dropbox/UWA/Research Associate/PowAnalysis_for1sParksMeeting/Desktop/shapefiles")
gb <- readOGR(paste(s.dir, "GeoBay.shp", sep ='/'))
plot(gb)


# fix class levels for plotting --
xx <-levels(pred)[[1]]
xx
class(xx)
xx <- xx[-1,]
xx$ID <- xx$ID[xx$ID != "0",]
xx$ID <- c('3','2', '1')
xx$category <- c('Unconsolidated',  'Seagrasses', 'Algae' )
levels(pred) <- xx
pred
#xx <-levels(pred)[[1]]

## Plot using tmap ----
# https://geocompr.robinlovelace.net/adv-map.html


map1 <- tm_shape(pred) + tm_raster(palette=pal1)

map2 <- map1 + tm_shape(gb)  + tm_borders(col ='black', lwd = 2) +
  tm_compass(type = "arrow", position = c(0.84, 0.25), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  tm_layout(legend.text.size = 1,
            legend.position = c(0.76, 0.1),
            legend.title.color = 'white') +
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 


map2
class(map2)

## save map ----

tmap_save(map2, paste(p.dir, "GB-Fine-Bruvs.tiff", sep='/'))