#################################################

############ Prepare data for RF ################

### Load libraries --

library(ggplot2)
library(cowplot)
library(sp)
library(rgdal)
library(raster)
library(dplyr)

# Clear memory ----
rm(list=ls())

### Set directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
dt.dir <- paste(w.dir, "data/tidy", sep ='/')
dr.dir <- paste(w.dir, "data/raw", sep ='/')
s.dir <- paste(w.dir, "spatial_data", sep ='/')


#### DOMINANT DATA ----

### Load data ----

# Coord data --
xy <- read.csv(paste(dr.dir, "BOSS_downwards.xy.csv", sep='/')) %>% 
  mutate_at(vars(Sample, Filename), list(as.factor)) %>%
  distinct(Filename, .keep_all = T) %>% # one set of coords per sample
  dplyr::select(c(Filename, Latitude, Longitude)) %>%
  rename(longitude = Longitude, latitude = Latitude) %>%
  glimpse()

# Habitat data --
df <- read.csv(paste(dr.dir, "dominant-BOSS-downwards_broad.percent.cover.csv", sep='/')) %>% 
  mutate_at(vars(Filename, row_max), list(as.factor)) %>%
  left_join(xy, by = 'Filename') %>%
  glimpse()

str(df) # 4 categories


# Bathy 250m all Geo Bay --
bgb <- raster(paste(s.dir, "GB_Bathy_250m.tif", sep='/'))
bgb
plot(bgb)

#bx <- raster(paste(s.dir, "GBmultib_lidarUTM_CMR.tif", sep='/'))
#bx # in UTM
#plot(bx)

# Reproject lidat and multibeam data
#b2 <- projectRaster(b, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#b2
#plot(b2)

# save
#writeRaster(b2, paste(s.dir, "GBmultib_lidar_CMR.tif", sep='/'))

# Bathy Lidar and Multibeam in latlong --
#b <- raster(paste(s.dir, "GBmultib_lidar_CMR.tif", sep='/'))
#butm <- raster(paste(s.dir, "bathy-for-Boss.tif", sep='/'))
b <- raster(paste(s.dir, "bathy-for-Boss.tif", sep='/'))
plot(b)
b

b <- projectRaster(b, crs = proj4string(bgb))
plot(b)

# Check data points --
dfs <- df
coordinates(dfs)  <- ~longitude+latitude
points(dfs, pch = 20, cex = 0.7)



# Filter data to use just where there is lidar and multib --
dnew <- raster::extract(b, dfs, sp=T)

h <- dnew # to keep consisten with the script

# str(dnew)
# 
# 
# dfnew <- as.data.frame(dnew)
# dfnew <- na.omit(dfnew)
# str(dfnew)
# 
# dfsnew <- dfnew
# coordinates(dfsnew) <- ~longitude+latitude
# points(dfsnew, pch = 20, col="red")
# h <- dfsnew

# save points --
#write.csv(dfnew, paste(d.dir, "GB_Bruvs_fine_bathy_habitat_presence_absence_broad.csv"))
#writeOGR(dfsnew, dsn= s.dir, layer= "GB_Bruvs_fine_bathy_habitat_presence_absence_broad", driver="ESRI Shapefile", overwrite_layer=TRUE)

## load shape file with habitat data ----
#h <- readOGR(paste(s.dir, "GB_Bruvs_fine_bathy_habitat_presence_absence_broad.shp", sep='/')) 


######################################

#### Calculate Bathy Derivatives ####

slope4 <- raster::terrain(b, "slope", unit="degrees", neighboors=4)
slope4@data@names
slope4@data@names <- "slope4"
slope8 <- raster::terrain(b, "slope", unit="degrees", neighboors=8)
slope8@data@names
slope8@data@names <- "slope8"
aspect4 <- raster::terrain(b, "aspect", unit="degrees", neighboors=4)
aspect4@data@names
aspect4@data@names <- "aspect4"
aspect8 <- raster::terrain(b, "aspect", unit="degrees", neighboors=8)
aspect8@data@names
aspect8@data@names <- "aspect8"
tpi <- raster::terrain(b, "TPI")
tri <- raster::terrain(b, "TRI")
roughness <- raster::terrain(b, "roughness")
flowdir <- raster::terrain(b, "flowdir")
b@data@names <- "depth"

predictors <- stack(b, slope4, slope8, aspect4, aspect8, tpi, tri, roughness, flowdir)
plot(predictors)

names(predictors)
namesp <- names(predictors)

# save
#writeRaster(predictors, paste(s.dir,"predictors_boss.tif", sep='/'), overwrite=T)
#write.csv(namesp, paste(s.dir, "namespredictors_boss.csv", sep='/'))

###  Extract predictor values for each observation ----
hp <- raster::extract(predictors, h, sp=T)
hp

hab_pred <- as.data.frame(hp)
head(hab_pred)
str(hab_pred)

names(hab_pred)

## remove unwanted columns ----
boss_dom <- hab_pred[,c(2, 11:20)]
names(boss_dom)


sp1 <- boss_dom
coordinates(sp1) <- ~longitude+latitude
plot(b)
plot(sp1, border="white", col="lightgrey", add=TRUE)
plot(sp1, col=rainbow(7), pch=20, fill=sp1$row_max, add=TRUE)

# save
#writeOGR(sp1, dsn= s.dir, layer= "GB_BOSS_fine_bathy_habitat_dominant_broad", driver="ESRI Shapefile", overwrite_layer=TRUE)
#write.csv(boss_dom, paste(dt.dir, "GB_BOSS_fine_bathy_habitat_dominant_broad.csv", sep='/'))

