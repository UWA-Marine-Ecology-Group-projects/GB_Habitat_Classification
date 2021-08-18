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
# w.dir <- "Y:/GB_Habitat_Classification"
# d.dir <- "Y:/GB_Habitat_Classification/data"
# s.dir <- "Y:/GB_Habitat_Classification/spatial_data"
# p.dir <- "Y:/GB_Habitat_Classification/plots"
# o.dir <- "Y:/GB_Habitat_Classification/outputs"


w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
d.dir <- paste(w.dir, "data", sep='/')
dt.dir <- paste(w.dir, "data/tidy", sep ='/')
dr.dir <- paste(w.dir, "data/raw", sep ='/')
s.dir <- paste(w.dir, "spatial_data", sep='/')
p.dir <- paste(w.dir, "plots", sep='/')
o.dir <- paste(w.dir, "outputs", sep='/')


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
df <- read.csv(paste(dr.dir, "dominant-BOSS-forwards_broad.percent.cover.csv", sep='/')) %>% 
  mutate_at(vars(Filename, row_max), list(as.factor)) %>%
  left_join(xy, by = 'Filename') %>%
  glimpse()


# Bathy 250m all Geo Bay --
b <- raster(paste(s.dir, "GB_Bathy_250m.tif", sep='/'))
b
plot(b)

#bx <- raster(paste(s.dir, "GBmultib_lidarUTM_CMR.tif", sep='/'))
#bx # in UTM
#plot(bx)

# Reproject lidat and multibeam data
#b2 <- projectRaster(b, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#b2
#plot(b2)

# save
#writeRaster(b2, paste(s.dir, "GBmultib_lidar_CMR.tif", sep='/'))



# Check data points --
dfs <- df
head(dfs)
any(is.na(dfs))
dfs <- na.omit(dfs)
coordinates(dfs)  <- ~longitude+latitude
points(dfs, pch = 20, cex = 0.8)

# Filter data to use just where there is lidar and multib --

dnew <- raster::extract(b, dfs, sp=T)
str(dnew)

dfnew <- as.data.frame(dnew)
dfnew <- na.omit(dfnew)
str(dfnew)

dfsnew <- dfnew
coordinates(dfsnew) <- ~longitude+latitude
points(dfsnew, pch = 20, col="red")
h <- dfsnew

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
#writeRaster(predictors, paste(s.dir,"predictors_coarse.tif", sep='/'), overwrite=T)
#write.csv(namesp, paste(s.dir, "namespredictors.csv", sep='/'))

###  Extract predictor values for each observation ----
hp <- raster::extract(predictors, h, sp=T)
hp

hab_pred <- as.data.frame(hp)
head(hab_pred)
str(hab_pred)

names(hab_pred)

## remove unwanted columns ----
BOSS_dom <- hab_pred[,c(2,14:16, 18:26)]
names(BOSS_dom)


sp1 <- BOSS_dom
coordinates(sp1) <- ~longitude+latitude
plot(b)
plot(sp1, border="white", col="lightgrey", add=TRUE)
plot(sp1, col=rainbow(7), pch=20, fill=sp1$row_max, add=TRUE)

# save
#writeOGR(sp1, dsn= s.dir, layer= "GB_BOSS-fwd_coarse_dominant_broad", driver="ESRI Shapefile", overwrite_layer=TRUE)
#write.csv(BOSS_dom, paste(d.dir, "tidy", "GB_BOSS-fwd_coarse_dominant_broad.csv", sep='/'))


###

###


# Filter per cell ----
# BOSS data has many images / pixel
# More frequent dominant class/pixel

#e <- drawExtent()
ext <- extent(115.029,115.6675,-33.65969,-33.29265)
b2 <- crop(b, ext)
plot(b2)

# to get  coordinates of each cell and the value --
d <- data.frame(coordinates(b2), count=b2[])

head(d)
str(d)



# https://gis.stackexchange.com/questions/279079/extracting-value-of-raster-with-coordinates-using-r

# extract cell ids with points on them --
result <- raster::extract(b2, sp1, cellnumbers=T, df=T)
head(result)
str(result)
result$cells <- as.factor(result$cells)

cellid <- as.data.frame(result %>%
                          group_by(cells, depth) %>%
                          # group_by(Class) %>%
                          summarize(n()))
str(cellid) # 196 obs
head(cellid)

cellid1 <- aggregate(depth~cells, data=result, mean)
str(cellid1) # 196 obs -> 196 cells with AUV data

cellx <- cellid %>% filter_at(vars(3), any_vars(. > 1))


# extract as spatial points cell ids --
resultsp <- raster::extract(b2, sp1, cellnumbers=T, coordinates = T ,sp =T)
resultsp
head(resultsp)
# get the coordinates --
rdf <- as.data.frame(resultsp)
str(rdf) 
rdf$cells <- as.factor(rdf$cells)

# remove 116.jpg file name  because more than one boss in that cell

rdf2 <- rdf %>%
  filter(Filename != '116.jpg') %>%
  dplyr::select(-c(cells, depth.1)) %>%
  glimpse()

rdfsp <- rdf2

coordinates(rdfsp) <- ~longitude+latitude


##
# save
#writeOGR(rdfsp, dsn= s.dir, layer= "GB_BOSS-fwd_coarse_dominant_broad", driver="ESRI Shapefile", overwrite_layer=TRUE)
#write.csv(rdf2, paste(d.dir, "tidy", "GB_BOSS-fwd_coarse_dominant_broad.csv", sep='/'))

