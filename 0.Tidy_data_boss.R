
###
# Project: Geographe Bay
# Data:    BOSS
# Task:    Cleaning boss habitat data and adding spatial covariates
# author:  Kingsley Griffin
# date:    Aug 2021
# modified: Anita Giraldo 13/08/2021 # just added lines for directories and to save xy
##

# library(reshape2)
library(dplyr)
# library(googlesheets4)
library(raster)
library(sp)

# Clear memory ----
rm(list=ls())

### Set directories ----
w.dir<- dirname(rstudioapi::getActiveDocumentContext()$path)
d.dir <- paste(w.dir, "data", sep ='/')
dt.dir <- paste(w.dir, "data/tidy", sep ='/')
dr.dir <- paste(w.dir, "data/raw", sep ='/')

setwd(w.dir)

# get metadata from survey tracker googlesheet
# bossd <- as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1ZfW-XJKP0BmY2UXPNquTxnO5-iHnG9Kw3UuJbALCcrs/edit#gid=814068592",
#                                   sheet = "2021-03_Geographe_BOSS"))
# saveRDS(bossd, 'data/2103_Geographe_BOSS_metadata.rds')

# read in data
bosmet <- readRDS('data/2103_Geographe_BOSS_metadata.rds')                      # boss metadata
habdat <- read.table('data/raw/2021-03_Geographe_BOSS_Habitat_Dot Point Measurements.txt', 
                     skip = 5, sep = "\t")                                      # habitat annotations
downdat <- read.table('data/raw/2021-03_Geographe_BOSS_Downwards_Habitat_Dot Point Measurements.txt', 
                      skip = 5, sep = "\t")                                     # downwards habitat annotations

# clean all and merge to combine with metadata
bosmet <- bosmet[, colnames(bosmet) %in% c("Date", "Time.bottom", "Latitude",
                                           "Longitude", "Site", "Sample", 
                                           "Location", "Status", "Depth",
                                           "Type")]                             # only cols of interest
summary(habdat)
habdat <- habdat[ , c(1, 4, 5, 18:21, 23, 26)]                                  # omit bare columns
colnames(habdat) <- c("Filename", "Image row", "Image col", "Broad", 
                      "Morphology", "Type", "FOV", "CODE", "Radius")            # fix colnames
habdat$Site      <- gsub(".jpg", "", habdat$Filename)
downdat <- downdat[ , c(1, 4, 5, 18:21, 23, 26)]                                # omit bare columns
colnames(downdat) <- c("Filename", "Image row", "Image col", "Broad", 
                       "Morphology", "Type", "FOV", "CODE", "Radius")           # fix colnames
downdat$Site      <- gsub(".jpg", "", downdat$Filename)

allhab <- merge(bosmet, habdat,by.x = "Sample", by.y = "Site")
allhab$pa <- c(1)
head(allhab)

alldwn <- merge(bosmet, downdat,by.x = "Sample", by.y = "Site")
alldwn$pa <- c(1)
head(alldwn)

# save
#write.csv(alldwn, paste(dr.dir, "BOSS_downwards.xy.csv", sep ='/')) 

# convert to wide percent cover data
allhabw <- dcast(allhab, Sample + Latitude + Longitude + Depth ~ Broad, 
                 value.var = "pa", fun.aggregate = sum, drop = TRUE)
allhabw$totpts <- rowSums(allhabw[, 6:14]) - (allhabw$Unknown + allhabw$Var.5) 
head(allhabw)

allhabw_pc <- allhabw
allhabw_pc[6:14] <- sapply(allhabw_pc[, 6:14], 
                           FUN = function(x){
                             round((x/allhabw_pc$totpts)*100, 2)})              # calculate percent cover

alldwnw <- dcast(alldwn, Sample + Latitude + Longitude + Depth ~ Broad, 
                 value.var = "pa", fun.aggregate = sum, drop = TRUE)
alldwnw$totpts <- rowSums(alldwnw[, 5:12]) - (alldwnw$Unscorable + alldwnw$Var.5) 
alldwnw_pc <- alldwnw
alldwnw_pc[6:11] <- sapply(alldwnw_pc[, 6:11], 
                           FUN = function(x){
                             round((x/alldwnw_pc$totpts)*100, 2)})              # calculate percent cover

# bring in bathy raster and check alignment
b2 <- raster('spatial_data/GBmultib_lidar_CMR.tif')
allhab_sp <- SpatialPointsDataFrame(coords = cbind(allhabw_pc$Longitude, 
                                                   allhabw_pc$Latitude), 
                                    data = allhabw_pc)
alldwn_sp <- SpatialPointsDataFrame(coords = cbind(alldwnw_pc$Longitude, 
                                                   alldwnw_pc$Latitude), 
                                    data = alldwnw_pc)
plot(b2)
plot(allhab_sp, add = T)

# calculate bathy derivatives
slope4  <- terrain(b2, "slope", unit = "degrees", neighbors = 4)
slope8  <- terrain(b2, "slope", unit = "degrees", neighbors = 8)
aspect4 <- terrain(b2, "aspect", unit = "degrees", neighbors = 4)
aspect8 <- terrain(b2, "aspect", unit = "degrees", neighbors = 8)
tpi     <- terrain(b2, "TPI")
tri     <- terrain(b2, "TRI")
rough   <- terrain(b2, "roughness")
flowdir <- terrain(b2, "flowdir")

preds <- stack(b2, slope4, slope8, aspect4, aspect8, 
               tpi, tri, roughness, flowdir)
names(preds)[c(1:4, 8)] <- c("slope4", "slope8", "aspect4", 
                             "aspect8", "roughness")
namesp <- names(preds)

# extract predictor data
allhab <- cbind(allhabw_pc, extract(preds, allhab_sp))
head(allhab)
summary(allhab)

alldwn <- cbind(alldwnw_pc, extract(preds, alldwn_sp))
head(alldwn)
summary(alldwn)

# exclude sites outside of bathy area
allhab <- na.omit(allhab)
alldwn <- na.omit(alldwn)

# # if we want presence/absence at the image scale
# allhab[, 6:14] <- sapply(allhab[, 6:14], 
#                            FUN = function(x){ifelse(x > 1, 1, 0)})              # convert to binomial
# alldwn[, 6:11] <- sapply(alldwn[, 6:11], 
#                          FUN = function(x){ifelse(x > 1, 1, 0)})              # convert to binomial
# 

saveRDS(allhab, "data/tidy/2103_Geographe_BOSS_habitat.rds")
saveRDS(alldwn, "data/tidy/2103_Geographe_BOSS_downwardshabitat.rds")








