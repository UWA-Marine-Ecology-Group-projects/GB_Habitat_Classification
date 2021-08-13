#################################################

############ Prepare data for RF ################

### Load libraries --

library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyr)
library(tidyverse)


# Clear memory ----
rm(list=ls())

### Set directories ----
w.dir<- dirname(rstudioapi::getActiveDocumentContext()$path)
d.dir <- paste(w.dir, "data", sep ='/')
dt.dir <- paste(w.dir, "data/tidy", sep ='/')
dr.dir <- paste(w.dir, "data/raw", sep ='/')


#### 1. DOWNWARDS BOSS ----

### Load data ----

df <- read.csv(paste(dr.dir, "2021-03_Geographe_BOSS_Downwards_Habitat_Dot Point Measurements.csv", sep='/')) %>%
  select(-c(Type, Fine)) %>%
  mutate(Broad = na_if(Broad, "")) %>%
  mutate_at(vars(Filename, Broad), list(as.factor)) %>%
  mutate(count= 1) %>% 
  pivot_wider(names_from = Broad, values_from = count) %>%
  select(-c(Morphology, FieldOfView, Image.row, Image.col, CAAB_CODE, Radius..)) %>% 
  group_by(Filename) %>%
  replace(is.na(.), 0) %>%
  summarise_all(funs(sum)) %>%
  mutate(total.sum = rowSums(.[,2:(ncol(.))], na.rm = TRUE)) %>%
  group_by(Filename) %>%
  mutate_at(vars(2:(ncol(.))), funs(./total.sum*100)) %>%
  ungroup() %>%
  rename(not.scored = 'NA') %>%
  glimpse()

head(df)
str(df)


## get maximum class for each image ----
# https://stackoverflow.com/questions/17735859/for-each-row-return-the-column-name-of-the-largest-value

df2 <- df %>% 
  replace(is.na(.), 0) %>%
  select(-c(Filename, total.sum)) %>%
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(everything()))]) %>%
  cbind(Filename = df$Filename) %>%
  relocate(Filename) %>%
  glimpse()


# save --
write.csv(df2, paste(dr.dir, "dominant-BOSS-downwards_broad.percent.cover.csv", sep ='/'))


#### 2. FORWARDS BOSS ----

# Clear memory ----
rm(list=ls())

### Set directories ----
w.dir<- dirname(rstudioapi::getActiveDocumentContext()$path)
d.dir <- paste(w.dir, "data", sep ='/')
dt.dir <- paste(w.dir, "data/tidy", sep ='/')
dr.dir <- paste(w.dir, "data/raw", sep ='/')

### Load data ----

df <- read.csv(paste(dr.dir, "2021-03_Geographe_BOSS_Habitat_Dot Point Measurements.csv", sep='/')) %>% 
  mutate(BROAD = na_if(BROAD, "")) %>%
  mutate_at(vars(Filename, BROAD), list(as.factor)) %>% 
  mutate(count= 1) %>% 
  pivot_wider(names_from = BROAD, values_from = count) %>%
  select(-c(TYPE, MORPHOLOGY, FieldOfView, Image.row, Image.col, CODE, Radius..)) %>% 
  group_by(Filename) %>%
  replace(is.na(.), 0) %>%
  summarise_all(funs(sum)) %>%
  mutate(total.sum = rowSums(.[,2:(ncol(.))], na.rm = TRUE)) %>%
  group_by(Filename) %>%
  mutate_at(vars(2:(ncol(.))), funs(./total.sum*100)) %>%
  ungroup() %>%
  rename(not.scored = 'NA') %>%
  glimpse()

head(df)
str(df)


## get maximum class for each image ----
# https://stackoverflow.com/questions/17735859/for-each-row-return-the-column-name-of-the-largest-value

df2 <- df %>% 
  replace(is.na(.), 0) %>%
  select(-c(Filename, total.sum)) %>%
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(everything()))]) %>%
  cbind(Filename = df$Filename) %>%
  relocate(Filename) %>%
  glimpse()


# save --
write.csv(df2, paste(dr.dir, "dominant-BOSS-forwards_broad.percent.cover.csv", sep ='/'))


