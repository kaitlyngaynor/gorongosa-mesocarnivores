#Goal: create umf data object with GNP data (get it in the right format)

#Install packages (good group to start with)
library("unmarked")  #install.packages("unmarked") 
library(camtrapR) #install.packages("camtrapR")
library(tidyverse)
library(dplyr)
library(patchwork) #install.packages("patchwork")
library(ggpubr)  #install.packages("ggpubr")

#Import data
#detection histories for 2016 are already subset to the late dry season, set noon to noon b/c nocturnal animals
civet_dh_2016 <-read_csv("data/gorongosa-cameras/civet.csv", col_names = FALSE) %>% as.matrix()

#load occupancy covariates
#leaving the column names in for now
#includes distance to lake, tree cover, mound density, etc
occ_covs <- read_csv("data/gorongosa-cameras/GNP covariates.csv", col_names = TRUE) %>% as.data.frame()

#scaling all the non-binary covariates to address the NaN warnings
occ_covs$urema_dist = scale(occ_covs$urema_dist)
occ_covs$tree_hansen = scale(occ_covs$tree_hansen)
occ_covs$termite.large.count.100m = scale(occ_covs$termite.large.count.100m)
occ_covs$lion_latedry = scale(occ_covs$lion_latedry)
occ_covs$cover.ground = scale(occ_covs$cover.ground)

# load in GNP cam metadata
cam_meta <- read_csv("data/gorongosa-cameras/cam_metadata_fromfield_and_raw_raster_withlion.csv")

#Adjust dates
#need to figure out how to convert dates to Julian
#then need to do the cutting for where dates and observations don't overlap?

#Format data
yrs <- as.character(2016:2019) #creates a list with the relevant years 
yrs <- matrix(yrs, nrow(civet_dh_2016), 4, byrow=TRUE) 
