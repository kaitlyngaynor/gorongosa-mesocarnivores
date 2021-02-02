#Goal: create umf data object with GNP data (get it in the right format)
#this repeats a lot of what I already did, so I need to go through and edit better (comment written 2-2-21)

#Install packages (good group to start with)
library("unmarked")  #install.packages("unmarked") 
library(camtrapR) #install.packages("camtrapR")
library(tidyverse)
library(dplyr)
library(patchwork) #install.packages("patchwork")
library(ggpubr)  #install.packages("ggpubr")

#Import data
#detection histories for 2016 are already subset to the late dry season, set noon to noon b/c nocturnal animals
genet_dh_16_17 <-read_csv("data/gorongosa-cameras/genet_16_17.csv", col_names = FALSE) %>% as.matrix()


# load in GNP cam metadata
cam_meta <- read_csv("data/gorongosa-cameras/cam_metadata_fromfield_and_raw_raster_withlion.csv")

#make table with all covariates (environmental and detection)
#using GNP_covs instead of occ_covs because I needed one with study site included (to filter)
GNP_covs <- select(cam_meta, StudySite, urema_dist, tree_hansen, termite.large.count.100m, lion_camera, lion_latedry, fire_frequency, pans_100m, detect.obscured, cover.ground)

#remove unused sites
GNP_covs <- filter(GNP_covs, !StudySite %in% c("A06", "B05", "D09", "E12", "F09", "G10", "G12", "H09", "H11", "H13", "I14","J09","L09","L13","M08")) #this removes all records from cameras that were inoperable in 2017

#scale variables
GNP_covs$urema_dist = scale(GNP_covs$urema_dist)
GNP_covs$tree_hansen = scale(GNP_covs$tree_hansen)
GNP_covs$termite.large.count.100m = scale(GNP_covs$termite.large.count.100m)
GNP_covs$lion_latedry = scale(GNP_covs$lion_latedry)
GNP_covs$cover.ground = scale(GNP_covs$cover.ground)

write_csv(GNP_covs, "data/gorongosa-cameras/GNP covs.csv", col_names = T)


#Adjust dates
#need to figure out how to convert dates to Julian
#then need to do the cutting for where dates and observations don't overlap?

#Format data
yrs <- as.character(2016:2019) #creates a list with the relevant years 
yrs <- matrix(yrs, nrow(civet_dh_2016), 4, byrow=TRUE) 

#create data object
#I think this successfully creates a umf data object?
GNP_umf <- unmarkedMultFrame(y=genet_dh_16_17, #creates the actual data object; sets y to detection history (matrix of observed data)
                             siteCovs=GNP_covs[,2:4], yearlySiteCovs=list(year=yrs), #assigns siteCovs to the second three columns of occ_covs (Urema distance, tree hansen, termite); assigns the list of years as the yearlySiteCovs (covariates at the site-year level)
                             obsCovs=list(date=GNP_DATE), #sets obsCovs (covariates that vary within site-year-observation level) to DATE
                             numPrimary=2) #number of primary time periods (in this case, years)
