#install packages
library("unmarked") #install.packages("unmarked")
library(camtrapR) #install.packages("camtrapR")
library(tidyverse)
library(dplyr)

#use unmarkedFrameOccuMulti to create the data object

# load in detection histories
genet_dh <-read_csv("data/gorongosa-cameras/genet.csv", col_names = FALSE) %>% as.matrix()

civet_dh <-read_csv("data/gorongosa-cameras/civet.csv", col_names = FALSE) %>% as.matrix()

#create list of species
gc_y <- list(genet_dh, civet_dh)
names(gc_y) <- c('genet','civet')

#load occupancy covariates
#leaving the column names in for now
occ_covs <- read_csv("data/gorongosa-cameras/GNP covariates.csv", col_names = TRUE) %>% as.data.frame()

#scaling all the non-binary covariates to address the NaN warnings
occ_covs$urema_dist = scale(occ_covs$urema_dist)
occ_covs$tree_hansen = scale(occ_covs$tree_hansen)
occ_covs$termite.large.count.100m = scale(occ_covs$termite.large.count.100m)
occ_covs$lion_latedry = scale(occ_covs$lion_latedry)
occ_covs$cover.ground = scale(occ_covs$cover.ground)

data61 <- unmarkedFrameOccuMulti(y = gc_y, siteCovs = occ_covs)

#look at data
summary(data61)
plot(data61)

# Look at f parameter design matrix
data@fDesign
data@ylist

#f1: genet
#f2: civet
#f3: genet-civet

occFormulas61 <- c('~urema_dist','~urema_dist + termite.large.count.100m', '0')
detFormulas61 <- c('~detect.obscured+cover.ground', '~detect.obscured+cover.ground')

fit61 <- occuMulti(detFormulas61, stateformulas = occFormulas61, data = data61)

fit61

occFormulas62 <- c('~urema_dist','~urema_dist + termite.large.count.100m', '~1')

fit62 <- occuMulti(detFormulas61, stateformulas = occFormulas62, data = data61)

fit62

occFormulas63 <- c('~urema_dist','~urema_dist + termite.large.count.100m', '~lion_latedry')

fit63 <- occuMulti(detFormulas61, stateformulas = occFormulas63, data = data61)

fit63
