library("unmarked") #install.packages("unmarked")

# load in detection history
marshmongoose_dh <-read_csv("data/gorongosa-cameras/marsh_mongoose.csv", col_names = FALSE) %>% as.matrix()

#load occupancy covariates
#leaving the column names in for now
#occ_covs <- read_csv("data/gorongosa-cameras/GNP covariates.csv", col_names = TRUE) %>% as.data.frame()

#load occ covs with pan
occ_covs <- read_csv("data/gorongosa-cameras/GNP_covariates_with_pan.csv", col_names = TRUE) %>% as.data.frame()

#scaling all the non-binary covariates to address the NaN warnings
occ_covs$urema_dist = scale(occ_covs$urema_dist)
occ_covs$tree_hansen = scale(occ_covs$tree_hansen)
occ_covs$termite.large.count.100m = scale(occ_covs$termite.large.count.100m)
occ_covs$lion_latedry = scale(occ_covs$lion_latedry)
occ_covs$cover.ground = scale(occ_covs$cover.ground)
occ_covs$fire_frequency = scale(occ_covs$fire_frequency)
occ_covs$pans_100m = scale(occ_covs$pans_100m)
occ_covs$water_dist = scale(occ_covs$water_dist)

marshmongoose_data <- unmarkedFrameOccu(marshmongoose_dh, siteCovs = occ_covs)

(marshmongoose_fit0 <- occu(~cover.ground+detect.obscured ~1, marshmongoose_data))

(marshmongoose_fit1 <- occu(~cover.ground+detect.obscured ~urema_dist, marshmongoose_data))

(marshmongoose_fit2 <- occu(~cover.ground+detect.obscured ~termite.large.count.100m, marshmongoose_data))

(marshmongoose_fit3 <- occu(~cover.ground+detect.obscured ~tree_hansen, marshmongoose_data))

(marshmongoose_fit4 <- occu(~cover.ground+detect.obscured ~lion_latedry, marshmongoose_data))

(marshmongoose_fit4.1 <- occu(~cover.ground+detect.obscured ~water_dist, marshmongoose_data))

(marshmongoose_fit5 <- occu(~cover.ground+detect.obscured ~urema_dist+termite.large.count.100m, marshmongoose_data))

(marshmongoose_fit6 <- occu(~cover.ground+detect.obscured ~urema_dist+tree_hansen, marshmongoose_data))

(marshmongoose_fit7 <- occu(~cover.ground+detect.obscured ~urema_dist+lion_latedry, marshmongoose_data))

(marshmongoose_fit8 <- occu(~cover.ground+detect.obscured ~termite.large.count.100m+tree_hansen, marshmongoose_data))

(marshmongoose_fit9 <- occu(~cover.ground+detect.obscured ~termite.large.count.100m+lion_latedry, marshmongoose_data))

(marshmongoose_fit10 <- occu(~cover.ground+detect.obscured ~tree_hansen+lion_latedry, marshmongoose_data))

(marshmongoose_fit11 <- occu(~cover.ground+detect.obscured ~fire_frequency, marshmongoose_data))

(marshmongoose_fit12 <- occu(~cover.ground+detect.obscured ~pans_100m, marshmongoose_data))

(marshmongoose_fit13 <- occu(~cover.ground+detect.obscured+water_dist ~1, marshmongoose_data))
