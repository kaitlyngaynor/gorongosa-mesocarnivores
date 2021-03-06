library("unmarked") #install.packages("unmarked")

# load in detection history
civet_dh <-read_csv("data/gorongosa-cameras/civet.csv", col_names = FALSE) %>% as.matrix()

#load occupancy covariates
#leaving the column names in for now
occ_covs <- read_csv("data/gorongosa-cameras/GNP covariates.csv", col_names = TRUE) %>% as.data.frame()

#scaling all the non-binary covariates to address the NaN warnings
occ_covs$urema_dist = scale(occ_covs$urema_dist)
occ_covs$tree_hansen = scale(occ_covs$tree_hansen)
occ_covs$termite.large.count.100m = scale(occ_covs$termite.large.count.100m)
occ_covs$lion_latedry = scale(occ_covs$lion_latedry)
occ_covs$cover.ground = scale(occ_covs$cover.ground)
occ_covs$fire_frequency = scale(occ_covs$fire_frequency)
occ_covs$pans_100m = scale(occ_covs$pans_100m)

civet_data <- unmarkedFrameOccu(civet_dh, siteCovs = occ_covs)

(civet_fit1 <- occu(~cover.ground+detect.obscured ~urema_dist, civet_data))

(civet_fit2 <- occu(~cover.ground+detect.obscured ~termite.large.count.100m, civet_data))

(civet_fit3 <- occu(~cover.ground+detect.obscured ~tree_hansen, civet_data))

(civet_fit4 <- occu(~cover.ground+detect.obscured ~lion_latedry, civet_data))

(civet_fit5 <- occu(~cover.ground+detect.obscured ~urema_dist+termite.large.count.100m, civet_data))

(civet_fit6 <- occu(~cover.ground+detect.obscured ~urema_dist+tree_hansen, civet_data))

(civet_fit7 <- occu(~cover.ground+detect.obscured ~urema_dist+lion_latedry, civet_data))

(civet_fit8 <- occu(~cover.ground+detect.obscured ~termite.large.count.100m+tree_hansen, civet_data))

(civet_fit9 <- occu(~cover.ground+detect.obscured ~termite.large.count.100m+lion_latedry, civet_data))

(civet_fit10 <- occu(~cover.ground+detect.obscured ~tree_hansen+lion_latedry, civet_data))

(civet_fit11 <- occu(~cover.ground+detect.obscured ~fire_frequency, civet_data))

(civet_fit12 <- occu(~cover.ground+detect.obscured ~pans_100m, civet_data))
