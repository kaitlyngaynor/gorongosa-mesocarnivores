library("unmarked") #install.packages("unmarked")

# load in detection history
honeybadger_dh <-read_csv("data/gorongosa-cameras/honey_badger.csv", col_names = FALSE) %>% as.matrix()

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


honeybadger_data <- unmarkedFrameOccu(honeybadger_dh, siteCovs = occ_covs)

(honeybadger_fit0 <- occu(~cover.ground+detect.obscured ~1, honeybadger_data))

(honeybadger_fit1 <- occu(~cover.ground+detect.obscured ~urema_dist, honeybadger_data))

(honeybadger_fit2 <- occu(~cover.ground+detect.obscured ~termite.large.count.100m, honeybadger_data))

(honeybadger_fit3 <- occu(~cover.ground+detect.obscured ~tree_hansen, honeybadger_data))

(honeybadger_fit4 <- occu(~cover.ground+detect.obscured ~lion_latedry, honeybadger_data))

(honeybadger_fit4.1 <- occu(~cover.ground+detect.obscured ~water_dist, honeybadger_data))

(honeybadger_fit5 <- occu(~cover.ground+detect.obscured ~urema_dist+termite.large.count.100m, honeybadger_data))

(honeybadger_fit6 <- occu(~cover.ground+detect.obscured ~urema_dist+tree_hansen, honeybadger_data))

(honeybadger_fit7 <- occu(~cover.ground+detect.obscured ~urema_dist+lion_latedry, honeybadger_data))

(honeybadger_fit8 <- occu(~cover.ground+detect.obscured ~termite.large.count.100m+tree_hansen, honeybadger_data))

(honeybadger_fit9 <- occu(~cover.ground+detect.obscured ~termite.large.count.100m+lion_latedry, honeybadger_data))

(honeybadger_fit10 <- occu(~cover.ground+detect.obscured ~tree_hansen+lion_latedry, honeybadger_data))

(honeybadger_fit11 <- occu(~cover.ground+detect.obscured ~fire_frequency, honeybadger_data))

(honeybadger_fit12 <- occu(~cover.ground+detect.obscured ~pans_100m, honeybadger_data))
