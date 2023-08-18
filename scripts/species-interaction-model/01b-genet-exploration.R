library("unmarked") #install.packages("unmarked")

# load in detection history
genet_dh <-read_csv("data/gorongosa-cameras/genet.csv", col_names = FALSE) %>% as.matrix()

#load occupancy covariates
#leaving the column names in for now
occ_covs <- read_csv("data/gorongosa-cameras/GNP covariates.csv", col_names = TRUE) %>% as.data.frame()

#load occ covs with pan (didn't use to test termite stuff)
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
occ_covs$termites_250m = scale(occ_covs$termites_250m)
occ_covs$termites_500m = scale(occ_covs$termites_500m)
occ_covs$termites_500m_count = scale(occ_covs$termites_500m_count)
occ_covs$termites_1km_count = scale(occ_covs$termites_1km_count)

genet_data <- unmarkedFrameOccu(genet_dh, siteCovs = occ_covs)

(genet_fit00 <- occu(~1 ~1, genet_data))

(genet_fit0 <- occu(~cover.ground+detect.obscured ~1, genet_data))

(genet_fit1 <- occu(~cover.ground+detect.obscured ~urema_dist, genet_data))

(genet_fit2 <- occu(~cover.ground+detect.obscured ~termite.large.count.100m, genet_data))

(genet_fit3 <- occu(~cover.ground+detect.obscured ~tree_hansen, genet_data))

(genet_fit4 <- occu(~cover.ground+detect.obscured ~lion_latedry, genet_data))

(genet_fit4.1 <- occu(~cover.ground+detect.obscured ~water_dist, genet_data))

(genet_fit5 <- occu(~cover.ground+detect.obscured ~urema_dist+termite.large.count.100m, genet_data))

(genet_fit6 <- occu(~cover.ground+detect.obscured ~urema_dist+tree_hansen, genet_data))

(genet_fit7 <- occu(~cover.ground+detect.obscured ~urema_dist+lion_latedry, genet_data))

(genet_fit8 <- occu(~cover.ground+detect.obscured ~termite.large.count.100m+tree_hansen, genet_data))

(genet_fit9 <- occu(~cover.ground+detect.obscured ~termite.large.count.100m+lion_latedry, genet_data))

(genet_fit10 <- occu(~cover.ground+detect.obscured ~tree_hansen+lion_latedry, genet_data))

(genet_fit11 <- occu(~cover.ground+detect.obscured ~fire_frequency, genet_data))

(genet_fit12 <- occu(~cover.ground+detect.obscured ~pans_100m, genet_data))

(genet_fit12 <- occu(~cover.ground+detect.obscured ~urema_dist+fire_frequency, genet_data))

(genet_fit14 <- occu(~cover.ground+detect.obscured ~water_dist + termite.large.count.100m, genet_data))

(genet_fit15 <- occu(~cover.ground+detect.obscured ~water_dist + tree_hansen, genet_data))

(genet_fit16 <- occu(~cover.ground+detect.obscured+ water_dist ~1, genet_data))

(genet_fit17 <- occu(~cover.ground+detect.obscured+ termite.large.count.100m ~1, genet_data))

(genet_fit18 <- occu(~cover.ground+detect.obscured+ tree_hansen ~1, genet_data))

(genet_fit19 <- occu(~cover.ground+detect.obscured+ tree_hansen + water_dist ~1, genet_data))

(genet_fit20 <- occu(~cover.ground+detect.obscured+ tree_hansen + termite.large.count.100m ~1, genet_data))

(genet_fit21 <- occu(~cover.ground+detect.obscured+ water_dist + termite.large.count.100m ~1, genet_data))

(genet_fit22 <- occu(~cover.ground+detect.obscured+ water_dist + termite.large.count.100m + tree_hansen ~1, genet_data))

(genet_fit23 <- occu(~cover.ground+detect.obscured+ lion_latedry ~1, genet_data))

(genet_fit28 <- occu(~cover.ground+detect.obscured+ termites_250m ~1, genet_data))

(genet_fit27 <- occu(~cover.ground+detect.obscured+ termites_500m ~1, genet_data))

(genet_fit26 <- occu(~cover.ground+detect.obscured+ termites_500m_count ~1, genet_data))

(genet_fit29 <- occu(~cover.ground+detect.obscured+ termites_1km_count ~1, genet_data))

(genet_fit30 <- occu(~cover.ground+detect.obscured+ termites_1km_count +tree_hansen ~1, genet_data))

(genet_fit31 <- occu(~cover.ground+detect.obscured+tree_hansen + lion_latedry ~1, genet_data))

(genet_fit31 <- occu(~cover.ground+detect.obscured+tree_hansen + lion_latedry ~1, genet_data))

(genet_fit32 <- occu(~cover.ground+detect.obscured+termites_1km_count +water_dist ~1, genet_data))

(genet_fit33 <- occu(~cover.ground+detect.obscured+termites_1km_count +lion_latedry ~1, genet_data))

(genet_fit34 <- occu(~cover.ground+detect.obscured+water_dist +lion_latedry ~1, genet_data))

(genet_fit35 <- occu(~cover.ground+detect.obscured+water_dist +termites_1km_count+tree_hansen ~1, genet_data))

(genet_fit36 <- occu(~cover.ground+detect.obscured+water_dist +tree_hansen+lion_latedry ~1, genet_data))

(genet_fit37 <- occu(~cover.ground+detect.obscured+water_dist +termites_1km_count+lion_latedry ~1, genet_data))

(genet_fit38 <- occu(~cover.ground+detect.obscured+tree_hansen +termites_1km_count+lion_latedry ~1, genet_data))

(genet_fit39 <- occu(~cover.ground+detect.obscured+tree_hansen +termites_1km_count+lion_latedry +water_dist ~1, genet_data))
