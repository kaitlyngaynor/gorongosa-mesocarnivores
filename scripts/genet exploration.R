library("unmarked") #install.packages("unmarked")

# load in detection history
genet_dh <-read_csv("data/gorongosa-cameras/genet.csv", col_names = FALSE) %>% as.matrix()

#load occupancy covariates
#leaving the column names in for now
occ_covs <- read_csv("data/gorongosa-cameras/GNP covariates.csv", col_names = TRUE) %>% as.data.frame()

#scaling all the non-binary covariates to address the NaN warnings
occ_covs$urema_dist = scale(occ_covs$urema_dist)
occ_covs$tree_hansen = scale(occ_covs$tree_hansen)
occ_covs$termite.large.count.100m = scale(occ_covs$termite.large.count.100m)
occ_covs$lion_latedry = scale(occ_covs$lion_latedry)
occ_covs$cover.ground = scale(occ_covs$cover.ground)

genet_data <- unmarkedFrameOccu(genet_dh, siteCovs = occ_covs)

(genet_fit1 <- occu(~cover.ground+detect.obscured ~urema_dist, genet_data))

(genet_fit2 <- occu(~cover.ground+detect.obscured ~termite.large.count.100m, genet_data))

(genet_fit3 <- occu(~cover.ground+detect.obscured ~tree_hansen, genet_data))

(genet_fit4 <- occu(~cover.ground+detect.obscured ~lion_latedry, genet_data))

(genet_fit5 <- occu(~cover.ground+detect.obscured ~urema_dist+termite.large.count.100m, genet_data))

(genet_fit6 <- occu(~cover.ground+detect.obscured ~urema_dist+tree_hansen, genet_data))

(genet_fit7 <- occu(~cover.ground+detect.obscured ~urema_dist+lion_latedry, genet_data))

(genet_fit8 <- occu(~cover.ground+detect.obscured ~termite.large.count.100m+tree_hansen, genet_data))

(genet_fit9 <- occu(~cover.ground+detect.obscured ~termite.large.count.100m+lion_latedry, genet_data))

(genet_fit10 <- occu(~cover.ground+detect.obscured ~tree_hansen+lion_latedry, genet_data))
