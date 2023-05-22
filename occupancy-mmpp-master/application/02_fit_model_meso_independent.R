#let's try fully independent (spatial and temporal) models

library(Rcpp) #install.packages("Rcpp") 
library(tidyverse)
library(RcppArmadillo) #install.packages("RcppArmadillo")
source("occupancy-mmpp-master/application/01_format_data.R") # loading and cleaning data
#^^ above line needs 01_format_data to have run already
sourceCpp("occupancy-mmpp-master/likelihood/mmpp_covs_independent.cpp") # load likelihood function for independent model
#^^I renamed the function in the above script to make sure I'm using the independent function when running these models

#not sure if I need all the prep code or if I can call it here

# Construct model matrices-----------------------------------------------------

# Occupancy natural parameters
#KLG: model.matrix creates a design (or model) matrix, e.g., by expanding factors to a set of 
#KLG: dummy variables (depending on the contrasts) and expanding interactions similarly.
#KLG: these are identical matrices
#update: it calls site_covs_GNP no problem here
X_f1_GNP0 <- model.matrix(~1, site_covs_GNP) #civet
X_f2_GNP0 <- model.matrix(~1, site_covs_GNP) #genet
#X_f12_GNP <- model.matrix(~lion_latedry_scaled, site_covs_GNP) #no spatial interaction

# Detection intensity depends time of day
# KLG: makes three large matrices with an intercept column and a column for f1c, f2c, f1s, and f2s
# KLG: those 4 variables represent the Fourier series applied to the time vector
# KLG: these are identical matrices

#species 1 (regardless of species 2)
X_lam1_GNP0 <- model.matrix(~1, obs_covs_GNP)

#NOT USED species 1, species 2 absent
#X_lam2_GNP <- model.matrix(~f1c_GNP + f2c_GNP + f1s_GNP + f2s_GNP + detect.obscured + cover.ground, obs_covs_GNP)

#species 2
X_lam2_GNP0 <- model.matrix(~1, obs_covs_GNP)

# Save model matrices for use elsewhere
save(X_f1_GNP0, X_f2_GNP0, X_lam1_GNP0, X_lam2_GNP0, file='model_matrices_GNP0.Rdata')

# Indicator matrix to divide up single vector of parameters into
# subvectors for each parameter to estimate
#KLG: not sure I follow why the different values are used
pind_GNP0 <- matrix(NA, nrow=6, ncol=2) #KLG: makes an empty matrix with 6 rows and 2 columns
#KLG: each row is filled in individually
pind_GNP0[1,] <- c(0, 0+ncol(X_f1_GNP0)-1)                #f1, KLG: fills in first row
pind_GNP0[2,] <- c(pind_GNP0[1,2]+1, pind_GNP0[1,2]+1+ncol(X_f2_GNP0)-1)    #f2
#pind_GNP[3,] <- c(pind_GNP[2,2]+1, pind_GNP[2,2]+1+ncol(X_f12_GNP)-1)   #f12
pind_GNP0[3,] <- c(pind_GNP0[2,2]+1, pind_GNP0[2,2]+2)                 #mu (species 1)
pind_GNP0[4,] <- c(pind_GNP0[3,2]+1, pind_GNP0[3,2]+2)                 #mu (species 2)
pind_GNP0[5,] <- c(pind_GNP0[4,2]+1, pind_GNP0[4,2]+1+ncol(X_lam1_GNP0)-1)  #lambda sp1
pind_GNP0[6,] <- c(pind_GNP0[5,2]+1, pind_GNP0[5,2]+1+ncol(X_lam2_GNP0)-1)  #lambda sp2
#pind_GNP[8,] <- c(pind_GNP[7,2]+1, pind_GNP[7,2]+1+ncol(X_lam3_GNP)-1)  #lambda sp2 (NOT)

# Optimization-----------------------------------------------------------------

set.seed(123)

# Quickly get reasonable start values with SANN
# KLG: this is used to feed into the next optimization thing, it generates start values
#KLG: rep() replicates the values in x
starts_GNP0 <- optim(rep(0,max(pind_GNP0)+1), mmpp_covs_i, method = 'SANN',
                    control = list(maxit=400, trace=1, REPORT =5),
                    pind=pind_GNP0, X_f1=X_f1_GNP0, X_f2=X_f2_GNP0, X_lam1=X_lam1_GNP0,
                    X_lam2=X_lam2_GNP0, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                    yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                    yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)
# Final SANN NLL value should be 30050.156712
# KLG: ~12-15 min to run above code

# Do optimization and calculate hessian
fit_GNP0 <- optim(starts_GNP0$par, mmpp_covs_i, method = 'L-BFGS-B', hessian=TRUE,
                 control = list(trace = 1, REPORT = 5, maxit=400),
                 pind=pind_GNP0, X_f1=X_f1_GNP0, X_f2=X_f2_GNP0, X_lam1=X_lam1_GNP0,
                 X_lam2=X_lam2_GNP0, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                 yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                 yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)
# Final NLL value should be ~ 26461.08
# May take several runs of optimization to get past local minima to this value

#Format and save results
#KLG: saveRDS saves an R object for it to be called later (it serializes an R object into a 
#KLG: format that can be called later), but it forgets the original name of the object
saveRDS(fit_GNP0, "fit_covs3_GNP0.Rds")

est_GNP0 <- fit_GNP0$par 
names(est_GNP0) <- c(paste0("f1_",colnames(X_f1_GNP0)), paste0("f2_",colnames(X_f2_GNP0)),
                    "log_mu1[1]","log_mu1[2]","log_mu2[1]","log_mu2[2]",
                    paste0("loglam1_",colnames(X_lam1_GNP0)),
                    paste0("loglam2_",colnames(X_lam2_GNP0)))
se_GNP0 <- sqrt(diag(solve(fit_GNP0$hessian))) 

results_GNP0 <- data.frame(est = round(est_GNP0, 3), se=round(se_GNP0,3))
#results_GNP <- data.frame(est_GNP = round(est_GNP, 3))
results_GNP0$lower <- results_GNP0$est - 1.96*results_GNP0$se
results_GNP0$upper <- results_GNP0$est + 1.96*results_GNP0$se
results_GNP0

saveRDS(results_GNP0, 'results_GNP0.Rds')

#AIC
(2*length(fit_GNP0$par))-(2*-fit_GNP0$value)
