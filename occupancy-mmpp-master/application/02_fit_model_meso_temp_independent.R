#okay now let's try spatial dependence (incl f12) and temporal independence
#need 7 rows in pind
#this is "no latent genet"

library(Rcpp) #install.packages("Rcpp") 
library(tidyverse)
library(RcppArmadillo) #install.packages("RcppArmadillo")
source("occupancy-mmpp-master/application/01_format_data.R") # loading and cleaning data
#^^ above line needs 01_format_data to have run already
sourceCpp("occupancy-mmpp-master/likelihood/mmpp_covs_f12_temp_ind.cpp") # load likelihood function for independent model
#^^I renamed the function in the above script to make sure I'm using the temp independent function when running these models

# Construct model matrices-----------------------------------------------------
#I believe I need this for every time I run the model
#this is a null model with dependence

# Occupancy natural parameters
#KLG: model.matrix creates a design (or model) matrix, e.g., by expanding factors to a set of 
#KLG: dummy variables (depending on the contrasts) and expanding interactions similarly.
#KLG: these are identical matrices
#I don't understand this well enough to know which one of my covariates to choose
X_f1_GNP1.1 <- model.matrix(~1, site_covs_GNP)
X_f2_GNP1.1 <- model.matrix(~1, site_covs_GNP)
X_f12_GNP1.1 <- model.matrix(~1, site_covs_GNP) 

# Detection intensity depends time of day
# KLG: makes three large matrices with an intercept column and a column for f1c, f2c, f1s, and f2s
# KLG: those 4 variables represent the Fourier series applied to the time vector
# KLG: these are identical matrices
#species 1
X_lam1_GNP1.1 <- model.matrix(~1, obs_covs_GNP)

#species 2
X_lam2_GNP1.1 <- model.matrix(~1, obs_covs_GNP)

#species 2 (NOT HERE)
#X_lam3_GNP1 <- model.matrix(~1, obs_covs_GNP)

# Save model matrices for use elsewhere
save(X_f1_GNP1.1, X_f2_GNP1.1, X_f12_GNP1.1, X_lam1_GNP1.1, X_lam2_GNP1.1, file='model_matrices_GNP1.1.Rdata')

# Indicator matrix to divide up single vector of parameters into
# subvectors for each parameter to estimate
#KLG: not sure I follow why the different values are used
#I THINK I NEED TO MAKE A NEW MATRIX FOR THIS EVERY RUN
pind_GNP1.1 <- matrix(NA, nrow=7, ncol=2) #KLG: makes an empty matrix with 8 rows and 2 columns
#KLG: each row is filled in individually
pind_GNP1.1[1,] <- c(0, 0+ncol(X_f1_GNP1.1)-1)                #f1, KLG: fills in first row
pind_GNP1.1[2,] <- c(pind_GNP1.1[1,2]+1, pind_GNP1.1[1,2]+1+ncol(X_f2_GNP1.1)-1)    #f2
pind_GNP1.1[3,] <- c(pind_GNP1.1[2,2]+1, pind_GNP1.1[2,2]+1+ncol(X_f12_GNP1.1)-1)   #f12
pind_GNP1.1[4,] <- c(pind_GNP1.1[3,2]+1, pind_GNP1.1[3,2]+2)                 #mu (species 1)
pind_GNP1.1[5,] <- c(pind_GNP1.1[4,2]+1, pind_GNP1.1[4,2]+2)                 #mu (species 2)
pind_GNP1.1[6,] <- c(pind_GNP1.1[5,2]+1, pind_GNP1.1[5,2]+1+ncol(X_lam1_GNP1.1)-1)  #lambda sp1
pind_GNP1.1[7,] <- c(pind_GNP1.1[6,2]+1, pind_GNP1.1[6,2]+1+ncol(X_lam2_GNP1.1)-1)  #lambda sp2
#pind_GNP1[8,] <- c(pind_GNP1[7,2]+1, pind_GNP1[7,2]+1+ncol(X_lam3_GNP1)-1)  #lambda sp2

# Optimization-----------------------------------------------------------------

set.seed(123)

# Quickly get reasonable start values with SANN
# Initial SANN NLL value should be 41111.166130
# KLG: I do not understand what SANN NLL is
# KLG: this is used to feed into the next optimization thing, it generates start values
#KLG: rep() replicates the values in x
starts_GNP1.1 <- optim(rep(0,max(pind_GNP1.1)+1), mmpp_covs_temp_i, method = 'SANN',
                     control = list(maxit=400, trace=1, REPORT =5),
                     pind=pind_GNP1.1, X_f1=X_f1_GNP1.1, X_f2=X_f2_GNP1.1, X_f12=X_f12_GNP1.1, X_lam1=X_lam1_GNP1.1,
                     X_lam2=X_lam2_GNP1.1, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                     yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                     yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)

# Do optimization and calculate hessian
fit_GNP1.1 <- optim(starts_GNP1.1$par, mmpp_covs_temp_i, method = 'L-BFGS-B', hessian=TRUE,
                  control = list(trace = 1, REPORT = 5, maxit=400),
                  pind=pind_GNP1.1, X_f1=X_f1_GNP1.1, X_f2=X_f2_GNP1.1, X_f12=X_f12_GNP1.1, X_lam1=X_lam1_GNP1.1,
                  X_lam2=X_lam2_GNP1.1, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                  yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                  yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)
# Final NLL value should be ~ 26461.08
# May take several runs of optimization to get past local minima to this value

#Format and save results
#KLG: saveRDS saves an R object for it to be called later (it serializes an R object into a 
#KLG: format that can be called later), but it forgets the original name of the object
saveRDS(fit_GNP1.1, "fit_covs3_GNP1.1.Rds")

est_GNP1.1 <- fit_GNP1.1$par 
names(est_GNP1.1) <- c(paste0("f1_",colnames(X_f1_GNP1.1)), paste0("f2_",colnames(X_f2_GNP1.1)),
                     paste0("f12_",colnames(X_f12_GNP1.1)),
                     "log_mu1[1]","log_mu1[2]","log_mu2[1]","log_mu2[2]",
                     paste0("loglam1_",colnames(X_lam1_GNP1.1)),
                     paste0("loglam2_",colnames(X_lam2_GNP1.1)))
se_GNP1.1 <- sqrt(diag(solve(fit_GNP1.1$hessian))) 

results_GNP1.1 <- data.frame(est = round(est_GNP1.1, 3), se = round(se_GNP1.1,3))
results_GNP1.1$lower <- results_GNP1.1$est - 1.96*results_GNP1.1$se
results_GNP1.1$upper <- results_GNP1.1$est + 1.96*results_GNP1.1$se
results_GNP1.1

saveRDS(results_GNP1.1, 'results_GNP1.1.Rds')

#AIC
(2*length(fit_GNP1.1$par))-(2*-fit_GNP1.1$value)
