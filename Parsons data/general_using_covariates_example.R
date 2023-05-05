library(Rcpp)
library(dplyr)
library(tidyverse)
library(RcppArmadillo) #install.packages("RcppArmadillo") #

source('Parsons data/splt_lik.R')
sourceCpp("Parsons data/mmpp_covs.cpp")

#Loading some example data
load("Parsons data/example_data.rda")

#species detection lists are times of detections for each species
#on each deployment relative to the start of the deployment
sp1<-dat$sp1
sp2<-dat$sp2

#The number of camera deployments
deps<-dat$deps

#The deployment lengths in days
dep_len<-dat$dep_len

#Number of hours in each deployment
ndet<-dep_len*24

#The deployment start date/time
dep_start<-dat$dep_start

#Some additional data required for cpp version
#this stuff is also calculated for R version, 
#but inside the actual lik function
y1_i <- sapply(sp1, function(x) ifelse(is.null(x), 0, 1))
y2_i <- sapply(sp2, function(x) ifelse(is.null(x), 0, 1))

#Get time difference between detection times and interval boundaries
# y = detection times at a camera site
# J = total time length of survey at a camera site
# inc = amount of time in each interval (in units of days)
get_yd <- function(y, J, inc=1){
  d <- seq(0, J, by=inc)
  if((J-d[length(d)]) > 0){
    d <- c(d, J)
  }
  
  if(is.null(y)){
    groups <- lapply(1:(length(d)-1), function(x) numeric(0))
  } else{
    groups <- split(y, cut(y, d))
  }
  
  groups2 <- lapply(1:length(groups), function(i){
    c(d[i], groups[[i]], d[i+1])
  })
  
  out <- lapply(groups2, function(x) diff(x))
  
}

# Get time differences yd for each species at each site
yd_sp1 <- lapply(1:length(sp1), function(i) get_yd(sp1[[i]], 
                                                   dep_len[i], inc=1/24))
yd_sp2 <- lapply(1:length(sp2), function(i) get_yd(sp2[[i]], 
                                                   dep_len[i], inc=1/24))

yd1 <- unlist(yd_sp1)
yd2 <- unlist(yd_sp2)

######################## Compiling spatial covariates ##########################
#Some fake covariates
site_covs<-data.frame(Name=deps,
                      Percent_forest=sample(seq(0,100, by=1),length(deps), replace=TRUE),
                      Housing_density=sample(seq(0,3000, by=1),length(deps), replace=TRUE))

#Scale the covariates
site_covs$Percent_forest_scaled<-scale(site_covs$Percent_forest)
site_covs$Housing_density_scaled<-scale(site_covs$Housing_density)
summary(site_covs)

################## Compiling observation covariates ############################
#
#Time of day for activity patterns
#

#Make sequence of times for each deployment
names(yd_sp1) <- deps
ndet <- sapply(yd_sp1, length)

sec_in_inc <- 60*60 #seconds in each increment (1 hr)

time_list <- lapply(1:length(ndet), function(i){
  tseq <- as.POSIXlt(seq(dep_start[i], by=sec_in_inc, length.out=ndet[i]))
  tseq$hour + tseq$min/60 + tseq$sec/3600
})
time_vec <- unlist(time_list)

#
#TSL (time since last detection of sp2 - the dominant species)
#

#Vectorize the time since the last detection of the dominant species
#on the same scale as time_list
time_list2 <- lapply(1:length(ndet), function(i){
  seq(0, dep_len[[i]], by=1/24)
})
time_vec2 <- unlist(time_list2)
length(time_vec)==length(time_vec2)

#Convert the day/hours of the deployment to the time since the last sp2 passed
TSL_c<-list()
for(i in 1:length(time_list2)){
  
  if(length(sp2[[i]])==0){TSL_c[[i]]<-rep(NA, length(time_list2[[i]]))}
  
  else{
    sp2[[i]]<-sp2[[i]][order(sp2[[i]])]
    
    hr2<-hr2b<-time_list2[[i]]
    
    if(length(sp2[[i]])>1){
      for(k in 2:length(sp2[[i]])){
        hr2[which(hr2b>sp2[[i]][k-1]&hr2b<=sp2[[i]][k])]<-
          hr2[which(hr2b>sp2[[i]][k-1]&hr2b<=sp2[[i]][k])]-sp2[[i]][k-1]
      }
      hr2[which(hr2b>sp2[[i]][length(sp2[[i]])])]<-
        hr2[which(hr2b>sp2[[i]][length(sp2[[i]])])]-sp2[[i]][length(sp2[[i]])]
      hr2[which(hr2b<sp2[[i]][1])]<-NA
    }
    
    else{
      hr2[which(hr2b>sp2[[i]][length(sp2[[i]])])]<-
        hr2[which(hr2b>sp2[[i]][length(sp2[[i]])])]-sp2[[i]][length(sp2[[i]])]
      hr2[which(hr2b<sp2[[i]][1])]<-NA
    }
    
    TSL_c[[i]]<-hr2
  }
}

TSL_vec<-unlist(TSL_c)
summary(TSL_vec)

#Replace the NA values with the overall mean
#This allows the model to run while remaining uninformative for those instances
TSL_vec[is.na(TSL_vec)]<-mean(TSL_vec, na.rm=TRUE)
summary(TSL_vec)

length(time_vec)==length(TSL_vec)#should match

#
#Example of adding a spatial covariate as an observation covariate
#
forest_obs<-list()
for(i in 1:length(time_list)){
  forest<-site_covs$Percent_forest[which(site_covs$Name==deps[i])]
  forest_obs[[i]]<-rep(forest,length(time_list[[i]]))
}
forest_vec<-unlist(forest_obs)
summary(forest_vec)

length(time_vec)==length(forest_vec)#should match

#Covs
obs_covs <- data.frame(int=1,
                       f1c=cos(pi*time_vec/12),
                       f2c=cos(2*pi*time_vec/12),
                       f1s=sin(pi*time_vec/12),
                       f2s=sin(2*pi*time_vec/12),
                       TSL=scale(TSL_vec),
                       forest=scale(forest_vec))

summary(obs_covs)

################## Index to subset lambda values ###############################
#By site
lidx_i <- matrix(NA, nrow=length(yd_sp1), ncol=2)
idx <- 0
for (i in seq_along(yd_sp1)){
  lidx_i[i,1] <- idx
  lidx_i[i,2] <- idx + length(yd_sp1[[i]]) - 1
  idx <- idx + length(yd_sp1[[i]])
}

#Index to subset yd (y-d) values by site i and interval j
#yd is now a vector instead of a list of lists so the index is needed
maxj <- max(sapply(yd_sp1, length))
yd1_st_idx <- yd1_en_idx <- matrix(NA, nrow=length(yd_sp1), ncol=maxj)
idx <- 0
for (i in seq_along(yd_sp1)){
  yd_sub <- yd_sp1[[i]]
  for (j in seq_along(yd_sub)){
    yd1_st_idx[i,j] <- idx
    yd1_en_idx[i,j] <- idx + length(yd_sub[[j]]) - 1
    idx <- idx + length(yd_sub[[j]])
  }
}

yd2_st_idx <- yd2_en_idx <- matrix(NA, nrow=length(yd_sp2), ncol=maxj)
idx <- 0
for (i in seq_along(yd_sp2)){
  yd_sub <- yd_sp2[[i]]
  for (j in seq_along(yd_sub)){
    yd2_st_idx[i,j] <- idx
    yd2_en_idx[i,j] <- idx + length(yd_sub[[j]]) - 1
    idx <- idx + length(yd_sub[[j]])
  }
}

################# Build the model matrices for each parameter ##################
#f parameters
X_f1 <- model.matrix(~Percent_forest_scaled, site_covs)
X_f2 <- model.matrix(~Percent_forest_scaled, site_covs)
X_f12 <- model.matrix(~Housing_density_scaled, site_covs)

#Detection intensity
#TSL should only appear in X_lam1 (sp1 in the presence of sp2)
#This example also includes activity pattern (Fourier series coefficients)
#And an example of tests for whether activity patterns are different based on
#the amount of forest
X_lam1 <- model.matrix(~f1c+f2c+f1s+f2s+TSL+TSL*forest+
                         forest+f1c*forest+f2c*forest+
                         f1s*forest+f2s*forest, obs_covs)

#No TSL for X_lam2 since this is sp1 in the absence of sp2
X_lam2 <- model.matrix(~f1c+f2c+f1s+f2s+
                         forest+f1c*forest+f2c*forest+
                         f1s*forest+f2s*forest, obs_covs)

#No TSL for X_lam3 since this is sp2 detection intensity
X_lam3 <- model.matrix(~f1c+f2c+f1s+f2s+
                         forest+f1c*forest+f2c*forest+
                         f1s*forest+f2s*forest, obs_covs)

########################### Set up and run the model ###########################
pind <- matrix(NA, nrow=8, ncol=2)
pind[1,] <- c(0, 0+ncol(X_f1)-1)
pind[2,] <- c(pind[1,2]+1, pind[1,2]+1+ncol(X_f2)-1)
pind[3,] <- c(pind[2,2]+1, pind[2,2]+1+ncol(X_f12)-1)
pind[4,] <- c(pind[3,2]+1, pind[3,2]+2)
pind[5,] <- c(pind[4,2]+1, pind[4,2]+2)
pind[6,] <- c(pind[5,2]+1, pind[5,2]+1+ncol(X_lam1)-1)
pind[7,] <- c(pind[6,2]+1, pind[6,2]+1+ncol(X_lam2)-1)
pind[8,] <- c(pind[7,2]+1, pind[7,2]+1+ncol(X_lam3)-1)

#Initial values
starts <- optim(rep(-1,max(pind)+1), mmpp_covs, method = 'SANN',
                control = list(maxit=300, trace=1, REPORT =5),
                pind=pind, X_f1=X_f1, X_f2=X_f2, X_f12=X_f12,
                X_lam1=X_lam1, X_lam2=X_lam2, X_lam3=X_lam3, 
                yd1=yd1, yd2=yd2,
                lidx_i=lidx_i, yd1_st_idx=yd1_st_idx, yd1_en_idx=yd1_en_idx, 
                yd2_st_idx=yd2_st_idx, yd2_en_idx=yd2_en_idx, 
                y1_i=y1_i, y2_i=y2_i, threads=10)

#Fit the model until convergence
fit <- optim(starts$par, mmpp_covs, method = 'L-BFGS-B', hessian=TRUE,
             control = list(maxit=100, trace = 1, REPORT = 5),
             pind=pind, X_f1=X_f1, X_f2=X_f2, X_f12=X_f12,
             X_lam1=X_lam1, X_lam2=X_lam2, X_lam3=X_lam3, 
             yd1=yd1, yd2=yd2,
             lidx_i=lidx_i, yd1_st_idx=yd1_st_idx, yd1_en_idx=yd1_en_idx, 
             yd2_st_idx=yd2_st_idx, yd2_en_idx=yd2_en_idx, 
             y1_i=y1_i, y2_i=y2_i, threads=10)

#Save the model
saveRDS(fit, "fit.Rds")

#Return coefficient values
est <- fit$par
names(est) <- c(paste0("f1_",colnames(X_f1)), paste0("f2_",colnames(X_f2)),
                paste0("f12_",colnames(X_f12)),
                "log_mu1[1]","log_mu1[2]","log_mu2[1]","log_mu2[2]",
                paste0("loglam1_",colnames(X_lam1)),
                paste0("loglam2_",colnames(X_lam2)), 
                paste0("loglam3_",colnames(X_lam3)))
se <- sqrt(diag(solve(fit$hessian)))
beta <- fit$par
beta

results <- data.frame(est=round(est, 3), se=round(se,3))
results$lower <- results$est - 1.96*results$se
results$upper <- results$est + 1.96*results$se
results

write.table(results, file = "results.csv", sep = ",", dec = ".")

#AIC
(2*length(fit$par))-(2*-fit$value)

