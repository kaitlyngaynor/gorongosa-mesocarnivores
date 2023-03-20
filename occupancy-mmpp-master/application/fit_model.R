library(Rcpp) #install.packages("Rcpp") 
library(tidyverse)
library(RcppArmadillo) #install.packages("RcppArmadillo")
source("occupancy-mmpp-master/application/01_format_data.R") # loading and cleaning data
#^^ above line needs 01_format_data to have run already
sourceCpp("occupancy-mmpp-master/likelihood/likelihood.cpp") # load likelihood function

# Organize response variable---------------------------------------------------

#Indicator variable: was species ever detected at each site
# KLG: goes through every deployment in deer and coyote
# KLG: sapply applies the function to every element of x; function(x) is an anonymous function
# KLG: if the deployment has no entry for deer/coyote, give it a 0; else, give it a 1
y1_i <- sapply(deer, function(x) ifelse(is.null(x), 0, 1))
y2_i <- sapply(coys, function(x) ifelse(is.null(x), 0, 1))

#Get time difference between detection times and interval boundaries
# y = detection times at a camera site
# J = total time length of survey at a camera site
# inc = amount of time in each interval (in units of days)
# KLG: this is creating a function for an individual cam trap deployment
get_yd <- function(y, J, inc=1){ # KLG: function inputs
  d <- seq(0, J, by=inc) #KLG: seq() generates a sequence from 0 to total time length of a survey 
                         # at a site by 1 (which I believe is days)
  if((J-d[length(d)]) > 0){ #KLG: length of a sequence returns the number of items in that sequence
    # KLG: I think d[length(d)] is the item in sequence d at its last location (length of d)
    # KLG: so this is saying if there is time between the last detection and the total time length of survey
    d <- c(d, J) #KLG: then add total time to sequence (I think)
  }
  #KLG: these two if statements are separate from each other, with the above one creating your sequence d
  if(is.null(y)){ #if there are no detections 
    groups <- lapply(1:(length(d)-1), function(x) numeric(0)) #KLG: lapply applies a function over a list or vector
  #KLG: run the length of the sequence (not sure why minus 1)
  #KLG: numeric(0) returns a numeric vector of *length 0*, so when you add anything to it you get the same result (it's basically a numeric NULL)

    } else{ #KLG: if there are detections
    groups <- split(y, cut(y, d)) #KLG: split() takes a vector as an argument and divides the information into groups
    #KLG: cut divides the range of x into intervals and codes the values in x according to which interval they fall. 
    #KLG: I think this is figuring out what the intervals are between detections
    #KLG: so 'groups' contains some info on times between detections
     }

  #KLG: create another object, run through to the length of 'groups', create a vector with the value of 
  #d at that index, the value from groups at that index, and the value of d at the next index
  groups2 <- lapply(1:length(groups), function(i){
              c(d[i], groups[[i]], d[i+1])
              })
  #KLG: diff function computes the difference between pairs of consecutive elements of a numeric vector.
  out <- lapply(groups2, function(x) diff(x))
}

# KLG attempting to break apart the above function----
# makes more sense now
f <- seq(0, dep_len[1], by=1/24)

#I think this is just adding the final time to be considered (the total deployment time) because 
# breaking it up by hour can get rid of that 
# so you have a sequence from 0 to the actual end of your deployment by your chosen interval
if((dep_len[1]-f[length(f)]) > 0){
  f <- c(f, dep_len[1]) #this adds one numeric value to f
}


# cut(deer[[1]], f) yields the intervals from f (hourly intervals for whole deployment)
# that include deer detections (assigns a 1 (yes deer detection) or a 0 (no deer detection) 
# to all hourly intervals)
c <- cut(deer[[1]], f)

# KLG: if there are detections, this breaks the vector with deer detections into the intervals
# determined by f (hourly intervals of the whole deployment)
# groups_KLG contains the intervals, the assigned factor for the interval (0 or 1 based on 
# deer detection), and the the time value within the intervals that have a deer detection
# of that deer detection
if(is.null(deer[[1]])) { #if there are no detections
  groups_KLG <- lapply(1:(length(f) - 1), function(x)
    numeric(0))
} else{ #KLG: if there are detections
  groups_KLG <- split(deer[[1]], cut(deer[[1]], f))
} 

#KLG:this runs through the whole list and creates a new list
#KLG: with at least two doubles for every element (the start and end time of the hourly interval)
#KLG: if there was a deer detection in that interval, that time value is also included 
#KLG: (in between the start and end interval times)
#KLG: this must just be putting it into a usable format for what's to come
groups2_KLG <- lapply(1:length(groups_KLG), function(i){
  c(f[i], groups_KLG[[i]], f[i+1])
})

#KLG: this yields a list of length 512 (number of hourly intervals)
#KLG: if no deer detection in a given interval, the value is just the amount of time in the 
#KLG: interval
#KLG: if there is a deer detection in a given interval, there are two doubles
#KLG: the first is the time between start of interval and deer detection, and the second is the 
#KLG: time between deer detection and end of interval
out_KLG <- lapply(groups2_KLG, function(x) diff(x))

# Get time differences yd for each species at each site----
# KLG: back to regularly scheduled programming
# KLG: deer[[i]] is the detection times for deer at a certain camera site (y in above function)
# KLG: dep_len[i] is the length of the deployment for that site (J in above function)
# KLG: inc = 1/24 I'm pretty sure refers to days/24, so gives hours
# KLG: this runs for every deployment and creates a list as described above (for out_KLG)
yd_deer <- lapply(1:length(deer), function(i) get_yd(deer[[i]], dep_len[i], inc=1/24))
yd_coy <- lapply(1:length(coys), function(i) get_yd(coys[[i]], dep_len[i], inc=1/24))

# Convert them to vector from list (working around limitations of lists in C++)
# KLG: holy cow this produces a large vector containing the time intervals for every deployment
# KLG: so most are 0.416666 (one full interval, no detections) and some of the numbers are the 
# KLG: smaller portions of the interval (broken when there's a detection in the interval)
yd1 <- unlist(yd_deer)
yd2 <- unlist(yd_coy)

# Index needed to match elements of above vectors to correct site
lidx_i <- matrix(NA, nrow=length(yd_deer), ncol=2)
idx <- 0
for (i in seq_along(yd_deer)){
  lidx_i[i,1] <- idx
  lidx_i[i,2] <- idx + length(yd_deer[[i]]) - 1
  idx <- idx + length(yd_deer[[i]])
}

#Index to subset yd (y-d) values by site i and interval j
#yd is now a vector instead of a list of lists so the index is needed
maxj <- max(sapply(yd_deer, length))
yd1_st_idx <- yd1_en_idx <- matrix(NA, nrow=length(yd_deer), ncol=maxj)
idx <- 0
for (i in seq_along(yd_deer)){
  yd_sub <- yd_deer[[i]]
  for (j in seq_along(yd_sub)){
    yd1_st_idx[i,j] <- idx
    yd1_en_idx[i,j] <- idx + length(yd_sub[[j]]) - 1
    idx <- idx + length(yd_sub[[j]])
  }
}

yd2_st_idx <- yd2_en_idx <- matrix(NA, nrow=length(yd_coy), ncol=maxj)
idx <- 0
for (i in seq_along(yd_coy)){
  yd_sub <- yd_coy[[i]]
  for (j in seq_along(yd_sub)){
    yd2_st_idx[i,j] <- idx
    yd2_en_idx[i,j] <- idx + length(yd_sub[[j]]) - 1
    idx <- idx + length(yd_sub[[j]])
  }
}


# Get covariates---------------------------------------------------------------

#Match deployemnts to site ID
dep_to_site <- dets %>%
  group_by(deployment_id, title) %>%
  summarize()

#Get covariates at each site
site_covs <- data.frame(deployment_id=deps) %>%
  left_join(dep_to_site) %>%
  select(deployment_id, title) %>%
  rename(Camsite=title) %>%
  left_join(covs[,c(1:20)]) %>%
  select(Camsite, deployment_id, Dist_5km, HDens_250m, Hunting) %>%
  as_tibble()

#Make observation covariate (time of day)

#Make sequence of times for each deployment
names(yd_deer) <- deps
ndet <- sapply(yd_deer, length)

sec_in_inc <- 60*60 #seconds in each increment (1 hr)
#sec_in_inc <- 2*60*60 #seconds in each increment (2 hr)

time_list <- lapply(1:length(ndet), function(i){
  tseq <- as.POSIXlt(seq(dep_start[i], by=sec_in_inc, length.out=ndet[i]))
  tseq$hour + tseq$min/60 + tseq$sec/3600
})
time_vec <- unlist(time_list)

#Fourier series values for each time
obs_covs <- data.frame(deploy = rep(deps, ndet),
                       f1c = cos(pi*time_vec/12),
                       f2c = cos(2*pi*time_vec/12),
                       f1s = sin(pi*time_vec/12),
                       f2s = sin(2*pi*time_vec/12))

# Construct model matrices-----------------------------------------------------

# Occupancy natural parameters
X_f1 <- model.matrix(~Hunting, site_covs)
X_f2 <- model.matrix(~Hunting, site_covs)
X_f12 <- model.matrix(~Hunting, site_covs)

# Detection intensity depends time of day
X_lam1 <- X_lam2 <- X_lam3 <- model.matrix(~f1c + f2c + f1s + f2s, obs_covs)

# Save model matrices for use elsewhere
save(X_f1, X_f2, X_f12, X_lam1, X_lam2, X_lam3, file='model_matrices.Rdata')

# Indicator matrix to divide up single vector of parameters into
# subvectors for each parameter to estimate
pind <- matrix(NA, nrow=8, ncol=2)
pind[1,] <- c(0, 0+ncol(X_f1)-1)                        #f1
pind[2,] <- c(pind[1,2]+1, pind[1,2]+1+ncol(X_f2)-1)    #f2
pind[3,] <- c(pind[2,2]+1, pind[2,2]+1+ncol(X_f12)-1)   #f12
pind[4,] <- c(pind[3,2]+1, pind[3,2]+2)                 #mu (species 1)
pind[5,] <- c(pind[4,2]+1, pind[4,2]+2)                 #mu (species 2)
pind[6,] <- c(pind[5,2]+1, pind[5,2]+1+ncol(X_lam1)-1)  #lambda sp1|sp2 present
pind[7,] <- c(pind[6,2]+1, pind[6,2]+1+ncol(X_lam2)-1)  #lambda sp1|sp2 absent
pind[8,] <- c(pind[7,2]+1, pind[7,2]+1+ncol(X_lam3)-1)  #lambda sp2

# Optimization-----------------------------------------------------------------

set.seed(123)

# Quickly get reasonable start values with SANN
# Initial SANN NLL value should be 41111.166130
starts <- optim(rep(0,max(pind)+1), mmpp_covs, method = 'SANN',
             control = list(maxit=400, trace=1, REPORT =5),
            pind=pind, X_f1=X_f1, X_f2=X_f2, X_f12=X_f12, X_lam1=X_lam1,
            X_lam2=X_lam2, X_lam3=X_lam3, yd1=yd1, yd2=yd2, lidx_i=lidx_i,
            yd1_st_idx=yd1_st_idx, yd1_en_idx=yd1_en_idx, yd2_st_idx=yd2_st_idx,
            yd2_en_idx=yd2_en_idx, y1_i=y1_i, y2_i=y2_i, threads=2)
# Final SANN NLL value should be 30050.156712
# ^^this is not the value I get

# Do optimization and calculate hessian
fit <- optim(starts$par, mmpp_covs, method = 'L-BFGS-B', hessian=TRUE,
             control = list(trace = 1, REPORT = 5, maxit=400),
             pind=pind, X_f1=X_f1, X_f2=X_f2, X_f12=X_f12, X_lam1=X_lam1,
            X_lam2=X_lam2, X_lam3=X_lam3, yd1=yd1, yd2=yd2, lidx_i=lidx_i,
            yd1_st_idx=yd1_st_idx, yd1_en_idx=yd1_en_idx, yd2_st_idx=yd2_st_idx,
            yd2_en_idx=yd2_en_idx, y1_i=y1_i, y2_i=y2_i, threads=2)
# Final NLL value should be ~ 26461.08
# May take several runs of optimization to get past local minima to this value

#Format and save results
saveRDS(fit, "fit_covs3.Rds")

est <- fit$par
names(est) <- c(paste0("f1_",colnames(X_f1)), paste0("f2_",colnames(X_f2)),
                paste0("f12_",colnames(X_f12)),
                "log_mu1[1]","log_mu1[2]","log_mu2[1]","log_mu2[2]",
                paste0("loglam1_",colnames(X_lam1)),
                paste0("loglam2_",colnames(X_lam2)), paste0("loglam3_",colnames(X_lam3)))
se <- sqrt(diag(solve(fit$hessian)))

results <- data.frame(est=round(est, 3), se=round(se,3))
results$lower <- results$est - 1.96*results$se
results$upper <- results$est + 1.96*results$se
results

saveRDS(results, 'results.Rds')
