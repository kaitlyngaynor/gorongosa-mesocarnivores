#GNP code starts ~line 308
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
lidx_i <- matrix(NA, nrow=length(yd_deer), ncol=2) #KLG: creates a matrix with a row for 
# KLG: every deployment and two columns
idx <- 0 
# KLG: I believe this establishes start and end elements for different deployments in the massive vector
for (i in seq_along(yd_deer)){ #KLG: seq_along() is a function that creates a vector that 
  #KLG: contains a sequence of numbers from 1 to the length of the object
  lidx_i[i,1] <- idx #KLG: ith row, first column; starting element of the ith deployment
  lidx_i[i,2] <- idx + length(yd_deer[[i]]) - 1 #KLG: ith row, second column; last element of the ith deployment
  idx <- idx + length(yd_deer[[i]]) #set value of idx for the next deployment
}

#Index to subset yd (y-d) values by site i and interval j
#yd is now a vector instead of a list of lists so the index is needed
#KLG: sapply(yd_deer, length) pulls the length of each deployment (number of hourly intervals)
maxj <- max(sapply(yd_deer, length)) #KLG: this finds the longest deployment

# KLG: run for deer
yd1_st_idx <- yd1_en_idx <- matrix(NA, nrow=length(yd_deer), ncol=maxj) #KLG: creates two large empty matrices
# KLG: with a row for every deployment and a column for every hour/interval of the longest deployment
idx <- 0
for (i in seq_along(yd_deer)){ #KLG: for every deployment, 1 to 1945
  yd_sub <- yd_deer[[i]] # KLG: set yd_sub to the list of intervals for that deployment
  for (j in seq_along(yd_sub)){ #KLG: then for every interval in that deployment (1-512 for the first deployment)
    #KLG: so advancing along for every element in the list
    yd1_st_idx[i,j] <- idx #KLG: set the value for one matrix starting at 0
    yd1_en_idx[i,j] <- idx + length(yd_sub[[j]]) - 1 #KLG: length(yd_sub[[j]]) is usually 1, 
    #KLG: except when there was a detection in that interval and there are two (or more) values
    idx <- idx + length(yd_sub[[j]]) #KLG: again, this is usually 1 for "empty" intervals
    #KLG: I think the two matrices only differ when there are intervals with 2 (or more) values
    #KLG: but both matrices are affected by intervals with detections because that's how idx advances
  }
}

##KLG figuring out the for loop----
yd_test <- yd_deer[[1]]
yd_test[[155]] #example interval with two values

#KLG: run same code for coyote data----
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
#KLG: almost 1:1, though some sites had more than one deployment
dep_to_site <- dets %>%
  group_by(deployment_id, title) %>%
  summarize()

#Get covariates at each site
site_covs <- data.frame(deployment_id=deps) %>% #KLG: 1945 deployments
  left_join(dep_to_site) %>% #KLG: add sites for each deployment, but this is only for them
  select(deployment_id, title) %>% #KLG: select these columns
  rename(Camsite=title) %>%
  left_join(covs[,c(1:20)]) %>% #KLG: add covariates (I thought you usually needed a by = argument)
  select(Camsite, deployment_id, Dist_5km, HDens_250m, Hunting) %>%
  as_tibble()

#Make observation covariate (time of day)

#Make sequence of times for each deployment
names(yd_deer) <- deps #KLG: instead of yd_deer[[1]], this now has the deployment ID
ndet <- sapply(yd_deer, length) #KLG: for every element in yd_deer, returns the length of that list 
#KLG: number of hourly intervals

sec_in_inc <- 60*60 #seconds in each increment (1 hr)
#sec_in_inc <- 2*60*60 #seconds in each increment (2 hr)

#KLG: for every element in ndet (list of the length of every deployment (number of hourly
#KLG: intervals for each deployment))
time_list <- lapply(1:length(ndet), function(i){
  #KLG: I believe the next lines converts every relevant time for each deployment (start time, and
  #KLG: every hour after--each interval)
  #KLG: length.out determines the length of the sequence, here it's the length of that deployment's list
  tseq <- as.POSIXlt(seq(dep_start[i], by=sec_in_inc, length.out=ndet[i]))
  tseq$hour + tseq$min/60 + tseq$sec/3600 #KLG: then this converts every time into a decimal
})
time_vec <- unlist(time_list) #KLG: convert list to vector

#Fourier series values for each time
#KLG: I don't actually understand what a Fourier series is, but this is just filling in 4 values
#KLG: for every time
#KLG: It consists of an infinite sum of sines and cosines, and because it is periodic (i.e., 
#KLG: its values repeat over fixed intervals), it is a useful tool in analyzing periodic functions.
#KLG: By adding infinite sine (and or cosine) waves we can make other functions
obs_covs <- data.frame(deploy = rep(deps, ndet), #KLG: creates ndet rows for every deployment 
                       #KLG: ndet is the length of the deployment (number of hourly intervals)
                       f1c = cos(pi*time_vec/12),
                       f2c = cos(2*pi*time_vec/12),
                       f1s = sin(pi*time_vec/12),
                       f2s = sin(2*pi*time_vec/12))

# Construct model matrices-----------------------------------------------------

# Occupancy natural parameters
#KLG: model.matrix creates a design (or model) matrix, e.g., by expanding factors to a set of 
#KLG: dummy variables (depending on the contrasts) and expanding interactions similarly.
#KLG: these are identical matrices
X_f1 <- model.matrix(~Hunting, site_covs)
X_f2 <- model.matrix(~Hunting, site_covs)
X_f12 <- model.matrix(~Hunting, site_covs)

# Detection intensity depends time of day
# KLG: makes three large matrices with an intercept column and a column for f1c, f2c, f1s, and f2s
# KLG: those 4 variables represent the Fourier series applied to the time vector
# KLG: these are identical matrices
X_lam1 <- X_lam2 <- X_lam3 <- model.matrix(~f1c + f2c + f1s + f2s, obs_covs)

# Save model matrices for use elsewhere
save(X_f1, X_f2, X_f12, X_lam1, X_lam2, X_lam3, file='model_matrices.Rdata')

# Indicator matrix to divide up single vector of parameters into
# subvectors for each parameter to estimate
#KLG: not sure I follow why the different values are used
pind <- matrix(NA, nrow=8, ncol=2) #KLG: makes an empty matrix with 8 rows and 2 columns
#KLG: each row is filled in individually
pind[1,] <- c(0, 0+ncol(X_f1)-1)                        #f1, KLG: fills in first row
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
# KLG: I do not understand what SANN NLL is
# KLG: this is used to feed into the next optimization thing, it generates start values
#KLG: rep() replicates the values in x
starts <- optim(rep(0,max(pind)+1), mmpp_covs, method = 'SANN',
                control = list(maxit=400, trace=1, REPORT =5),
                pind=pind, X_f1=X_f1, X_f2=X_f2, X_f12=X_f12, X_lam1=X_lam1,
                X_lam2=X_lam2, X_lam3=X_lam3, yd1=yd1, yd2=yd2, lidx_i=lidx_i,
                yd1_st_idx=yd1_st_idx, yd1_en_idx=yd1_en_idx, yd2_st_idx=yd2_st_idx,
                yd2_en_idx=yd2_en_idx, y1_i=y1_i, y2_i=y2_i, threads=2)
# Final SANN NLL value should be 30050.156712
# KLG: ~12-15 min to run above code

# Do optimization and calculate hessian
#KLG: this took ~11 min to get to the first iteration value, 18 (total) for the second
#KLG: function optimization describes a class of problems for finding the input to a 
#KLG: given function that results in the minimum or maximum output from the function (default is minimizing)
#KLG: I'm struggling with what this is doing, but to break apart pieces I understand:
#KLG: maxit = max number of iterations, REPORT = frequency of reports
#KLG: hessian = TRUE -> return a numerically differentiated Hessian matrix
#KLG: mmpp_covs seems to be the function to be optimmized, with the parameters listed after the
#KLG: control variables (it's basically defined/established here)
#KLG: starts$par are the initial values for the parameters to be optimized over
#KLG: this method (L-BFGS-B) allows each variable to be given a lower and/or upper bound
#KLG: this took somewhere around 5-6 hours to run
fit <- optim(starts$par, mmpp_covs, method = 'L-BFGS-B', hessian=TRUE,
             control = list(trace = 1, REPORT = 5, maxit=400),
             pind=pind, X_f1=X_f1, X_f2=X_f2, X_f12=X_f12, X_lam1=X_lam1,
             X_lam2=X_lam2, X_lam3=X_lam3, yd1=yd1, yd2=yd2, lidx_i=lidx_i,
             yd1_st_idx=yd1_st_idx, yd1_en_idx=yd1_en_idx, yd2_st_idx=yd2_st_idx,
             yd2_en_idx=yd2_en_idx, y1_i=y1_i, y2_i=y2_i, threads=2)
# Final NLL value should be ~ 26461.08
# May take several runs of optimization to get past local minima to this value

#Format and save results
#KLG: saveRDS saves an R object for it to be called later (it serializes an R object into a 
#KLG: format that can be called later), but it forgets the original name of the object
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

##GNP version, let's go!----
#Indicator variable: was species ever detected at each site
# KLG: species 1 = sub (deer, genet here), species 2 = dom (coyote, civet here)
# KLG: goes through every deployment in civet and genet
# KLG: sapply applies the function to every element of x; function(x) is an anonymous function
# KLG: if the deployment has no entry for civet/genet, give it a 0; else, give it a 1
y1_i_GNP_gs <- sapply(genet, function(x) ifelse(is.null(x), 0, 1)) #gs for genet sub
y2_i_GNP_gs <- sapply(civet, function(x) ifelse(is.null(x), 0, 1))

#Get time difference between detection times and interval boundaries
# y = detection times at a camera site
# J = total time length of survey at a camera site
# inc = amount of time in each interval (in units of days)
# KLG: this is creating a function for an individual cam trap deployment
# I'm pretty sure this is just creating the function, so does not need to be edited/repeated
# for GNP stuff
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

# Get time differences yd for each species at each site----
# KLG: deer[[i]] is the detection times for deer at a certain camera site (y in above function)
# KLG: dep_len[i] is the length of the deployment for that site (J in above function)
# KLG: inc = 1/24 I'm pretty sure refers to days/24, so gives hours
# KLG: this runs for every deployment and creates a list as described above (for out_KLG)
#this runs for GNP data, not sure how to check if it's working as expected
#this is the same whether genet or civet is sub/dom
yd_civet <- lapply(1:length(civet), function(i) get_yd(civet[[i]], dep_len_GNP[i], inc=1/24))
yd_genet <- lapply(1:length(genet), function(i) get_yd(genet[[i]], dep_len_GNP[i], inc=1/24))

# Convert them to vector from list (working around limitations of lists in C++)
# KLG: holy cow this produces a large vector containing the time intervals for every deployment
# KLG: so most are 0.416666 (one full interval, no detections) and some of the numbers are the 
# KLG: smaller portions of the interval (broken when there's a detection in the interval)
yd1_GNP_gs <- unlist(yd_genet)
yd2_GNP_gs <- unlist(yd_civet)

#civet = deer, genet = coyote
# Index needed to match elements of above vectors to correct site
#again, I think this is the same regardless of dominant spp
lidx_i_GNP <- matrix(NA, nrow=length(yd_civet), ncol=2) #KLG: creates a matrix with a row for 
# KLG: every deployment and two columns
idx_GNP <- 0 
# KLG: I believe this establishes start and end elements for different deployments in the massive vector
for (i in seq_along(yd_civet)){ #KLG: seq_along() is a function that creates a vector that 
  #KLG: contains a sequence of numbers from 1 to the length of the object
  lidx_i_GNP[i,1] <- idx_GNP #KLG: ith row, first column; starting element of the ith deployment
  lidx_i_GNP[i,2] <- idx_GNP + length(yd_civet[[i]]) - 1 #KLG: ith row, second column; last element of the ith deployment
  idx_GNP <- idx_GNP + length(yd_civet[[i]]) #set value of idx for the next deployment
}

#Index to subset yd (y-d) values by site i and interval j
#yd is now a vector instead of a list of lists so the index is needed
#KLG: sapply(yd_deer, length) pulls the length of each deployment (number of hourly intervals)
#again, same regardless of dominant spp
maxj_GNP <- max(sapply(yd_civet, length)) #KLG: this finds the longest deployment

# KLG: run for genet
yd1_st_idx_GNP_gs <- yd1_en_idx_GNP_gs <- matrix(NA, nrow=length(yd_genet), ncol=maxj_GNP) #KLG: creates two large empty matrices
# KLG: with a row for every deployment and a column for every hour/interval of the longest deployment
idx_GNP <- 0
for (i in seq_along(yd_genet)){ #KLG: for every deployment, 1 to 1945
  yd_sub_GNP <- yd_genet[[i]] # KLG: set yd_sub to the list of intervals for that deployment
  for (j in seq_along(yd_sub_GNP)){ #KLG: then for every interval in that deployment (1-512 for the first deployment)
    #KLG: so advancing along for every element in the list
    yd1_st_idx_GNP_gs[i,j] <- idx_GNP #KLG: set the value for one matrix starting at 0
    yd1_en_idx_GNP_gs[i,j] <- idx_GNP + length(yd_sub_GNP[[j]]) - 1 #KLG: length(yd_sub[[j]]) is usually 1, 
    #KLG: except when there was a detection in that interval and there are two (or more) values
    idx_GNP <- idx_GNP + length(yd_sub_GNP[[j]]) #KLG: again, this is usually 1 for "empty" intervals
    #KLG: I think the two matrices only differ when there are intervals with 2 (or more) values
    #KLG: but both matrices are affected by intervals with detections because that's how idx advances
  }
}

#KLG: run same code for civet data----
yd2_st_idx_GNP_gs <- yd2_en_idx_GNP_gs <- matrix(NA, nrow=length(yd_civet), ncol=maxj_GNP)
idx_GNP <- 0
for (i in seq_along(yd_civet)){
  yd_sub_GNP <- yd_civet[[i]]
  for (j in seq_along(yd_sub_GNP)){
    yd2_st_idx_GNP_gs[i,j] <- idx_GNP
    yd2_en_idx_GNP_gs[i,j] <- idx_GNP + length(yd_sub_GNP[[j]]) - 1
    idx_GNP <- idx_GNP + length(yd_sub_GNP[[j]])
  }
}


# Get covariates---------------------------------------------------------------

#Match deployemnts to site ID
#KLG: almost 1:1, though some sites had more than one deployment
#again, I don't think I have to deal with this
#this is for the Kellner sites still
dep_to_site <- dets %>%
  group_by(deployment_id, title) %>%
  summarize()

#Get covariates at each site
#I chose covariates I'd used in my thesis

site_covs_GNP <- data.frame(deployment_id_GNP=deps_GNP) %>% #KLG: 1945 deployments
  #left_join(dep_to_site) %>% #KLG: add sites for each deployment
  #select(deployment_id, title) %>% #KLG: select these columns
  rename(StudySite = deployment_id_GNP) %>% #so the covs matrix can be joined
  left_join(GNP_covs) %>% #KLG: add covariates (I thought you usually needed a by = argument)
  select(StudySite, termite.large.count.100m, tree_hansen, urema_dist, lion_latedry) %>% #they started with camsite and deployment_id
  as_tibble()

#scale site covariates
site_covs_GNP$termite.large.count.100m.scaled <- scale(site_covs_GNP$termite.large.count.100m)
site_covs_GNP$tree_hansen_scaled <- scale(site_covs_GNP$tree_hansen)
site_covs_GNP$urema_dist_scaled <- scale(site_covs_GNP$urema_dist)
site_covs_GNP$lion_latedry_scaled <- scale(site_covs_GNP$lion_latedry)

#add int column
site_covs_GNP$int <- 1


#Make observation covariate (time of day)

#Make sequence of times for each deployment
#I don't think this changes for civet or genet dom/sub
names(yd_civet) <- deps_GNP #KLG: instead of yd_deer[[1]], this now has the deployment ID
ndet_GNP <- sapply(yd_civet, length) #KLG: for every element in yd_deer, returns the length of that list 
#KLG: number of hourly intervals
#^^this is just using yd_civet for the length of the deployments, not specific to civet over genet (I'm pretty sure)

sec_in_inc <- 60*60 #seconds in each increment (1 hr), same for Kellner and GNP
#sec_in_inc <- 2*60*60 #seconds in each increment (2 hr)

#PAUSE HERE

#KLG: for every element in ndet (list of the length of every deployment (number of hourly
#KLG: intervals for each deployment))
#I don't *think* anything about this is species specific
time_list_GNP <- lapply(1:length(ndet_GNP), function(i){
  #KLG: I believe the next lines converts every relevant time for each deployment (start time, and
  #KLG: every hour after--each interval)
  #KLG: length.out determines the length of the sequence, here it's the length of that deployment's list
  tseq_GNP <- as.POSIXlt(seq(dep_start_GNP[i], by=sec_in_inc, length.out=ndet_GNP[i]))
  tseq_GNP$hour + tseq_GNP$min/60 + tseq_GNP$sec/3600 #KLG: then this converts every time into a decimal
})
time_vec_GNP <- unlist(time_list_GNP) #KLG: convert list to vector

##from Arielle, how to calculate time since last detection of the dominant
#
#TSL (time since last detection of sp2 - the dominant species)
#

#Vectorize the time since the last detection of the dominant species
#on the same scale as time_list
time_list2_GNP <- lapply(1:length(ndet_GNP), function(i){
  seq(0, dep_len_GNP[[i]], by=1/24)
})
time_vec2_GNP <- unlist(time_list2_GNP)
length(time_vec_GNP)==length(time_vec2_GNP) #TRUE

#species 1: civet, species 2: genet
#Convert the day/hours of the deployment to the time since the last sp2 passed
TSL_GNP <- list() #I got rid of _c (because I think that was for coyote and it's confusing with civet)
for(i in 1:length(time_list2_GNP)){
  
  if(length(genet[[i]])==0){TSL_GNP[[i]]<-rep(NA, length(time_list2_GNP[[i]]))}
  
  else{
    genet[[i]]<-genet[[i]][order(genet[[i]])]
    
    hr2_GNP <- hr2b_GNP <- time_list2_GNP[[i]]
    
    if(length(genet[[i]]) > 1) {
      for(k in 2:length(genet[[i]])){
        hr2_GNP[which(hr2b_GNP > genet[[i]][k-1] & hr2b_GNP <= genet[[i]][k])]<-
          hr2_GNP[which(hr2b_GNP > genet[[i]][k-1]& hr2b_GNP <= genet[[i]][k])] - genet[[i]][k-1]
      }
      hr2_GNP[which(hr2b_GNP > genet[[i]][length(genet[[i]])])]<-
        hr2_GNP[which(hr2b_GNP > genet[[i]][length(genet[[i]])])] - genet[[i]][length(genet[[i]])]
      hr2_GNP[which(hr2b_GNP < genet[[i]][1])]<-NA
    }
    
    else{
      hr2_GNP[which(hr2b_GNP > genet[[i]][length(genet[[i]])])]<-
        hr2_GNP[which(hr2b_GNP > genet[[i]][length(genet[[i]])])] - genet[[i]][length(genet[[i]])]
      hr2_GNP[which(hr2b_GNP < genet[[i]][1])]<-NA
    }
    
    TSL_GNP[[i]] <- hr2_GNP
  }
}

TSL_vec_GNP <- unlist(TSL_GNP)
summary(TSL_vec_GNP)

#Replace the NA values with the overall mean
#This allows the model to run while remaining uninformative for those instances
TSL_vec_GNP[is.na(TSL_vec_GNP)] <- mean(TSL_vec_GNP, na.rm=TRUE)
summary(TSL_vec_GNP)

length(time_vec_GNP)==length(TSL_vec_GNP)#should match, TRUE
#end from Arielle
####

###from Arielle for using a spatial covariate as a detection covariate
#Example of adding a spatial covariate as an observation covariate
# KLG: make a lake observations list
# KLG: just makes a repeating list so you have a lake value for every time point
#run for distance to lake
lake_obs <- list()
for(i in 1:length(time_list_GNP)){
  lake <- site_covs_GNP$urema_dist[which(site_covs_GNP$StudySite==deps_GNP[i])]
  lake_obs[[i]] <- rep(lake,length(time_list_GNP[[i]]))
}
lake_vec <- unlist(lake_obs)
summary(lake_vec)

length(time_vec_GNP)==length(lake_vec) #should match, TRUE

#run for detect obscured
detect_obs <- list()
for(i in 1:length(time_list_GNP)){
  detect <- GNP_covs$detect.obscured[which(GNP_covs$StudySite==deps_GNP[i])]
  detect_obs[[i]] <- rep(detect,length(time_list_GNP[[i]]))
}
detect_vec <- unlist(detect_obs)
summary(detect_vec)

length(time_vec_GNP)==length(detect_vec) #should match, TRUE

#and for ground cover
ground_obs <- list()
for(i in 1:length(time_list_GNP)){
  ground <- GNP_covs$cover.ground[which(GNP_covs$StudySite==deps_GNP[i])]
  ground_obs[[i]] <- rep(ground,length(time_list_GNP[[i]]))
}
ground_vec <- unlist(ground_obs)
summary(ground_vec)

length(time_vec_GNP)==length(ground_vec) #should match, TRUE

#Fourier series values for each time
#KLG: I don't actually understand what a Fourier series is, but this is just filling in 4 values
#KLG: for every time
#KLG: It consists of an infinite sum of sines and cosines, and because it is periodic (i.e., 
#KLG: its values repeat over fixed intervals), it is a useful tool in analyzing periodic functions.
#KLG: By adding infinite sine (and or cosine) waves we can make other functions
#KLG: first argument here just creates a column with the site name
obs_covs_GNP <- data.frame(deploy_GNP = rep(deps_GNP, ndet_GNP), #KLG: creates ndet rows for every deployment 
                           #KLG: ndet is the length of the deployment (number of hourly intervals)
                           int=1,
                           f1c_GNP = cos(pi*time_vec_GNP/12),
                           f2c_GNP = cos(2*pi*time_vec_GNP/12),
                           f1s_GNP = sin(pi*time_vec_GNP/12),
                           f2s_GNP = sin(2*pi*time_vec_GNP/12),
                           TSL_GNP = scale(TSL_vec_GNP),
                           lake = scale(lake_vec),
                           detect.obscured = scale(detect_vec),
                           cover.ground = scale(ground_vec))

# Construct model matrices-----------------------------------------------------

# Occupancy natural parameters
#KLG: model.matrix creates a design (or model) matrix, e.g., by expanding factors to a set of 
#KLG: dummy variables (depending on the contrasts) and expanding interactions similarly.
#KLG: these are identical matrices
#I don't understand this well enough to know which one of my covariates to choose
X_f1_GNP <- model.matrix(~urema_dist_scaled, site_covs_GNP)
X_f2_GNP <- model.matrix(~urema_dist_scaled + termite.large.count.100m.scaled, site_covs_GNP)
X_f12_GNP <- model.matrix(~lion_latedry_scaled, site_covs_GNP)

# Detection intensity depends time of day
# KLG: makes three large matrices with an intercept column and a column for f1c, f2c, f1s, and f2s
# KLG: those 4 variables represent the Fourier series applied to the time vector
# KLG: these are identical matrices
#species 1, species 2 present
X_lam1_GNP <- model.matrix(~f1c_GNP + f2c_GNP + f1s_GNP + f2s_GNP + detect.obscured + cover.ground + TSL_GNP, obs_covs_GNP)

#species 1, species 2 absent
X_lam2_GNP <- model.matrix(~f1c_GNP + f2c_GNP + f1s_GNP + f2s_GNP + detect.obscured + cover.ground, obs_covs_GNP)

#species 2
X_lam3_GNP <- model.matrix(~f1c_GNP + f2c_GNP + f1s_GNP + f2s_GNP + detect.obscured + cover.ground, obs_covs_GNP)

# Save model matrices for use elsewhere
save(X_f1_GNP, X_f2_GNP, X_f12_GNP, X_lam1_GNP, X_lam2_GNP, X_lam3_GNP, file='model_matrices_GNP.Rdata')

# Indicator matrix to divide up single vector of parameters into
# subvectors for each parameter to estimate
#KLG: not sure I follow why the different values are used
pind_GNP <- matrix(NA, nrow=8, ncol=2) #KLG: makes an empty matrix with 8 rows and 2 columns
#KLG: each row is filled in individually
pind_GNP[1,] <- c(0, 0+ncol(X_f1_GNP)-1)                #f1, KLG: fills in first row
pind_GNP[2,] <- c(pind_GNP[1,2]+1, pind_GNP[1,2]+1+ncol(X_f2_GNP)-1)    #f2
pind_GNP[3,] <- c(pind_GNP[2,2]+1, pind_GNP[2,2]+1+ncol(X_f12_GNP)-1)   #f12
pind_GNP[4,] <- c(pind_GNP[3,2]+1, pind_GNP[3,2]+2)                 #mu (species 1)
pind_GNP[5,] <- c(pind_GNP[4,2]+1, pind_GNP[4,2]+2)                 #mu (species 2)
pind_GNP[6,] <- c(pind_GNP[5,2]+1, pind_GNP[5,2]+1+ncol(X_lam1_GNP)-1)  #lambda sp1|sp2 present
pind_GNP[7,] <- c(pind_GNP[6,2]+1, pind_GNP[6,2]+1+ncol(X_lam2_GNP)-1)  #lambda sp1|sp2 absent
pind_GNP[8,] <- c(pind_GNP[7,2]+1, pind_GNP[7,2]+1+ncol(X_lam3_GNP)-1)  #lambda sp2

# Optimization-----------------------------------------------------------------

set.seed(123)

# Quickly get reasonable start values with SANN
# Initial SANN NLL value should be 41111.166130
# KLG: I do not understand what SANN NLL is
# KLG: this is used to feed into the next optimization thing, it generates start values
#KLG: rep() replicates the values in x
starts_GNP <- optim(rep(0,max(pind_GNP)+1), mmpp_covs, method = 'SANN',
                    control = list(maxit=400, trace=1, REPORT =5),
                    pind=pind_GNP, X_f1=X_f1_GNP, X_f2=X_f2_GNP, X_f12=X_f12_GNP, X_lam1=X_lam1_GNP,
                    X_lam2=X_lam2_GNP, X_lam3=X_lam3_GNP, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                    yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                    yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)
# Final SANN NLL value should be 30050.156712
# KLG: ~12-15 min to run above code

# Do optimization and calculate hessian
#KLG: this took ~11 min to get to the first iteration value, 18 (total) for the second
#KLG: function optimization describes a class of problems for finding the input to a 
#KLG: given function that results in the minimum or maximum output from the function (default is minimizing)
#KLG: I'm struggling with what this is doing, but to break apart pieces I understand:
#KLG: maxit = max number of iterations, REPORT = frequency of reports
#KLG: hessian = TRUE -> return a numerically differentiated Hessian matrix
#KLG: mmpp_covs seems to be the function to be optimmized, with the parameters listed after the
#KLG: control variables (it's basically defined/established here)
#KLG: starts$par are the initial values for the parameters to be optimized over
#KLG: this method (L-BFGS-B) allows each variable to be given a lower and/or upper bound
#KLG: this took somewhere around 5-6 hours to run
fit_GNP <- optim(starts_GNP$par, mmpp_covs, method = 'L-BFGS-B', hessian=TRUE,
                 control = list(trace = 1, REPORT = 5, maxit=400),
                 pind=pind_GNP, X_f1=X_f1_GNP, X_f2=X_f2_GNP, X_f12=X_f12_GNP, X_lam1=X_lam1_GNP,
                 X_lam2=X_lam2_GNP, X_lam3=X_lam3_GNP, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                 yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                 yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)
# Final NLL value should be ~ 26461.08
# May take several runs of optimization to get past local minima to this value

#Format and save results
#KLG: saveRDS saves an R object for it to be called later (it serializes an R object into a 
#KLG: format that can be called later), but it forgets the original name of the object
saveRDS(fit_GNP, "fit_covs3_GNP.Rds")

est_GNP <- fit_GNP$par 
names(est_GNP) <- c(paste0("f1_",colnames(X_f1_GNP)), paste0("f2_",colnames(X_f2_GNP)),
                    paste0("f12_",colnames(X_f12_GNP)),
                    "log_mu1[1]","log_mu1[2]","log_mu2[1]","log_mu2[2]",
                    paste0("loglam1_",colnames(X_lam1_GNP)),
                    paste0("loglam2_",colnames(X_lam2_GNP)), paste0("loglam3_",colnames(X_lam3_GNP)))
se_GNP <- sqrt(diag(solve(fit_GNP$hessian))) 
#this throws an error
#Error in solve.default(fit_GNP$hessian) : 
#Lapack routine dgesv: system is exactly singular: U[16,16] = 0
#there are a bunch of zeros here, maybe that's why?
#the matrix (fit_GNP$hessian) is considered singular, its determinant is 0, there is no inverse
#if it's just for the se, maybe not a big issue? I'm not sure
#note: This only happens when matrix is singular or when it's singular on your 
#machine (due to approximation you can have a really small number be considered 0)
#could be singular bc the rows are collinear
#all the numbers in this matrix are close to 0, which I'm thinking is the problem
#my best guess is that's because it's a relatively small data set without a crazy number of detections

#I can't run the last few lines because I don't have the se
#results_GNP <- data.frame(est_GNP = round(est_GNP, 3), se=round(se,3))
results_GNP <- data.frame(est_GNP = round(est_GNP, 3))
results$lower <- results$est - 1.96*results$se
results$upper <- results$est + 1.96*results$se
results_GNP

saveRDS(results, 'results_GNP.Rds')

#AIC
(2*length(fit_GNP$par))-(2*-fit_GNP$value)

# Construct model matrices-----------------------------------------------------
#I believe I need this for every time I run the model
#this is a null model with dependence

# Occupancy natural parameters
#KLG: model.matrix creates a design (or model) matrix, e.g., by expanding factors to a set of 
#KLG: dummy variables (depending on the contrasts) and expanding interactions similarly.
#KLG: these are identical matrices
#I don't understand this well enough to know which one of my covariates to choose
X_f1_GNP1 <- model.matrix(~1, site_covs_GNP)
X_f2_GNP1 <- model.matrix(~1, site_covs_GNP)
X_f12_GNP1 <- model.matrix(~1, site_covs_GNP) 

# Detection intensity depends time of day
# KLG: makes three large matrices with an intercept column and a column for f1c, f2c, f1s, and f2s
# KLG: those 4 variables represent the Fourier series applied to the time vector
# KLG: these are identical matrices
#species 1, species 2 present
X_lam1_GNP1 <- model.matrix(~1, obs_covs_GNP)

#species 1, species 2 absent
X_lam2_GNP1 <- model.matrix(~1, obs_covs_GNP)

#species 2
X_lam3_GNP1 <- model.matrix(~1, obs_covs_GNP)

# Save model matrices for use elsewhere
save(X_f1_GNP1, X_f2_GNP1, X_f12_GNP1, X_lam1_GNP1, X_lam2_GNP1, X_lam3_GNP1, file='model_matrices_GNP1.Rdata')

# Indicator matrix to divide up single vector of parameters into
# subvectors for each parameter to estimate
#KLG: not sure I follow why the different values are used
#I THINK I NEED TO MAKE A NEW MATRIX FOR THIS EVERY RUN
pind_GNP1 <- matrix(NA, nrow=8, ncol=2) #KLG: makes an empty matrix with 8 rows and 2 columns
#KLG: each row is filled in individually
pind_GNP1[1,] <- c(0, 0+ncol(X_f1_GNP1)-1)                #f1, KLG: fills in first row
pind_GNP1[2,] <- c(pind_GNP1[1,2]+1, pind_GNP1[1,2]+1+ncol(X_f2_GNP1)-1)    #f2
pind_GNP1[3,] <- c(pind_GNP1[2,2]+1, pind_GNP1[2,2]+1+ncol(X_f12_GNP1)-1)   #f12
pind_GNP1[4,] <- c(pind_GNP1[3,2]+1, pind_GNP1[3,2]+2)                 #mu (species 1)
pind_GNP1[5,] <- c(pind_GNP1[4,2]+1, pind_GNP1[4,2]+2)                 #mu (species 2)
pind_GNP1[6,] <- c(pind_GNP1[5,2]+1, pind_GNP1[5,2]+1+ncol(X_lam1_GNP1)-1)  #lambda sp1|sp2 present
pind_GNP1[7,] <- c(pind_GNP1[6,2]+1, pind_GNP1[6,2]+1+ncol(X_lam2_GNP1)-1)  #lambda sp1|sp2 absent
pind_GNP1[8,] <- c(pind_GNP1[7,2]+1, pind_GNP1[7,2]+1+ncol(X_lam3_GNP1)-1)  #lambda sp2

# Optimization-----------------------------------------------------------------

set.seed(123)

# Quickly get reasonable start values with SANN
# Initial SANN NLL value should be 41111.166130
# KLG: I do not understand what SANN NLL is
# KLG: this is used to feed into the next optimization thing, it generates start values
#KLG: rep() replicates the values in x
starts_GNP1 <- optim(rep(0,max(pind_GNP1)+1), mmpp_covs, method = 'SANN',
                     control = list(maxit=400, trace=1, REPORT =5),
                     pind=pind_GNP1, X_f1=X_f1_GNP1, X_f2=X_f2_GNP1, X_f12=X_f12_GNP1, X_lam1=X_lam1_GNP1,
                     X_lam2=X_lam2_GNP1, X_lam3=X_lam3_GNP1, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                     yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                     yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)
# Final SANN NLL value should be 30050.156712
# KLG: ~12-15 min to run above code

# Do optimization and calculate hessian
#KLG: this took ~11 min to get to the first iteration value, 18 (total) for the second
#KLG: function optimization describes a class of problems for finding the input to a 
#KLG: given function that results in the minimum or maximum output from the function (default is minimizing)
#KLG: I'm struggling with what this is doing, but to break apart pieces I understand:
#KLG: maxit = max number of iterations, REPORT = frequency of reports
#KLG: hessian = TRUE -> return a numerically differentiated Hessian matrix
#KLG: mmpp_covs seems to be the function to be optimmized, with the parameters listed after the
#KLG: control variables (it's basically defined/established here)
#KLG: starts$par are the initial values for the parameters to be optimized over
#KLG: this method (L-BFGS-B) allows each variable to be given a lower and/or upper bound
#KLG: this took somewhere around 5-6 hours to run
fit_GNP1 <- optim(starts_GNP1$par, mmpp_covs, method = 'L-BFGS-B', hessian=TRUE,
                  control = list(trace = 1, REPORT = 5, maxit=400),
                  pind=pind_GNP1, X_f1=X_f1_GNP1, X_f2=X_f2_GNP1, X_f12=X_f12_GNP1, X_lam1=X_lam1_GNP1,
                  X_lam2=X_lam2_GNP1, X_lam3=X_lam3_GNP1, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                  yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                  yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)
# Final NLL value should be ~ 26461.08
# May take several runs of optimization to get past local minima to this value

#Format and save results
#KLG: saveRDS saves an R object for it to be called later (it serializes an R object into a 
#KLG: format that can be called later), but it forgets the original name of the object
saveRDS(fit_GNP1, "fit_covs3_GNP1.Rds")

est_GNP1 <- fit_GNP1$par 
names(est_GNP1) <- c(paste0("f1_",colnames(X_f1_GNP1)), paste0("f2_",colnames(X_f2_GNP1)),
                     paste0("f12_",colnames(X_f12_GNP1)),
                     "log_mu1[1]","log_mu1[2]","log_mu2[1]","log_mu2[2]",
                     paste0("loglam1_",colnames(X_lam1_GNP1)),
                     paste0("loglam2_",colnames(X_lam2_GNP1)), paste0("loglam3_",colnames(X_lam3_GNP1)))
se_GNP1 <- sqrt(diag(solve(fit_GNP1$hessian))) 

results_GNP1 <- data.frame(est_GNP1 = round(est_GNP1, 3), se_GNP1=round(se_GNP1,3))
results_GNP1$lower <- results_GNP1$est - 1.96*results_GNP1$se
results_GNP1$upper <- results_GNP1$est + 1.96*results_GNP1$se
results_GNP1

saveRDS(results_GNP1, 'results_GNP1.Rds')

#AIC
(2*length(fit_GNP1$par))-(2*-fit_GNP1$value)

# Construct model matrices-----------------------------------------------------
#I believe I need this for every time I run the model
#this is a mm5

# Occupancy natural parameters
#KLG: model.matrix creates a design (or model) matrix, e.g., by expanding factors to a set of 
#KLG: dummy variables (depending on the contrasts) and expanding interactions similarly.
#KLG: these are identical matrices
#I don't understand this well enough to know which one of my covariates to choose
X_f1_GNP5 <- model.matrix(~urema_dist_scaled, site_covs_GNP)
X_f2_GNP5 <- model.matrix(~urema_dist_scaled + termite.large.count.100m.scaled, site_covs_GNP)
X_f12_GNP5 <- model.matrix(~lion_latedry_scaled, site_covs_GNP) 

# Detection intensity depends time of day
# KLG: makes three large matrices with an intercept column and a column for f1c, f2c, f1s, and f2s
# KLG: those 4 variables represent the Fourier series applied to the time vector
# KLG: these are identical matrices
#species 1, species 2 present
X_lam1_GNP5 <- model.matrix(~cover.ground + detect.obscured, obs_covs_GNP)

#species 1, species 2 absent
X_lam2_GNP5 <- model.matrix(~cover.ground + detect.obscured, obs_covs_GNP)

#species 2
X_lam3_GNP5 <- model.matrix(~cover.ground + detect.obscured, obs_covs_GNP)

# Save model matrices for use elsewhere
save(X_f1_GNP5, X_f2_GNP5, X_f12_GNP5, X_lam1_GNP5, X_lam2_GNP5, X_lam3_GNP5, file='model_matrices_GNP5.Rdata')

# Indicator matrix to divide up single vector of parameters into
# subvectors for each parameter to estimate
#KLG: not sure I follow why the different values are used
#I THINK I NEED TO MAKE A NEW MATRIX FOR THIS EVERY RUN
pind_GNP5 <- matrix(NA, nrow=8, ncol=2) #KLG: makes an empty matrix with 8 rows and 2 columns
#KLG: each row is filled in individually
pind_GNP5[1,] <- c(0, 0+ncol(X_f1_GNP5)-1)                #f1, KLG: fills in first row
pind_GNP5[2,] <- c(pind_GNP5[1,2]+1, pind_GNP5[1,2]+1+ncol(X_f2_GNP5)-1)    #f2
pind_GNP5[3,] <- c(pind_GNP5[2,2]+1, pind_GNP5[2,2]+1+ncol(X_f12_GNP5)-1)   #f12
pind_GNP5[4,] <- c(pind_GNP5[3,2]+1, pind_GNP5[3,2]+2)                 #mu (species 1)
pind_GNP5[5,] <- c(pind_GNP5[4,2]+1, pind_GNP5[4,2]+2)                 #mu (species 2)
pind_GNP5[6,] <- c(pind_GNP5[5,2]+1, pind_GNP5[5,2]+1+ncol(X_lam1_GNP5)-1)  #lambda sp1|sp2 present
pind_GNP5[7,] <- c(pind_GNP5[6,2]+1, pind_GNP5[6,2]+1+ncol(X_lam2_GNP5)-1)  #lambda sp1|sp2 absent
pind_GNP5[8,] <- c(pind_GNP5[7,2]+1, pind_GNP5[7,2]+1+ncol(X_lam3_GNP5)-1)  #lambda sp2

# Optimization-----------------------------------------------------------------

set.seed(123)

# Quickly get reasonable start values with SANN
# Initial SANN NLL value should be 41111.166130
# KLG: I do not understand what SANN NLL is
# KLG: this is used to feed into the next optimization thing, it generates start values
#KLG: rep() replicates the values in x
starts_GNP5 <- optim(rep(0,max(pind_GNP5)+1), mmpp_covs, method = 'SANN',
                     control = list(maxit=400, trace=1, REPORT =5),
                     pind=pind_GNP5, X_f1=X_f1_GNP5, X_f2=X_f2_GNP5, X_f12=X_f12_GNP5, X_lam1=X_lam1_GNP5,
                     X_lam2=X_lam2_GNP5, X_lam3=X_lam3_GNP5, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                     yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                     yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)

# Do optimization and calculate hessian
#KLG: this took ~11 min to get to the first iteration value, 18 (total) for the second
#KLG: function optimization describes a class of problems for finding the input to a 
#KLG: given function that results in the minimum or maximum output from the function (default is minimizing)
#KLG: I'm struggling with what this is doing, but to break apart pieces I understand:
#KLG: maxit = max number of iterations, REPORT = frequency of reports
#KLG: hessian = TRUE -> return a numerically differentiated Hessian matrix
#KLG: mmpp_covs seems to be the function to be optimmized, with the parameters listed after the
#KLG: control variables (it's basically defined/established here)
#KLG: starts$par are the initial values for the parameters to be optimized over
#KLG: this method (L-BFGS-B) allows each variable to be given a lower and/or upper bound
#KLG: this took somewhere around 5-6 hours to run
fit_GNP5 <- optim(starts_GNP5$par, mmpp_covs, method = 'L-BFGS-B', hessian=TRUE,
                  control = list(trace = 1, REPORT = 5, maxit=400),
                  pind=pind_GNP5, X_f1=X_f1_GNP5, X_f2=X_f2_GNP5, X_f12=X_f12_GNP5, X_lam1=X_lam1_GNP5,
                  X_lam2=X_lam2_GNP5, X_lam3=X_lam3_GNP5, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                  yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                  yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)
# Final NLL value should be ~ 26461.08
# May take several runs of optimization to get past local minima to this value

#Format and save results
#KLG: saveRDS saves an R object for it to be called later (it serializes an R object into a 
#KLG: format that can be called later), but it forgets the original name of the object
saveRDS(fit_GNP5, "fit_covs3_GNP5.Rds")

est_GNP5 <- fit_GNP5$par 
names(est_GNP5) <- c(paste0("f1_",colnames(X_f1_GNP5)), paste0("f2_",colnames(X_f2_GNP5)),
                     paste0("f12_",colnames(X_f12_GNP5)),
                     "log_mu1[1]","log_mu1[2]","log_mu2[1]","log_mu2[2]",
                     paste0("loglam1_",colnames(X_lam1_GNP5)),
                     paste0("loglam2_",colnames(X_lam2_GNP5)), paste0("loglam3_",colnames(X_lam3_GNP5)))
se_GNP5 <- sqrt(diag(solve(fit_GNP5$hessian))) 

results_GNP5 <- data.frame(est = round(est_GNP5, 3), se = round(se_GNP5,3))
#results_GNP5 <- data.frame(est = round(est_GNP5, 3))
results_GNP5$lower <- results_GNP5$est - 1.96*results_GNP5$se
results_GNP5$upper <- results_GNP5$est + 1.96*results_GNP5$se
results_GNP5

saveRDS(results_GNP5, 'results_GNP5.Rds')

#AIC
(2*length(fit_GNP5$par))-(2*-fit_GNP5$value)

# Construct model matrices-----------------------------------------------------
#I believe I need this for every time I run the model
#this is a mm7

# Occupancy natural parameters
#KLG: model.matrix creates a design (or model) matrix, e.g., by expanding factors to a set of 
#KLG: dummy variables (depending on the contrasts) and expanding interactions similarly.
#KLG: these are identical matrices
#I don't understand this well enough to know which one of my covariates to choose
X_f1_GNP7 <- model.matrix(~urema_dist_scaled, site_covs_GNP)
X_f2_GNP7 <- model.matrix(~urema_dist_scaled + termite.large.count.100m.scaled, site_covs_GNP)
X_f12_GNP7 <- model.matrix(~lion_latedry_scaled, site_covs_GNP) 

# Detection intensity depends time of day
# KLG: makes three large matrices with an intercept column and a column for f1c, f2c, f1s, and f2s
# KLG: those 4 variables represent the Fourier series applied to the time vector
# KLG: these are identical matrices
#species 1, species 2 present
X_lam1_GNP7 <- model.matrix(~cover.ground + detect.obscured + TSL_GNP, obs_covs_GNP)

#species 1, species 2 absent
X_lam2_GNP7 <- model.matrix(~cover.ground + detect.obscured, obs_covs_GNP)

#species 2
X_lam3_GNP7 <- model.matrix(~cover.ground + detect.obscured, obs_covs_GNP)

# Save model matrices for use elsewhere
save(X_f1_GNP7, X_f2_GNP7, X_f12_GNP7, X_lam1_GNP7, X_lam2_GNP7, X_lam3_GNP7, file='model_matrices_GNP7.Rdata')

# Indicator matrix to divide up single vector of parameters into
# subvectors for each parameter to estimate
#KLG: not sure I follow why the different values are used
#I THINK I NEED TO MAKE A NEW MATRIX FOR THIS EVERY RUN
pind_GNP7 <- matrix(NA, nrow=8, ncol=2) #KLG: makes an empty matrix with 8 rows and 2 columns
#KLG: each row is filled in individually
pind_GNP7[1,] <- c(0, 0+ncol(X_f1_GNP7)-1)                #f1, KLG: fills in first row
pind_GNP7[2,] <- c(pind_GNP7[1,2]+1, pind_GNP7[1,2]+1+ncol(X_f2_GNP7)-1)    #f2
pind_GNP7[3,] <- c(pind_GNP7[2,2]+1, pind_GNP7[2,2]+1+ncol(X_f12_GNP7)-1)   #f12
pind_GNP7[4,] <- c(pind_GNP7[3,2]+1, pind_GNP7[3,2]+2)                 #mu (species 1)
pind_GNP7[5,] <- c(pind_GNP7[4,2]+1, pind_GNP7[4,2]+2)                 #mu (species 2)
pind_GNP7[6,] <- c(pind_GNP7[5,2]+1, pind_GNP7[5,2]+1+ncol(X_lam1_GNP7)-1)  #lambda sp1|sp2 present
pind_GNP7[7,] <- c(pind_GNP7[6,2]+1, pind_GNP7[6,2]+1+ncol(X_lam2_GNP7)-1)  #lambda sp1|sp2 absent
pind_GNP7[8,] <- c(pind_GNP7[7,2]+1, pind_GNP7[7,2]+1+ncol(X_lam3_GNP7)-1)  #lambda sp2

# Optimization-----------------------------------------------------------------

set.seed(123)

# Quickly get reasonable start values with SANN
# Initial SANN NLL value should be 41111.166130
# KLG: I do not understand what SANN NLL is
# KLG: this is used to feed into the next optimization thing, it generates start values
#KLG: rep() replicates the values in x
starts_GNP7 <- optim(rep(0,max(pind_GNP7)+1), mmpp_covs, method = 'SANN',
                     control = list(maxit=400, trace=1, REPORT =5),
                     pind=pind_GNP7, X_f1=X_f1_GNP7, X_f2=X_f2_GNP7, X_f12=X_f12_GNP7, X_lam1=X_lam1_GNP7,
                     X_lam2=X_lam2_GNP7, X_lam3=X_lam3_GNP7, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                     yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                     yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)
# Final SANN NLL value should be 30050.156712
# KLG: ~12-15 min to run above code

# Do optimization and calculate hessian
#KLG: this took ~11 min to get to the first iteration value, 18 (total) for the second
#KLG: function optimization describes a class of problems for finding the input to a 
#KLG: given function that results in the minimum or maximum output from the function (default is minimizing)
#KLG: I'm struggling with what this is doing, but to break apart pieces I understand:
#KLG: maxit = max number of iterations, REPORT = frequency of reports
#KLG: hessian = TRUE -> return a numerically differentiated Hessian matrix
#KLG: mmpp_covs seems to be the function to be optimmized, with the parameters listed after the
#KLG: control variables (it's basically defined/established here)
#KLG: starts$par are the initial values for the parameters to be optimized over
#KLG: this method (L-BFGS-B) allows each variable to be given a lower and/or upper bound
#KLG: this took somewhere around 5-6 hours to run
fit_GNP7 <- optim(starts_GNP7$par, mmpp_covs, method = 'L-BFGS-B', hessian=TRUE,
                  control = list(trace = 1, REPORT = 5, maxit=400),
                  pind=pind_GNP7, X_f1=X_f1_GNP7, X_f2=X_f2_GNP7, X_f12=X_f12_GNP7, X_lam1=X_lam1_GNP7,
                  X_lam2=X_lam2_GNP7, X_lam3=X_lam3_GNP7, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                  yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                  yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)
# Final NLL value should be ~ 26461.08
# May take several runs of optimization to get past local minima to this value

#Format and save results
#KLG: saveRDS saves an R object for it to be called later (it serializes an R object into a 
#KLG: format that can be called later), but it forgets the original name of the object
saveRDS(fit_GNP7, "fit_covs3_GNP7.Rds")

est_GNP7 <- fit_GNP7$par 
names(est_GNP7) <- c(paste0("f1_",colnames(X_f1_GNP7)), paste0("f2_",colnames(X_f2_GNP7)),
                     paste0("f12_",colnames(X_f12_GNP7)),
                     "log_mu1[1]","log_mu1[2]","log_mu2[1]","log_mu2[2]",
                     paste0("loglam1_",colnames(X_lam1_GNP7)),
                     paste0("loglam2_",colnames(X_lam2_GNP7)), paste0("loglam3_",colnames(X_lam3_GNP7)))
se_GNP7 <- sqrt(diag(solve(fit_GNP7$hessian))) 

results_GNP7 <- data.frame(est = round(est_GNP7, 3), se = round(se_GNP7,3))
#results_GNP7 <- data.frame(est_GNP7 = round(est_GNP7, 3))
results_GNP7$lower <- results_GNP7$est - 1.96*results_GNP7$se
results_GNP7$upper <- results_GNP7$est + 1.96*results_GNP7$se
results_GNP7

saveRDS(results_GNP7, 'results_GNP7.Rds')

#AIC
(2*length(fit_GNP7$par))-(2*-fit_GNP7$value)

# Construct model matrices-----------------------------------------------------
#I believe I need this for every time I run the model
#this is a mm9

# Occupancy natural parameters
#KLG: model.matrix creates a design (or model) matrix, e.g., by expanding factors to a set of 
#KLG: dummy variables (depending on the contrasts) and expanding interactions similarly.
#KLG: these are identical matrices
#I don't understand this well enough to know which one of my covariates to choose
X_f1_GNP9 <- model.matrix(~urema_dist_scaled, site_covs_GNP)
X_f2_GNP9 <- model.matrix(~urema_dist_scaled + termite.large.count.100m.scaled, site_covs_GNP)
X_f12_GNP9 <- model.matrix(~lion_latedry_scaled, site_covs_GNP) 

# Detection intensity depends time of day
# KLG: makes three large matrices with an intercept column and a column for f1c, f2c, f1s, and f2s
# KLG: those 4 variables represent the Fourier series applied to the time vector
# KLG: these are identical matrices
#species 1, species 2 present
X_lam1_GNP9 <- model.matrix(~cover.ground + detect.obscured + f1c_GNP + f2c_GNP + f1s_GNP + f2s_GNP, obs_covs_GNP)

#species 1, species 2 absent
X_lam2_GNP9 <- model.matrix(~cover.ground + detect.obscured + f1c_GNP + f2c_GNP + f1s_GNP + f2s_GNP, obs_covs_GNP)

#species 2
X_lam3_GNP9 <- model.matrix(~cover.ground + detect.obscured + f1c_GNP + f2c_GNP + f1s_GNP + f2s_GNP, obs_covs_GNP)

# Save model matrices for use elsewhere
save(X_f1_GNP9, X_f2_GNP9, X_f12_GNP9, X_lam1_GNP9, X_lam2_GNP9, X_lam3_GNP9, file='model_matrices_GNP9.Rdata')

# Indicator matrix to divide up single vector of parameters into
# subvectors for each parameter to estimate
#KLG: not sure I follow why the different values are used
#I THINK I NEED TO MAKE A NEW MATRIX FOR THIS EVERY RUN
pind_GNP9 <- matrix(NA, nrow=8, ncol=2) #KLG: makes an empty matrix with 8 rows and 2 columns
#KLG: each row is filled in individually
pind_GNP9[1,] <- c(0, 0+ncol(X_f1_GNP9)-1)                #f1, KLG: fills in first row
pind_GNP9[2,] <- c(pind_GNP9[1,2]+1, pind_GNP9[1,2]+1+ncol(X_f2_GNP9)-1)    #f2
pind_GNP9[3,] <- c(pind_GNP9[2,2]+1, pind_GNP9[2,2]+1+ncol(X_f12_GNP9)-1)   #f12
pind_GNP9[4,] <- c(pind_GNP9[3,2]+1, pind_GNP9[3,2]+2)                 #mu (species 1)
pind_GNP9[5,] <- c(pind_GNP9[4,2]+1, pind_GNP9[4,2]+2)                 #mu (species 2)
pind_GNP9[6,] <- c(pind_GNP9[5,2]+1, pind_GNP9[5,2]+1+ncol(X_lam1_GNP9)-1)  #lambda sp1|sp2 present
pind_GNP9[7,] <- c(pind_GNP9[6,2]+1, pind_GNP9[6,2]+1+ncol(X_lam2_GNP9)-1)  #lambda sp1|sp2 absent
pind_GNP9[8,] <- c(pind_GNP9[7,2]+1, pind_GNP9[7,2]+1+ncol(X_lam3_GNP9)-1)  #lambda sp2

# Optimization-----------------------------------------------------------------

set.seed(123)

# Quickly get reasonable start values with SANN
# Initial SANN NLL value should be 41111.166130
# KLG: I do not understand what SANN NLL is
# KLG: this is used to feed into the next optimization thing, it generates start values
#KLG: rep() replicates the values in x
starts_GNP9 <- optim(rep(0,max(pind_GNP9)+1), mmpp_covs, method = 'SANN',
                     control = list(maxit=400, trace=1, REPORT =5),
                     pind=pind_GNP9, X_f1=X_f1_GNP9, X_f2=X_f2_GNP9, X_f12=X_f12_GNP9, X_lam1=X_lam1_GNP9,
                     X_lam2=X_lam2_GNP9, X_lam3=X_lam3_GNP9, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                     yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                     yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)
# Final SANN NLL value should be 30050.156712
# KLG: ~12-15 min to run above code

# Do optimization and calculate hessian
#KLG: this took ~11 min to get to the first iteration value, 18 (total) for the second
#KLG: function optimization describes a class of problems for finding the input to a 
#KLG: given function that results in the minimum or maximum output from the function (default is minimizing)
#KLG: I'm struggling with what this is doing, but to break apart pieces I understand:
#KLG: maxit = max number of iterations, REPORT = frequency of reports
#KLG: hessian = TRUE -> return a numerically differentiated Hessian matrix
#KLG: mmpp_covs seems to be the function to be optimmized, with the parameters listed after the
#KLG: control variables (it's basically defined/established here)
#KLG: starts$par are the initial values for the parameters to be optimized over
#KLG: this method (L-BFGS-B) allows each variable to be given a lower and/or upper bound
#KLG: this took somewhere around 5-6 hours to run
fit_GNP9 <- optim(starts_GNP9$par, mmpp_covs, method = 'L-BFGS-B', hessian=TRUE,
                  control = list(trace = 1, REPORT = 5, maxit=400),
                  pind=pind_GNP9, X_f1=X_f1_GNP9, X_f2=X_f2_GNP9, X_f12=X_f12_GNP9, X_lam1=X_lam1_GNP9,
                  X_lam2=X_lam2_GNP9, X_lam3=X_lam3_GNP9, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                  yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                  yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)
# Final NLL value should be ~ 26461.08
# May take several runs of optimization to get past local minima to this value

#Format and save results
#KLG: saveRDS saves an R object for it to be called later (it serializes an R object into a 
#KLG: format that can be called later), but it forgets the original name of the object
saveRDS(fit_GNP9, "fit_covs3_GNP9.Rds")

est_GNP9 <- fit_GNP9$par 
names(est_GNP9) <- c(paste0("f1_",colnames(X_f1_GNP9)), paste0("f2_",colnames(X_f2_GNP9)),
                     paste0("f12_",colnames(X_f12_GNP9)),
                     "log_mu1[1]","log_mu1[2]","log_mu2[1]","log_mu2[2]",
                     paste0("loglam1_",colnames(X_lam1_GNP9)),
                     paste0("loglam2_",colnames(X_lam2_GNP9)), paste0("loglam3_",colnames(X_lam3_GNP9)))
se_GNP9 <- sqrt(diag(solve(fit_GNP9$hessian))) 

results_GNP9 <- data.frame(est = round(est_GNP9, 3), se = round(se_GNP9,3))
#results_GNP9 <- data.frame(est_GNP9 = round(est_GNP9, 3))
results_GNP9$lower <- results_GNP9$est - 1.96*results_GNP9$se
results_GNP9$upper <- results_GNP9$est + 1.96*results_GNP9$se
results_GNP9

saveRDS(results_GNP9, 'results_GNP9.Rds')

#AIC
(2*length(fit_GNP9$par))-(2*-fit_GNP9$value)

# Construct model matrices-----------------------------------------------------
#I believe I need this for every time I run the model
#this is a null model with dependence

# Occupancy natural parameters
#KLG: model.matrix creates a design (or model) matrix, e.g., by expanding factors to a set of 
#KLG: dummy variables (depending on the contrasts) and expanding interactions similarly.
#KLG: these are identical matrices
#I don't understand this well enough to know which one of my covariates to choose
X_f1_GNP3 <- model.matrix(~1, site_covs_GNP)
X_f2_GNP3 <- model.matrix(~1, site_covs_GNP)
X_f12_GNP3 <- model.matrix(~1, site_covs_GNP) 

# Detection intensity depends time of day
# KLG: makes three large matrices with an intercept column and a column for f1c, f2c, f1s, and f2s
# KLG: those 4 variables represent the Fourier series applied to the time vector
# KLG: these are identical matrices
#species 1, species 2 present
X_lam1_GNP3 <- model.matrix(~cover.ground + detect.obscured, obs_covs_GNP)

#species 1, species 2 absent
X_lam2_GNP3 <- model.matrix(~cover.ground + detect.obscured, obs_covs_GNP)

#species 2
X_lam3_GNP3 <- model.matrix(~cover.ground + detect.obscured, obs_covs_GNP)

# Save model matrices for use elsewhere
save(X_f1_GNP3, X_f2_GNP3, X_f12_GNP3, X_lam1_GNP3, X_lam2_GNP3, X_lam3_GNP3, file='model_matrices_GNP3.Rdata')

# Indicator matrix to divide up single vector of parameters into
# subvectors for each parameter to estimate
#KLG: not sure I follow why the different values are used
#I THINK I NEED TO MAKE A NEW MATRIX FOR THIS EVERY RUN
pind_GNP3 <- matrix(NA, nrow=8, ncol=2) #KLG: makes an empty matrix with 8 rows and 2 columns
#KLG: each row is filled in individually
pind_GNP3[1,] <- c(0, 0+ncol(X_f1_GNP3)-1)                #f1, KLG: fills in first row
pind_GNP3[2,] <- c(pind_GNP3[1,2]+1, pind_GNP3[1,2]+1+ncol(X_f2_GNP3)-1)    #f2
pind_GNP3[3,] <- c(pind_GNP3[2,2]+1, pind_GNP3[2,2]+1+ncol(X_f12_GNP3)-1)   #f12
pind_GNP3[4,] <- c(pind_GNP3[3,2]+1, pind_GNP3[3,2]+2)                 #mu (species 1)
pind_GNP3[5,] <- c(pind_GNP3[4,2]+1, pind_GNP3[4,2]+2)                 #mu (species 2)
pind_GNP3[6,] <- c(pind_GNP3[5,2]+1, pind_GNP3[5,2]+1+ncol(X_lam1_GNP3)-1)  #lambda sp1|sp2 present
pind_GNP3[7,] <- c(pind_GNP3[6,2]+1, pind_GNP3[6,2]+1+ncol(X_lam2_GNP3)-1)  #lambda sp1|sp2 absent
pind_GNP3[8,] <- c(pind_GNP3[7,2]+1, pind_GNP3[7,2]+1+ncol(X_lam3_GNP3)-1)  #lambda sp2

# Optimization-----------------------------------------------------------------

set.seed(123)

# Quickly get reasonable start values with SANN
# Initial SANN NLL value should be 41111.166130
# KLG: I do not understand what SANN NLL is
# KLG: this is used to feed into the next optimization thing, it generates start values
#KLG: rep() replicates the values in x
starts_GNP3 <- optim(rep(0,max(pind_GNP3)+1), mmpp_covs, method = 'SANN',
                     control = list(maxit=400, trace=1, REPORT =5),
                     pind=pind_GNP3, X_f1=X_f1_GNP3, X_f2=X_f2_GNP3, X_f12=X_f12_GNP3, X_lam1=X_lam1_GNP3,
                     X_lam2=X_lam2_GNP3, X_lam3=X_lam3_GNP3, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                     yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                     yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)
# Final SANN NLL value should be 30050.156712
# KLG: ~12-15 min to run above code

# Do optimization and calculate hessian
#KLG: this took ~11 min to get to the first iteration value, 18 (total) for the second
#KLG: function optimization describes a class of problems for finding the input to a 
#KLG: given function that results in the minimum or maximum output from the function (default is minimizing)
#KLG: I'm struggling with what this is doing, but to break apart pieces I understand:
#KLG: maxit = max number of iterations, REPORT = frequency of reports
#KLG: hessian = TRUE -> return a numerically differentiated Hessian matrix
#KLG: mmpp_covs seems to be the function to be optimmized, with the parameters listed after the
#KLG: control variables (it's basically defined/established here)
#KLG: starts$par are the initial values for the parameters to be optimized over
#KLG: this method (L-BFGS-B) allows each variable to be given a lower and/or upper bound
#KLG: this took somewhere around 5-6 hours to run
fit_GNP3 <- optim(starts_GNP3$par, mmpp_covs, method = 'L-BFGS-B', hessian=TRUE,
                  control = list(trace = 1, REPORT = 5, maxit=400),
                  pind=pind_GNP3, X_f1=X_f1_GNP3, X_f2=X_f2_GNP3, X_f12=X_f12_GNP3, X_lam1=X_lam1_GNP3,
                  X_lam2=X_lam2_GNP3, X_lam3=X_lam3_GNP3, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                  yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                  yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)
# Final NLL value should be ~ 26461.08
# May take several runs of optimization to get past local minima to this value

#Format and save results
#KLG: saveRDS saves an R object for it to be called later (it serializes an R object into a 
#KLG: format that can be called later), but it forgets the original name of the object
saveRDS(fit_GNP3, "fit_covs3_GNP3.Rds")

est_GNP3 <- fit_GNP3$par 
names(est_GNP3) <- c(paste0("f1_",colnames(X_f1_GNP3)), paste0("f2_",colnames(X_f2_GNP3)),
                     paste0("f12_",colnames(X_f12_GNP3)),
                     "log_mu1[1]","log_mu1[2]","log_mu2[1]","log_mu2[2]",
                     paste0("loglam1_",colnames(X_lam1_GNP3)),
                     paste0("loglam2_",colnames(X_lam2_GNP3)), paste0("loglam3_",colnames(X_lam3_GNP3)))
se_GNP3 <- sqrt(diag(solve(fit_GNP3$hessian))) 

results_GNP3 <- data.frame(est_GNP3 = round(est_GNP3, 3))
results_GNP1$lower <- results_GNP1$est - 1.96*results_GNP1$se
results_GNP1$upper <- results_GNP1$est + 1.96*results_GNP1$se
results_GNP1

saveRDS(results_GNP3, 'results_GNP3.Rds')

#AIC
(2*length(fit_GNP3$par))-(2*-fit_GNP3$value)

# Construct model matrices-----------------------------------------------------
#this is mm13

# Occupancy natural parameters
#KLG: model.matrix creates a design (or model) matrix, e.g., by expanding factors to a set of 
#KLG: dummy variables (depending on the contrasts) and expanding interactions similarly.
#KLG: these are identical matrices
#I don't understand this well enough to know which one of my covariates to choose
X_f1_GNP13 <- model.matrix(~urema_dist_scaled, site_covs_GNP)
X_f2_GNP13 <- model.matrix(~urema_dist_scaled + termite.large.count.100m.scaled, site_covs_GNP)
X_f12_GNP13 <- model.matrix(~lion_latedry_scaled, site_covs_GNP)

# Detection intensity depends time of day
# KLG: makes three large matrices with an intercept column and a column for f1c, f2c, f1s, and f2s
# KLG: those 4 variables represent the Fourier series applied to the time vector
# KLG: these are identical matrices
#species 1, species 2 present
X_lam1_GNP13 <- model.matrix(~f1c_GNP + f2c_GNP + f1s_GNP + f2s_GNP + detect.obscured + cover.ground + TSL_GNP + lake, obs_covs_GNP)

#species 1, species 2 absent
X_lam2_GNP13 <- model.matrix(~f1c_GNP + f2c_GNP + f1s_GNP + f2s_GNP + detect.obscured + cover.ground + lake, obs_covs_GNP)

#species 2
X_lam3_GNP13 <- model.matrix(~f1c_GNP + f2c_GNP + f1s_GNP + f2s_GNP + detect.obscured + cover.ground + lake, obs_covs_GNP)

# Save model matrices for use elsewhere
save(X_f1_GNP13, X_f2_GNP13, X_f12_GNP13, X_lam1_GNP13, X_lam2_GNP13, X_lam3_GNP13, file='model_matrices_GNP13.Rdata')

# Indicator matrix to divide up single vector of parameters into
# subvectors for each parameter to estimate
#KLG: not sure I follow why the different values are used
pind_GNP13 <- matrix(NA, nrow=8, ncol=2) #KLG: makes an empty matrix with 8 rows and 2 columns
#KLG: each row is filled in individually
pind_GNP13[1,] <- c(0, 0+ncol(X_f1_GNP13)-1)                #f1, KLG: fills in first row
pind_GNP13[2,] <- c(pind_GNP13[1,2]+1, pind_GNP13[1,2]+1+ncol(X_f2_GNP13)-1)    #f2
pind_GNP13[3,] <- c(pind_GNP13[2,2]+1, pind_GNP13[2,2]+1+ncol(X_f12_GNP13)-1)   #f12
pind_GNP13[4,] <- c(pind_GNP13[3,2]+1, pind_GNP13[3,2]+2)                 #mu (species 1)
pind_GNP13[5,] <- c(pind_GNP13[4,2]+1, pind_GNP13[4,2]+2)                 #mu (species 2)
pind_GNP13[6,] <- c(pind_GNP13[5,2]+1, pind_GNP13[5,2]+1+ncol(X_lam1_GNP13)-1)  #lambda sp1|sp2 present
pind_GNP13[7,] <- c(pind_GNP13[6,2]+1, pind_GNP13[6,2]+1+ncol(X_lam2_GNP13)-1)  #lambda sp1|sp2 absent
pind_GNP13[8,] <- c(pind_GNP13[7,2]+1, pind_GNP13[7,2]+1+ncol(X_lam3_GNP13)-1)  #lambda sp2

# Optimization-----------------------------------------------------------------

set.seed(123)

# Quickly get reasonable start values with SANN
# Initial SANN NLL value should be 41111.166130
# KLG: I do not understand what SANN NLL is
# KLG: this is used to feed into the next optimization thing, it generates start values
#KLG: rep() replicates the values in x
starts_GNP13 <- optim(rep(0,max(pind_GNP13)+1), mmpp_covs, method = 'SANN',
                      control = list(maxit=400, trace=1, REPORT =5),
                      pind=pind_GNP13, X_f1=X_f1_GNP13, X_f2=X_f2_GNP13, X_f12=X_f12_GNP13, X_lam1=X_lam1_GNP13,
                      X_lam2=X_lam2_GNP13, X_lam3=X_lam3_GNP13, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                      yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                      yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)
# Final SANN NLL value should be 30050.156712
# KLG: ~12-15 min to run above code

# Do optimization and calculate hessian
#KLG: this took ~11 min to get to the first iteration value, 18 (total) for the second
#KLG: function optimization describes a class of problems for finding the input to a 
#KLG: given function that results in the minimum or maximum output from the function (default is minimizing)
#KLG: I'm struggling with what this is doing, but to break apart pieces I understand:
#KLG: maxit = max number of iterations, REPORT = frequency of reports
#KLG: hessian = TRUE -> return a numerically differentiated Hessian matrix
#KLG: mmpp_covs seems to be the function to be optimmized, with the parameters listed after the
#KLG: control variables (it's basically defined/established here)
#KLG: starts$par are the initial values for the parameters to be optimized over
#KLG: this method (L-BFGS-B) allows each variable to be given a lower and/or upper bound
#KLG: this took somewhere around 5-6 hours to run
fit_GNP13 <- optim(starts_GNP13$par, mmpp_covs, method = 'L-BFGS-B', hessian=TRUE,
                   control = list(trace = 1, REPORT = 5, maxit=400),
                   pind=pind_GNP13, X_f1=X_f1_GNP13, X_f2=X_f2_GNP13, X_f12=X_f12_GNP13, X_lam1=X_lam1_GNP13,
                   X_lam2=X_lam2_GNP13, X_lam3=X_lam3_GNP13, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                   yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                   yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)
# Final NLL value should be ~ 26461.08
# May take several runs of optimization to get past local minima to this value

#Format and save results
#KLG: saveRDS saves an R object for it to be called later (it serializes an R object into a 
#KLG: format that can be called later), but it forgets the original name of the object
saveRDS(fit_GNP13, "fit_covs3_GNP13.Rds")

est_GNP13 <- fit_GNP13$par 
names(est_GNP13) <- c(paste0("f1_",colnames(X_f1_GNP13)), paste0("f2_",colnames(X_f2_GNP13)),
                      paste0("f12_",colnames(X_f12_GNP13)),
                      "log_mu1[1]","log_mu1[2]","log_mu2[1]","log_mu2[2]",
                      paste0("loglam1_",colnames(X_lam1_GNP13)),
                      paste0("loglam2_",colnames(X_lam2_GNP13)), paste0("loglam3_",colnames(X_lam3_GNP13)))
se_GNP13 <- sqrt(diag(solve(fit_GNP13$hessian))) 
#this throws an error
#Error in solve.default(fit_GNP$hessian) : 
#Lapack routine dgesv: system is exactly singular: U[16,16] = 0
#there are a bunch of zeros here, maybe that's why?
#the matrix (fit_GNP$hessian) is considered singular, its determinant is 0, there is no inverse
#if it's just for the se, maybe not a big issue? I'm not sure
#note: This only happens when matrix is singular or when it's singular on your 
#machine (due to approximation you can have a really small number be considered 0)
#could be singular bc the rows are collinear
#all the numbers in this matrix are close to 0, which I'm thinking is the problem
#my best guess is that's because it's a relatively small data set without a crazy number of detections

#I can't run the last few lines because I don't have the se
#results_GNP <- data.frame(est_GNP = round(est_GNP, 3), se=round(se,3))
results_GNP13 <- data.frame(est_GNP13 = round(est_GNP13, 3))
results$lower <- results$est - 1.96*results$se
results$upper <- results$est + 1.96*results$se
results_GNP

hessian_GNP13 <- fit_GNP13$hessian

saveRDS(results_GNP13, 'results_GNP13.Rds')

#AIC
(2*length(fit_GNP13$par))-(2*-fit_GNP13$value)

# Construct model matrices-----------------------------------------------------
#this is trying without detect.obscured

# Occupancy natural parameters
#KLG: model.matrix creates a design (or model) matrix, e.g., by expanding factors to a set of 
#KLG: dummy variables (depending on the contrasts) and expanding interactions similarly.
#KLG: these are identical matrices
#I don't understand this well enough to know which one of my covariates to choose
X_f1_GNP3.1 <- model.matrix(~1, site_covs_GNP)
X_f2_GNP3.1 <- model.matrix(~1, site_covs_GNP)
X_f12_GNP3.1 <- model.matrix(~1, site_covs_GNP)

# Detection intensity depends time of day
# KLG: makes three large matrices with an intercept column and a column for f1c, f2c, f1s, and f2s
# KLG: those 4 variables represent the Fourier series applied to the time vector
# KLG: these are identical matrices
#species 1, species 2 present
X_lam1_GNP3.1 <- model.matrix(~cover.ground, obs_covs_GNP)

#species 1, species 2 absent
X_lam2_GNP3.1 <- model.matrix(~cover.ground, obs_covs_GNP)

#species 2
X_lam3_GNP3.1 <- model.matrix(~cover.ground, obs_covs_GNP)

# Save model matrices for use elsewhere
save(X_f1_GNP3.1, X_f2_GNP3.1, X_f12_GNP3.1, X_lam1_GNP3.1, X_lam2_GNP3.1, X_lam3_GNP3.1, file='model_matrices_GNP3.1.Rdata')

# Indicator matrix to divide up single vector of parameters into
# subvectors for each parameter to estimate
#KLG: not sure I follow why the different values are used
pind_GNP3.1 <- matrix(NA, nrow=8, ncol=2) #KLG: makes an empty matrix with 8 rows and 2 columns
#KLG: each row is filled in individually
pind_GNP3.1[1,] <- c(0, 0+ncol(X_f1_GNP3.1)-1)                #f1, KLG: fills in first row
pind_GNP3.1[2,] <- c(pind_GNP3.1[1,2]+1, pind_GNP3.1[1,2]+1+ncol(X_f2_GNP3.1)-1)    #f2
pind_GNP3.1[3,] <- c(pind_GNP3.1[2,2]+1, pind_GNP3.1[2,2]+1+ncol(X_f12_GNP3.1)-1)   #f12
pind_GNP3.1[4,] <- c(pind_GNP3.1[3,2]+1, pind_GNP3.1[3,2]+2)                 #mu (species 1)
pind_GNP3.1[5,] <- c(pind_GNP3.1[4,2]+1, pind_GNP3.1[4,2]+2)                 #mu (species 2)
pind_GNP3.1[6,] <- c(pind_GNP3.1[5,2]+1, pind_GNP3.1[5,2]+1+ncol(X_lam1_GNP3.1)-1)  #lambda sp1|sp2 present
pind_GNP3.1[7,] <- c(pind_GNP3.1[6,2]+1, pind_GNP3.1[6,2]+1+ncol(X_lam2_GNP3.1)-1)  #lambda sp1|sp2 absent
pind_GNP3.1[8,] <- c(pind_GNP3.1[7,2]+1, pind_GNP3.1[7,2]+1+ncol(X_lam3_GNP3.1)-1)  #lambda sp2

# Optimization-----------------------------------------------------------------

set.seed(123)

# Quickly get reasonable start values with SANN
# Initial SANN NLL value should be 41111.166130
# KLG: I do not understand what SANN NLL is
# KLG: this is used to feed into the next optimization thing, it generates start values
#KLG: rep() replicates the values in x
starts_GNP3.1 <- optim(rep(0,max(pind_GNP3.1)+1), mmpp_covs, method = 'SANN',
                       control = list(maxit=400, trace=1, REPORT =5),
                       pind=pind_GNP3.1, X_f1=X_f1_GNP3.1, X_f2=X_f2_GNP3.1, X_f12=X_f12_GNP3.1, X_lam1=X_lam1_GNP3.1,
                       X_lam2=X_lam2_GNP3.1, X_lam3=X_lam3_GNP3.1, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                       yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                       yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)
# Final SANN NLL value should be 30050.156712
# KLG: ~12-15 min to run above code

# Do optimization and calculate hessian
#KLG: this took ~11 min to get to the first iteration value, 18 (total) for the second
#KLG: function optimization describes a class of problems for finding the input to a 
#KLG: given function that results in the minimum or maximum output from the function (default is minimizing)
#KLG: I'm struggling with what this is doing, but to break apart pieces I understand:
#KLG: maxit = max number of iterations, REPORT = frequency of reports
#KLG: hessian = TRUE -> return a numerically differentiated Hessian matrix
#KLG: mmpp_covs seems to be the function to be optimmized, with the parameters listed after the
#KLG: control variables (it's basically defined/established here)
#KLG: starts$par are the initial values for the parameters to be optimized over
#KLG: this method (L-BFGS-B) allows each variable to be given a lower and/or upper bound
#KLG: this took somewhere around 5-6 hours to run
fit_GNP3.1 <- optim(starts_GNP3.1$par, mmpp_covs, method = 'L-BFGS-B', hessian=TRUE,
                    control = list(trace = 1, REPORT = 5, maxit=400),
                    pind=pind_GNP3.1, X_f1=X_f1_GNP3.1, X_f2=X_f2_GNP3.1, X_f12=X_f12_GNP3.1, X_lam1=X_lam1_GNP3.1,
                    X_lam2=X_lam2_GNP3.1, X_lam3=X_lam3_GNP3.1, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                    yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                    yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)
# Final NLL value should be ~ 26461.08
# May take several runs of optimization to get past local minima to this value

#Format and save results
#KLG: saveRDS saves an R object for it to be called later (it serializes an R object into a 
#KLG: format that can be called later), but it forgets the original name of the object
saveRDS(fit_GNP3.1, "fit_covs3_GNP3.1.Rds")

est_GNP3.1 <- fit_GNP3.1$par 
names(est_GNP3.1) <- c(paste0("f1_",colnames(X_f1_GNP3.1)), paste0("f2_",colnames(X_f2_GNP3.1)),
                       paste0("f12_",colnames(X_f12_GNP3.1)),
                       "log_mu1[1]","log_mu1[2]","log_mu2[1]","log_mu2[2]",
                       paste0("loglam1_",colnames(X_lam1_GNP3.1)),
                       paste0("loglam2_",colnames(X_lam2_GNP3.1)), paste0("loglam3_",colnames(X_lam3_GNP3.1)))
se_GNP3.1 <- sqrt(diag(solve(fit_GNP3.1$hessian))) 

#I can't run the last few lines because I don't have the se
results_GNP3.1 <- data.frame(est_GNP = round(est_GNP3.1, 3), se=round(se_GNP3.1,3))
results_GNP3.1$lower <- results_GNP3.1$est - 1.96*results_GNP3.1$se
results_GNP3.1$upper <- results_GNP3.1$est + 1.96*results_GNP3.1$se
results_GNP3.1

saveRDS(results_GNP3.1, 'results_GNP3.1.Rds')

#AIC
(2*length(fit_GNP3.1$par))-(2*-fit_GNP3.1$value)

# Construct model matrices-----------------------------------------------------
#I believe I need this for every time I run the model
#I may not rerun this for every one, but this is mm3.2 (trying with scaled detect.obscured)

# Occupancy natural parameters
#KLG: model.matrix creates a design (or model) matrix, e.g., by expanding factors to a set of 
#KLG: dummy variables (depending on the contrasts) and expanding interactions similarly.
#KLG: these are identical matrices
#I don't understand this well enough to know which one of my covariates to choose
X_f1_GNP3.2 <- model.matrix(~1, site_covs_GNP)
X_f2_GNP3.2 <- model.matrix(~1, site_covs_GNP)
X_f12_GNP3.2 <- model.matrix(~1, site_covs_GNP) 

# Detection intensity depends time of day
# KLG: makes three large matrices with an intercept column and a column for f1c, f2c, f1s, and f2s
# KLG: those 4 variables represent the Fourier series applied to the time vector
# KLG: these are identical matrices
#species 1, species 2 present
X_lam1_GNP3.2 <- model.matrix(~cover.ground + detect.obscured, obs_covs_GNP)

#species 1, species 2 absent
X_lam2_GNP3.2 <- model.matrix(~cover.ground + detect.obscured, obs_covs_GNP)

#species 2
X_lam3_GNP3.2 <- model.matrix(~cover.ground + detect.obscured, obs_covs_GNP)

# Save model matrices for use elsewhere
save(X_f1_GNP3.2, X_f2_GNP3.2, X_f12_GNP3.2, X_lam1_GNP3.2, X_lam2_GNP3.2, X_lam3_GNP3.2, file='model_matrices_GNP3.2.Rdata')

# Indicator matrix to divide up single vector of parameters into
# subvectors for each parameter to estimate
#KLG: not sure I follow why the different values are used
#I THINK I NEED TO MAKE A NEW MATRIX FOR THIS EVERY RUN
pind_GNP3.2 <- matrix(NA, nrow=8, ncol=2) #KLG: makes an empty matrix with 8 rows and 2 columns
#KLG: each row is filled in individually
pind_GNP3.2[1,] <- c(0, 0+ncol(X_f1_GNP3.2)-1)                #f1, KLG: fills in first row
pind_GNP3.2[2,] <- c(pind_GNP3.2[1,2]+1, pind_GNP3.2[1,2]+1+ncol(X_f2_GNP3.2)-1)    #f2
pind_GNP3.2[3,] <- c(pind_GNP3.2[2,2]+1, pind_GNP3.2[2,2]+1+ncol(X_f12_GNP3.2)-1)   #f12
pind_GNP3.2[4,] <- c(pind_GNP3.2[3,2]+1, pind_GNP3.2[3,2]+2)                 #mu (species 1)
pind_GNP3.2[5,] <- c(pind_GNP3.2[4,2]+1, pind_GNP3.2[4,2]+2)                 #mu (species 2)
pind_GNP3.2[6,] <- c(pind_GNP3.2[5,2]+1, pind_GNP3.2[5,2]+1+ncol(X_lam1_GNP3.2)-1)  #lambda sp1|sp2 present
pind_GNP3.2[7,] <- c(pind_GNP3.2[6,2]+1, pind_GNP3.2[6,2]+1+ncol(X_lam2_GNP3.2)-1)  #lambda sp1|sp2 absent
pind_GNP3.2[8,] <- c(pind_GNP3.2[7,2]+1, pind_GNP3.2[7,2]+1+ncol(X_lam3_GNP3.2)-1)  #lambda sp2

# Optimization-----------------------------------------------------------------

set.seed(123)

# Quickly get reasonable start values with SANN
# Initial SANN NLL value should be 41111.166130
# KLG: I do not understand what SANN NLL is
# KLG: this is used to feed into the next optimization thing, it generates start values
#KLG: rep() replicates the values in x
starts_GNP3.2 <- optim(rep(0,max(pind_GNP3.2)+1), mmpp_covs, method = 'SANN',
                       control = list(maxit=400, trace=1, REPORT =5),
                       pind=pind_GNP3.2, X_f1=X_f1_GNP3.2, X_f2=X_f2_GNP3.2, X_f12=X_f12_GNP3.2, X_lam1=X_lam1_GNP3.2,
                       X_lam2=X_lam2_GNP3.2, X_lam3=X_lam3_GNP3.2, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                       yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                       yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)
# Final SANN NLL value should be 30050.156712
# KLG: ~12-15 min to run above code

# Do optimization and calculate hessian
#KLG: this took ~11 min to get to the first iteration value, 18 (total) for the second
#KLG: function optimization describes a class of problems for finding the input to a 
#KLG: given function that results in the minimum or maximum output from the function (default is minimizing)
#KLG: I'm struggling with what this is doing, but to break apart pieces I understand:
#KLG: maxit = max number of iterations, REPORT = frequency of reports
#KLG: hessian = TRUE -> return a numerically differentiated Hessian matrix
#KLG: mmpp_covs seems to be the function to be optimmized, with the parameters listed after the
#KLG: control variables (it's basically defined/established here)
#KLG: starts$par are the initial values for the parameters to be optimized over
#KLG: this method (L-BFGS-B) allows each variable to be given a lower and/or upper bound
#KLG: this took somewhere around 5-6 hours to run
fit_GNP3.2 <- optim(starts_GNP3.2$par, mmpp_covs, method = 'L-BFGS-B', hessian=TRUE,
                    control = list(trace = 1, REPORT = 5, maxit=400),
                    pind=pind_GNP3.2, X_f1=X_f1_GNP3.2, X_f2=X_f2_GNP3.2, X_f12=X_f12_GNP3.2, X_lam1=X_lam1_GNP3.2,
                    X_lam2=X_lam2_GNP3.2, X_lam3=X_lam3_GNP3.2, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                    yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                    yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)
# Final NLL value should be ~ 26461.08
# May take several runs of optimization to get past local minima to this value

#Format and save results
#KLG: saveRDS saves an R object for it to be called later (it serializes an R object into a 
#KLG: format that can be called later), but it forgets the original name of the object
saveRDS(fit_GNP3.2, "fit_covs3_GNP3.2.Rds")

est_GNP3.2 <- fit_GNP3.2$par 
names(est_GNP3.2) <- c(paste0("f1_",colnames(X_f1_GNP3.2)), paste0("f2_",colnames(X_f2_GNP3.2)),
                       paste0("f12_",colnames(X_f12_GNP3.2)),
                       "log_mu1[1]","log_mu1[2]","log_mu2[1]","log_mu2[2]",
                       paste0("loglam1_",colnames(X_lam1_GNP3.2)),
                       paste0("loglam2_",colnames(X_lam2_GNP3.2)), paste0("loglam3_",colnames(X_lam3_GNP3.2)))
se_GNP3.2 <- sqrt(diag(solve(fit_GNP3.2$hessian))) 

results_GNP3.2 <- data.frame(est_GNP3.2 = round(est_GNP3.2, 3), se=round(se_GNP3.2,3))
results_GNP3.2$lower <- results_GNP3.2$est - 1.96*results_GNP3.2$se
results_GNP3.2$upper <- results_GNP3.2$est + 1.96*results_GNP3.2$se
results_GNP3.2

saveRDS(results_GNP3.2, 'results_GNP3.2.Rds')

#AIC
(2*length(fit_GNP3.2$par))-(2*-fit_GNP3.2$value)

# Construct model matrices-----------------------------------------------------
#I believe I need this for every time I run the model
#I may not rerun this for every one, but this is mm3.3 (trying with ONLY scaled detect.obscured)

# Occupancy natural parameters
#KLG: model.matrix creates a design (or model) matrix, e.g., by expanding factors to a set of 
#KLG: dummy variables (depending on the contrasts) and expanding interactions similarly.
#KLG: these are identical matrices
#I don't understand this well enough to know which one of my covariates to choose
X_f1_GNP3.3 <- model.matrix(~1, site_covs_GNP)
X_f2_GNP3.3 <- model.matrix(~1, site_covs_GNP)
X_f12_GNP3.3 <- model.matrix(~1, site_covs_GNP) 

# Detection intensity depends time of day
# KLG: makes three large matrices with an intercept column and a column for f1c, f2c, f1s, and f2s
# KLG: those 4 variables represent the Fourier series applied to the time vector
# KLG: these are identical matrices
#species 1, species 2 present
X_lam1_GNP3.3 <- model.matrix(~detect.obscured, obs_covs_GNP)

#species 1, species 2 absent
X_lam2_GNP3.3 <- model.matrix(~detect.obscured, obs_covs_GNP)

#species 2
X_lam3_GNP3.3 <- model.matrix(~detect.obscured, obs_covs_GNP)

# Save model matrices for use elsewhere
save(X_f1_GNP3.3, X_f2_GNP3.3, X_f12_GNP3.3, X_lam1_GNP3.3, X_lam2_GNP3.3, X_lam3_GNP3.3, file='model_matrices_GNP3.3.Rdata')

# Indicator matrix to divide up single vector of parameters into
# subvectors for each parameter to estimate
#KLG: not sure I follow why the different values are used
#I THINK I NEED TO MAKE A NEW MATRIX FOR THIS EVERY RUN
pind_GNP3.3 <- matrix(NA, nrow=8, ncol=2) #KLG: makes an empty matrix with 8 rows and 2 columns
#KLG: each row is filled in individually
pind_GNP3.3[1,] <- c(0, 0+ncol(X_f1_GNP3.3)-1)                #f1, KLG: fills in first row
pind_GNP3.3[2,] <- c(pind_GNP3.3[1,2]+1, pind_GNP3.3[1,2]+1+ncol(X_f2_GNP3.3)-1)    #f2
pind_GNP3.3[3,] <- c(pind_GNP3.3[2,2]+1, pind_GNP3.3[2,2]+1+ncol(X_f12_GNP3.3)-1)   #f12
pind_GNP3.3[4,] <- c(pind_GNP3.3[3,2]+1, pind_GNP3.3[3,2]+2)                 #mu (species 1)
pind_GNP3.3[5,] <- c(pind_GNP3.3[4,2]+1, pind_GNP3.3[4,2]+2)                 #mu (species 2)
pind_GNP3.3[6,] <- c(pind_GNP3.3[5,2]+1, pind_GNP3.3[5,2]+1+ncol(X_lam1_GNP3.3)-1)  #lambda sp1|sp2 present
pind_GNP3.3[7,] <- c(pind_GNP3.3[6,2]+1, pind_GNP3.3[6,2]+1+ncol(X_lam2_GNP3.3)-1)  #lambda sp1|sp2 absent
pind_GNP3.3[8,] <- c(pind_GNP3.3[7,2]+1, pind_GNP3.3[7,2]+1+ncol(X_lam3_GNP3.3)-1)  #lambda sp2

# Optimization-----------------------------------------------------------------

set.seed(123)

# Quickly get reasonable start values with SANN
# Initial SANN NLL value should be 41111.166130
# KLG: I do not understand what SANN NLL is
# KLG: this is used to feed into the next optimization thing, it generates start values
#KLG: rep() replicates the values in x
starts_GNP3.3 <- optim(rep(0,max(pind_GNP3.3)+1), mmpp_covs, method = 'SANN',
                       control = list(maxit=400, trace=1, REPORT =5),
                       pind=pind_GNP3.3, X_f1=X_f1_GNP3.3, X_f2=X_f2_GNP3.3, X_f12=X_f12_GNP3.3, X_lam1=X_lam1_GNP3.3,
                       X_lam2=X_lam2_GNP3.3, X_lam3=X_lam3_GNP3.3, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                       yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                       yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)
# Final SANN NLL value should be 30050.156712
# KLG: ~12-15 min to run above code

# Do optimization and calculate hessian
#KLG: this took ~11 min to get to the first iteration value, 18 (total) for the second
#KLG: function optimization describes a class of problems for finding the input to a 
#KLG: given function that results in the minimum or maximum output from the function (default is minimizing)
#KLG: I'm struggling with what this is doing, but to break apart pieces I understand:
#KLG: maxit = max number of iterations, REPORT = frequency of reports
#KLG: hessian = TRUE -> return a numerically differentiated Hessian matrix
#KLG: mmpp_covs seems to be the function to be optimmized, with the parameters listed after the
#KLG: control variables (it's basically defined/established here)
#KLG: starts$par are the initial values for the parameters to be optimized over
#KLG: this method (L-BFGS-B) allows each variable to be given a lower and/or upper bound
#KLG: this took somewhere around 5-6 hours to run
fit_GNP3.3 <- optim(starts_GNP3.3$par, mmpp_covs, method = 'L-BFGS-B', hessian=TRUE,
                    control = list(trace = 1, REPORT = 5, maxit=400),
                    pind=pind_GNP3.3, X_f1=X_f1_GNP3.3, X_f2=X_f2_GNP3.3, X_f12=X_f12_GNP3.3, X_lam1=X_lam1_GNP3.3,
                    X_lam2=X_lam2_GNP3.3, X_lam3=X_lam3_GNP3.3, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                    yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                    yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)
# Final NLL value should be ~ 26461.08
# May take several runs of optimization to get past local minima to this value

#Format and save results
#KLG: saveRDS saves an R object for it to be called later (it serializes an R object into a 
#KLG: format that can be called later), but it forgets the original name of the object
saveRDS(fit_GNP3.3, "fit_covs3_GNP3.3.Rds")

est_GNP3.3 <- fit_GNP3.3$par 
names(est_GNP3.3) <- c(paste0("f1_",colnames(X_f1_GNP3.3)), paste0("f2_",colnames(X_f2_GNP3.3)),
                       paste0("f12_",colnames(X_f12_GNP3.3)),
                       "log_mu1[1]","log_mu1[2]","log_mu2[1]","log_mu2[2]",
                       paste0("loglam1_",colnames(X_lam1_GNP3.3)),
                       paste0("loglam2_",colnames(X_lam2_GNP3.3)), paste0("loglam3_",colnames(X_lam3_GNP3.3)))
se_GNP3.3 <- sqrt(diag(solve(fit_GNP3.3$hessian))) 

results_GNP3.3 <- data.frame(est_GNP3.3 = round(est_GNP3.3, 3), se=round(se_GNP3.3,3))
results_GNP3.3$lower <- results_GNP3.3$est - 1.96*results_GNP3.3$se
results_GNP3.3$upper <- results_GNP3.3$est + 1.96*results_GNP3.3$se
results_GNP3.3

saveRDS(results_GNP3.3, 'results_GNP3.3.Rds')

#AIC
(2*length(fit_GNP3.3$par))-(2*-fit_GNP3.3$value)

# Construct model matrices-----------------------------------------------------
#I believe I need this for every time I run the model
#this is a mm5.2

# Occupancy natural parameters
#KLG: model.matrix creates a design (or model) matrix, e.g., by expanding factors to a set of 
#KLG: dummy variables (depending on the contrasts) and expanding interactions similarly.
#KLG: these are identical matrices
#I don't understand this well enough to know which one of my covariates to choose
X_f1_GNP5.2 <- model.matrix(~urema_dist_scaled, site_covs_GNP)
X_f2_GNP5.2 <- model.matrix(~urema_dist_scaled + termite.large.count.100m.scaled, site_covs_GNP)
X_f12_GNP5.2 <- model.matrix(~lion_latedry_scaled, site_covs_GNP) 

# Detection intensity depends time of day
# KLG: makes three large matrices with an intercept column and a column for f1c, f2c, f1s, and f2s
# KLG: those 4 variables represent the Fourier series applied to the time vector
# KLG: these are identical matrices
#species 1, species 2 present
X_lam1_GNP5.2 <- model.matrix(~cover.ground, obs_covs_GNP)

#species 1, species 2 absent
X_lam2_GNP5.2 <- model.matrix(~cover.ground, obs_covs_GNP)

#species 2
X_lam3_GNP5.2 <- model.matrix(~cover.ground, obs_covs_GNP)

# Save model matrices for use elsewhere
save(X_f1_GNP5.2, X_f2_GNP5.2, X_f12_GNP5.2, X_lam1_GNP5.2, X_lam2_GNP5.2, X_lam3_GNP5.2, file='model_matrices_GNP5.2.Rdata')

# Indicator matrix to divide up single vector of parameters into
# subvectors for each parameter to estimate
#KLG: not sure I follow why the different values are used
#I THINK I NEED TO MAKE A NEW MATRIX FOR THIS EVERY RUN
pind_GNP5.2 <- matrix(NA, nrow=8, ncol=2) #KLG: makes an empty matrix with 8 rows and 2 columns
#KLG: each row is filled in individually
pind_GNP5.2[1,] <- c(0, 0+ncol(X_f1_GNP5.2)-1)                #f1, KLG: fills in first row
pind_GNP5.2[2,] <- c(pind_GNP5.2[1,2]+1, pind_GNP5.2[1,2]+1+ncol(X_f2_GNP5.2)-1)    #f2
pind_GNP5.2[3,] <- c(pind_GNP5.2[2,2]+1, pind_GNP5.2[2,2]+1+ncol(X_f12_GNP5.2)-1)   #f12
pind_GNP5.2[4,] <- c(pind_GNP5.2[3,2]+1, pind_GNP5.2[3,2]+2)                 #mu (species 1)
pind_GNP5.2[5,] <- c(pind_GNP5.2[4,2]+1, pind_GNP5.2[4,2]+2)                 #mu (species 2)
pind_GNP5.2[6,] <- c(pind_GNP5.2[5,2]+1, pind_GNP5.2[5,2]+1+ncol(X_lam1_GNP5.2)-1)  #lambda sp1|sp2 present
pind_GNP5.2[7,] <- c(pind_GNP5.2[6,2]+1, pind_GNP5.2[6,2]+1+ncol(X_lam2_GNP5.2)-1)  #lambda sp1|sp2 absent
pind_GNP5.2[8,] <- c(pind_GNP5.2[7,2]+1, pind_GNP5.2[7,2]+1+ncol(X_lam3_GNP5.2)-1)  #lambda sp2

# Optimization-----------------------------------------------------------------

set.seed(123)

# Quickly get reasonable start values with SANN
# Initial SANN NLL value should be 41111.166130
# KLG: I do not understand what SANN NLL is
# KLG: this is used to feed into the next optimization thing, it generates start values
#KLG: rep() replicates the values in x
starts_GNP5.2 <- optim(rep(0, max(pind_GNP5.2)+1), mmpp_covs, method = 'SANN',
                       control = list(maxit=400, trace=1, REPORT =5),
                       pind=pind_GNP5.2, X_f1=X_f1_GNP5.2, X_f2=X_f2_GNP5.2, X_f12=X_f12_GNP5.2, X_lam1=X_lam1_GNP5.2,
                       X_lam2=X_lam2_GNP5.2, X_lam3=X_lam3_GNP5.2, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                       yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                       yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)

# Do optimization and calculate hessian
fit_GNP5.2 <- optim(starts_GNP5.2$par, mmpp_covs, method = 'L-BFGS-B', hessian=TRUE,
                    control = list(trace = 1, REPORT = 5, maxit=400),
                    pind=pind_GNP5.2, X_f1=X_f1_GNP5.2, X_f2=X_f2_GNP5.2, X_f12=X_f12_GNP5.2, X_lam1=X_lam1_GNP5.2,
                    X_lam2=X_lam2_GNP5.2, X_lam3=X_lam3_GNP5.2, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                    yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                    yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)
# Final NLL value should be ~ 26461.08
# May take several runs of optimization to get past local minima to this value

#Format and save results
#KLG: saveRDS saves an R object for it to be called later (it serializes an R object into a 
#KLG: format that can be called later), but it forgets the original name of the object
saveRDS(fit_GNP5.2, "fit_covs3_GNP5.2.Rds")

est_GNP5.2 <- fit_GNP5.2$par 
names(est_GNP5.2) <- c(paste0("f1_",colnames(X_f1_GNP5.2)), paste0("f2_",colnames(X_f2_GNP5.2)),
                       paste0("f12_",colnames(X_f12_GNP5.2)),
                       "log_mu1[1]","log_mu1[2]","log_mu2[1]","log_mu2[2]",
                       paste0("loglam1_",colnames(X_lam1_GNP5.2)),
                       paste0("loglam2_",colnames(X_lam2_GNP5.2)), paste0("loglam3_",colnames(X_lam3_GNP5.2)))
se_GNP5.2 <- sqrt(diag(solve(fit_GNP5.2$hessian))) 

results_GNP5.2 <- data.frame(est = round(est_GNP5.2, 3), se = round(se_GNP5.2,3))
#results_GNP5 <- data.frame(est = round(est_GNP5, 3))
results_GNP5.2$lower <- results_GNP5.2$est - 1.96*results_GNP5.2$se
results_GNP5.2$upper <- results_GNP5.2$est + 1.96*results_GNP5.2$se
results_GNP5.2

saveRDS(results_GNP5.2, 'results_GNP5.2.Rds')

#AIC
(2*length(fit_GNP5.2$par))-(2*-fit_GNP5.2$value)

# Construct model matrices-----------------------------------------------------
#this is mm13.1

# Occupancy natural parameters
#KLG: model.matrix creates a design (or model) matrix, e.g., by expanding factors to a set of 
#KLG: dummy variables (depending on the contrasts) and expanding interactions similarly.
#KLG: these are identical matrices
#I don't understand this well enough to know which one of my covariates to choose
X_f1_GNP13.1 <- model.matrix(~urema_dist_scaled, site_covs_GNP)
X_f2_GNP13.1 <- model.matrix(~urema_dist_scaled + termite.large.count.100m.scaled, site_covs_GNP)
X_f12_GNP13.1 <- model.matrix(~lion_latedry_scaled, site_covs_GNP)

# Detection intensity depends time of day
# KLG: makes three large matrices with an intercept column and a column for f1c, f2c, f1s, and f2s
# KLG: those 4 variables represent the Fourier series applied to the time vector
# KLG: these are identical matrices
#species 1, species 2 present
X_lam1_GNP13.1 <- model.matrix(~f1c_GNP + f2c_GNP + f1s_GNP + f2s_GNP + cover.ground + TSL_GNP + lake, obs_covs_GNP)

#species 1, species 2 absent
X_lam2_GNP13.1 <- model.matrix(~f1c_GNP + f2c_GNP + f1s_GNP + f2s_GNP + cover.ground + lake, obs_covs_GNP)

#species 2
X_lam3_GNP13.1 <- model.matrix(~f1c_GNP + f2c_GNP + f1s_GNP + f2s_GNP + cover.ground + lake, obs_covs_GNP)

# Save model matrices for use elsewhere
save(X_f1_GNP13.1, X_f2_GNP13.1, X_f12_GNP13.1, X_lam1_GNP13.1, X_lam2_GNP13.1, X_lam3_GNP13.1, file='model_matrices_GNP13.1.Rdata')

# Indicator matrix to divide up single vector of parameters into
# subvectors for each parameter to estimate
#KLG: not sure I follow why the different values are used
pind_GNP13.1 <- matrix(NA, nrow=8, ncol=2) #KLG: makes an empty matrix with 8 rows and 2 columns
#KLG: each row is filled in individually
pind_GNP13.1[1,] <- c(0, 0+ncol(X_f1_GNP13.1)-1)                #f1, KLG: fills in first row
pind_GNP13.1[2,] <- c(pind_GNP13.1[1,2]+1, pind_GNP13.1[1,2]+1+ncol(X_f2_GNP13.1)-1)    #f2
pind_GNP13.1[3,] <- c(pind_GNP13.1[2,2]+1, pind_GNP13.1[2,2]+1+ncol(X_f12_GNP13.1)-1)   #f12
pind_GNP13.1[4,] <- c(pind_GNP13.1[3,2]+1, pind_GNP13.1[3,2]+2)                 #mu (species 1)
pind_GNP13.1[5,] <- c(pind_GNP13.1[4,2]+1, pind_GNP13.1[4,2]+2)                 #mu (species 2)
pind_GNP13.1[6,] <- c(pind_GNP13.1[5,2]+1, pind_GNP13.1[5,2]+1+ncol(X_lam1_GNP13.1)-1)  #lambda sp1|sp2 present
pind_GNP13.1[7,] <- c(pind_GNP13.1[6,2]+1, pind_GNP13.1[6,2]+1+ncol(X_lam2_GNP13.1)-1)  #lambda sp1|sp2 absent
pind_GNP13.1[8,] <- c(pind_GNP13.1[7,2]+1, pind_GNP13.1[7,2]+1+ncol(X_lam3_GNP13.1)-1)  #lambda sp2

# Optimization-----------------------------------------------------------------

set.seed(123)

# Quickly get reasonable start values with SANN
# Initial SANN NLL value should be 41111.166130
# KLG: I do not understand what SANN NLL is
# KLG: this is used to feed into the next optimization thing, it generates start values
#KLG: rep() replicates the values in x
starts_GNP13.1 <- optim(rep(0,max(pind_GNP13.1)+1), mmpp_covs, method = 'SANN',
                        control = list(maxit=400, trace=1, REPORT =5),
                        pind=pind_GNP13.1, X_f1=X_f1_GNP13.1, X_f2=X_f2_GNP13.1, X_f12=X_f12_GNP13.1, X_lam1=X_lam1_GNP13.1,
                        X_lam2=X_lam2_GNP13.1, X_lam3=X_lam3_GNP13.1, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                        yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                        yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)
# Final SANN NLL value should be 30050.156712
# KLG: ~12-15 min to run above code

# Do optimization and calculate hessian
fit_GNP13.1 <- optim(starts_GNP13.1$par, mmpp_covs, method = 'L-BFGS-B', hessian=TRUE,
                     control = list(trace = 1, REPORT = 5, maxit=400),
                     pind=pind_GNP13.1, X_f1=X_f1_GNP13.1, X_f2=X_f2_GNP13.1, X_f12=X_f12_GNP13.1, X_lam1=X_lam1_GNP13.1,
                     X_lam2=X_lam2_GNP13.1, X_lam3=X_lam3_GNP13.1, yd1=yd1_GNP, yd2=yd2_GNP, lidx_i=lidx_i_GNP,
                     yd1_st_idx=yd1_st_idx_GNP, yd1_en_idx=yd1_en_idx_GNP, yd2_st_idx=yd2_st_idx_GNP,
                     yd2_en_idx=yd2_en_idx_GNP, y1_i=y1_i_GNP, y2_i=y2_i_GNP, threads=2)
# Final NLL value should be ~ 26461.08
# May take several runs of optimization to get past local minima to this value

#Format and save results
#KLG: saveRDS saves an R object for it to be called later (it serializes an R object into a 
#KLG: format that can be called later), but it forgets the original name of the object
saveRDS(fit_GNP13.1, "fit_covs3_GNP13.1.Rds")

est_GNP13.1 <- fit_GNP13.1$par 
names(est_GNP13.1) <- c(paste0("f1_",colnames(X_f1_GNP13.1)), paste0("f2_",colnames(X_f2_GNP13.1)),
                        paste0("f12_",colnames(X_f12_GNP13.1)),
                        "log_mu1[1]","log_mu1[2]","log_mu2[1]","log_mu2[2]",
                        paste0("loglam1_",colnames(X_lam1_GNP13.1)),
                        paste0("loglam2_",colnames(X_lam2_GNP13.1)), paste0("loglam3_",colnames(X_lam3_GNP13.1)))
se_GNP13.1 <- sqrt(diag(solve(fit_GNP13.1$hessian))) 

#I can't run the last few lines because I don't have the se
results_GNP13.1 <- data.frame(est = round(est_GNP13.1, 3), se=round(se_GNP13.1,3))
#results_GNP13 <- data.frame(est_GNP13 = round(est_GNP13, 3))
results_GNP13.1$lower <- results_GNP13.1$est - 1.96*results_GNP13.1$se
results_GNP13.1$upper <- results_GNP13.1$est + 1.96*results_GNP13.1$se
results_GNP13.1

hessian_GNP13.1 <- fit_GNP13.1$hessian

saveRDS(results_GNP13.1, 'results_GNP13.1.Rds')

#AIC
(2*length(fit_GNP13.1$par))-(2*-fit_GNP13.1$value)

write.csv(results_GNP13.1, file = 'occupancy-mmpp-master/model_results/results/results_GNP13.1.csv')
