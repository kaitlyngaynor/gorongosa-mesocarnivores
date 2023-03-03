#Load required libraries and likelihood function
library(Rcpp)
sourceCpp("../likelihood/likelihood.cpp")

mmpp_simulation <- function(N, truth, maxt, psi, mu1, mu2, lam1, lam2, lam3, threads=1){
  #Simulate true occupancy state z for each site
  cat("Simulating true occupancy state...\n")
  z <- sample(1:4, N, prob=psi, replace=T)

  #Get MMPP data for each site based on true occupancy state
  cat("Simulating MMPP detection data...\n")
  y1 <- y2 <- vector("list", N)
  for (i in 1:N){
    if(z[i] == 1){
      y1[[i]] <- gen_mmpp_obs(mu1, lam1)
      y2[[i]] <- gen_mmpp_obs(mu2, lam3)
    } else if(z[i] == 2){
      y1[[i]] <- gen_mmpp_obs(mu1, lam2)
      y2[[i]] <- NA
    } else if(z[i] == 3){
      y1[[i]] <- NA
      y2[[i]] <- gen_mmpp_obs(mu2, lam3)
    } else{
      y1[[i]] <- NA
      y2[[i]] <- NA
    }
  }

  #Replace NA with NULL
  #Can't just use NULL above because R will just make them disappear...
  y1 <- lapply(y1, function(x) if(is.na(x[1])) return(NULL) else return(x))
  y2 <- lapply(y2, function(x) if(is.na(x[1])) return(NULL) else return(x))

  cat("Formatting data for C++...\n")
  #Indicator vectors - was species ever detected at site?
  y1_i <- sapply(y1, function(x) ifelse(is.null(x), 0, 1))
  y2_i <- sapply(y2, function(x) ifelse(is.null(x), 0, 1))

  #Make sure some sites didn't get lost
  stopifnot(length(y1_i)==length(y2_i))

  #Convert observations (y) to time difference between y and interval boundary (yd)
  yd_sp1 <- lapply(1:length(y1), function(i) get_yd(y1[[i]], maxt, inc=1/24))
  yd_sp2 <- lapply(1:length(y2), function(i) get_yd(y2[[i]], maxt, inc=1/24))

  #Index to subset lambda values by site, needed for likelihood
  lidx_i <- matrix(NA, nrow=length(yd_sp1), ncol=2)
  idx <- 0
  for (i in seq_along(yd_sp1)){
    lidx_i[i,1] <- idx
    lidx_i[i,2] <- idx + length(yd_sp1[[i]]) - 1
    idx <- idx + length(yd_sp1[[i]])
  }

  #Indices to subset yd (y-d) values by site i and interval j
  #yd is converted to a vector instead of a list of lists so the index is needed
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

  #Site and obs covs matrices (dummy since we don't have covariates)
  site_covs <- data.frame(dummy=rep(1,N))
  obs_covs <- data.frame(dummy=rep(1, N*length(yd_sp1[[1]])))

  #Convert yd lists to vectors to pass to C++
  yd1 <- unlist(yd_sp1)
  yd2 <- unlist(yd_sp2)

  #Generate model matrices for f and lambda, no covariates
  X_f1 <- X_f2 <- X_f12 <-  model.matrix(~1, site_covs)
  X_lam1 <- X_lam2 <- X_lam3 <- model.matrix(~1, obs_covs)

  #Matrix of indices to divide single par value input vector needed by optim()
  #into parts for each separate parameter
  pind <- matrix(NA, nrow=8, ncol=2)
  pind[1,] <- c(0, 0+ncol(X_f1)-1)
  pind[2,] <- c(pind[1,2]+1, pind[1,2]+1+ncol(X_f2)-1)
  pind[3,] <- c(pind[2,2]+1, pind[2,2]+1+ncol(X_f12)-1)
  pind[4,] <- c(pind[3,2]+1, pind[3,2]+2)
  pind[5,] <- c(pind[4,2]+1, pind[4,2]+2)
  pind[6,] <- c(pind[5,2]+1, pind[5,2]+1+ncol(X_lam1)-1)
  pind[7,] <- c(pind[6,2]+1, pind[6,2]+1+ncol(X_lam2)-1)
  pind[8,] <- c(pind[7,2]+1, pind[7,2]+1+ncol(X_lam3)-1)

  saveRDS(list(yd1=yd1,yd2=yd2,lidx_i=lidx_i,yd1_st_idx=yd1_st_idx,
               yd1_en_idx=yd1_en_idx,yd2_st_idx=yd2_st_idx,yd2_en_idx=yd2_en_idx,
               y1_i=y1_i,y2_i=y2_i,pind=pind), "opt_inputs.Rds")

  tryCatch({

  #Run SANN
  #cat("\nOptimizing with SANN to get good start values...\n")
  #starts <- optim(rep(0,max(pind)+1), mmpp_covs, method = 'SANN',
  #            control = list(maxit=400, trace=1, REPORT =5),
  #            pind=pind, X_f1=X_f1, X_f2=X_f2, X_f12=X_f12, X_lam1=X_lam1,
  #            X_lam2=X_lam2, X_lam3=X_lam3, yd1=yd1, yd2=yd2, lidx_i=lidx_i,
  #            yd1_st_idx=yd1_st_idx, yd1_en_idx=yd1_en_idx, yd2_st_idx=yd2_st_idx,
  #            yd2_en_idx=yd2_en_idx, y1_i=y1_i, y2_i=y2_i, threads=threads)
  starts <- truth

  #Final optimization
  cat("\nOptimizing with L-BFGS-B...\n")
  optim(starts, mmpp_covs, method = 'L-BFGS-B', hessian=FALSE,
              control = list(trace = 1, REPORT = 5, maxit=400),
              pind=pind, X_f1=X_f1, X_f2=X_f2, X_f12=X_f12, X_lam1=X_lam1,
              X_lam2=X_lam2, X_lam3=X_lam3, yd1=yd1, yd2=yd2, lidx_i=lidx_i,
              yd1_st_idx=yd1_st_idx, yd1_en_idx=yd1_en_idx, yd2_st_idx=yd2_st_idx,
              yd2_en_idx=yd2_en_idx, y1_i=y1_i, y2_i=y2_i, threads=threads)

  }, error=function(e){
            message("\nOptimization failed\n")
            return(NULL)
  })

}

#Function to convert observation times (y) to time since interval boundary (yd)
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


#Function to simulate from MMPP
gen_mmpp_obs <- function(mu, lam){
  st <- tstate <- rep(NA, 1000)

  #initial state
  st[1] <- sample(1:2, 1, prob=rev(mu)/sum(mu))
  #initial time
  tstate[1] <- rexp(1, mu[st[1]])

  idx <- 2
  sumt <- tstate[1]
  while(sumt < maxt){
    st[idx] <- ifelse(st[idx-1]==1, 2, 1)
    tstate[idx] <- rexp(1, mu[st[idx]])
    sumt <- sumt + tstate[idx]
    idx <- idx + 1
  }
  st <- st[!is.na(st)]
  tstate <- tstate[!is.na(tstate)]
  tstate[length(tstate)] <- tstate[length(tstate)] - (sum(tstate)-maxt)

  t_int <- cumsum(tstate)

  idx <- 1   # indexing state, as above
  a <- 0  # arrival time, initializing at 0. will ultimately be vector
  tmp <- rexp(1, lam[st[1]])  # potential arrival time
  # if potential arrival time < duration in 1st state, this is first arrival
  if(tmp < tstate[1]) a <- tmp

  # if first observation not in state 1,
  # iterating through states until first arrival is made
  while(a == 0){ # a == 0 indicates no arrivals yet

    idx <- idx + 1  # iterating to next state
    if(idx > length(st)) return(NA)
    tmp <- rexp(1, lam[st[idx]]) # simulating arrival time
    # is potential arrival time shorter that duration in state?
    if(tmp < tstate[idx]) a <- t_int[idx - 1] + tmp
  }

  #Iterate through each idx until you are past the maximum index
  while(idx <= length(t_int)){

    #Baseline for new point
    #either the last point or the interval start
    #whichever is larger
    st_point <- max(max(a), t_int[idx-1], na.rm=T)

    tmp <- rexp(1, lam[st[idx]])  # potential arrival time, relative to start point
    cand <- st_point + tmp # create complete candidate point (start + arrival)

    #if candidate point is less than the end of the interval, add it to list
    if(cand < t_int[idx]){
      a <- c(a, cand)
    #Otherwise move into the next interval
    } else {
      idx <- idx + 1
    }
  }

  return(a)
}

get_p <- function(mu, lambda, maxt){
  Q <- matrix(c(-mu[1], mu[2], mu[1], -mu[2]), nrow = 2)
  C <- Q - diag(lambda)
  pi <- c(mu[2], mu[1]) / sum(mu)
  1 - (pi %*% expm::expm(C * maxt) %*% matrix(c(1, 1), ncol = 1))[1, 1]
}

