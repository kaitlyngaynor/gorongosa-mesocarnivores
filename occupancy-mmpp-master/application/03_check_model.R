library(lubridate)
library(Matrix)
source('occupancy-mmpp-master/application/01_format_data.R')  # loading and cleaning data

# EMPIRICAL SURVIVAL CURVE-----------------------------------------------------

# matrix of time to first detection
# assume 3 weeks in length. each column is an hour increment
# KLG: length(dep_len) gives a row for every deployment, and a column for every hour of 21 days
tfd <- tfd_c <- matrix(nrow = length(dep_len), ncol = 21 * 24) 

hr <- 1 / 24  # hour expressed as day

# deer
for(i in 1:nrow(tfd)){  # looping through all sites

  if(is.null(deer[[i]])){  # no deer detected

    # 'survives' entire interval (KLG: what does 'survives' mean here?)
    #KLG: hour is used to get/set hours component of a date-time
    #KLG: row i, columns from hour(dep_start[i]) to 21*24 (= 504))
    tfd[i, hour(dep_start[i]):(21 * 24)] <- 1

  } else{  # deer detected

    # hour the deer was first detected (KLG: hfd = hour first detected)
    #KLG: floor takes a single numeric argument x and returns a numeric vector containing the 
    #KLG: largest integers not greater than the corresponding elements of x.
    #KLG: so this gives the largest integer that's not bigger than the given element of x (I think)
    #KLG: take the hour of the deployment start and add the time of earliest deer detection (relative
    #KLG: to camera setup) times 24
     hfd <- hour(dep_start[i]) + floor(min(deer[[i]]) / hr)

    if(hfd <= (21 * 24)){  # detected within 3 weeks?
      
      #KLG: fill in the row with 1s from the start until the hour of first detection
      tfd[i, hour(dep_start[i]):hfd] <- 1 # deer 'survives' this interval
      #KLG: and a 0 at that hour of first detection
      tfd[i, hfd] <- 0  # deer 'dies'

    } else{  # detection took longer than 3 weeks?

      # deer 'survives' entire interval
      #KLG: fill in the entire row with 1s
      tfd[i, hour(dep_start[i]):(21 * 24)] <- 1

    }

  }

}

# predicting to sites open to hunting
# need to load X_f1 from 'fitting_model.R' script
#KLG: I think they mean fit_model.R, which is now 02_fit_model.R
load('model_matrices.Rdata') #KLG: line still works because it's called the same
hnt <- X_f1[, 2] == 1

sf <- numeric(21 * 24)  # probability of 'surviving' interval
for(i in 1:length(sf)){  # looping through all 1-hour intervals

  sf[i] <- mean(tfd[hnt, i], na.rm = T)  # empirical prob of surviving interval

}

# quick and dirty plot of results
plot(y = cumprod(sf), x = cumsum(rep(1 / 24, 21 * 24)), type = 'l',
     ylab = 'Survival probability', xlab = 'Days')

saveRDS(cumprod(sf), 'deer_empr.rds')


## COYOTE ##
for(i in 1:nrow(tfd_c)){  # looping through all sites

  if(is.null(coys[[i]])){  # no coyotes detected

    # 'survives' entire interval
    tfd_c[i, hour(dep_start[i]):(21 * 24)] <- 1

  } else{  # coyote detected

    # hour the coyote was first detected
    hfd <- hour(dep_start[i]) + floor(min(coys[[i]]) / hr)

    if(hfd <= (21 * 24)){  # detected within 3 weeks?

      tfd_c[i, hour(dep_start[i]):hfd] <- 1 # coyote 'survives' this interval
      tfd_c[i, hfd] <- 0  # coyote 'dies'

    } else{  # detection took longer than 3 weeks?

      # coyote 'survives' entire interval
      tfd_c[i, hour(dep_start[i]):(21 * 24)] <- 1

    }

  }

}

sf_c <- numeric(21 * 24)  # probability of 'surviving' interval
for(i in 1:length(sf_c)){  # looping through all 1-hour intervals

  sf_c[i] <- mean(tfd_c[hnt, i], na.rm = T)  # empirical prob of surviving interval

}

# quick and dirty plot of results
plot(y = cumprod(sf_c), x = cumsum(rep(1 / 24, 21 * 24)), type = 'l',
     ylab = 'Survival probability', xlab = 'Days', main = 'Coyote')

saveRDS(cumprod(sf_c), 'coys_empr.rds')


# THEORETICAL SURVIVAL CURVE---------------------------------------------------

# loading fitted model
fit <- readRDS('fit_covs3.Rds')

# AIC 52980.58
2 * fit$value + 2 * length(fit$par)

est <- fit$par  # extracting parameter coefficients

# naming parameter coefficients
names(est) <- c("f1_(Intercept)", "f1_Hunting", "f2_(Intercept)", "f2_Hunting",
                "f12_(Intercept)", "f12_Hunting", "log_mu1[1]", "log_mu1[2]", "log_mu2[1]",
                "log_mu2[2]", "loglam1_(Intercept)", "loglam1_f1c",
                "loglam1_f2c", "loglam1_f1s", "loglam1_f2s",
                "loglam2_(Intercept)", "loglam2_f1c", "loglam2_f2c",
                "loglam2_f1s", "loglam2_f2s", "loglam3_(Intercept)",
                "loglam3_f1c", "loglam3_f2c", "loglam3_f1s", "loglam3_f2s")

# beginning hour of each 1-hour increment over 3 weeks
hr <- rep(0:23, times = 21)

e <- matrix(c(1, 1), ncol = 1)  # converting matrix to scalar

## POINT ESTIMATE ##

# probability of each state, deer
pi_1 <- c(exp(est[8]) / (exp(est[7]) + exp(est[8])),
          exp(est[7]) / (exp(est[7]) + exp(est[8])))

# probability of each state, coyote
pi_2 <- c(exp(est[10]) / (exp(est[9]) + exp(est[10])),
          exp(est[9]) / (exp(est[9]) + exp(est[10])))

# generator matrix deer
Q1 <- matrix(c(-exp(est[7]), exp(est[8]), exp(est[7]), -exp(est[8])), nrow = 2)

# generator matrix coyote
Q2 <- matrix(c(-exp(est[9]), exp(est[10]), exp(est[9]), -exp(est[10])), nrow = 2)

# deer detection intensity, coyote present
lam_1 <- cbind(0, exp(est[11] + est[12] * cos(pi * hr / 12) +
                        est[13] * cos(2 * pi * hr /12) +
                        est[14] * sin(pi * hr / 12) +
                        est[15] * sin(2 * pi * hr / 12)))

# deer detection intensity, coyote absent
lam_2 <- cbind(0, exp(est[16] + est[17] * cos(pi * hr / 12) +
                        est[18] * cos(2 * pi * hr /12) +
                        est[19] * sin(pi * hr / 12) +
                        est[20] * sin(2 * pi * hr / 12)))

# coyote detection intensity
lam_3 <- cbind(0, exp(est[21] + est[22] * cos(pi * hr / 12) +
                        est[23] * cos(2 * pi * hr /12) +
                        est[24] * sin(pi * hr / 12) +
                        est[25] * sin(2 * pi * hr / 12)))

s1 <- s2 <- s3 <- numeric(21 * 24) # probability of 'surviving' each interval

for(i in 1:(21 * 24)){  # looping through all 1-hour intervals

  # probability of surviving interval, deer when coyote present
  s1[i] <- (pi_1 %*% expm((Q1 - diag(lam_1[i, ])) * (1 / 24)) %*% e)[1, 1]

  # probability of surviving interval, deer when coyote absent
  s2[i] <- (pi_1 %*% expm((Q1 - diag(lam_2[i, ])) * (1 / 24)) %*% e)[1, 1]

  # probability of surviving interval, coyote
  s3[i] <- (pi_2 %*% expm((Q2 - diag(lam_3[i, ])) * (1 / 24)) %*% e)[1, 1]

}

# occurrence probability, hunted sites
psi <- c(exp(sum(est[1:6])), exp(sum(est[1:2])), exp(sum(est[3:4])), 1)
psi <- psi / sum(psi)

# weighted average, deer
ds <- psi[1] * cumprod(s1) + psi[2] * cumprod(s2) + sum(psi[3:4])
saveRDS(ds, 'deer_theo.rds')

# weighted average, coyote
ds_coy <- (psi[1] + psi[3]) * cumprod(s3) + (psi[2] + psi[4])
saveRDS(ds_coy, 'coys_theo.rds')
