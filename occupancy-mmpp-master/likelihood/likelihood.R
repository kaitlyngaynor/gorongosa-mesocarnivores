mmpp <- function(params, y1, y2, J, inc = 1 / 24){

  # Args:
    # y1: list of species 1 detection times. null if no detections
    # y2: list of species 2 detection times. null if no detections
    # J: camera trap duration (days)
    # inc: length of quadrature intervals

  # natural parameters
  f1 <- params[1]
  f2 <- params[2]
  f12 <- params[3]

  # elements of Q
  mu1 <- numeric(2)
  mu1[1] <- exp(params[4])
  mu1[2] <- exp(params[5])

  mu2 <- numeric(2)
  mu2[1] <- exp(params[6])
  mu2[2] <- exp(params[7])

  # count intensity in each state
  lambda1 <- numeric(2)
  lambda1[1] <- 0  # no detections while in 1 state
  lambda1[2] <- exp(params[8])

  lambda2 <- numeric(2)
  lambda2[1] <- 0  # no detections while in 1 state
  lambda2[2] <- exp(params[9])

  lambda3 <- numeric(2)
  lambda3[1] <- 0  # no detections while in 1 state
  lambda3[2] <- exp(params[10])

  # index of whether species detected at site i
  y1_i <- sapply(y1, function(x) ifelse(is.null(x), 0, 1))
  y2_i <- sapply(y2, function(x) ifelse(is.null(x), 0, 1))

  N <- length(y1_i) # number of sites
  psi <- matrix(nrow = N, ncol = 4)  # occurrence probability

  # probability of initial state
  zet1 <- matrix(c(NA, NA), nrow = 1)
  zet1[1, 1] <- mu1[2] / sum(mu1)
  zet1[1, 2] <- mu1[1] / sum(mu1)

  zet2 <- matrix(c(NA, NA), nrow = 1)
  zet2[1, 1] <- mu2[2] / sum(mu2)
  zet2[1, 2] <- mu2[1] / sum(mu2)

  # closed form expression for matrix exponential
  # really speeds things up
  # exp(l(Q - Lambda)), from Skaug 2006
  S1 <- lambda1 + mu1
  S2 <- lambda2 + mu1
  S3 <- lambda3 + mu2

  K1 <- prod(lambda1) + lambda1[1] * mu1[2] + mu1[1] * lambda1[2]
  K2 <- prod(lambda2) + lambda2[1] * mu1[2] + mu1[1] * lambda2[2]
  K3 <- prod(lambda3) + lambda3[1] * mu2[2] + mu2[1] * lambda3[2]

  U1 <- sqrt(sum(S1) ^ 2 - 4 * K1)
  U2 <- sqrt(sum(S2) ^ 2 - 4 * K2)
  U3 <- sqrt(sum(S3) ^ 2 - 4 * K3)

  theta1 <- c((sum(S1) + U1) / 2, (sum(S1) - U1) / 2)
  theta2 <- c((sum(S2) + U2) / 2, (sum(S2) - U2) / 2)
  theta3 <- c((sum(S3) + U3) / 2, (sum(S3) - U3) / 2)

  m1_1 <- matrix(c(S1[2] - theta1[2], mu1[2], mu1[1], S1[1] - theta1[2]),
                 nrow = 2)
  m1_2 <- matrix(c(S2[2] - theta2[2], mu1[2], mu1[1], S2[1] - theta2[2]),
                 nrow = 2)
  m1_3 <- matrix(c(S3[2] - theta3[2], mu2[2], mu2[1], S3[1] - theta3[2]),
                 nrow = 2)

  m2_1 <- matrix(c(S1[2] - theta1[1], mu1[2], mu1[1], S1[1] - theta1[1]),
                 nrow = 2)
  m2_2 <- matrix(c(S2[2] - theta2[1], mu1[2], mu1[1], S2[1] - theta2[1]),
                 nrow = 2)
  m2_3 <- matrix(c(S3[2] - theta3[1], mu2[2], mu2[1], S3[1] - theta3[1]),
                 nrow = 2)

  e <- matrix(c(1, 1), ncol = 1)

  ll1 <- numeric(N)  # log detection likelihood, species 1 (sp2 present)
  ll2 <- numeric(N)  # log detection likelihood, species 1 (sp2 absent)
  ll3 <- numeric(N)  # log detection likelihood, species 2

  ll <- numeric(N)  # total log likelihood

  for(i in 1:N){  # looping through all sites

    # probability of occurrence
    psi[i, 1] <- exp(f1 + f2 + f12)
    psi[i, 2] <- exp(f1)
    psi[i, 3] <- exp(f2)
    psi[i, 4] <- 1

    psi[i, ] <- psi[i, ] / sum(psi[i, ])

    d <- 0  # initialize end time of each split

    for(j in 1:ceiling(J[i] / inc)){  # looping through each split

      d <- d + inc
      d <- min(d, J[i])  # adjusting d for last time interval

      # making increment smaller at last time interval
      inc_j <- ifelse(j < ceiling(J[i] / inc), inc, J[i] - inc * (j - 1))

      # SPECIES 1 DETECTION INTENSITY (sp2 present)
      ll1[i] <- ll1[i] + splt_lik(y1[[i]], d, inc_j, zet1, U1, theta1, m1_1,
                                  m2_1, lambda1, e)

      # SPECIES 1 DETECTION INTENSITY (sp2 absent)
      ll2[i] <- ll2[i] + splt_lik(y1[[i]], d, inc_j, zet1, U2, theta2, m1_2,
                                  m2_2, lambda2, e)

      # SPECIES 2 DETECTION INTENSITY
      ll3[i] <- ll3[i] + splt_lik(y2[[i]], d, inc_j, zet2, U3, theta3, m1_3,
                                  m2_3, lambda3, e)

    }  # j loop

    ll[i] <-
      y1_i[i] * y2_i[i] * log(psi[i, 1] * exp(ll1[i]) * exp(ll3[i])) +
      y1_i[i] * (1 - y2_i[i]) * log(psi[i, 1] * exp(ll1[i]) * exp(ll3[i]) +
                                      psi[i, 2] * exp(ll2[i])) +
      (1 - y1_i[i]) * y2_i[i] * log(psi[i, 1] * exp(ll1[i]) * exp(ll3[i]) +
                                      psi[i, 3] * exp(ll3[i])) +
      (1 - y1_i[i]) * (1 - y2_i[i]) *
      log(psi[i, 1] * exp(ll1[i]) * exp(ll3[i]) +
            psi[i, 2] * exp(ll2[i]) +
            psi[i, 3] * exp(ll3[i]) +
            psi[i, 4])

  }  # N Loop

  return(-1 * sum(ll))

}

splt_lik <- function(y, d, ic, zet, U, theta, m1, m2, lambda, e){

  # Args:
  #   y: vector of detections
  #   d: end time of split
  #   ic: length of time increment
  #   zet, U, theta, m1, m2, lambda, e: mmpp arguments

  # any observations within split?
  if(any((y > (d - ic)) & (y <= d))){

    # which observations in split?
    ind <- which((y > (d - ic)) & (y <= d))

    if(length(ind) == 1){  # only 1 observation in split

      #ll <-
      #  log((zet %*%
      val <- (zet %*%
               (1 / U * (exp(-theta[2] * (y[ind] - (d - ic))) * m1 -
                           exp(-theta[1] * (y[ind] - (d - ic))) * m2)) %*%
               diag(lambda) %*%
               (1 / U * (exp(-theta[2] * (d - y[ind])) * m1 -
                           exp(-theta[1] * (d - y[ind])) * m2)) %*% e)[1, 1]
        #)

    } else{  # more than 1 observation in split

      ll_m <-
        zet %*%
        (1 / U * (exp(-theta[2] * (y[ind[1]] - (d - ic))) * m1 -
                    exp(-theta[1] * (y[ind[1]] - (d - ic))) * m2)) %*%
        diag(lambda)

      for(l in 2:length(ind)){  # looping through all remaining detections

        ll_m <- ll_m %*%
          (1 / U * (exp(-theta[2] * (y[ind[l]] - y[ind[l - 1]])) * m1 -
                      exp(-theta[1] * (y[ind[l]] - y[ind[l - 1]])) * m2)) %*%
          diag(lambda)

      }

      #ll <-
      #  log((ll_m %*%
      val <- (ll_m %*%
               (1 / U * (exp(-theta[2] * (d - y[ind[l]])) * m1 -
                           exp(-theta[1] * (d - y[ind[l]])) * m2)) %*%
               e)[1, 1]
      #)

    }

  } else{  # no observations in split

    #ll <-
    #  log((zet %*%
    #         (1 / U * (exp(-theta[2] * (d - (d - ic))) * m1 -
    #                     exp(-theta[1] * (d - (d - ic))) * m2)) %*% e)[1, 1])
    val <-
      (zet %*%
             (1 / U * (exp(-theta[2] * (d - (d - ic))) * m1 -
                         exp(-theta[1] * (d - (d - ic))) * m2)) %*% e)[1, 1]

    #ll <- log(val)

  }

  if(is.na(val)) return(Inf)
  if(val < 0) return(Inf)

  return(log(val))

}
