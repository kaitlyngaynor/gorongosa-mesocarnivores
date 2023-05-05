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
