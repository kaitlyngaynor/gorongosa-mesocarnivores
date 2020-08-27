# define plotting functions

# Function for plotting 1 species (as polygon)
timeplot1 <-function (A, n.grid = 128, kmax = 3, polygoncol = "lightgrey", ...) 
{
  
  bwA <- getBandWidth(A, kmax = kmax)
  
  xsc <- 24/(2 * pi)
  xxRad <- seq(0, 2 * pi, length = n.grid)
  xx <- xxRad * xsc
  densA <- densityFit(A, xxRad, bwA)/xsc
  
  ylim <- c(0, max(densA))
  plot(0, 0, type = "n", ylim = ylim, xlim = range(xx), xlab = "Time", 
       ylab = "Activity", xaxt = "n", ...)
  axis(1, at = c(0, 6, 12, 18, 24), labels = c("Midnight", 
                                               "Sunrise", "Noon", "Sunset", "Midnight"))
  polygon(c(c(2 * pi, 0), xx,24), c(0, 0, densA,0), border = NA, 
          col = polygoncol)
  lines(xx, densA, lty = 1, col = "black")
  return(invisible(list(x = xx, densityA = densA)))
}


# Function for plotting 2 species with overlap polygon -----------------------------
timeplot2<-function (A, B, xscale = 24, linetype = c(1, 1), linecol = c("#F8766D", "#00BFC4"),  linewidth = c(2, 2),
                         n.grid = 128, kmax = 3, adjust = 1, 
                        ...) 
{
  bwA <- getBandWidth(A, kmax = kmax)/adjust
  bwB <- getBandWidth(B, kmax = kmax)/adjust
  if (is.na(bwA) || is.na(bwB)) 
    stop("Bandwidth estimation failed.")
  xsc <- if (is.na(xscale))
    1
  else xscale/(2 * pi)
  xxRad <- seq(0, 2 * pi, length = n.grid)
  xx <- xxRad * xsc
  densA <- densityFit(A, xxRad, bwA)/xsc
  densB <- densityFit(B, xxRad, bwB)/xsc
  densOL <- pmin(densA, densB)
  ylim <- c(0, max(densA, densB))
  plot(0, 0, type = "n", ylim = ylim, xlim = range(xx), xlab = "Time", 
       ylab = "Density", xaxt = "n", ...)
  if (is.na(xscale)) {
    axis(1, at = c(0, pi/2, pi, 3 * pi/2, 2 * pi), labels = c("0", 
                                                              expression(pi/2), expression(pi), expression(3 * 
                                                                                                             pi/2), expression(2 * pi)))
  }
  else if (xscale == 24) {
    axis(1, at = c(0, 6, 12, 18, 24), labels = c("Midnight", 
                                                 "Sunrise", "Noon", "Sunset", "Midnight"))
  }
  else {
    axis(1)
  }
  lines(xx, densA, lty = linetype[1], col = linecol[1], lwd = linewidth[[1]])
  lines(xx, densB, lty = linetype[2], col = linecol[2], lwd = linewidth[[2]])
  return(invisible(list(x = xx, densityA = densA, densityB = densB)))
}

# Function for plotting 2 species with no overlap ----------------------------

timeplot2_overlap <-function (A, B, xscale = 24, linetype = c(1, 1), linecol = c("#4575b4", 
                                                                           "red"), n.grid = 128, kmax = 3, adjust = 1, 
                        ...) 
{
  bwA <- getBandWidth(A, kmax = kmax)/adjust
  bwB <- getBandWidth(B, kmax = kmax)/adjust
  if (is.na(bwA) || is.na(bwB)) 
    stop("Bandwidth estimation failed.")
  xsc <- if (is.na(xscale))
    1
  else xscale/(2 * pi)
  xxRad <- seq(0, 2 * pi, length = n.grid)
  xx <- xxRad * xsc
  densA <- densityFit(A, xxRad, bwA)/xsc
  densB <- densityFit(B, xxRad, bwB)/xsc
  densOL <- pmin(densA, densB)
  ylim <- c(0, max(densA, densB))
  plot(0, 0, type = "n", ylim = ylim, xlim = range(xx), xlab = "Time", 
       ylab = "Density", xaxt = "n", ...)
  if (is.na(xscale)) {
    axis(1, at = c(0, pi/2, pi, 3 * pi/2, 2 * pi), labels = c("0", 
                                                              expression(pi/2), expression(pi), expression(3 * 
                                                                                                             pi/2), expression(2 * pi)))
  }
  else if (xscale == 24) {
    axis(1, at = c(0, 6, 12, 18, 24), labels = c("Midnight", 
                                                 "Sunrise", "Noon", "Sunset", "Midnight"))
  }
  else {
    axis(1)
  }
  lines(xx, densA, lty = linetype[1], col = linecol[1])
  lines(xx, densB, lty = linetype[2], col = linecol[2])
  return(invisible(list(x = xx, densityA = densA, densityB = densB)))
}


# Function for plotting 3 species ------------------------------------------

timeplot3 <-function (A, B, C, xscale = 24, linecol = c("#e41a1c","#377eb8","#4daf4a"), 
                       linetype = c(1,2,3), n.grid = 128, kmax = 3, adjust = 1, 
                       ...) 
{
  bwA <- getBandWidth(A, kmax = kmax)/adjust
  bwB <- getBandWidth(B, kmax = kmax)/adjust
  bwC <- getBandWidth(C, kmax = kmax)/adjust
  if (is.na(bwA) || is.na(bwB)) 
    stop("Bandwidth estimation failed.")
  xsc <- if (is.na(xscale))
    1
  else xscale/(2 * pi)
  xxRad <- seq(0, 2 * pi, length = n.grid)
  xx <- xxRad * xsc
  densA <- densityFit(A, xxRad, bwA)/xsc
  densB <- densityFit(B, xxRad, bwB)/xsc
  densC <- densityFit(C, xxRad, bwC)/xsc
  densOL <- pmin(densA, densB, densC)
  ylim <- c(0, max(densA, densB, densC))
  plot(0, 0, type = "n", ylim = ylim, xlim = range(xx), xlab = "Time", 
       ylab = "Density", xaxt = "n", ...)
  if (is.na(xscale)) {
    axis(1, at = c(0, pi/2, pi, 3 * pi/2, 2 * pi), labels = c("0", 
                                                              expression(pi/2), expression(pi), expression(3 * 
                                                                                                             pi/2), expression(2 * pi)))
  }
  else if (xscale == 24) {
    axis(1, at = c(0, 6, 12, 18, 24), labels = c("Midnight", 
                                                 "Sunrise", "Noon", "Sunset", "Midnight"))
  }
  else {
    axis(1)
  }
  lines(xx, densA, col = linecol[1], lwd=2, lty = linetype[1])
  lines(xx, densB, col = linecol[2], lwd=2, lty = linetype[2])
  lines(xx, densC, col = linecol[3], lwd=2, lty = linetype[3])
  return(invisible(list(x = xx, densityA = densA, densityB = densB, densityC = densC)))
}




# Function for plotting 4 species ------------------------------------------

timeplot4 <-function (A, B, C, D, xscale = 24, linecol = c("#e41a1c","#377eb8","#4daf4a", "black"),
                        linetype = c(1,2,4,3),
                        n.grid = 128, kmax = 3, adjust = 1, 
                        ...) 
{
  bwA <- getBandWidth(A, kmax = kmax)/adjust
  bwB <- getBandWidth(B, kmax = kmax)/adjust
  bwC <- getBandWidth(C, kmax = kmax)/adjust
  bwD <- getBandWidth(D, kmax = kmax)/adjust
  if (is.na(bwA) || is.na(bwB)) 
    stop("Bandwidth estimation failed.")
  xsc <- if (is.na(xscale))
    1
  else xscale/(2 * pi)
  xxRad <- seq(0, 2 * pi, length = n.grid)
  xx <- xxRad * xsc
  densA <- densityFit(A, xxRad, bwA)/xsc
  densB <- densityFit(B, xxRad, bwB)/xsc
  densC <- densityFit(C, xxRad, bwC)/xsc
  densD <- densityFit(D, xxRad, bwD)/xsc
  densOL <- pmin(densA, densB, densC, densD)
  ylim <- c(0, max(densA, densB, densC, densD))
  plot(0, 0, type = "n", ylim = ylim, xlim = range(xx), xlab = "Time", 
       ylab = "Density", xaxt = "n", ...)
  if (is.na(xscale)) {
    axis(1, at = c(0, pi/2, pi, 3 * pi/2, 2 * pi), labels = c("0", 
                                                              expression(pi/2), expression(pi), expression(3 * 
                                                                                                             pi/2), expression(2 * pi)))
  }
  else if (xscale == 24) {
    axis(1, at = c(0, 6, 12, 18, 24), labels = c("Midnight", 
                                                 "Sunrise", "Noon", "Sunset", "Midnight"))
  }
  else {
    axis(1)
  }
  lines(xx, densA, col = linecol[1], lwd=2, lty = linetype[1])
  lines(xx, densB, col = linecol[2], lwd=2, lty = linetype[2])
  lines(xx, densC, col = linecol[3], lwd=2, lty = linetype[3])
  lines(xx, densD, col = linecol[4], lwd=2, lty = linetype[4])
  return(invisible(list(x = xx, densityA = densA, densityB = densB, densityC = densC, densityD=densB)))
}
