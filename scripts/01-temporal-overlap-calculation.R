# Temporal overlap analysis
# Kaitlyn Gaynor, July 13 2020


# Import data -------------------------------------------------------------

library(overlap) #install.packages("overlap")
library(tidyverse)
library(circular) #install.packages("circular")

# Define figure functions --------------------------------------------------------
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


# load in Gorongosa record table (note: if you use read_csv from tidyverse instead of read.csv, it will automatically format date)
record_table <- read_csv("data/gorongosa-cameras/recordtable_allrecordscleaned_speciesmetadata.csv")

# this already has the "TimeSun" column, where times have been scaled to radians 
# where pi/2 = sunrise, pi = solar noon, 3pi/2 = sunset, and 2pi = solar midnight
# so this is what we need for analysis!


# Subset data to periods and species of interest --------------------------

# subset record table to dates of interest 
record_table_subset <- record_table[record_table$Date >= as.Date("8/1/16", format = "%m/%d/%y") #inclusive dates
                                    & record_table$Date <= as.Date("11/30/16", format = "%m/%d/%y"),]

# just extract the data for civets for the dates of interest
civets <- record_table_subset %>% 
    filter(Species == "Civet") 

# just extract the data for genets for the dates of interest
genets <- record_table_subset %>% 
    filter(Species == "Genet") 

# just extract the data for honey badgers for the dates of interest
honey_badgers <- record_table_subset %>%
    filter(Species == "Honey_badger")

#just extract the data for marsh mongooses for the dates of interest
marsh_mongoose <-  record_table_subset %>%
  filter(Species == "Mongoose_marsh")

# Make overlap plots ------------------------------------------------------

#genet:civet
genet_civet <- overlapPlot(genets$Time.Sun, civets$Time.Sun)

#genet:honey badger
genet_honey_badger <- overlapPlot(genets$Time.Sun, honey_badgers$Time.Sun)

#genet:marsh mongoose
genet_marsh_mongoose <- overlapPlot(genets$Time.Sun, marsh_mongoose$Time.Sun)

#civet:honey badger
civet_honey_badger <- overlapPlot(civets$Time.Sun, honey_badgers$Time.Sun)

#civet:marsh mongoose
civet_marsh_mongoose <- overlapPlot(civets$Time.Sun, marsh_mongoose$Time.Sun)

#honey badger:marsh mongoose
honey_badger_marsh_mongoose <- overlapPlot(honey_badgers$Time.Sun, marsh_mongoose$Time.Sun)
# shades the area corresponding to the coefficient of overlap
# I have code for making these prettier when the time comes

# Make overlap plots using Kaitlyn's functions --------------------------------
source("scripts/temporal-figure-functions.R")
timpeplot1(honey_badgers$Time.Sun)

# Compare distributions with Watson test ----------------------------------

watson.two.test(genets$Time.Sun, civets$Time.Sun)
# will compare means of two distributions - p value < 0.05 indicates that they are significantly different
# you'll get a warning message to tell you that it's assuming these are radians

# Calculate overlap coefficient -------------------------------------------

overlapEst(genets$Time.Sun, civets$Time.Sun)
# Dhat4 is what we want (they are just different ways to calculate; dhat4 is for sample sizes > 50)
# this ranges from 0 (no overlap) to 1 (complete overlap)
# it's the area of the grey polygon under the curves (area under each curve is 1)

# if you want a confidence interval, you have to get it by bootstrapping (takes a LONG time, be warned)
Dhats_inout <- overlapEst(civets$Time.Sun, genets$Time.Sun)
bs_civets <- resample(civets$Time.Sun, 10000)
bs_genets <- resample(genets$Time.Sun, 10000)
bsOut <- bootEst(bs_civets, bs_genets)
colMeans(bsOut) ## dhat bootstrapped
bs <- as.vector(bsOut[,2])
(bsCI_inout <- bootCI(Dhats_inout[2], bs)) ## use basic
