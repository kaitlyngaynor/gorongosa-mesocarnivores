# Temporal overlap analysis

# Import data -------------------------------------------------------------

library(overlap)
library(tidyverse)
library(circular)
library(ggplot2)
library(ggpubr)
library(grid)
library(ggplotify)
library(tidyverse)
library(dplyr)

# load in Gorongosa record table
record_table <- read_csv("data-publication/recordtable_allrecordscleaned.csv")

# this already has the "Time.Sun" column, where times have been scaled to radians 
# where pi/2 = sunrise, pi = solar noon, 3pi/2 = sunset, and 2pi = solar midnight
# so this is what we need for analysis


# Subset data to species of interest --------------------------

# extract the data for civets
civets <- record_table %>% 
    filter(Species == "Civet") 

# extract the data for genets
genets <- record_table %>% 
    filter(Species == "Genet") 

# extract the data for honey badgers
honey_badgers <- record_table %>%
    filter(Species == "Honey_badger")

# extract the data for marsh mongooses
marsh_mongoose <-  record_table %>%
  filter(Species == "Mongoose_marsh")

# Make activity pattern plot ------------------------------------------

# define function for plotting 4 species (noon to noon) 
timeplot4_noon <-function (A, B, C, D, xscale = 24, linecol = c("#e41a1c","#377eb8","#4daf4a", "black"),
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
    xxRad <- seq(pi, 3 * pi, length = n.grid)
    xx <- xxRad * xsc
    densA <- densityFit(A, xxRad, bwA)/xsc
    densB <- densityFit(B, xxRad, bwB)/xsc
    densC <- densityFit(C, xxRad, bwC)/xsc
    densD <- densityFit(D, xxRad, bwD)/xsc
    densOL <- pmin(densA, densB, densC, densD)
    ylim <- c(0, max(densA, densB, densC, densD))
    plot(0, 0, type = "n", ylim = ylim, xlim = range(xx), xlab = "Time of Day", 
         ylab = "Density of Activity", xaxt = "n", ...)
    if (is.na(xscale)) {
        axis(1, at = c(pi, 3*pi/2, 2*pi, 5 * pi/2, 3 * pi), labels = c("0", 
                                                                       expression(pi/2), expression(pi), expression(3 * 
                                                                                                                        pi/2), expression(2 * pi)))
    }
    else if (xscale == 24) {
        axis(1, at = c(12, 18, 24, 30, 36), labels = c("Noon", 
                                                       "Sunset", "Midnight", "Sunrise", "Noon"))
    }
    else {
        axis(1)
    }
    lines(xx, densA, col = linecol[1], lwd=3, lty = linetype[1])
    lines(xx, densB, col = linecol[2], lwd=3, lty = linetype[2])
    lines(xx, densC, col = linecol[3], lwd=3, lty = linetype[3])
    lines(xx, densD, col = linecol[4], lwd=3, lty = linetype[4])
    return(invisible(list(x = xx, densityA = densA, densityB = densB, densityC = densC, densityD=densB)))
}

#generate plot
timeplot4_noon(genets$Time.Sun, civets$Time.Sun, honey_badgers$Time.Sun, marsh_mongoose$Time.Sun, linecol = c("#d55e00", "#0072b2", "#f0e442", "#009e73"),
               linetype = c(1,1,1,1))
legend("topleft", c("Genet", "Civet", "Honey Badger", "Marsh Mongoose"), lty= c(1,1,1,1), col=c("#d55e00", "#0072b2", "#f0e442", "#009e73"),
       bg="white", cex = 0.8)


# Calculate overlap coefficient -------------------------------------------

#genet:civet
overlapEst(genets$Time.Sun, civets$Time.Sun)
# Dhat4 is what we want (for sample sizes > 50)
# this ranges from 0 (no overlap) to 1 (complete overlap)

#genet:honey badger
overlapEst(genets$Time.Sun, honey_badgers$Time.Sun)

#genet:marsh mongoose
overlapEst(genets$Time.Sun, marsh_mongoose$Time.Sun)

#civet:honey badger
overlapEst(civets$Time.Sun, honey_badgers$Time.Sun)

#civet:marsh mongoose
overlapEst(civets$Time.Sun, marsh_mongoose$Time.Sun)

#honey badger:marsh mongoose
overlapEst(honey_badgers$Time.Sun, marsh_mongoose$Time.Sun)


# Calculate confidence intervals with bootstrapping -------------------------------------------

#genet:civet
Dhats_inout <- overlapEst(genets$Time.Sun, civets$Time.Sun)
bs_civets <- resample(civets$Time.Sun, 10000)
bs_genets <- resample(genets$Time.Sun, 10000)
bsOut <- bootEst(bs_genets, bs_civets)
colMeans(bsOut) ## dhat bootstrapped
bs <- as.vector(bsOut[,2])
(bsCI_inout <- bootCI(Dhats_inout[2], bs)) ## use basic

#genet:honey badger
Dhats_inout_gh <- overlapEst(genets$Time.Sun, honey_badgers$Time.Sun)
bs_genets <- resample(genets$Time.Sun, 10000)
bs_honey_badgers <- resample(honey_badgers$Time.Sun, 10000)
bsOut_gh <- bootEst(bs_genets, bs_honey_badgers)
colMeans(bsOut_gh) ## dhat bootstrapped
bs_gh <- as.vector(bsOut_gh[,2])
(bsCI_inout_gh <- bootCI(Dhats_inout_gh[2], bs_gh)) ## use basic

#genet: marsh mongoose
Dhats_inout_gm <- overlapEst(genets$Time.Sun, marsh_mongoose$Time.Sun)
bs_genets <- resample(genets$Time.Sun, 10000)
bs_marsh_mongoose <- resample(marsh_mongoose$Time.Sun, 10000)
bsOut_gm <- bootEst(bs_genets, bs_marsh_mongoose)
colMeans(bsOut_gm) ## dhat bootstrapped
bs_gm <- as.vector(bsOut_gm[,2])
(bsCI_inout_gm <- bootCI(Dhats_inout_gm[2], bs_gm)) ## use basic

#civet: honey badger
Dhats_inout_ch <- overlapEst(civets$Time.Sun, honey_badgers$Time.Sun)
bsOut_ch <- bootEst(bs_civets, bs_honey_badgers)
colMeans(bsOut_ch) ## dhat bootstrapped
bs_ch <- as.vector(bsOut_ch[,2])
(bsCI_inout_ch <- bootCI(Dhats_inout_ch[2], bs_ch)) ## use basic

#civert: marsh mongoose
Dhats_inout_cm <- overlapEst(civets$Time.Sun, marsh_mongoose$Time.Sun)
bsOut_cm <- bootEst(bs_civets, bs_marsh_mongoose)
colMeans(bsOut_cm) ## dhat bootstrapped
bs_cm <- as.vector(bsOut_cm[,2])
(bsCI_inout_cm <- bootCI(Dhats_inout_cm[2], bs_cm)) ## use basic

#honey badger: marsh mongoose
Dhats_inout_hm <- overlapEst(honey_badgers$Time.Sun, marsh_mongoose$Time.Sun)
bsOut_hm <- bootEst(bs_honey_badgers, bs_marsh_mongoose)
colMeans(bsOut_hm) ## dhat bootstrapped
bs_hm <- as.vector(bsOut_hm[,2])
(bsCI_inout_hm <- bootCI(Dhats_inout_hm[2], bs_hm)) ## use basic

