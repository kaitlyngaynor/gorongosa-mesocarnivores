# Temporal overlap analysis
# Kaitlyn Gaynor, July 13 2020


# Import data -------------------------------------------------------------

library(overlap) #install.packages("overlap")
library(tidyverse)
library(circular) #install.packages("circular")

# load in Gorongosa record table (note: if you use read_csv from tidyverse instead of read.csv, it will automatically format date)
record_table <- read_csv("data/gorongosa-cameras/recordtable_allrecordscleaned_speciesmetadata.csv")

# this already has the "TimeSun" column, where times have been scaled to radians 
# where pi/2 = sunrise, pi = solar noon, 3pi/2 = sunset, and 2pi = solar midnight
# so this is what we need for analysis!


# Subset data to periods and species of interest --------------------------

# subset record table to dates of interest - you'll need to change, this is arbitrary!
record_table_subset <- record_table[record_table$Date >= as.Date("7/1/16", format = "%m/%d/%y") #inclusive dates
                                    & record_table$Date <= as.Date("9/30/16", format = "%m/%d/%y"),]

# just extract the data for civets for the dates of interest
civets <- record_table_subset %>% 
    filter(Species == "Civet") 

# just extract the data for genets for the dates of interest
genets <- record_table_subset %>% 
    filter(Species == "Genet") 


# Make overlap plots ------------------------------------------------------

overlapPlot(civets$Time.Sun, genets$Time.Sun)
# I have code for making these prettier when the time comes

# Compare distributions with Watson test ----------------------------------

watson.two.test(civets$Time.Sun, genets$Time.Sun)
# will compare means of two distributions - p value < 0.05 indicates that they are significantly different
# you'll get a warning message to tell you that it's assuming these are radians

# Calculate overlap coefficient -------------------------------------------

overlapEst(civets$Time.Sun, genets$Time.Sun)
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
