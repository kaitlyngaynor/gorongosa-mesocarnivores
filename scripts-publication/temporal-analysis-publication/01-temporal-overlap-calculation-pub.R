# Temporal overlap analysis
# Kaitlyn Gaynor, July 13 2020


# Import data -------------------------------------------------------------

library(overlap) #install.packages("overlap")
library(tidyverse)
library(circular) #install.packages("circular")
library(ggplot2)
library(ggpubr)
library("grid") #install.packages("grid") 
library("ggplotify") #install.packages("ggplotify") 
library(tidyverse)
library(dplyr)

# load in Gorongosa record table (note: if you use read_csv from tidyverse instead of read.csv, it will automatically format date)
record_table <- read_csv("data-publication/recordtable_allrecordscleaned.csv")

# this already has the "Time.Sun" column, where times have been scaled to radians 
# where pi/2 = sunrise, pi = solar noon, 3pi/2 = sunset, and 2pi = solar midnight
# so this is what we need for analysis


# Subset data to periods and species of interest --------------------------

start.date <- "2016-08-01"
end.date <- "2016-11-30"

# subset record table to dates of interest 
record_table_subset <- record_table %>% 
  mutate(Date = as.Date(DateTimeOriginal, # format date column as date for subsetting
                        format = "%m/%d/%Y %H:%M")) %>% 
  filter(Date >= as.Date(start.date) & Date <= as.Date(end.date))

# extract the data for civets for the dates of interest
civets <- record_table_subset %>% 
    filter(Species == "Civet") 

# extract the data for genets for the dates of interest
genets <- record_table_subset %>% 
    filter(Species == "Genet") 

# extract the data for honey badgers for the dates of interest
honey_badgers <- record_table_subset %>%
    filter(Species == "Honey_badger")

# extract the data for marsh mongooses for the dates of interest
marsh_mongoose <-  record_table_subset %>%
  filter(Species == "Mongoose_marsh")

# Make overlap plots using source code --------------------------------
source("scripts-publication/temporal-analysis-publication/02-temporal-figure-functions-pub.R")

#4 species temporal overlap
pdf("scripts-publication/figures-publication/activity-patterns-all_23.pdf", width = 8, height = 5)
timeplot4_noon(genets$Time.Sun, civets$Time.Sun, honey_badgers$Time.Sun, marsh_mongoose$Time.Sun, linecol = c("#d55e00", "#0072b2", "#f0e442", "#009e73"),
               linetype = c(1,1,1,1))
legend("topleft", c("Genet", "Civet", "Honey Badger", "Marsh Mongoose"), lty= c(1,1,1,1), col=c("#d55e00", "#0072b2", "#f0e442", "#009e73"),
       bg="white", cex = 0.8)
dev.off()

# Calculate overlap coefficient -------------------------------------------

#genet:civet
overlapEst(genets$Time.Sun, civets$Time.Sun)
# Dhat4 is what we want (they are just different ways to calculate; dhat4 is for sample sizes > 50)
# this ranges from 0 (no overlap) to 1 (complete overlap)
# it's the area of the grey polygon under the curves (area under each curve is 1)

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

# if you want a confidence interval, you have to get it by bootstrapping (takes a LONG time, be warned)
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

