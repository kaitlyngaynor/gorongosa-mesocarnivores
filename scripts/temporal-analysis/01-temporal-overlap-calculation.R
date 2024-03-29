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

#magic loop from Kaitlyn--------------------------------------------------

#should be able to work with cleaned data
#make one record table for 1 min between records
record.table.1 <- read_csv("data-publication/recordtable_cleaned_EE.csv")

# Make an empty column to mark photos in
record.table.1$Check <- as.character("")

# store datetimeoriginal in new column, and format it as date
record.table.1$DTNew <- as.POSIXct(record.table.1$DateTimeOriginal, format = "%m/%d/%Y %H:%M")

# Make sure we're actually sorted in chronological/alphabetical order
record.table.1 <- record.table.1[with(record.table.1, order(Camera, Species, DTNew)),]

# Run the magic loop
#### Assuming that all photos are timestamped sequentially, this loop compares each line of data to the line
#### before it, and sees if they have the same camera/species/date, and, if so, whether they fall within 30 seconds
for(i in 1:nrow(record.table.1)){
  ifelse(as.numeric(difftime(record.table.1$DTNew[i+1], record.table.1$DTNew[i], units = "mins")) < 1 # if less than 15 min apart
         && record.table.1[i, "Camera"] == record.table.1[i+1, "Camera"] # and same camera
         && record.table.1[i, "Species"] == record.table.1[i+1, "Species"], # and same species
         record.table.1[i+1, "Check"] <- "REMOVE", record.table.1[i,]) 
}
# then filter out all records where "Check" == "REMOVE"

sum(record.table.1$Check == "REMOVE")

#make another record table with >10 min between records

#should be able to work with cleaned data
#make one record table for 1 min between records
record.table.10 <- read_csv("data-publication/recordtable_cleaned_EE.csv")

# Make an empty column to mark photos in
record.table.10$Check <- as.character("")

# store datetimeoriginal in new column, and format it as date
record.table.10$DTNew <- as.POSIXct(record.table.10$DateTimeOriginal, format = "%m/%d/%Y %H:%M")

# Make sure we're actually sorted in chronological/alphabetical order
record.table.10 <- record.table.10[with(record.table.10, order(Camera, Species, DTNew)),]

# Run the magic loop
#### Assuming that all photos are timestamped sequentially, this loop compares each line of data to the line
#### before it, and sees if they have the same camera/species/date, and, if so, whether they fall within 30 seconds
for(i in 1:nrow(record.table.10)){
  ifelse(as.numeric(difftime(record.table.10$DTNew[i+1], record.table.10$DTNew[i], units = "mins")) < 10 # if less than 15 min apart
         && record.table.10[i, "Camera"] == record.table.10[i+1, "Camera"] # and same camera
         && record.table.10[i, "Species"] == record.table.10[i+1, "Species"], # and same species
         record.table.10[i+1, "Check"] <- "REMOVE", record.table.10[i,]) 
}
# then filter out all records where "Check" == "REMOVE"

sum(record.table.10$Check == "REMOVE")

#make record table using original 30 sec
#make one record table for 1 min between records
record.table <- read_csv("data-publication/recordtable_cleaned_EE.csv")

# Make an empty column to mark photos in
record.table$Check <- as.character("")

# store datetimeoriginal in new column, and format it as date
record.table$DTNew <- as.POSIXct(record.table$DateTimeOriginal, format = "%m/%d/%Y %H:%M")

# Make sure we're actually sorted in chronological/alphabetical order
record.table <- record.table[with(record.table, order(Camera, Species, DTNew)),]

# Run the magic loop
#### Assuming that all photos are timestamped sequentially, this loop compares each line of data to the line
#### before it, and sees if they have the same camera/species/date, and, if so, whether they fall within 30 seconds
for(i in 1:nrow(record.table.1)){
  ifelse(as.numeric(difftime(record.table.1$DTNew[i+1], record.table.1$DTNew[i], units = "mins")) < 1 # if less than 15 min apart
         && record.table.1[i, "Camera"] == record.table.1[i+1, "Camera"] # and same camera
         && record.table.1[i, "Species"] == record.table.1[i+1, "Species"], # and same species
         record.table.1[i+1, "Check"] <- "REMOVE", record.table.1[i,]) 
}
# then filter out all records where "Check" == "REMOVE"

sum(record.table.1$Check == "REMOVE")


# load in Gorongosa record table (note: if you use read_csv from tidyverse instead of read.csv, it will automatically format date)
# already done above
# record_table <- read_csv("data/gorongosa-cameras/recordtable_allrecordscleaned_speciesmetadata.csv")

# this already has the "TimeSun" column, where times have been scaled to radians 
# where pi/2 = sunrise, pi = solar noon, 3pi/2 = sunset, and 2pi = solar midnight
# so this is what we need for analysis!


# Subset data to periods and species of interest --------------------------

# subset record table to dates of interest 
#already done with new input file
#record_table_subset <- record_table[record_table$Date >= as.Date("8/1/16", format = "%m/%d/%y") #inclusive dates
#                                    & record_table$Date <= as.Date("11/30/16", format = "%m/%d/%y"),]

#for 1 min
# just extract the data for civets for the dates of interest
civets.1 <- record.table.1 %>% 
    filter(Species == "Civet") 

# just extract the data for genets for the dates of interest
genets.1 <- record.table.1 %>% 
    filter(Species == "Genet") 

# just extract the data for honey badgers for the dates of interest
honey_badgers.1 <- record.table.1 %>%
    filter(Species == "Honey_badger")

#just extract the data for marsh mongooses for the dates of interest
marsh_mongoose.1 <-  record.table.1 %>%
  filter(Species == "Mongoose_marsh")

#for 10 min
# just extract the data for civets for the dates of interest
civets.10 <- record.table.10 %>% 
  filter(Species == "Civet") 

# just extract the data for genets for the dates of interest
genets.10 <- record.table.10 %>% 
  filter(Species == "Genet") 

# just extract the data for honey badgers for the dates of interest
honey_badgers.10 <- record.table.10 %>%
  filter(Species == "Honey_badger")

#just extract the data for marsh mongooses for the dates of interest
marsh_mongoose.10 <-  record.table.10 %>%
  filter(Species == "Mongoose_marsh")

#for OG 30 seconds
# just extract the data for civets for the dates of interest
civets <- record.table %>% 
  filter(Species == "Civet") 

# just extract the data for genets for the dates of interest
genets <- record.table %>% 
  filter(Species == "Genet") 

# just extract the data for honey badgers for the dates of interest
honey_badgers <- record.table %>%
  filter(Species == "Honey_badger")

#just extract the data for marsh mongooses for the dates of interest
marsh_mongoose <-  record.table %>%
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
source("scripts/temporal-analysis/02-temporal-figure-functions.R")

#seeing what they all create
timeplot1(honey_badgers$Time.Sun)

timeplot2(honey_badgers$Time.Sun, marsh_mongoose$Time.Sun)

timeplot2_overlap(honey_badgers$Time.Sun, marsh_mongoose$Time.Sun)

#only shades overlap polygon, not all banded areas
timeplot2_overlap_katie(honey_badgers$Time.Sun, marsh_mongoose$Time.Sun, linetype = c(1, 2), linecol = c("black", "blue"),linewidth = c(5,5))

#from noon to noon, rather than midnight to midnight
honey_badger_marsh_mongoose_plot <- timeplot2_overlap_noon(honey_badgers$Time.Sun, marsh_mongoose$Time.Sun, linetype = c(1, 2), linecol = c("black", "blue"),linewidth = c(5, 5))
legend("topleft", c("Honey Badger", "Marsh Mongoose"), lty= c(1,2), col=c("black", "blue"), bg="white")

timeplot3(civets$Time.Sun, genets$Time.Sun, honey_badgers$Time.Sun)

timeplot4(civets$Time.Sun, genets$Time.Sun, honey_badgers$Time.Sun, marsh_mongoose$Time.Sun)

#making one for each pair

#genet:civet
genet_civet_plot <- timeplot2_overlap_noon(genets$Time.Sun, civets$Time.Sun, linetype = c(1, 2), linecol = c("black", "blue"),linewidth = c(5, 5))
legend("topleft", c("Genet", "Civet"), lty= c(1,2), col=c("black", "blue"), bg="white")

#genet:honey badger
genet_honey_badger_plot <- timeplot2_overlap_noon(genets$Time.Sun, honey_badgers$Time.Sun, linetype = c(1, 2), linecol = c("black", "blue"),linewidth = c(5, 5))
legend("topleft", c("Genet", "Honey Badger"), lty= c(1,2), col=c("black", "blue"), bg="white")

#genet:marsh mongoose
genet_marsh_mongoose_plot <- timeplot2_overlap_noon(genets$Time.Sun, marsh_mongoose$Time.Sun, linetype = c(1, 2), linecol = c("black", "blue"),linewidth = c(5, 5))
legend("topleft", c("Genet", "Mongoose"), lty= c(1,2), col=c("black", "blue"), bg="white")

#civet: honey badger
civet_honey_badger_plot <- timeplot2_overlap_noon(civets$Time.Sun, honey_badgers$Time.Sun, linetype = c(1, 2), linecol = c("black", "blue"),linewidth = c(5, 5))
legend("topleft", c("Civet", "Honey Badger"), lty= c(1,2), col=c("black", "blue"), bg="white")

#civet: marsh mongoose
civet_marsh_mongoose_plot <- timeplot2_overlap_noon(civets$Time.Sun, marsh_mongoose$Time.Sun, linetype = c(1, 2), linecol = c("black", "blue"),linewidth = c(5, 5))
legend("topleft", c("Civet", "Mongoose"), lty= c(1,2), col=c("black", "blue"), bg="white")

#honey badger: marsh mongoose
honey_badger_marsh_mongoose_plot <- timeplot2_overlap_noon(honey_badgers$Time.Sun, marsh_mongoose$Time.Sun, linetype = c(1, 2), linecol = c("black", "blue"),linewidth = c(5, 5))
legend("topleft", c("Honey Badger", "Marsh Mongoose"), lty= c(1,2), col=c("black", "blue"), bg="white")

#arrange the plots [currently not working] - will not work if not ggplot!
ggarrange(genet_civet_plot, genet_honey_badger_plot, genet_marsh_mongoose_plot, civet_honey_badger_plot, civet_marsh_mongoose_plot, 
          honey_badger_marsh_mongoose_plot, ncol = 2, nrow = 3)

# here's a workaround
#genet: darkorange, 1 [HAS NOT BEEN FULLY IMPLEMENTED THROUGHOUT, PAY ATTENTION]
#civet: blue, 2
#honey badger: red, 3
#marsh mongoose: darkgreen, 4
par(mfrow=c(2,3))
timeplot2_overlap_noon(genets$Time.Sun, civets$Time.Sun, linetype = c(1, 2), linecol = c("black", "blue"),linewidth = c(5, 5))
legend("topleft", c("Genet", "Civet"), lty= c(1,2), col=c("black", "blue"), bg="white")
genet_honey_badger_plot <- timeplot2_overlap_noon(genets$Time.Sun, honey_badgers$Time.Sun, linetype = c(1, 3), linecol = c("black", "red"),linewidth = c(5, 5))
legend("topleft", c("Genet", "Honey Badger"), lty= c(1,3), col=c("black", "red"), bg="white")
timeplot2_overlap_noon(genets$Time.Sun, marsh_mongoose$Time.Sun, linetype = c(1, 4), linecol = c("black", "darkgreen"),linewidth = c(5, 5))
legend("topleft", c("Genet", "Mongoose"), lty= c(1,4), col=c("black", "darkgreen"), bg="white")
timeplot2_overlap_noon(civets$Time.Sun, honey_badgers$Time.Sun, linetype = c(2, 3), linecol = c("blue", "red"),linewidth = c(5, 5))
legend("topleft", c("Civet", "Honey Badger"), lty= c(2,3), col=c("blue", "red"), bg="white")
timeplot2_overlap_noon(civets$Time.Sun, marsh_mongoose$Time.Sun, linetype = c(2, 4), linecol = c("blue", "darkgreen"),linewidth = c(5, 5))
legend("topleft", c("Civet", "Mongoose"), lty= c(2,4), col=c("blue", "darkgreen"), bg="white")
timeplot2_overlap_noon(honey_badgers$Time.Sun, marsh_mongoose$Time.Sun, linetype = c(3,4), linecol = c("red", "darkgreen"),linewidth = c(5, 5))
legend("topleft", c("Honey Badger", "Marsh Mongoose"), lty= c(3,4), col=c("red", "darkgreen"), bg="white")
# you then need to clear the plot if you want normal plots in here afterward

# or just use the 4 line version
pdf("scripts/figures/activity-patterns-all.pdf", width = 8, height = 5)
timeplot4_noon(genets$Time.Sun, civets$Time.Sun, honey_badgers$Time.Sun, marsh_mongoose$Time.Sun, linecol = c("darkorange", "blue", "red", "darkgreen"),
               linetype = c(1,2,3,4))
legend("topleft", c("Genet", "Civet", "Honey Badger", "Marsh Mongoose"), lty= c(1,2,3,4), col=c("darkorange", "blue", "red", "darkgreen"),
       bg="white", cex = 0.8)
dev.off()

#4 line version for 2023
pdf("scripts/figures/activity-patterns-all_23.pdf", width = 8, height = 5)
timeplot4_noon(genets$Time.Sun, civets$Time.Sun, honey_badgers$Time.Sun, marsh_mongoose$Time.Sun, linecol = c("#d55e00", "#0072b2", "#f0e442", "#009e73"),
               linetype = c(1,1,1,1))
legend("topleft", c("Genet", "Civet", "Honey Badger", "Marsh Mongoose"), lty= c(1,1,1,1), col=c("#d55e00", "#0072b2", "#f0e442", "#009e73"),
       bg="white", cex = 0.8)
dev.off()

#4 line version for 2023, 1 min filter
pdf("scripts/figures/activity-patterns-all_23_1min.pdf", width = 8, height = 5)
timeplot4_noon(genets.1$Time.Sun, civets.1$Time.Sun, honey_badgers.1$Time.Sun, marsh_mongoose.1$Time.Sun, linecol = c("#d55e00", "#0072b2", "#f0e442", "#009e73"),
               linetype = c(1,1,1,1))
legend("topleft", c("Genet", "Civet", "Honey Badger", "Marsh Mongoose"), lty= c(1,1,1,1), col=c("#d55e00", "#0072b2", "#f0e442", "#009e73"),
       bg="white", cex = 0.8)
dev.off()

#4 line version for 2023, 10 min filter
pdf("scripts/figures/activity-patterns-all_23_10min.pdf", width = 8, height = 5)
timeplot4_noon(genets.10$Time.Sun, civets.10$Time.Sun, honey_badgers.10$Time.Sun, marsh_mongoose.10$Time.Sun, linecol = c("#d55e00", "#0072b2", "#f0e442", "#009e73"),
               linetype = c(1,1,1,1))
legend("topleft", c("Genet", "Civet", "Honey Badger", "Marsh Mongoose"), lty= c(1,1,1,1), col=c("#d55e00", "#0072b2", "#f0e442", "#009e73"),
       bg="white", cex = 0.8)
dev.off()

# Compare distributions with Watson test ----------------------------------

#genet:civet
watson.two.test(genets$Time.Sun, civets$Time.Sun)
# will compare means of two distributions - p value < 0.05 indicates that they are significantly different
# you'll get a warning message to tell you that it's assuming these are radians

#genet:honey badger
watson.two.test(genets$Time.Sun, honey_badgers$Time.Sun)

#genet:marsh mongoose
watson.two.test(genets$Time.Sun, marsh_mongoose$Time.Sun)

#civet:honey badger
watson.two.test(civets$Time.Sun, honey_badgers$Time.Sun)

#civet:marsh mongoose
watson.two.test(civets$Time.Sun, marsh_mongoose$Time.Sun)

#honey badger:marsh mongoose
watson.two.test(honey_badgers$Time.Sun, marsh_mongoose$Time.Sun)

# Calculate overlap coefficient -------------------------------------------

#genet:civet 1 min
overlapEst(genets.1$Time.Sun, civets.1$Time.Sun) #0.8145
# Dhat4 is what we want (they are just different ways to calculate; dhat4 is for sample sizes > 50)
# this ranges from 0 (no overlap) to 1 (complete overlap)
# it's the area of the grey polygon under the curves (area under each curve is 1)

#genet:civet 10 min
overlapEst(genets.10$Time.Sun, civets.10$Time.Sun) #0.8145

#genet:honey badger 1 min
overlapEst(genets.1$Time.Sun, honey_badgers.1$Time.Sun) #0.7328

#genet:honey badger 10 min
overlapEst(genets.10$Time.Sun, honey_badgers.10$Time.Sun) #0.7328

#genet:marsh mongoose 1 min
overlapEst(genets.1$Time.Sun, marsh_mongoose.1$Time.Sun) #0.868

#genet:marsh mongoose 10 min
overlapEst(genets.10$Time.Sun, marsh_mongoose.10$Time.Sun)  #0.868

#genet:marsh mongoose OG 30 seconds
overlapEst(genets$Time.Sun, marsh_mongoose$Time.Sun) #0.868

#civet:honey badger 1 min
overlapEst(civets.1$Time.Sun, honey_badgers.1$Time.Sun) #0.747

#civet:honey badger 10 min
overlapEst(civets.10$Time.Sun, honey_badgers.10$Time.Sun) #0.747

#civet:marsh mongoose 1 min
overlapEst(civets.1$Time.Sun, marsh_mongoose.1$Time.Sun) #0.8403

#civet:marsh mongoose 10 min
overlapEst(civets.10$Time.Sun, marsh_mongoose.10$Time.Sun) #0.8403

#honey badger:marsh mongoose 1 min
overlapEst(honey_badgers.1$Time.Sun, marsh_mongoose.1$Time.Sun) #0.8017

#honey badger:marsh mongoose 10 min
overlapEst(honey_badgers.10$Time.Sun, marsh_mongoose.10$Time.Sun) #0.8017

#honey badger:marsh mongoose OG 30 seconds
overlapEst(honey_badgers$Time.Sun, marsh_mongoose$Time.Sun) #0.8017

# if you want a confidence interval, you have to get it by bootstrapping (takes a LONG time, be warned)
#genet:civet
Dhats_inout <- overlapEst(genets$Time.Sun, civets$Time.Sun)
bs_civets <- resample(civets$Time.Sun, 10000)
bs_genets <- resample(genets$Time.Sun, 10000)
bsOut <- bootEst(bs_genets, bs_civets)
colMeans(bsOut) ## dhat bootstrapped
bs <- as.vector(bsOut[,2])
(bsCI_inout <- bootCI(Dhats_inout[2], bs)) ## use basic

#genet:honey badger; added "gh" to avoid running the same thing as above accidentally?
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
colMeans(bsOut_hm) ## dhat bootstrapped, Dhat4 = 0.7868437
bs_hm <- as.vector(bsOut_hm[,2])
(bsCI_inout_hm <- bootCI(Dhats_inout_hm[2], bs_hm)) ## use basic
#basic: 0.7119402 - 0.9338601 (rerun 11-14-23)
