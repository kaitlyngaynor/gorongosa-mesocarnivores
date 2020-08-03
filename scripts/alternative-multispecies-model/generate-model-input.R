# The goal here is to be able to easily generate summarized occupancy records for different time periods in the necessary format for analysis using Lindsey's code.

library(tidyverse)
library(sjmisc)

# read in master matrix from master folder (locally on Kaitlyn's computer)
matrix.all <- read.csv("~/Documents/github-repos/gorongosa/gorongosa-camera-traps/data/raw-data/All_species_by_date_year1and2_082919.csv", header=T) %>% dplyr::rename(StudySite = Camera)
# note that because column names are dates, and we use read.csv rather than read_csv, there is an X added to date (could have been done differently but here we are)

# Write function for generating files -------------------------------------

# function clean_for_occupancy takes the arguments:
    # start and end dates of the period (formatted like they are in the column names
    # the name of the period (for the file name)
    # the minimum number of days for which camera must be operating, otherwise it's considered to be 0 (default = 0)
clean_for_occupancy <- function(matrix.all = matrix.all, start.date, end.date, name, min.operation = 0) {

    # GENERATE CAMERA OPERATION SPREADSHEET
    
    # selects columns within specified dates
    matrix.subset.2 <- select(matrix.all, StudySite, Species, start.date:end.date) 
    
    # count the columns with zeroes and ones
    zeroes <- row_count(matrix.subset.2, start.date:end.date, count = 0, append = FALSE) 
    ones <- row_count(matrix.subset.2, start.date:end.date, count = 1, append = FALSE)
    
    # add zeroes and ones together into a new data frame
    Operation <- zeroes + ones 
    
    # combine this count dataframe with the matrix subset
    matrix.subset.2 <- cbind(matrix.subset.2, Operation) 
    
    # put the row count into a new variable called "Operation"
    matrix.subset.2$Operation <- matrix.subset.2$rowcount 
    
    # get rid of the day detections and species, just select StudySite, Operation
    matrix.subset.2 <- select(matrix.subset.2, StudySite, Operation) 
    
    # extract only the top 60 rows, since they are duplicated 42 times (one for each species)
    matrix.subset.2 <- matrix.subset.2[1:60,] 
    
    # if operation < minimum, then just set to 0
    for(i in 1:60) {
        if(matrix.subset.2[i,2] < min.operation) {
            matrix.subset.2[i,2] <- 0
        }
    }
    
    # export csv
    start.date.2 <- gsub("[.]", "_", start.date)
    start.date.2 <- gsub("X", "", start.date.2)
    
    end.date.2 <- gsub("[.]", "_", end.date)
    end.date.2 <- gsub("X", "", end.date.2)
    
    write.csv(matrix.subset.2, file = paste("scripts/alternative-multispecies-model/camoperation-", name, "-", start.date.2, "-", end.date.2, ".csv", collapse = "", sep = ""),row.names=F)
    
    
    # GENERATE DETECTION SPREADSHEET
    
    # selects columns within specified dates
    matrix.subset <- select(matrix.all, StudySite, Species, start.date:end.date) 
    
    # sum rows
    matrix.subset$Detections <- rowSums(select(matrix.subset, start.date:end.date), na.rm=TRUE) 
    
    # get rid of the day detections, just select StudySite, Species, Detections
    matrix.subset <- select(matrix.subset, StudySite, Species, Detections) 
    
    # spread data so one column per species, one row per camera
    matrix.final <- spread(matrix.subset, key = Species, value = Detections)
    
    # merge with operation to drop dates outside minimum operation value
    matrix.final <- left_join(matrix.final, matrix.subset.2)
    
    # if operation = 0, then set detections = 0
    for(i in 1:60) {
        for(j in 2:39)
            if(matrix.final[i,40] == 0) {
                matrix.final[i,j] <- 0
            }
    }
    
    # remove Operation
    matrix.final <- select(matrix.final, -Operation)
    
    # export csv
    write.csv(matrix.final, file = paste("scripts/alternative-multispecies-model/detections-", name, "-", start.date.2, "-", end.date.2, ".csv", collapse = "", sep = ""),row.names=F)
    
}

# Run for dry season that matches Katie
clean_for_occupancy(matrix.all, "X8.1.16", "X11.30.16", "dry2016_katie", min.operation = 0)

