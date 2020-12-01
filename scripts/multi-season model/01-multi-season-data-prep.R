#Need to create detection histories for each species of interest for each year

library(camtrapR) #install.packages("camtrapR")
library(tidyverse)
library(dplyr)

# define start and end date - these are used to subset both operation matrix and record table
start.date.17 <- "2017-08-01"
end.date.17 <- "2017-11-30"

#load in Gorongosa camera operations data table
#correct for 2016 and 2017 (as far as I can tell)
camtraps <- read_csv("data/gorongosa-cameras/Camera_operation_years1and2.csv")

# create camera operation matrix, correct for 2016 and 2017 
camop <- cameraOperation(CTtable      = camtraps,
                         stationCol   = "Camera",
                         setupCol     = "Start",
                         retrievalCol = "End",
                         hasProblems  = TRUE,
                         dateFormat   = "mdy"
)

# subset to dates of interest 2017
camop_subset_17 <- camop %>% 
  as.data.frame %>% # first need to convert matrix to data frame
  select(start.date.17:end.date.17) %>% # select columns of interest
  as.matrix() # get it back into matrix form for calculating detection history

# load in Gorongosa record table, I think the record table includes 2017 as well
record_table <- read_csv("data/gorongosa-cameras/recordtable_allrecordscleaned_speciesmetadata.csv")

# subset to dates of interest
record_table_subset_17 <- record_table %>% 
  mutate(Date = as.Date(DateTimeOriginal, # format date column as date for subsetting
                        format = "%m/%d/%y %H:%M")) %>% 
  filter(Date >= as.Date(start.date.17) & Date <= as.Date(end.date.17))

# make detection history for genets (without trapping effort)
# I think this is cranky because some of the cameras were removed? need to check
DetHist_genet_17 <- detectionHistory(recordTable     = record_table_subset_17,
                                  camOp                = camop_subset_17,
                                  stationCol           = "Camera",
                                  speciesCol           = "Species",
                                  recordDateTimeCol    = "DateTimeOriginal",
                                  recordDateTimeFormat = "%m/%d/%y %H:%M",
                                  timeZone             = "Africa/Maputo",
                                  species              = "Genet",
                                  occasionLength       = 1, #sampling period (in days) represented by a single column in the occupancy matrix
                                  day1                 = "survey", #dates/columns in resulting matrix will match up (starts each row on the date the first camera was set up)
                                  includeEffort        = FALSE,
                                  occasionStartTime    = 12  #start at noon b/c nocturnal animals
)

DetHist_genet <- as.data.frame(DetHist_genet)

write_csv(DetHist_genet, "data/gorongosa-cameras/genet.csv", col_names = F)
