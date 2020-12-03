#Need to create detection histories for each species of interest for each year

library(camtrapR) #install.packages("camtrapR")
library(tidyverse)
library(dplyr)

# define start and end date - these are used to subset both operation matrix and record table
start.date.16 <- "2016-08-01"
end.date.16 <- "2016-11-30"
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

#subset to dates of interest for 2016, need to remove cameras that were inoperable in 2017
#list of sites to be removed: A06(1), B05(4), D09(12), E12(18), F09(23), G10(29), G12(30), H09(34), H11(35), H13(36), I14(42), J09(46), L09(56), L13(58), M08(59)
camop_subset_16 <- camop %>% 
  as.data.frame %>% # first need to convert matrix to data frame
  select(start.date.16:end.date.16) #%>% # select columns of interest
  #camop_subset_16[-c(1,4,12,18,23,29,30,34,35,36,42,46,56,58,59),] %>% #delete cameras that were inoperational during 2017 late dry (all NAs)
  #as.matrix() # get it back into matrix form for calculating detection history

#trying to remove rows from my data frame
#I couldn't get it to let me do this within the above piping. I also know this is a poorly re-creatable/messy way to do this
camop_subset_16 <- camop_subset_16[-c(1,4,12,18,23,29,30,34,35,36,42,46,56,58,59),] %>%
  as.matrix()

# subset to dates of interest 2017 and remove cameras that weren't operating during this period
# another line to achieve removal of NAs: camop_subset_17[complete.cases(camop_subset_17), ] (only works on data frames)
camop_subset_17 <- camop %>% 
  as.data.frame %>% # first need to convert matrix to data frame
  select(start.date.17:end.date.17) %>% # select columns of interest
  filter_all(any_vars(!is.na(.))) %>% #delete cameras that were inoperational during this period (all NAs)
  as.matrix() # get it back into matrix form for calculating detection history

# load in Gorongosa record table, I think the record table includes 2017 as well
record_table <- read_csv("data/gorongosa-cameras/recordtable_allrecordscleaned_speciesmetadata.csv")

# subset to dates of interest 2016
record_table_subset_16 <- record_table %>% 
  mutate(Date = as.Date(DateTimeOriginal, # format date column as date for subsetting
                        format = "%m/%d/%y %H:%M")) %>% 
  filter(Date >= as.Date(start.date.16) & Date <= as.Date(end.date.16))

# subset to dates of interest 2017
record_table_subset_17 <- record_table %>% 
  mutate(Date = as.Date(DateTimeOriginal, # format date column as date for subsetting
                        format = "%m/%d/%y %H:%M")) %>% 
  filter(Date >= as.Date(start.date.17) & Date <= as.Date(end.date.17))

# make detection history for genets 2016 (without trapping effort)
DetHist_genet_17 <- detectionHistory(recordTable     = record_table_subset_16,
                                     camOp                = camop_subset_16,
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

DetHist_genet_16 <- as.data.frame(DetHist_genet_16) #doesn't run due to mis-matched names

write_csv(DetHist_genet_16, "data/gorongosa-cameras/genet_16.csv", col_names = F) #n/a yet

# make detection history for genets 2017 (without trapping effort)
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

DetHist_genet_17 <- as.data.frame(DetHist_genet_17)

write_csv(DetHist_genet_17, "data/gorongosa-cameras/genet.17.csv", col_names = F)
