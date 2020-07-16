#goal: take GNP data and make it look like Rota data

library(camtrapR) #install.packages("camtrapR")
library(tidyverse)

# define start and end date - these are used to subset both operation matrix and record table
start.date <- "2016-07-01"
end.date <- "2016-09-30"

#load in Gorongosa camera operations data table
camtraps <- read_csv("data/gorongosa-cameras/Camera_operation_years1and2.csv")

# create camera operation matrix
camop <- cameraOperation(CTtable      = camtraps,
                         stationCol   = "Camera",
                         setupCol     = "Start",
                         retrievalCol = "End",
                         hasProblems  = TRUE,
                         dateFormat   = "mdy"
)

# subset to dates of interest
camop_subset <- camop %>% 
    as.data.frame %>% # first need to convert matrix to data frame
    select(start.date:end.date) %>% # select columns of interest
    as.matrix() # get it back into matrix form for calculating detection history

# load in Gorongosa record table
record_table <- read_csv("data/gorongosa-cameras/recordtable_allrecordscleaned_speciesmetadata.csv")

# subset to dates of interest
record_table_subset <- record_table %>% 
    mutate(Date = as.Date(DateTimeOriginal, # format date column as date for subsetting
                          format = "%m/%d/%y %H:%M")) %>% 
    filter(Date >= as.Date(start.date) & Date <= as.Date(end.date))

# make detection history for genets (without trapping effort)
DetHist_genet <- detectionHistory(recordTable     = record_table_subset,
                             camOp                = camop_subset,
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

# make detection history for civets (without trapping effort)
DetHist_civet <- detectionHistory(recordTable          = record_table_subset,
                                  camOp                = camop_subset,
                                  stationCol           = "Camera",
                                  speciesCol           = "Species",
                                  recordDateTimeCol    = "DateTimeOriginal",
                                  recordDateTimeFormat = "%m/%d/%y %H:%M",
                                  timeZone             = "Africa/Maputo",
                                  species              = "Civet",
                                  occasionLength       = 1, #sampling period (in days) represented by a single column in the occupancy matrix
                                  day1                 = "survey", #dates/columns in resulting matrix will match up (starts each row on the date the first camera was set up)
                                  includeEffort        = FALSE,
                                  occasionStartTime    = 12  #start at noon b/c nocturnal animals
)

DetHist_civet <- as.data.frame(DetHist_civet)

write_csv(DetHist_civet, "data/gorongosa-cameras/civet.csv", col_names = F)
