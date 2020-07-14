#goal: take GNP data and make it look like Rota data

library(camtrapR) #install.packages("camtrapR")
library(tidyverse)

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

#unnecessary because record table already exists
# define image directory
#wd_images_ID <- system.file("pictures/sample_images_species_dir", package = "camtrapR")

# make record table
#recordTableSample <- recordTable(inDir               = wd_images_ID,
#                                 IDfrom              = "directory",
#                                 minDeltaTime        = 15,
#                                 deltaTimeComparedTo = "lastIndependentRecord",
#                                 timeZone            = "Asia/Kuala_Lumpur"
#)

#load in Gorongosa record table
record_table <- read_csv("data/gorongosa-cameras/recordtable_allrecordscleaned_speciesmetadata.csv")

# make detection history for genets (without trapping effort)
DetHist_genet <- detectionHistory(recordTable     = record_table,
                             camOp                = camop,
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
