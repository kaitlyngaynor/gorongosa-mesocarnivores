#goal: take GNP data and make it look like Rota data

library(camtrapR) #install.packages("camtrapR")
library(tidyverse)
# setwd("")

#load in Gorongosa camera operability data table
camtraps <- read_csv("data/gorongosa-cameras/Camera_operation_years1and2.csv")

# create camera operation matrix
camop <- cameraOperation(CTtable      = camtraps,
                         stationCol   = "Camera",
                         setupCol     = "Start",
                         retrievalCol = "End",
                         hasProblems  = TRUE,
                         dateFormat   = "mdy"
)

# define image directory
wd_images_ID <- system.file("pictures/sample_images_species_dir", package = "camtrapR")

# make record table
recordTableSample <- recordTable(inDir               = wd_images_ID,
                                 IDfrom              = "directory",
                                 minDeltaTime        = 60,
                                 deltaTimeComparedTo = "lastIndependentRecord",
                                 timeZone            = "Asia/Kuala_Lumpur"
)

# make detection history (without trapping effort)
DetHist1 <- detectionHistory(recordTable         = recordTableSample,
                             camOp                = camop_no_problem,
                             stationCol           = "Station",
                             speciesCol           = "Species",
                             recordDateTimeCol    = "DateTimeOriginal",
                             species              = "VTA",
                             occasionLength       = 7,
                             day1                 = "station",
                             includeEffort        = FALSE
)