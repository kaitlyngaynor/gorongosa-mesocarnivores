#Need to create detection histories for each species of interest for each year

library(camtrapR) #install.packages("camtrapR")
library(tidyverse)
library(dplyr)
library(unmarked)

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
# also need to cut the cameras we're not using (otherwise the function to create the detection history gets cranky)
record_table_subset_16 <- record_table %>% 
  mutate(Date = as.Date(DateTimeOriginal, # format date column as date for subsetting
                        format = "%m/%d/%y %H:%M")) %>% 
  filter(Date >= as.Date(start.date.16) & Date <= as.Date(end.date.16)) %>%
  filter(!Camera %in% c("A06", "B05", "D09", "E12", "F09", "G10", "G12", "H09", "H11", "H13", "I14","J09","L09","L13","M08")) #this removes all records from cameras that were inoperable in 2017

# subset to dates of interest 2017
record_table_subset_17 <- record_table %>% 
  mutate(Date = as.Date(DateTimeOriginal, # format date column as date for subsetting
                        format = "%m/%d/%y %H:%M")) %>% 
  filter(Date >= as.Date(start.date.17) & Date <= as.Date(end.date.17))

# make detection history for genets 2016 (without trapping effort)
#problem: the camera names from camOp don't match "Camera" (which come from record table csv); NOW SOLVED
DetHist_genet_16 <- detectionHistory(recordTable     = record_table_subset_16,
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

DetHist_genet_16 <- as.data.frame(DetHist_genet_16) 

write_csv(DetHist_genet_16, "data/gorongosa-cameras/genet_16.csv", col_names = F) 

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

write_csv(DetHist_genet_17, "data/gorongosa-cameras/genet_17.csv", col_names = F)

#will need to repeat for other species, not sure if there's a shortcut other than just copying, changing names and going with that

#need to combine '16 and '17 data frames
DetHist_genet_16_17 <- merge(DetHist_genet_16, DetHist_genet_17, by = 0) #merges two detection histories by row name

DetHist_genet_16_17$Row.names <- NULL #merging the two data frames created a Row.names column, which I don't need in the final detection history 

write_csv(DetHist_genet_16_17, "data/gorongosa-cameras/genet_16_17.csv", col_names = F)
#in double season excel sheet, DS starts 2017
#I *believe* this did what I wanted it to, but I'm also not entirely sure how to check? I checked visually, looking at the
#excel sheets for 2016, 2017 and then 2016/17 combined, and it looks right, but that also depends on the ones for 2016 and 2017
#having been created correctly

#need to convert dates to Julian dates (I think)
#which means creating a data frame with the Julain dates of the dry season for 2016 and 2017 as a row, repeated 45 times (number of sites)
GNP_DATE_16 <- seq(as.Date("2016-08-01"), as.Date("2016-11-30"), by="days") #create sequence of dates for 2016 late dry season
GNP_DATE_17 <- seq(as.Date("2017-08-01"), as.Date("2017-11-30"), by="days") #create sequence of dates for 2017 late dry season
GNP_DATE_16_17 <- c(GNP_DATE_16,GNP_DATE_17) #combine date sequences together
GNP_JDATE_16_17 <- julian(GNP_DATE_16_17, origin = as.Date("1970-01-01")) #converts to Julian dates (maybe?)
GNP_DATE <- matrix(GNP_JDATE_16_17, nrow(DetHist_genet_16_17), 244, byrow=TRUE) #creates a matrix with repeated rows; 244 is total number of columns/days
##^ this is a matrix with the Julian dates of every "survey" (camera trap day)

##This step is taken from the colext pdf, I still don't fully understand why I need it
DetHist_genet_16_17[is.na(GNP_DATE) != is.na(DetHist_genet_16_17)] <- NA #I think this sets any values where either the date or the 
#detection data is missing equal to NA (if both have data, nothing happens, and if neither has data, it's already NA, I believe)

#scale dates
GNP_DATE <- scale(GNP_DATE)

#create list of years
yrs <- as.character(2016:2017) #creates a list with the relevant years 
yrs <- matrix(yrs, nrow(DetHist_genet_16_17), 2, byrow=TRUE) 

#load occupancy covariates
#leaving the column names in for now
#includes distance to lake, tree cover, mound density, etc
occ_covs <- read_csv("data/gorongosa-cameras/GNP covariates.csv", col_names = TRUE) %>% as.data.frame()

#scaling all the non-binary covariates to address the NaN warnings
occ_covs$urema_dist = scale(occ_covs$urema_dist)
occ_covs$tree_hansen = scale(occ_covs$tree_hansen)
occ_covs$termite.large.count.100m = scale(occ_covs$termite.large.count.100m)
occ_covs$lion_latedry = scale(occ_covs$lion_latedry)
occ_covs$cover.ground = scale(occ_covs$cover.ground)

# load in GNP cam metadata
cam_meta <- read_csv("data/gorongosa-cameras/cam_metadata_fromfield_and_raw_raster_withlion.csv")

#make table with all covariates (environmental and detection)
GNP_covs <- select(cam_meta, StudySite, urema_dist, tree_hansen, termite.large.count.100m, lion_camera, lion_latedry, fire_frequency, pans_100m, detect.obscured, cover.ground)

#remove unused sites
GNP_covs <- filter(GNP_covs, !StudySite %in% c("A06", "B05", "D09", "E12", "F09", "G10", "G12", "H09", "H11", "H13", "I14","J09","L09","L13","M08")) #this removes all records from cameras that were inoperable in 2017

write_csv(GNP_covs, "data/gorongosa-cameras/GNP covs.csv", col_names = T)

#create data object
#I think this successfully creates a umf data object?
GNP_umf <- unmarkedMultFrame(y=DetHist_genet_16_17, #creates the actual data object; sets y to detection history (matrix of observed data)
                         siteCovs=GNP_covs[,2:4], yearlySiteCovs=list(year=yrs), #assigns siteCovs to the second three columns of occ_covs (Urema distance, tree hansen, termite); assigns the list of years as the yearlySiteCovs (covariates at the site-year level)
                         obsCovs=list(date=GNP_DATE), #sets obsCovs (covariates that vary within site-year-observation level) to DATE
                         numPrimary=2) #number of primary time periods (in this case, years)
