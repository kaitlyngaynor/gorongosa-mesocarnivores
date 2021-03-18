#Need to create detection histories for each species of interest for each year


# Setup -------------------------------------------------------------------

library(camtrapR) #install.packages("camtrapR")
library(tidyverse)
library(dplyr)
library(unmarked)

# define start and end date - these are used to subset both operation matrix and record table
start.date.16 <- "2016-08-01"
end.date.16 <- "2016-11-30"
start.date.17 <- "2017-08-01"
end.date.17 <- "2017-11-30"
start.date.18 <- "2018-08-01"
end.date.18 <- "2018-11-30"
start.date.19 <- "2019-08-01"
end.date.19 <- "2019-10-13" #until we have more data


# Format camera operation matrices ----------------------------------------

#load in Gorongosa camera operations data table
#updated with consolidated data for 2016-2019 (THANK YOU KAITLYN AND MEREDITH, YOU ROCK)
camtraps <- read_csv("data/gorongosa-cameras/Camera_operation_years1-4_consolidated.csv")

# create camera operation matrix, correct for 2016-2019
camop <- cameraOperation(CTtable      = camtraps,
                         stationCol   = "Camera",
                         #sessionCol = "session" #I think I need to work with this variable
                         setupCol     = "Start",
                         retrievalCol = "End",
                         hasProblems  = TRUE,
                         dateFormat   = "mdy"
)

# big picture question is whether I need to do this? (posted to the google group)
#would this be worth writing into a single function?

#subset to dates of interest for 2016, need to remove cameras that were inoperable in 2017
#list of sites to be removed: A06(1), B05(4), D09(12), E12(18), F09(23), G10(29), G12(30), H09(34), H11(35), H13(36), I14(42), J09(46), L09(56), L13(58), M08(59)
#need to run nex chunk of code first (NOT TRUE if we use all operational cameras)
camop_subset_16 <- camop %>% 
  as.data.frame %>% # first need to convert matrix to data frame
  select(start.date.16:end.date.16) %>% # select columns of interest
  #rownames_to_column("Camera") %>% # make row names into column so they can be filtered
  #filter(Camera %in% rownames(camop_subset_17)) %>% #selects only the cameras that are in the 2017 data
  #column_to_rownames("Camera") %>%  # put column back into row names (silly)
  as.matrix() # get it back into matrix form for calculating detection history


# subset to dates of interest 2017 and remove cameras that weren't operating during this period
# another line to achieve removal of NAs: camop_subset_17[complete.cases(camop_subset_17), ] (only works on data frames)
camop_subset_17 <- camop %>% 
  as.data.frame %>% # first need to convert matrix to data frame
  select(start.date.17:end.date.17) %>% # select columns of interest
  filter_all(any_vars(!is.na(.))) %>% #delete cameras that were inoperational during this period (all NAs); still need to do so to keep detectionhistory happy
  as.matrix() # get it back into matrix form for calculating detection history

camop_subset_18 <- camop %>% 
  as.data.frame %>% # first need to convert matrix to data frame
  select(start.date.18:end.date.18) %>% # select columns of interest
  filter_all(any_vars(!is.na(.))) %>% #delete cameras that were inoperational during this period (all NAs); still need to do so to keep detectionhistory happy
  as.matrix() # get it back into matrix form for calculating detection history

camop_subset_19 <- camop %>% 
  as.data.frame %>% # first need to convert matrix to data frame
  select(start.date.19:end.date.19) %>% # select columns of interest
  filter_all(any_vars(!is.na(.))) %>% #delete cameras that were inoperational during this period (all NAs); still need to do so to keep detectionhistory happy
  as.matrix() # get it back into matrix form for calculating detection history



# Format record tables ----------------------------------------------------

# load in Gorongosa record table, updated for 2016-2019
record_table <- read_csv("data/gorongosa-cameras/wildcam_mesocarnivores.csv")

# subset to dates of interest 2016
# also need to cut the cameras we're not using (otherwise the function to create the detection history gets cranky)
record_table_subset_16 <- record_table %>% 
  mutate(Date = as.Date(datetime, # format date column as date for subsetting (edited for this spreadsheet)
                        format = "%m/%d/%Y %H:%M:%S")) %>% 
  filter(Date >= as.Date(start.date.16) & Date <= as.Date(end.date.16)) # %>%
  #filter(Camera %in% rownames(camop_subset_17))  #selects only the cameras that are in the 2017 data; trying it out with all cameras

# subset to dates of interest 2017
# don't need to worry about cutting inoperable cameras from 2016 because they're already cut with selecting dates
record_table_subset_17 <- record_table %>% 
  mutate(Date = as.Date(datetime, # format date column as date for subsetting
                        format = "%m/%d/%Y %H:%M:%S")) %>% 
  filter(Date >= as.Date(start.date.17) & Date <= as.Date(end.date.17))

#2018
record_table_subset_18 <- record_table %>% 
  mutate(Date = as.Date(datetime, # format date column as date for subsetting
                        format = "%m/%d/%Y %H:%M:%S")) %>% 
  filter(Date >= as.Date(start.date.18) & Date <= as.Date(end.date.18))

#2019
#might need to cut records from E04 (I think it was actually down during the late dry season this year)
record_table_subset_19 <- record_table %>% 
  mutate(Date = as.Date(datetime, # format date column as date for subsetting
                        format = "%m/%d/%Y %H:%M:%S")) %>% 
  filter(Date >= as.Date(start.date.19) & Date <= as.Date(end.date.19)) %>%
  filter(site != "E04") #I believe this cut all records from E04 because they were incorrectly dated


# Make detection history for species --------------------------------------
#I *think* that this can be done in a single step? still working on how

detectionHistoryfourseasons <- function(species_name) {
  
  # make detection history for 2016 (without trapping effort)
  #problem: the camera names from camOp don't match "Camera" (which come from record table csv); NOW SOLVED
  DetHist_16 <- detectionHistory(recordTable     = record_table_subset_16,
                                       camOp                = camop_subset_16, #trying with just general camop
                                       stationCol           = "site", #also had to edit for new spreadsheet
                                       speciesCol           = "species", #also edited for new spreadsheet
                                       recordDateTimeCol    = "datetime", #edited for new spreadsheet
                                       recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                                       timeZone             = "Africa/Maputo",
                                       species              = species_name,
                                       occasionLength       = 1, #sampling period (in days) represented by a single column in the occupancy matrix
                                       day1                 = "survey", #dates/columns in resulting matrix will match up (starts each row on the date the first camera was set up)
                                       includeEffort        = FALSE,
                                       occasionStartTime    = 12  #start at noon b/c nocturnal animals
  )
  
  DetHist_16 <- as.data.frame(DetHist_16) 
  
  write_csv(DetHist_16, paste("data/gorongosa-cameras/derived/", species_name, "_16.csv", sep = ""), col_names = F) 
  
  # make detection history for 2017 (without trapping effort)
  DetHist_17 <- detectionHistory(recordTable     = record_table_subset_17,
                                       camOp                = camop_subset_17,
                                       stationCol           = "site",
                                       speciesCol           = "species",
                                       recordDateTimeCol    = "datetime",
                                       recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                                       timeZone             = "Africa/Maputo",
                                       species              = species_name,
                                       occasionLength       = 1, #sampling period (in days) represented by a single column in the occupancy matrix
                                       day1                 = "survey", #dates/columns in resulting matrix will match up (starts each row on the date the first camera was set up)
                                       includeEffort        = FALSE,
                                       occasionStartTime    = 12  #start at noon b/c nocturnal animals
  )
  
  DetHist_17 <- as.data.frame(DetHist_17)
  
  write_csv(DetHist_17, paste("data/gorongosa-cameras/derived/", species_name, "_17.csv", sep = ""), col_names = F) 
  
  # make detection history for 2018 (without trapping effort)
  DetHist_18 <- detectionHistory(recordTable     = record_table_subset_18,
                                 camOp                = camop_subset_18,
                                 stationCol           = "site",
                                 speciesCol           = "species",
                                 recordDateTimeCol    = "datetime",
                                 recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                                 timeZone             = "Africa/Maputo",
                                 species              = species_name,
                                 occasionLength       = 1, #sampling period (in days) represented by a single column in the occupancy matrix
                                 day1                 = "survey", #dates/columns in resulting matrix will match up (starts each row on the date the first camera was set up)
                                 includeEffort        = FALSE,
                                 occasionStartTime    = 12  #start at noon b/c nocturnal animals
  )
  
  DetHist_18 <- as.data.frame(DetHist_18)
  
  write_csv(DetHist_18, paste("data/gorongosa-cameras/derived/", species_name, "_18.csv", sep = ""), col_names = F) 
  
  # make detection history for 2019 (without trapping effort)
  # no records from E04 for 2019
  DetHist_19 <- detectionHistory(recordTable     = record_table_subset_19,
                                 camOp                = camop_subset_19,
                                 stationCol           = "site",
                                 speciesCol           = "species",
                                 recordDateTimeCol    = "datetime",
                                 recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                                 timeZone             = "Africa/Maputo",
                                 species              = species_name,
                                 occasionLength       = 1, #sampling period (in days) represented by a single column in the occupancy matrix
                                 day1                 = "survey", #dates/columns in resulting matrix will match up (starts each row on the date the first camera was set up)
                                 includeEffort        = FALSE,
                                 occasionStartTime    = 12  #start at noon b/c nocturnal animals
  )
  
  DetHist_19 <- as.data.frame(DetHist_19)
  
  write_csv(DetHist_19, paste("data/gorongosa-cameras/derived/", species_name, "_19.csv", sep = ""), col_names = F) 
  
  #need to combine all data frames
  DetHist_16_17 <- merge(DetHist_16, DetHist_17, by = 0, all = TRUE)%>% #merges 16 & 17 detection histories by row name, keeps all rows
    column_to_rownames('Row.names') #puts the camera names as row names again for merging
  #write_csv(DetHist_16_17, "data/gorongosa-cameras/derived/genet_16_17.csv", col_names = F)
  
  
  DetHist_16_17_18 <- merge(DetHist_16_17, DetHist_18, by = 0, all = TRUE)%>% #merges 16&17 with 18 detection histories by row name
    column_to_rownames('Row.names') #puts camera names back as row names to allow for merging
  #write_csv(DetHist_16_17_18, "data/gorongosa-cameras/derived/genet_16_17_18.csv", col_names = F)
  
  DetHist_complete <- merge(DetHist_16_17_18, DetHist_19, by = 0, all = TRUE) #merges 16/17/18 with 19; throws a warning
  DetHist_complete$Row.names <- NULL #merging the two data frames created a Row.names column, which I don't need in the final detection history 
  write_csv(DetHist_complete, paste("data/gorongosa-cameras/derived/", species_name, "_complete.csv", sep = ""), col_names = F)
  
  options(warning.length = 6000L) #need to extend the warning messages
  
}
#-------------------------------------------------------------------------------------------------------------------------------------

# now run the above function for different species
detectionHistoryfourseasons(species_name = "genet") 
DetHist_genet_complete <- read.csv("data/gorongosa-cameras/derived/genet_complete.csv", header = FALSE)

detectionHistoryfourseasons(species_name = "civet")

#create list of years
yrs <- as.character(2016:2019) #creates a list with the relevant years 
yrs <- matrix(yrs, nrow(camop), 4, byrow=TRUE) #I think we're just making a matrix with a row for every site (so it doesn't matter what object you use to get that number)

#import yearly site covs (created outside of R)
yearlysitecovs <- read_csv("data/gorongosa-cameras/yearlysitecovs.csv", col_names = TRUE)

year <- yearlysitecovs[1:60, 2:5] #select columns with years
dog <- yearlysitecovs[1:60, 7:10] # select columns with rough dog info 
yearlysitecovs <- list(year = year, dog = dog) 

#load occupancy covariates
#leaving the column names in for now
#includes distance to lake, tree cover, mound density, etc
# occ_covs <- read_csv("data/gorongosa-cameras/GNP covariates.csv", col_names = TRUE) %>% as.data.frame() #done below

# load in GNP cam metadata
cam_meta <- read_csv("data/gorongosa-cameras/cam_metadata_fromfield_and_raw_raster_withlion.csv")

#make table with all covariates (environmental and detection)
#calling it GNP_covs because I needed to keep study site names
GNP_covs <- select(cam_meta, StudySite, urema_dist, tree_hansen, termite.large.count.100m, lion_latedry, fire_frequency, pans_100m, detect.obscured, cover.ground) %>%
  rename(Camera = StudySite) #%>% # to match column name in camop_subset_17
  #filter(Camera %in% rownames(camop_subset_17))  # remove unused sites (USING ALL SITES NOW)

#scaling all the non-binary covariates to address the NaN warnings
GNP_covs$urema_dist = scale(GNP_covs$urema_dist)
GNP_covs$tree_hansen = scale(GNP_covs$tree_hansen)
GNP_covs$termite.large.count.100m = scale(GNP_covs$termite.large.count.100m)
GNP_covs$lion_latedry = scale(GNP_covs$lion_latedry)
GNP_covs$cover.ground = scale(GNP_covs$cover.ground)

write_csv(GNP_covs, "data/gorongosa-cameras/GNP covs.csv", col_names = T)

#I think I need to create a matrix with the Julian dates for the obscovs?

#create data object
#I think this successfully creates a umf data object?
GNP_umf <- unmarkedMultFrame(y=DetHist_genet_complete, #creates the actual data object; sets y to detection history (matrix of observed data)
                         siteCovs=GNP_covs[,2:5], yearlySiteCovs=yearlysitecovs, #assigns siteCovs to the second four columns of occ_covs (Urema distance, tree hansen, termite, lion); assigns yearlySiteCovs as created(covariates at the site-year level)
                         numPrimary=4) #number of primary time periods (in this case, years)
