# Setup -------------------------------------------------------------------

library(camtrapR) #install.packages("camtrapR")
library(tidyverse) #install.packages("tidyverse") 
library(dplyr) #install.packages("dplyr") 
library(unmarked) #install.packages("unmarked")

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

#need to reformat data to include sessions--------------------------------------------------------------------------------------------
#PROBABLY NOT USING THIS CHUNK (going to try to work from the consolidated version)
#rm(list=ls()) #SCARY LINE THAT CLEARS THE GLOBAL ENVIRONMENT
#dat1 <- read.csv("data-cleaning-master/operation-files/Camera_operation_years1and2.csv") #read in file
#dat1$Notes <- NA 
#dat1$Session <- "1" #assign session to this set (need to figure out how to differentiate between year 1 & 2)

dat2 <- read.csv("data-cleaning-master/operation-files/Camera_operation_year3.csv")
dat2$X <- NULL; dat2$X.1 <- NULL; dat2$X.2 <- NULL
dat2$Problem2_from <- NA; dat2$Problem2_to <- NA; dat2$Problem3_from <- NA; dat2$Problem3_to <- NA
dat2$Session <- "3" #assign a session value
dat2 <- dat2 %>% rename(Camera = ï..Camera) #the camera column randomly had a weird name...

dat3 <- read.csv("data-cleaning-master/operation-files/Camera_operation_year4.csv")
dat3$X <- NULL
dat3$Problem3_from <- NA; dat3$Problem3_to <- NA #creating these columns and filling with NAs
dat3$Session <- "4" #assign a session value
dat3 <- dat3 %>% rename(Camera = ï..Camera) #the camera column randomly had a weird name...
 
dat4 <- rbind(dat1, dat2, dat3)

myfun <- function(x) strptime(x, format="%m/%d/%y")
dat4[,c(2:9)] <- lapply(dat4[,c(2:9)], myfun)
dat4 <- dat4[!is.na(dat4$Start),] #one invalid roll 
max(dat4$End) 
dat4[] <- lapply(dat4[], as.character)
dat4[is.na(dat4)] <- ""

# save 
write.csv(dat4, "data/gorongosa-cameras/Camera_operation_year1-4_sessions.csv", row.names=F)

#-------------------------------------------------------------------------------------------------------------------------------------

# create camera operation matrix, correct for 2016-2019
# this has a row for each camera and a column for each date, with
# NA: cam not set up; 0: cam not operational; 1: cam operational
#added occasionStartTime because it changed functions (JK?)
camop <- cameraOperation(CTtable      = camtraps,
                         stationCol   = "Camera",
                         #sessionCol = "session" #I might need to work with this variable (for the moment, I've worked around it)
                         setupCol     = "Start",
                         retrievalCol = "End",
                         hasProblems  = TRUE,
                         dateFormat   = "mdy"
)

# big picture question is whether I need to do this? (posted to the google group)
#would this be worth writing into a single function?

#create 2016 camera operation matrix; subsets to 2016 data
camop_subset_16 <- camop %>% 
  as.data.frame %>% # first need to convert matrix to data frame
  select(start.date.16:end.date.16) %>% # select columns of interest
  #rownames_to_column("Camera") %>% # make row names into column so they can be filtered
  #filter(Camera %in% rownames(camop_subset_17)) %>% #selects only the cameras that are in the 2017 data
  #column_to_rownames("Camera") %>%  # put column back into row names (silly)
  as.matrix() # get it back into matrix form for calculating detection history

#2017 operation matrix
camop_subset_17 <- camop %>% 
  as.data.frame %>% # first need to convert matrix to data frame
  select(start.date.17:end.date.17) %>% # select columns of interest
  filter_all(any_vars(!is.na(.))) %>% #delete cameras that were inoperational during this period (all NAs); still need to do so to keep detectionhistory happy
  as.matrix() # get it back into matrix form for calculating detection history

#2018 operation matrix
camop_subset_18 <- camop %>% 
  as.data.frame %>% # first need to convert matrix to data frame
  select(start.date.18:end.date.18) %>% # select columns of interest
  filter_all(any_vars(!is.na(.))) %>% #delete cameras that were inoperational during this period (all NAs); still need to do so to keep detectionhistory happy
  as.matrix() # get it back into matrix form for calculating detection history

#2019 operation matrix
camop_subset_19 <- camop %>% 
  as.data.frame %>% # first need to convert matrix to data frame
  select(start.date.19:end.date.19) %>% # select columns of interest
  filter_all(any_vars(!is.na(.))) %>% #delete cameras that were inoperational during this period (all NAs); still need to do so to keep detectionhistory happy
  as.matrix() # get it back into matrix form for calculating detection history


# Format record tables ----------------------------------------------------

# load in Gorongosa record table, updated for 2016-2019
# columns for site, species, datetime, behavior
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
#E04 was inoperable during this period 
record_table_subset_19 <- record_table %>% 
  mutate(Date = as.Date(datetime, # format date column as date for subsetting
                        format = "%m/%d/%Y %H:%M:%S")) %>% 
  filter(Date >= as.Date(start.date.19) & Date <= as.Date(end.date.19)) %>%
  filter(site != "E04") #I believe this cut all records from E04 because they were incorrectly dated


# Make detection history for species ------------------------------------------------------------------------------------------------
#THIS IS FOR FOUR YEARS
# This yields a spreadsheet with a row for each camera, a column
# for each date in the season. 0: not detected, 1: detected, NA: camera not operational
detectionHistoryfourseasons <- function(species_name) {
  
  # make detection history for 2016 (without trapping effort)
  DetHist_16 <- detectionHistory(recordTable     = record_table_subset_16,
                                       camOp                = camop_subset_16, 
                                       stationCol           = "site", 
                                       speciesCol           = "species", 
                                       recordDateTimeCol    = "datetime", 
                                       recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                                       timeZone             = "Africa/Maputo",
                                       species              = species_name,
                                       occasionLength       = 1, #sampling period (in days) represented by a single column in the occupancy matrix
                                       day1                 = "survey", #dates/columns in resulting matrix will match up (starts each row on the date the first camera was set up)
                                       includeEffort        = FALSE
                                       #occasionStartTime    = 12  #start at noon b/c nocturnal animals
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
                                       includeEffort        = FALSE
                                       #occasionStartTime    = 12  #start at noon b/c nocturnal animals
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
                                 includeEffort        = FALSE
                                 #occasionStartTime    = 12  #start at noon b/c nocturnal animals
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
                                 includeEffort        = FALSE
                                 #occasionStartTime    = 12  #start at noon b/c nocturnal animals
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
  padding <- data.frame(matrix(NA, nrow = 60, ncol = 48)) #creating a matrix to pad the "missing" data from 2019
  padding$Row.names <- DetHist_complete$Row.names #need a shared column to merge the two data frames
  DetHist_complete <- merge(DetHist_complete, padding, by = "Row.names")
  DetHist_complete$Row.names <- NULL #merging the two data frames created a Row.names column, which I don't need in the final detection history 
  
  write_csv(DetHist_complete, paste("data/gorongosa-cameras/derived/", species_name, "_complete.csv", sep = ""), col_names = F)
  
  options(warning.length = 6000L) #need to extend the warning messages
  
}
#-------------------------------------------------------------------------------------------------------------------------------------


# now run the above function for different species

#genet
detectionHistoryfourseasons(species_name = "genet") 
DetHist_genet_complete <- read.csv("data/gorongosa-cameras/derived/genet_complete.csv", header = FALSE) #reads in detection history that comes out of the function

#civet
detectionHistoryfourseasons(species_name = "civet")
DetHist_civet_complete <- read.csv("data/gorongosa-cameras/derived/civet_complete.csv", header = FALSE) #reads in detection history that comes out of the function

#honey badger
detectionHistoryfourseasons(species_name = "honey_badger")
DetHist_honey_badger_complete <- read.csv("data/gorongosa-cameras/derived/honey_badger_complete.csv", header = FALSE) #reads in detection history that comes out of the function

#marsh mongoose
#can't run yet, 2019 data hasn't been cleaned for different mongoose species
detectionHistoryfourseasons(species_name = "mongoose_marsh")
DetHist_civet_complete <- read.csv("data/gorongosa-cameras/derived/civet_complete.csv", header = FALSE) #reads in detection history that comes out of the function

# Make detection history for species ------------------------------------------------------------------------------------------------
#THIS IS FOR THREE YEARS
# This yields a spreadsheet with a row for each camera, a column
# for each date in the season. 0: not detected, 1: detected, NA: camera not operational
detectionHistorythreeseasons <- function(species_name) {
  
  # make detection history for 2016 (without trapping effort)
  DetHist_16 <- detectionHistory(recordTable     = record_table_subset_16,
                                 camOp                = camop_subset_16, 
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
  
  #need to combine all data frames
  DetHist_16_17 <- merge(DetHist_16, DetHist_17, by = 0, all = TRUE)%>% #merges 16 & 17 detection histories by row name, keeps all rows
    column_to_rownames('Row.names') #puts the camera names as row names again for merging
  #write_csv(DetHist_16_17, "data/gorongosa-cameras/derived/genet_16_17.csv", col_names = F)
  
  
  DetHist_three <- merge(DetHist_16_17, DetHist_18, by = 0, all = TRUE)%>% #merges 16&17 with 18 detection histories by row name
    column_to_rownames('Row.names') #puts camera names back as row names to allow for merging
  #write_csv(DetHist_16_17_18, "data/gorongosa-cameras/derived/genet_16_17_18.csv", col_names = F)
  
  write_csv(DetHist_three, paste("data/gorongosa-cameras/derived/", species_name, "_three.csv", sep = ""), col_names = F)
  
  options(warning.length = 6000L) #need to extend the warning messages
  
}
#--------------------------------

# now run the above function for different species THREE YEARS

#genet
detectionHistorythreeseasons(species_name = "genet") 
DetHist_genet_three <- read.csv("data/gorongosa-cameras/derived/genet_three.csv", header = FALSE) #reads in detection history that comes out of the function

#civet
detectionHistorythreeseasons(species_name = "civet")
DetHist_civet_three <- read.csv("data/gorongosa-cameras/derived/civet_three.csv", header = FALSE) #reads in detection history that comes out of the function

#honey badger
detectionHistorythreeseasons(species_name = "honey_badger")
DetHist_honey_badger_three <- read.csv("data/gorongosa-cameras/derived/honey_badger_three.csv", header = FALSE) #reads in detection history that comes out of the function

#marsh mongoose
#can't run yet, 2019 data hasn't been cleaned for different mongoose species
detectionHistorythreeseasons(species_name = "mongoose_marsh")
DetHist_civet_three <- read.csv("data/gorongosa-cameras/derived/civet_three.csv", header = FALSE) #reads in detection history that comes out of the function

#create list of years
#yrs <- as.character(2016:2019) #creates a list with the relevant years 
#yrs <- matrix(yrs, nrow(camop), 4, byrow=TRUE) #I think we're just making a matrix with a row for every site (so it doesn't matter what object you use to get that number)

#import yearly site covs (created outside of R, has year and dog data)
yearlysitecovs <- read_csv("data/gorongosa-cameras/yearlysitecovs.csv", col_names = TRUE)
wd_layer <- read_csv("data/gorongosa-cameras/GNP wild dogs camera trap values.csv", col_names = TRUE)#I don't know how to read from another project, so I just manually copied the spreadsheet into my folder)
yearlysitecovs_wd <- merge(yearlysitecovs, wd_layer, by = 0, all = TRUE) %>% #merges original site covariates with new dog info by row name, keeps all rows
  column_to_rownames('Row.names') #puts the columns back as row names

year <- yearlysitecovs_wd[1:60, 2:4] #select columns with years (2016, 2017, 2018); 2019 camera data isn't prepped
cols <- c(7:8, 21) #selecting three dog columns for 2016, 2017, 2018 (latedry.50.dens) 
dog <- yearlysitecovs_wd[1:60, cols] # select columns with dog info 
yearlysitecovs_wd <- list(year = year, dog = dog) #wd for wild dog

#load occupancy covariates (ACTUALLY DONE BELOW)
#leaving the column names in for now
#includes distance to lake, tree cover, mound density, etc
# occ_covs <- read_csv("data/gorongosa-cameras/GNP covariates.csv", col_names = TRUE) %>% as.data.frame()

# load in GNP cam metadata
cam_meta <- read_csv("data/gorongosa-cameras/cam_metadata_fromfield_and_raw_raster_withlion.csv")

#make table with all covariates (environmental and detection)
#calling it GNP_covs because I needed to keep study site names
GNP_covs <- select(cam_meta, StudySite, urema_dist, tree_hansen, termite.large.count.100m, lion_latedry, detect.obscured, cover.ground) %>%
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
#FOR THREE YEARS WITH DOG LAYER
GNP_umf_genet <- unmarkedMultFrame(y=DetHist_genet_three, #creates the actual data object; sets y to detection history (matrix of observed data)
                         siteCovs=GNP_covs[,2:7], yearlySiteCovs=yearlysitecovs_wd, #assigns siteCovs to the proposed environmental and detection covariates; assigns yearlySiteCovs as created (covariates at the site-year level)
                         numPrimary=3) #number of primary time periods (in this case, years)

summary(GNP_umf_genet) #look at UMF

#Let's try and run some models, shall we?---------------------------------------------------------------------------------------------
#notes included here so I don't have to keep switching between tabs
#colext(psiformula= ~1, gammaformula =  ~ 1, epsilonformula = ~ 1,
      # pformula = ~ 1, data, starts, method="BFGS", se=TRUE, ...)

#psiformula: right-hand sided formula for the initial probability of occupancy at each site
#gammaformula: right-hand sided formula for colonization probability
#epsilonformula: right-hand sided formula for extinction probability
#pformula: right-hand sided formula for detection probability
#data: unmarkedMultFrame object that supplies the data
#starts: optionally, initial values for parameters in the optimization
#method: optimization method used by optim
#se: logical specifying whether or not to compute standard errors

#building detection formula
Gfit0 <- colext(~1, ~1, ~1, ~1, GNP_umf_genet) #constant parameters
Gfit0

Gfit01 <- colext(~1, ~1, ~1, ~detect.obscured, GNP_umf_genet)
Gfit01

Gfit02 <- colext(~1, ~1, ~1, ~cover.ground, GNP_umf_genet)
Gfit02

Gfit03 <- colext(~1, ~1, ~1, ~detect.obscured + cover.ground, GNP_umf_genet) #winner
Gfit03

#building occupancy formula
Gfit1 <- colext(~urema_dist, ~1, ~1, ~detect.obscured + cover.ground, GNP_umf_genet)
Gfit1

Gfit11 <- colext(~tree_hansen, ~1, ~1, ~detect.obscured + cover.ground, GNP_umf_genet)
Gfit11

Gfit12 <- colext(~termite.large.count.100m, ~1, ~1, ~detect.obscured + cover.ground, GNP_umf_genet)
Gfit12

Gfit13 <- colext(~lion_latedry, ~1, ~1, ~detect.obscured + cover.ground, GNP_umf_genet)
Gfit13

#trying to add in colonization
Gfit2 <- colext(~1, ~year, ~1, ~detect.obscured + cover.ground, GNP_umf_genet)
Gfit2

Gfit21 <- colext(~1, ~dog, ~1, ~detect.obscured + cover.ground, GNP_umf_genet)
Gfit21

Gfit22 <- colext(~1, ~year+dog, ~1, ~detect.obscured + cover.ground, GNP_umf_genet)
Gfit22

#and trying to add in extinction
Gfit3 <- colext(~1, ~1, ~year, ~detect.obscured + cover.ground, GNP_umf_genet)
Gfit3

Gfit31 <- colext(~1, ~1, ~dog, ~detect.obscured + cover.ground, GNP_umf_genet)
Gfit31

Gfit32 <- colext(~1, ~1, ~year+dog, ~detect.obscured + cover.ground, GNP_umf_genet)
Gfit32

Gfit33 <- colext(~1, ~dog, ~dog, ~detect.obscured + cover.ground, GNP_umf_genet)
Gfit33

#Let's try another species
GNP_umf_civet <- unmarkedMultFrame(y=DetHist_civet_complete, #creates the actual data object; sets y to detection history (matrix of observed data)
                                   siteCovs=GNP_covs[,2:7], yearlySiteCovs=yearlysitecovs, #assigns siteCovs to the proposed environmental and detection covariates; assigns yearlySiteCovs as created (covariates at the site-year level)
                                   numPrimary=4) #number of primary time periods (in this case, years)

summary(GNP_umf_civet) #look at UMF

#going to assume that detection covariates are the same throughout
Cfit0 <- colext(~1, ~1, ~1, ~1, GNP_umf_civet) #constant
Cfit0

Cfit01 <- colext(~1, ~1, ~1, ~detect.obscured + cover.ground, GNP_umf_civet)
Cfit01

#testing occupancy covariates
Cfit1 <- colext(~urema_dist, ~1, ~1, ~detect.obscured + cover.ground, GNP_umf_civet)
Cfit1

Cfit12 <- colext(~tree_hansen, ~1, ~1, ~detect.obscured + cover.ground, GNP_umf_civet)
Cfit12

Cfit13 <- colext(~termite.large.count.100m, ~1, ~1, ~detect.obscured + cover.ground, GNP_umf_civet)
Cfit13

Cfit14 <- colext(~lion_latedry, ~1, ~1, ~detect.obscured + cover.ground, GNP_umf_civet)
Cfit14

Cfit15 <- colext(~urema_dist + termite.large.count.100m, ~1, ~1, ~detect.obscured + cover.ground, GNP_umf_civet)
Cfit15

#testing colonization covariates
Cfit2 <- colext(~urema_dist + termite.large.count.100m, ~year, ~1, ~detect.obscured + cover.ground, GNP_umf_civet)
Cfit2

Cfit21 <- colext(~urema_dist + termite.large.count.100m, ~dog, ~1, ~detect.obscured + cover.ground, GNP_umf_civet)
Cfit21

Cfit22 <- colext(~urema_dist + termite.large.count.100m, ~dog+year, ~1, ~detect.obscured + cover.ground, GNP_umf_civet)
Cfit22

#texting extinction covariates
Cfit3 <- colext(~urema_dist + termite.large.count.100m, ~1, ~year, ~detect.obscured + cover.ground, GNP_umf_civet)
Cfit3

Cfit31 <- colext(~urema_dist + termite.large.count.100m, ~1, ~dog, ~detect.obscured + cover.ground, GNP_umf_civet)
Cfit31

Cfit32 <- colext(~urema_dist + termite.large.count.100m, ~1, ~year+dog, ~detect.obscured + cover.ground, GNP_umf_civet)
Cfit32

Cfit33 <- colext(~urema_dist + termite.large.count.100m, ~dog, ~dog, ~detect.obscured + cover.ground, GNP_umf_civet)
Cfit33

#Let's try another species
GNP_umf_honey_badger <- unmarkedMultFrame(y=DetHist_honey_badger_complete, #creates the actual data object; sets y to detection history (matrix of observed data)
                                   siteCovs=GNP_covs[,2:7], yearlySiteCovs=yearlysitecovs, #assigns siteCovs to the proposed environmental and detection covariates; assigns yearlySiteCovs as created (covariates at the site-year level)
                                   numPrimary=4) #number of primary time periods (in this case, years)

summary(GNP_umf_honey_badger) #look at UMF

Hfit0 <- colext(~1, ~1, ~1, ~1, GNP_umf_honey_badger)
Hfit0

Hfit01 <- colext(~1, ~1, ~1, ~detect.obscured + cover.ground, GNP_umf_honey_badger)
Hfit01

Hfit1 <- colext(~urema_dist, ~1, ~1, ~detect.obscured + cover.ground, GNP_umf_honey_badger)
Hfit1

Hfit11 <- colext(~tree_hansen, ~1, ~1, ~detect.obscured + cover.ground, GNP_umf_honey_badger)
Hfit11

Hfit12 <- colext(~termite.large.count.100m, ~1, ~1, ~detect.obscured + cover.ground, GNP_umf_honey_badger)
Hfit12

Hfit13 <- colext(~lion_latedry, ~1, ~1, ~detect.obscured + cover.ground, GNP_umf_honey_badger)
Hfit13

Hfit14 <- colext(~tree_hansen+urema_dist, ~1, ~1, ~detect.obscured + cover.ground, GNP_umf_honey_badger)
Hfit14

Hfit2 <- colext(~1, ~year, ~1, ~detect.obscured + cover.ground, GNP_umf_honey_badger)
Hfit2

Hfit21 <- colext(~1, ~dog, ~1, ~detect.obscured + cover.ground, GNP_umf_honey_badger)
Hfit21

Hfit22 <- colext(~1, ~year+dog, ~1, ~detect.obscured + cover.ground, GNP_umf_honey_badger)
Hfit22

Hfit3 <- colext(~1, ~1, ~year, ~detect.obscured + cover.ground, GNP_umf_honey_badger)
Hfit3

Hfit31 <- colext(~1, ~1, ~dog, ~detect.obscured + cover.ground, GNP_umf_honey_badger)
Hfit31

Hfit32 <- colext(~1, ~1, ~year+dog, ~detect.obscured + cover.ground, GNP_umf_honey_badger)
Hfit32

Hfit33 <- colext(~1, ~year, ~dog, ~detect.obscured + cover.ground, GNP_umf_honey_badger)
Hfit33
