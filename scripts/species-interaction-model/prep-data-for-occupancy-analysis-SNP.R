#goal: prep data for Serengeti
#dry season: July - October

library(camtrapR) #install.packages("camtrapR")
library(tidyverse)
library(dplyr)

# define start and end date - these are used to subset both operation matrix and record table
# need to redefine for each data set because they run for different years
# trying July to October for SNP based on a quick google search, in 2012 (won't capture all cameras, they ran for different periods)
# using most recent year to be closer to GNP time
start.date_SNP <- "2012-07-01"
end.date_SNP <- "2012-10-31"


#load in Serengeti camera operations data table
camtraps_SNP <- read_csv("data/serengeti-cameras/derived/Camera_operation.csv")

# create camera operation matrix
camop_SNP <- cameraOperation(CTtable      = camtraps_SNP,
                             stationCol   = "SiteID", #reminder to check column name to avoid issues
                             setupCol     = "Start",
                             retrievalCol = "End",
                             hasProblems  = TRUE,
                             dateFormat   = "mdy"
)

# subset to dates of interest
camop_subset_SNP <- camop_SNP %>% 
  as.data.frame %>% # first need to convert matrix to data frame
  select(all_of(start.date_SNP):all_of(end.date_SNP)) %>% # select columns of interest; not sure why I had to add "all_of"?
  filter_all(any_vars(!is.na(.))) %>% #filters out cameras that weren't operating during the defined season
  as.matrix() # get it back into matrix form for calculating detection history

# load in Serengeti record table
record_table_SNP <- read_csv("data/serengeti-cameras/derived/record_table_cleaned.csv")

# subset to dates of interest
# double check this is working
record_table_subset_SNP <- record_table_SNP %>% 
  mutate(Date = as.Date(DateTime, # format date column as date for subsetting
                        format = "%m/%d/%y %H:%M")) %>% 
  filter(Date >= as.Date(start.date_SNP) & Date <= as.Date(end.date_SNP))

# make detection history for SNP genets (without trapping effort)
DetHist_genet_SNP <- detectionHistory(recordTable          = record_table_subset_SNP,
                                      camOp                = camop_subset_SNP,
                                      stationCol           = "SiteID",
                                      speciesCol           = "Species",
                                      recordDateTimeCol    = "DateTime",
                                      recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                                      timeZone             = "Africa/Maputo",
                                      species              = "Genet",
                                      occasionLength       = 1, #sampling period (in days) represented by a single column in the occupancy matrix
                                      day1                 = "survey", #dates/columns in resulting matrix will match up (starts each row on the date the first camera was set up)
                                      includeEffort        = FALSE,
                                      occasionStartTime    = 12  #start at noon b/c nocturnal animals
)

DetHist_genet_SNP <- as.data.frame(DetHist_genet_SNP)

write_csv(DetHist_genet_SNP, "data/serengeti-cameras/derived/genet_SNP.csv", col_names = F)

# make detection history for civets (without trapping effort)
DetHist_civet_SNP <- detectionHistory(recordTable          = record_table_subset_SNP,
                                      camOp                = camop_subset_SNP,
                                      stationCol           = "SiteID",
                                      speciesCol           = "Species",
                                      recordDateTimeCol    = "DateTime",
                                      recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                                      timeZone             = "Africa/Maputo",
                                      species              = "Civet",
                                      occasionLength       = 1, #sampling period (in days) represented by a single column in the occupancy matrix
                                      day1                 = "survey", #dates/columns in resulting matrix will match up (starts each row on the date the first camera was set up)
                                      includeEffort        = FALSE,
                                      occasionStartTime    = 12  #start at noon b/c nocturnal animals
)

DetHist_civet_SNP <- as.data.frame(DetHist_civet_SNP)

write_csv(DetHist_civet_SNP, "data/serengeti-cameras/derived/civet_SNP.csv", col_names = F)

# make detection history for honey badgers (without trapping effort)
# NO HONEY BADGERS RECORDED DURING THIS PERIOD (I think)
DetHist_honey_badger_SNP <- detectionHistory(recordTable          = record_table_subset_SNP,
                                      camOp                = camop_subset_SNP,
                                      stationCol           = "SiteID",
                                      speciesCol           = "Species",
                                      recordDateTimeCol    = "DateTime",
                                      recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                                      timeZone             = "Africa/Maputo",
                                      species              = "Honey_badger",
                                      occasionLength       = 1, #sampling period (in days) represented by a single column in the occupancy matrix
                                      day1                 = "survey", #dates/columns in resulting matrix will match up (starts each row on the date the first camera was set up)
                                      includeEffort        = FALSE,
                                      occasionStartTime    = 12  #start at noon b/c nocturnal animals
)

DetHist_honey_badger_SNP <- as.data.frame(DetHist_honey_badger_SNP)

write_csv(DetHist_honey_badger_SNP, "data/serengeti-cameras/derived/honey_badger_SNP.csv", col_names = F)

# make detection history for marsh mongooses (without trapping effort)
# NO MARSH MONGOOSES DETECTED AT SNP CAMERAS (I think)
DetHist_marshmongoose <- detectionHistory(recordTable  = record_table_subset,
                                          camOp                = camop_subset,
                                          stationCol           = "Camera",
                                          speciesCol           = "Species",
                                          recordDateTimeCol    = "DateTimeOriginal",
                                          recordDateTimeFormat = "%m/%d/%y %H:%M",
                                          timeZone             = "Africa/Maputo",
                                          species              = "Mongoose_marsh",
                                          occasionLength       = 1, #sampling period (in days) represented by a single column in the occupancy matrix
                                          day1                 = "survey", #dates/columns in resulting matrix will match up (starts each row on the date the first camera was set up)
                                          includeEffort        = FALSE,
                                          occasionStartTime    = 12  #start at noon b/c nocturnal animals
)

DetHist_marshmongoose <- as.data.frame(DetHist_marshmongoose)

write_csv(DetHist_marshmongoose, "data/gorongosa-cameras/marsh_mongoose.csv", col_names = F)

# make covariate tables

# load in GNP cam metadata
cam_meta <- read_csv("data/gorongosa-cameras/cam_metadata_fromfield_and_raw_raster_withlion.csv")

# make psi covariates table (environmental)
# need to add lion covariate when we decide
GNP_psi_covariates <- select(cam_meta, urema_dist, tree_hansen, termite.large.count.100m, lion_camera)

write_csv(GNP_psi_covariates, "data/gorongosa-cameras/GNP psi covariates.csv", col_names = T)

# make p covariates table (detection)
GNP_p_covariates <- select(cam_meta, detect.obscured, cover.ground)

write_csv(GNP_p_covariates, "data/gorongosa-cameras/GNP p covariates.csv", col_names = T)

#make table with all covariates (environmental and detection)
GNP_covariates <- select(cam_meta, urema_dist, tree_hansen, termite.large.count.100m, lion_camera, lion_latedry, fire_frequency, pans_100m, detect.obscured, cover.ground)

write_csv(GNP_covariates, "data/gorongosa-cameras/GNP covariates.csv", col_names = T)

