#Rmarkdown is messing me up with working directories
#so we're back to an R script
#I need the following for data:
# covs: covariates for each deployment (row for every site, all covariates in columns)
# dets: every detection in a row, with columns for when the camera was put out and what was detected
# coys and deer: list of length matching total deployments, gets filled in with detections for each species
# dt: date-time object with the date-time of every detection across all deployments
# 
# and the following values:
#   coys_ind: indices of coyote detections (not actually sure why this is 33)
# deer_ind: same thing for deer, again not sure why this is 8 10
# dep_len: list with how long is each deployment (in days)
# dep_start:list with start date/time of each deployment in a certain format
# deps: list with all deployment IDs
# i: index of all deployments (I think)
# ind: not sure
# sits_cov: list of all the sites that have covariates
# sits_det: list of all the sites that have detections

library(readxl) #install.packages("readxl")
library(readr)

# Convert raw excel to CSVs if you haven't already, doing some data cleanup
#KLG: if any of these files don't exist, tell the coder to download them
#KLG: this big loop writes out the corrected csv files; throws many warnings for me
if(!file.exists('occupancy-mmpp-master/data/Raw Data.csv') | !file.exists('occupancy-mmpp-master/data/Covariates.csv')){
  if(!file.exists('occupancy-mmpp-master/data/Raw Data.xlsx') | !file.exists('occupancy-mmpp-master/data/Covariates.xlsx')){
    stop("Download raw data in excel format from Dryad, https://datadryad.org/stash/dataset/doi:10.5061%2Fdryad.gv1dq")
  }
  suppressMessages(library(readxl))
  # Raw Data.csv
  input_raw <- as.data.frame(read_excel("occupancy-mmpp-master/data/Raw Data.xlsx"))
  # Convert date to format that format_data.R is expecting
  input_raw$begin_date_time <- as.character(format(input_raw$begin_date_time, '%m/%d/%Y %H:%M'))
  
  # Covariates.csv
  input_covs <- as.data.frame(read_excel("occupancy-mmpp-master/data/Covariates.xlsx"))
  fixed_covs <- input_covs
  
  # Some sites need to be changed from e.g. Cheraw 10A to Cheraw 10a
  # in order to match Raw Data.csv
  last_lower <- function(x){
    paste0(substr(x, 1, nchar(x)-1),
           tolower(substr(x, nchar(x), nchar(x))))
  }
  
  sites_fix_lower <- c("Cheraw", "Fall Creek Falls", "Frozen Head",
                       "Lone Mountain", "Morrow Mountain", "morrow mountain",
                       "Sandhills", "Umstead", "Uwharrie", "Weymouth")
  
  #KLG: grepl has to do with pattern matching
  for (site in sites_fix_lower){
    fixed_covs$Camsite[grepl(site,fixed_covs$Camsite)] <-
      last_lower(fixed_covs$Camsite[grepl(site,fixed_covs$Camsite)])
  }
  
  # Some other sites need to change final letter to camera position string
  # e.g. South Mountains Gameland 10A to South Mountains Gameland 10 on trail
  letter_to_pos <- function(x){
    out <- rep(NA, length(x))
    for (i in 1:length(x)){
      stub <- substr(x[i], 1, nchar(x[i])-1)
      let <- substr(x[i], nchar(x[i]), nchar(x[i]))
      str_end <- switch(let, A = "on trail", B = "50m off", C = "200m off")
      out[i] <- paste(stub, str_end)
    }
    out
  }
  
  sites_fix_pos <- c("South Mountains Gameland", "Stone Mountain",
                     "Thurmond Chatham", "South Mountains State Park")
  
  for (site in sites_fix_pos){
    fixed_covs$Camsite[grepl(site,fixed_covs$Camsite)] <-
      letter_to_pos(fixed_covs$Camsite[grepl(site,fixed_covs$Camsite)])
  }
  
  # Write out corrected CSVs
  write.csv(input_raw, "occupancy-mmpp-master/data/Raw Data.csv", row.names=FALSE, na="")
  write.csv(fixed_covs, "occupancy-mmpp-master/data/Covariates.csv", row.names=FALSE)
}

# reading in data
dets <- read.csv('occupancy-mmpp-master/data/Raw Data.csv')
covs <- read.csv('occupancy-mmpp-master/data/Covariates.csv')

# unique sites
#KLG: title and Camsite are the same; covs is just the metadata (I think)
sits_det <- unique(dets$title)
sits_cov <- unique(covs$Camsite)

# only 1951 sites with covariates, but 1966 sites with detections
length(sits_cov) #1951
length(sits_det) #1966

# identify those sites without covariates and ignore

# only 1937 names match up. some sites need name correction?
sum(sits_det %in% sits_cov) #1937; 
# %in% checks if the values of the first argument are present in the second argument 

# site names with detections but no covariates
sits_det[(sits_det %in% sits_cov) == F]

# site names with covariates but no detections
sits_cov[(sits_cov %in% sits_det) == F]

# some clear corrections to be made (KLG: from looking at list of mismatched sites)
dets$title[which(dets$title == 'Greenbelt 23A')] <- 'Greenbelt Park 23A'
dets$title[which(dets$title == 'Greenbelt Park 28b')] <- 'Greenbelt Park 28B'
dets$title[which(dets$title == 'Prince William FP 36a')] <-
  'Prince William FP 36A'
dets$title[which(dets$title == 'Rock Creek Park 24A_2')] <-
  'Rock Creek Park 24A-2'
dets$title[which(dets$title == 'Rock Creek Park 24B_2')] <-
  'Rock Creek Park 24B-2'
dets$title[which(dets$title == 'Rock Creek Park 24C_2')] <-
  'Rock Creek Park 24C-2'
dets$title[which(dets$title == 'Rock Creek 5C-2')] <- 'Rock Creek Park 5C-2'

# renaming unique sites
# KLG: The unique() function in R is used to eliminate or delete the duplicate values or the rows 
# present in the vector, data frame, or matrix as well (here, creates list of unique sites)
sits_det <- unique(dets$title)

# now 1944 site names that line up
sum(sits_det %in% sits_cov) 

# removing sites without any detections from covariate list
#KLG: keep only the rows with a Camsite that matches the list of sites with detections
covs <- covs[covs$Camsite %in% sits_det, ]

# removing sites without covariates from detection list
#KLG: keep only the rows with title (site) that have a corresponding Camsite in the detection list
dets <- dets[dets$title %in% covs$Camsite, ]

# some sites may have a camera placed more than once
# unique deployments (list of characters(names of deployment IDs))
deps <- unique(dets$deployment_id)

# removing deployments with < 1 day of data (can check again if you like)
deps <- deps[-c(110, 147, 1773, 1812)] #KLG: they end with 1945 deployments

# date / time of each detection
#KLG: function is just date-time conversion, this suggests that they only use the beginning 
#KLG: date-time value, which is good for GNP data
dt <- as.POSIXlt(dets$begin_date_time, tz = 'US/Eastern',
                 format = '%m/%d/%Y %H:%M')

# blank lists for storing detection times
#KLG: vector() produces a vector of the given length and mode.
coys <- deer <- vector('list', length(deps))

# deployment length (in days)
dep_len <- numeric(length(deps))

# KLG: rep() replicates the values in x; as.POSIXct is date/time conversion again
# KLG: this creates something like an empty list; getting ready to take the start date of every
# deployment
dep_start <- as.POSIXct(rep(NA, length(deps)))

# identifying start and end time of each deployment
# KLG: big for loops are hard to parse apart
for(i in 1:length(deps)){ #KLG: this runs through every individual deployment
  
  # index of deployment i
  ind <- which(dets$deployment_id == deps[i]) #KLG: which() returns the position or the index of the value which satisfies the given condition
  
  dep_start[i] <- min(dt[ind]) #KLG: the deployment start for deployment i is the minimum
  # value of dt (which has date-time info for every detection)
  
  # deployment length
  # KLG: deployment length for a given deployment
  # is difference between the date/time of the min and max values in days
  dep_len[i] <- as.numeric(difftime(max(dt[ind]), min(dt[ind]),
                                    units = 'days'))
  
  # any deer detected?
  # KLG: any() reports whether any of the values is true
  # so "if any of the detections' scientific names in this deployment is deer", then do something
  if(any(dets$Scientific.Name[ind] == 'Odocoileus virginianus')){
    
    # indices of deer detections
    # KLG: which deployment(s) has deer detection(s)?
    deer_ind <- which(dets$Scientific.Name[ind] == 'Odocoileus virginianus')
    
    # time of deer detection, relative to camera setup
    # KLG: this gets added to the deer list for the given deployment
    deer[[i]] <- as.numeric(difftime(dt[ind][deer_ind], min(dt[ind]),
                                     units = 'days'))
    
  }
  
  # any coyote detected?
  # KLG: repeat for coyotes
  if(any(dets$Scientific.Name[ind] == 'Canis latrans')){
    
    # indices of coyote detections
    coys_ind <- which(dets$Scientific.Name[ind] == 'Canis latrans')
    
    # time of coyote detection, relative to camera setup
    coys[[i]] <- as.numeric(difftime(dt[ind][coys_ind], min(dt[ind]),
                                     units = 'days'))
    
  }
  
}


##what happens if we start playing with GNP data? let's find out----
#reading in GNP data
#they have a ton of daily values, I need to think about getting the equivalent numbers for 
#GNP
GNP_covs <- read_csv("occupancy-mmpp-master/data/GNP/cam_metadata_fromfield_and_raw_raster_withlion.csv")
GNP_dets <- read_csv("occupancy-mmpp-master/data/GNP/recordtable_allrecordscleaned_speciesmetadata.csv")

#I believe this adds a date column in the right format (the current Date column doesn't match the
#DateTimeOriginal column, and I'm choosing to believe the former)
GNP_dets$correct_date <- as.Date(GNP_dets$DateTimeOriginal,tz = 'Africa/Maputo',
                                 format = '%m/%e/%y %H:%M') 

date <- as.Date(GNP_dets$DateTimeOriginal,
                format = '%m/%e/%y %H:%M')

##this seems to be the correct format for the date time stuff:
as.Date("7/11/16 16:01", format = "%m/%e/%y %H:%M")

#subset GNP detections to season of interest
#without changing name of file because I don't want to change everything following right now
#will need to edit if we use this for more than one season
#between is an inclusive function, so this should be good
GNP_dets <- GNP_dets %>% filter(between(correct_date, as.Date('2016-08-01'), as.Date('2016-11-30')))

#guide code
#df %>% filter(between(date_column, as.Date('2022-01-20'), as.Date('2022-02-20')))

# unique sites (checking for GNP)
#KLG: title and Camsite are the same; covs is just the metadata (I think)
sits_det_GNP <- unique(GNP_dets$Camera) #60 sites with detections
sits_cov_GNP <- unique(GNP_covs$StudySite) #60 sites with covariates (which is good)
#^^all of our sites have both detections and covariates (as far as I can tell), so we can skip
#a good chunk of their code

deps_GNP <- unique(GNP_dets$Camera) #I believe this should just be cameras for GNP data because
#each camera only has one deployment (different from the Kellner model example, in which cameras
#were put out in multiple locations and had many deployments)

# date / time of each detection
#KLG: function is just date-time conversion, this suggests that they only use the beginning 
#KLG: date-time value, which is good for GNP data
#I think this worked? edit: it had not worked but now I think it did
#I had to change the format info, but I *think* it's correct now
dt_GNP <- as.POSIXlt(GNP_dets$DateTimeOriginal, tz = 'Africa/Maputo',
                 format = '%m/%e/%y %H:%M')

# blank lists for storing detection times
#KLG: vector() produces a vector of the given length and mode.
civet <- genet <- vector('list', length(deps_GNP))

# deployment length (in days)
dep_len_GNP <- numeric(length(deps_GNP))

# KLG: rep() replicates the values in x; as.POSIXct is date/time conversion again
# KLG: this creates something like an empty list; getting ready to take the start date of every
# deployment
dep_start_GNP <- as.POSIXct(rep(NA, length(deps_GNP)))

# identifying start and end time of each deployment
# KLG: big for loops are hard to parse apart
for(i in 1:length(deps_GNP)){ #KLG: this runs through every individual deployment
  
  # index of deployment i
  #not sure it was necessary to rename ind to ind_GNP, but I saw ind as a value and I didn't want it
  #to be overwritten
  ind_GNP <- which(GNP_dets$Camera == deps_GNP[i]) #KLG: which() returns the position or the index of the value which satisfies the given condition
  
  dep_start_GNP[i] <- min(dt_GNP[ind_GNP]) #KLG: the deployment start for deployment i is the minimum
  # value of dt (which has date-time info for every detection)
  
  # deployment length
  # KLG: deployment length for a given deployment
  # is difference between the date/time of the min and max values in days
  dep_len_GNP[i] <- as.numeric(difftime(max(dt_GNP[ind_GNP]), min(dt_GNP[ind_GNP]),
                                    units = 'days'))
  
  # any civet detected?
  # KLG: any() reports whether any of the values is true
  # so "if any of the detections' scientific names in this deployment is civet", then do something
  if(any(GNP_dets$Species[ind_GNP] == 'Civet')){
    
    # indices of civet detections
    # KLG: which deployment(s) has civet detection(s)?
    civet_ind <- which(GNP_dets$Species[ind_GNP] == 'Civet')
    
    # time of civet detection, relative to camera setup
    # KLG: this gets added to the civet list for the given deployment
    civet[[i]] <- as.numeric(difftime(dt_GNP[ind_GNP][civet_ind], min(dt_GNP[ind_GNP]),
                                     units = 'days'))
    
  }
  
  # any genet detected?
  # KLG: repeat for genets
  if(any(GNP_dets$Species[ind_GNP] == 'Genet')){
    
    # indices of coyote detections
    genet_ind <- which(GNP_dets$Species[ind_GNP] == 'Genet')
    
    # time of coyote detection, relative to camera setup
    genet[[i]] <- as.numeric(difftime(dt_GNP[ind_GNP][genet_ind], min(dt_GNP[ind_GNP]),
                                     units = 'days'))
    
  }
  
}

#so now I have the input necessary to run this for GNP? maybe?