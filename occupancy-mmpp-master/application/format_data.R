library(readxl) #install.packages("readxl")

#I had to go in and manually save excel files as .csv files in the same folder
#I believe this first if loop now runs

# Convert raw excel to CSVs if you haven't already, doing some data cleanup
if(!file.exists('occupancy-mmpp-master/data/Raw Data.csv') | !file.exists('occupancy-mmpp-master/data/Covariates.csv')){
  if(!file.exists('occupancy-mmpp-master/data/Raw Data.xlsx') | !file.exists('occupancy-mmpp-master/data/Covariates.xlsx')){
    stop("Download raw data in excel format from Dryad, https://datadryad.org/stash/dataset/doi:10.5061%2Fdryad.gv1dq")
  }
  suppressMessages(library(readxl))
  # Raw Data.csv
  input_raw <- as.data.frame(read_excel("Raw Data.xlsx"))
  # Convert date to format that format_data.R is expecting
  input_raw$begin_date_time <- as.character(format(input_raw$begin_date_time, '%m/%d/%Y %H:%M'))

  # Covariates.csv
  input_covs <- as.data.frame(read_excel("Covariates.xlsx"))
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
  write.csv(input_raw, "Raw Data.csv", row.names=FALSE, na="")
  write.csv(fixed_covs, "Covariates.csv", row.names=FALSE)
}

# reading in data
dets <- read.csv('occupancy-mmpp-master/data/Raw Data.csv')
covs <- read.csv('occupancy-mmpp-master/data/Covariates.csv')

# unique sites
sits_det <- unique(dets$title)
sits_cov <- unique(covs$Camsite)

# only 1951 sites with covariates, but 1966 sites with detections
length(sits_cov) #I get 1952 from this line
length(sits_det)

# identify those sites without covariates and ignore

# only 1937 names match up. some sites need name correction?
sum(sits_det %in% sits_cov) #I get 1082 here

# site names with detections but no covariates
sits_det[(sits_det %in% sits_cov) == F]

# site names with covariates but no detections
sits_cov[(sits_cov %in% sits_det) == F]

# some clear corrections to be made
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
sits_det <- unique(dets$title)

# now 1944 site names that line up
sum(sits_det %in% sits_cov) #I get 1089 here

# removing sites without any detections from covariate list
covs <- covs[covs$Camsite %in% sits_det, ]

# removing sites without covariates from detection list
dets <- dets[dets$title %in% covs$Camsite, ]

# some sites may have a camera placed more than once
# unique deployments
deps <- unique(dets$deployment_id)

# removing deployments with < 1 day of data (can check again if you like)
deps <- deps[-c(110, 147, 1773, 1812)]

# date / time of each detection
dt <- as.POSIXlt(dets$begin_date_time, tz = 'US/Eastern',
                 format = '%m/%d/%Y %H:%M')

# blank lists for storing detection times
coys <- deer <- vector('list', length(deps))

# deployment length (in days)
dep_len <- numeric(length(deps))

dep_start <- as.POSIXct(rep(NA, length(deps)))

# identifying start and end time of each deployment
for(i in 1:length(deps)){

  # index of deployment i
  ind <- which(dets$deployment_id == deps[i])

  dep_start[i] <- min(dt[ind])

  # deployment length
  dep_len[i] <- as.numeric(difftime(max(dt[ind]), min(dt[ind]),
                                    units = 'days'))

  # any deer detected?
  if(any(dets$Scientific.Name[ind] == 'Odocoileus virginianus')){

    # indices of deer detections
    deer_ind <- which(dets$Scientific.Name[ind] == 'Odocoileus virginianus')

    # time of deer detection, relative to camera setup
    deer[[i]] <- as.numeric(difftime(dt[ind][deer_ind], min(dt[ind]),
                                     units = 'days'))

    }

  # any coyote detected?
  if(any(dets$Scientific.Name[ind] == 'Canis latrans')){

    # indices of coyote detections
    coys_ind <- which(dets$Scientific.Name[ind] == 'Canis latrans')

    # time of coyote detection, relative to camera setup
    coys[[i]] <- as.numeric(difftime(dt[ind][coys_ind], min(dt[ind]),
                                     units = 'days'))

    }

}


