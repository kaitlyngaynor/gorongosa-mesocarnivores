### Downloading images based on URL
## Kaitlyn Gaynor, June 2020

library(tidyverse)
library(here)
library(camtrapR)

# bring in spreadsheet with a column for the link called "URL" and/or partial link info called "URL_Partial" (see below)
# url_info <- read.csv("data/url_info.csv", delim = ",")

# bring in spreadsheet with a column for the link called "URL" and/or partial link info called "URL_Partial" (see below)
url_info_mongoose <- read.csv("Image ID/Data/wildcam_3.4_mongoose.csv") # I got an error when I left in the , delim = "," part

#converting "sbuject_id" to character for use later
url_info_mongoose$subject_id <- as.character(url_info_mongoose$subject_id)

# this is needed if the URL is incomplete (ex. with Snapshot Serengeti file names)
url_info$URL <- paste("https://snapshotserengeti.s3.msi.umn.edu", 
                      url_info$URL_Partial, 
                      sep = '/') 

# if you ONLY have URL, we'll need to do the opposite to generate "URL_Partial" for use in file names
# ex. use strsplit() to take the URL and split out the first/repeated part so you can use just the unique info in the file name

# need to generate "URL_Partial" for use in file names (I think)
# THIS IS NOT COMPLETE/RIGHT
url_info_mongoose <- url_info_mongoose %>%
  cbind(URL_Partial = strsplit(url_info_mongoose$zooniverse_url_0, ""))

# make sure it worked
head(url_info_mongoose)

# download all images to hard drive. you first need to create the "downloaded-images" file in this R project folder (working directory)
# mapply(download.file, 
#        url_info$URL, 
#        destfile = here::here("downloaded-images", 
#                             basename(url_info$URL_Partial))) 

# download all images to hard drive. you first need to create the "downloaded-images" file in this R project folder (working directory)
mapply(download.file, 
       url_info_mongoose$zooniverse_url_0, 
       destfile = here::here("downloaded-images", 
                             basename(url_info_mongoose$subject_id))) 

# then sort them into folders manually based on species (if you want; or just put in one dummy species folder) 
# and come back to this script to generate a csv with the date, time, file name, species, which you can export and populate with other columns 

# generate record table
records <- recordTable(inDir = here::here("downloaded-images"),
                       IDfrom = "directory",
                       timeZone = "Africa/Maputo",
                       removeDuplicateRecords = FALSE)

# export csv
write_csv(records, here::here("data", "records.csv"))


