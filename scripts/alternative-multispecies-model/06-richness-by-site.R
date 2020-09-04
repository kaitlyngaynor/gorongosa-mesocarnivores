#not sure I need all these, but it seemed like a good place to start
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plyr)
library(stringr)
library(here)
library(paletteer)
library("expss") #install.packages("expss")
library("lubridate") #install.packages("lubridate")
library(magrittr) #install.packages("magrittr")
library(readr) #install.packages("readr")
library(tibble) #install.packages("tibble")

#bring in detection data for study period
detection_dry <- read.csv("scripts/alternative-multispecies-model/detections-dry2016_katie-8_1_16-11_30_16.csv")

#subset only mesocarnivores 
meso_detection_dry <- select(detection_dry, CICI, GEGE, ICAL, MUMU, ATPA, MECA, GASA, BDCR, HEIC, LESE, HEPA)

#transpose meso detection (not sure I need this)
trans_meso_detection_dry <- t(meso_detection_dry)

#produces a column with the number of different mesocarnivore species at each camera (which I don't think I need?)
richness_raw <- colSums(trans_meso_detection_dry != 0)

#add raw richness counts to meso detection history 
#meso_detection_dry <- meso_detection_dry %>% add_column(richness_raw)

# sum all detections for each mesocarnivore species
total_detections_by_species <- colSums(meso_detection_dry)

#trying to sum for total records across all species, but this doesn't work...
#total_detections <- colSums(total_detections_by_species)

#add total detections for each species
#I can't figure out how to get this to add to my other data frame...
# trans_meso_detection_dry <- trans_meso_detection_dry %>% add_column(total_detections)

##ABOVE DOESN'T CALCULATE WHAT I THOUGHT I WAS CALCULATING, TRYING AGAIN

record <- read_csv("data/gorongosa-cameras/recordtable_allrecordscleaned_speciesmetadata.csv")
#Date = col_datetime(format = "")
#  Complete date plus hours, minutes and seconds:
# YYYY-MM-DDThh:mm:ssTZD (eg 1997-07-16T19:20:30+01:00)

record_filtered <- record %>% select(Species, Date)  %>% #filter for species and date columns
  filter(Date >= as.Date("2016-08-01T00:00:00+02:00") & Date <= as.Date("2016-11-30T23:59:59+02:00")) %>% #filter for late dry dates (Beira time)
  dplyr::count(Species)

#checking to see how this code actually works
record_test <- record %>% select(Species, Date)  %>% #filter for species and date columns
  filter(Date >= as.Date("2016-11-30T00:00:00+02:00") & Date <= as.Date("2016-12-05T00:00:00+02:00")) %>% #filter to include November 30-Dec 4 (Beira time)
  dplyr::count(Species)

#source("scripts/alternative-multispecies-model/01-generate-model-input.R")

# Run for dry season that matches Katie
# clean_for_occupancy(matrix.all = record, "8/1/16 00:00", "11/30/16 23:59", "dry2016_KLG", min.operation = 0)
