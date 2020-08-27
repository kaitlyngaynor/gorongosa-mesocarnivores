#not sure I need all these, but it seemed like a good place to start
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plyr)
library(stringr)
library(here)
library(paletteer)
library("expss") #install.packages("expss")
#install.packages("tibble")

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

