library(tidyverse)

# load in Gorongosa record table
record_table <- read_csv("data/gorongosa-cameras/recordtable_allrecordscleaned_speciesmetadata.csv")

# calculate total number of lion records at each camera
lion_cameras <- record_table %>% 
    filter(Species == "Lion") %>% 
    group_by(Camera) %>%
    summarise(lion_camera = n()) %>% 
    rename(StudySite = Camera) # rename for joining

# calculate lion records ONLY in late dry 2016
lion_cameras_dry2016 <- record_table %>% 
    mutate(Date = as.Date(DateTimeOriginal, format = "%m/%d/%y %H:%M")) %>% 
    filter(Species == "Lion") %>% 
    filter(Date >= as.Date("2016-08-01") & Date <= as.Date("2016-11-30")) %>%
    group_by(Camera) %>%
    summarise(lion_camera_dry16 = n()) %>% 
    rename(StudySite = Camera) # rename for joining

# and late dry 2017
lion_cameras_dry2017 <- record_table %>% 
    mutate(Date = as.Date(DateTimeOriginal, format = "%m/%d/%y %H:%M")) %>% 
    filter(Species == "Lion") %>% 
    filter(Date >= as.Date("2017-08-01") & Date <= as.Date("2017-11-30")) %>%
    group_by(Camera) %>%
    summarise(lion_camera_dry17 = n()) %>% 
    rename(StudySite = Camera) # rename for joining

# load in lion collar raster values, as determined by Kaitlyn dissertation script "Gorongosa_lion_spatial_analysis.Rmd"
lion_collars <- read_csv("data/gorongosa-cameras/lion_collar_rasters.csv") 

# join collar and camera dataframes
lions <- left_join(lion_collars, lion_cameras) %>% # left join cameras with collars (since all cameras are present in collar df)
    mutate(lion_camera = replace_na(lion_camera, 0)) %>%  # replace NA values with 0 for camera data
    left_join(lion_cameras_dry2016) %>% 
    mutate(lion_camera_dry16 = replace_na(lion_camera_dry16, 0)) %>% 
    left_join(lion_cameras_dry2017) %>% 
    mutate(lion_camera_dry17 = replace_na(lion_camera_dry17, 0)) %>% 
    mutate(lion_camera_dry1617 = lion_camera_dry16 + lion_camera_dry17)
head(lions)

# plot different lion covariates
plot(select(lions, -StudySite)) # plot all columns against each other (except StudySite)

# maybe weakly correlated, but not by much

# look specifically at dry vs all lions
plot(lions$lion_camera ~ lions$lion_camera_dry1617)

# append to the other metadata
metadata <- read_csv("data/gorongosa-cameras/cam_metadata_fromfield_and_raw_raster.csv")
metadata_lion <- left_join(metadata, lions)

# write file
write.csv(metadata_lion, "data/gorongosa-cameras/cam_metadata_fromfield_and_raw_raster_withlion.csv", row.names = F)
