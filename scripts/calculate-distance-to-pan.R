library(sf)
library(dplyr)
library(mapview)

# Bring in camera locations, convert to spatial object
cameras <- read.csv("data/gorongosa-cameras/cam_metadata_fromfield_and_raw_raster.csv") %>% 
    sf::st_as_sf(coords = c("Longitude", "Latitude"),
                 crs = "epsg:4326")

# Bring in pan locations
pans <- sf::read_sf("data/gis/Pan_largerthan1km2.shp") %>% 
    sf::st_combine() %>% # combine into single layer of features (it's weird now)
    sf::st_transform(crs = "epsg:4326") # match up the coordinate system

# Check that both worked
mapview(cameras) + mapview(pans)

# Calculate distance
cameras_dist <- cameras %>% 
    dplyr::mutate(pan_dist_cons = sf::st_distance(cameras, pans)) %>% 
    dplyr::select(StudySite, pan_dist_cons) %>% 
    sf::st_drop_geometry()

write.csv(cameras_dist, "data/gis/camera-pan-dist.csv")
