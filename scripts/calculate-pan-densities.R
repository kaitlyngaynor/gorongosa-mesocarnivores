library(sf)
library(dplyr)
library(mapview)

# Bring in mound locations
mounds <- sf::read_sf("data/gis/mound_digitize.shp")
mounds <- dplyr::mutate(mounds, area_meters2 = st_area(mounds))

# Bring in camera locations, convert to spatial object
cameras <- read.csv("data/gorongosa-cameras/cam_metadata_fromfield_and_raw_raster.csv") %>% 
    dplyr::select(StudySite, Longitude, Latitude)
cameras_sf <- cameras %>% 
    sf::st_as_sf(coords = c("Longitude", "Latitude"),
                 crs = "epsg:4326") %>% 
    sf::st_transform(sf::st_crs(mounds)) # match up the coordinate system

# Calculate buffers of various sizes
camera_buffer_100m <- sf::st_buffer(cameras_sf, 100)
camera_buffer_250m <- sf::st_buffer(cameras_sf, 250)
camera_buffer_500m <- sf::st_buffer(cameras_sf, 500)
camera_buffer_1km <- sf::st_buffer(cameras_sf, 1000)

# Calculate intersection
camera_100m_intersect <- sf::st_intersection(camera_buffer_100m, mounds)
camera_250m_intersect <- sf::st_intersection(camera_buffer_250m, mounds)
camera_500m_intersect <- sf::st_intersection(camera_buffer_500m, mounds)
camera_1km_intersect <- sf::st_intersection(camera_buffer_1km, mounds)

# Group and calculate total area
termites_100m <- camera_100m_intersect %>%
    sf::st_drop_geometry() %>% 
    dplyr::group_by(StudySite) %>% 
    dplyr::summarise(termites_100m = sum(area_meters2),
                     termites_100m_count = n())
termites_250m <- camera_250m_intersect %>%
    sf::st_drop_geometry() %>% 
    dplyr::group_by(StudySite) %>% 
    dplyr::summarise(termites_250m = sum(area_meters2),
                     termites_250m_count = n())
termites_500m <- camera_500m_intersect %>%
    sf::st_drop_geometry() %>% 
    dplyr::group_by(StudySite) %>% 
    dplyr::summarise(termites_500m = sum(area_meters2),
                     termites_500m_count = n())
termites_1km <- camera_1km_intersect %>%
    sf::st_drop_geometry() %>% 
    dplyr::group_by(StudySite) %>% 
    dplyr::summarise(termites_1km = sum(area_meters2),
                     termites_1km_count = n())

attributes(termites_100m$termites_100m) = NULL
attributes(termites_250m$termites_250m) = NULL
attributes(termites_500m$termites_500m) = NULL
attributes(termites_1km$termites_1km) = NULL

# merge together
summary_mounds <- cameras %>% 
    dplyr::left_join(termites_100m) %>% 
    dplyr::left_join(termites_250m) %>% 
    dplyr::left_join(termites_500m) %>% 
    dplyr::left_join(termites_1km) %>% 
    # replace blank cells with 0
    tidyr::replace_na(list(termites_100m = 0, termites_100m_count = 0,
                           termites_250m = 0, termites_250m_count = 0,
                           termites_500m = 0, termites_500m_count = 0,
                           termites_1km = 0, termites_1km_count = 0)) %>% 
    # convert to percentage by dividing area of circle
    dplyr::mutate(termites_100m = termites_100m/31401.57,
                  termites_250m = termites_250m/196259.8,
                  termites_500m = termites_500m/785039.3,
                  termites_1km = termites_1km/3140157) %>% 
    dplyr::select(-c(Longitude, Latitude))

write.csv(summary_mounds, "data/gis/camera-mound-density-and-count.csv", row.names = FALSE)
