Data for "Limited spatiotemporal niche partitioning among mesocarnivores in Gorongosa National Park, Mozambique"
---

Gorongosa National Park (GNP) is in central Mozambique at the southern extent of the Great Rift Valley.The protected area encompasses a variety of landscapes, including grassland, savanna, and woodland habitats, as well as seasonally-inundated floodplain that surrounds Lake Urema. The 4000 km² park experiences distinct wet and dry seasons, with most of the 700-900 mm of annual rainfall in the park’s interior during the wet season from December to March. 

We used camera traps to examine spatiotemporal patterns of mesocarnivore activity at GNP. We arranged 60 motion-activated camera traps (Bushnell TrophyCam) in a grid in a 300 km² area of woodland south of Lake Urema. We used data from the 2016 late dry season (August ‒ November; 122 days) to better meet model assumptions of closure. We chose the late dry season because there is less vegetation during that time to obscure mesocarnivores, and because resource scarcity at this time might be associated with the strongest patterns of spatiotemporal niche partitioning. Nine of the cameras were inoperable for some portion of the study period, but we retained them in the analysis as all were active for at least 23 days.

Details on camera placement can be found in Gaynor et al. (2021). After identifying all photographed animals to species level, we generated a detection record using the camtrapR package in R v3.6.3. We considered a species to be detected on a given date if there was at least one photograph containing the species and undetected if not. 


## Description of the data scripts

### Data files

#### Raw input data

##### **GNP_data.csv**

Camera operation information for the sixty cameras during the first two years of deployment (2016-2017) and covariate values for all camera locations in the study area. Used as input in a previous version of camtrapR to generate detection histories and in 01-multi-species-occupancy-analysis-pub.Rmd. Each row corresponds to an individual camera. Columns include: 

* Camera: unique camera ID
* Start: date camera was deployed, M/D/YY
* End: date camera was taken down, M/D/YY
* Problem1_from, Problem2_from, Problem3_from: start date(s) of any problems with the camera (up to three), M/D/YY
* Problem1_to, Problem2_to, Problem3_to: end date(s) of any problems with the camera (up to three), M/D/YY
* tree_hansen: Percent tree cover; from Global Forest Change database
* lion_latedry: scaled measure of lion presence, see text for calculation
* detect.obscured: binary measure of whether the camera’s maximum 10m range was obscured
* cover.ground: percentage of ground cover in a 10m radius around the camera (estimated visually)
* termites_1km_count: number of termite mounds within a 1km radius of the camera
* water_dist: distance to nearest water; available water included rivers, large pans (>1km2 in area, hand-digitized from 2015 DigitalGlobe imagery), and Lake Urema (dry season boundary)

##### **recordtable_allrecordscleaned_speciesmetadata.csv**

Record table of all camera trap detections. Each row corresponds to a record. Used to generate species detection histories using a previous version of camtrapR and in 01-temporal-overlap-calculation.R. Columns include: 

* Camera: unique camera ID
* Species: common name of species in record
* DateTimeOriginal: M/D/YY H:M, GMT+2
* Time.Sun: time of camera record, scaled in radians. pi/2 = sunrise, pi = solar noon, 3pi/2 = sunset, and 2pi = solar midnight

##### **genet.csv, civet.csv, honey_badger.csv, marsh_mongoose.csv**

Detection histories for four focal species: African civet, large-spotted genet, honey badger and marsh mongoose. Generated using detectionHistory in a previous version of camtrapR. Used in 02-multi-species-occupancy-analysis-GNP-2023.Rmd. Each row corresponds to a camera. Each column corresponds to a day during the study period (August 1-November 30, 2016; 122 days). 0: species not detected, 1: species detected, NA: camera not functioning. 

### Scripts

#### Spatial analysis

##### **01-multi-species-occupancy-analysis-GNP-2023.R**

Runs models using the unmarked package in R for models with no species interactions, those with constant species interactions, and those with interaction covariates. Generates figures of species detection probabilities against significant covariates. Takes genet.csv, civet.csv, honey_badger.csv, marsh_mongoose.csv, and GNP_covariates_with_pan.csv as inputs. 

#### Temporal analysis

##### **01-temporal-overlap-calculation.R**

Calculates pairwise temporal (24-hr) overlap between all pairs of species, using overlap package. Makes temporal overlap plot for four focal species. Input file is recordtable_allrecordscleaned_speciesmetadata.csv.

##### **02-temporal-figure-functions.R**

Function for plotting temporal activity of four species. Used in 01-temporal-overlap-calculation.R. 

## Sharing/access Information

The data was not derived from another source.
