Data for "Limited spatiotemporal niche partitioning among mesocarnivores in Gorongosa National Park, Mozambique"
---

Brief summary of dataset contents, contextualized in experimental procedures and results.

## Description of the data scripts

### Data files

#### Raw input data

##### **Camera_operation_years1and2.csv**

Camera operation information for the sixty cameras during the first two years of deployment (2016-2017). Used as input in a previous version of camtrapR to generate detection histories. Each row corresponds to an individual camera. Columns include: 

* Camera: unique camera ID
* Start: date camera was deployed
* End: date camera was taken down
* Problem1_from, Problem2_from, Problem3_from: start date(s) of any problems with the camera (up to three)
*Problem1_to, Problem2_to, Problem3_to: end date(s) of any problems with the camera (up to three)

##### **recordtable_allrecordscleaned_speciesmetadata.csv**

Record table of all camera trap detections. Each row corresponds to a record. Used to generate species detection histories using a previous version of camtrapR and in 01-temporal-overlap-calculation.R. Columns include: 

* Camera: unique camera ID
* Species: common name of species in record
* DateTimeOrginal: M/D/YY HH:MM:SS XM, Central Africa Time
* Time.Sun: time of camera record, scaled in radians. pi/2 = sunrise, pi = solar noon, 3pi/2 = sunset, and 2pi = solar midnight

##### **GNP_covariates_with_pan.csv**

Covariate values for all camera locations in the study area. Each row corresponds to a camera. Used in 02-multi-species-occupancy-analysis-GNP-2023.Rmd. Columns include: 

* StudySite: unique camera ID
* tree_hansen: Percent tree cover; from Global Forest Change database
* lion_latedry: scaled measure of lion presence, see text for calculation
* detect.obscured: binary measure of whether the camera’s maximum 10m range was obscured
* cover.ground: percentage of ground cover in a 10m radius around the camera (estimated visually)
* termites_1km_count: number of termite mounds within a 1km radius of the camera
* water_dist: distance to nearest water; available water included rivers, large pans (>1km2 in area, hand-digitized from 2015 DigitalGlobe imagery), and Lake Urema (dry season boundary)

##### **genet.csv, civet.csv, honey_badger.csv, marsh_mongoose.csv**

Detection histories for four focal species: African civet, large-spotted genet, honey badger and marsh mongoose. Generated using detectionHistory in a previous version of camtrapR. Used in 02-multi-species-occupancy-analysis-GNP-2023.Rmd. Each row corresponds to a camera. Each column corresponds to a day during the study period (August 1-November 30, 2016; 122 days). 0: species not detected, 1: species detected, NA: camera not functioning. 

### Scripts

#### Spatial analysis

##### **01-multi-species-occupancy-analysis-GNP-2023.R**

Runs models using the unmarked package in R. Takes genet.csv, civet.csv, honey_badger.csv, marsh_mongoose.csv, and GNP_covariates_with_pan.csv as inputs. 

#### Temporal analysis

##### **01-temporal-overlap-calculation.R**

Makes temporal overlap plot for four focal species. Compares distributions with the Watson test. Input file is recordtable_allrecordscleaned_speciesmetadata.csv.

##### **02-temporal-figure-functions.R**

Function for plotting temporal activity of four species. Used in 01-temporal-overlap-calculation.R. 

## Sharing/access Information

The data was not derived from another source.
