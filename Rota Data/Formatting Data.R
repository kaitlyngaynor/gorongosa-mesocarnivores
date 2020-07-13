## this script loads data and puts it in the 
## appropriate format for Stan (statistical 
## programming language for modelling) 

# importing detection / non-detection data
bobcat <- as.matrix(read.csv('Rota Data/Bobcat.csv', F))
coyote <- as.matrix(read.csv('Rota Data/Coyote.csv', F))
gryfox <- as.matrix(read.csv('Rota Data/Gray Fox.csv', F))
redfox <- as.matrix(read.csv('Rota Data/Red Fox.csv', F))

# take a look at the format of these input files
head(bobcat) 
# looks like it's a detection matrix of 0 (non-detection), 1 (detection), NA (not operating)
# each row is one camera site
# each column is a sampling period (ex. day)

# total number of camera days
cday <- apply(bobcat, 1, function(x) sum(is.na(x) == F))

# take a look at this output
head(cday)
# output is the number of days for which each camera was operating (0s or 1s; not NAs)
## these numbers are the same across species

# importing covariate data
## covariates for probability of occurrence models
psi.cov <- read.csv('Rota Data/psi covariates.csv')
## covariates for detection probability models
p.cov <- read.csv('Rota Data/p covariates.csv')

# take a look
head(psi.cov)
head(p.cov)
# one row per camera - has covariate values for occupancy (psi) and detection (p) 

# CREATING DESIGN MATRIX
X.array <- array(0, dim = c(nrow(bobcat), 16, 32))

# take a look - this doesn't really work because it's a 3-dimensional array
head(X.array) 
# just made a blank matrix full of 0s. 
# x dimension = number of cameras (rows in bobcat)
# y dimension = 16 (NOT SURE WHY)
# z dimension = 32 (NOT SURE WHY)

# scale all covariates to have a mean of 0 and SD of 1 - for modeling
d5km <- scale(psi.cov$Dist_5km)[, 1]
hden <- scale(psi.cov$HDens_5km)[, 1]
lati <- scale(psi.cov$Latitude)[, 1]
long <- scale(psi.cov$Longitude)[, 1]
lbyl <- lati * long
hike <- scale(psi.cov$People_site * 1000 / cday)

# take a look 
head(d5km)

# importing the general form of the design matrix
library(xlsx) # install.packages("xlsx")
dm <- read.xlsx('Rota Data/Design Matrix.xlsx', 3, rowIndex = 3:18, colIndex = 2:33,
                header = F)
# this gives a warning in the later versions of R, but it's okay
## the 3 means it's taking from the third sheet of
## that excel file, and the row/colIndex tells which rows/columns
## to extract. header = F because the first element of the 
## rowIndex vector does not contain the variables' names

# take a look at the design matrix; not totally sure what it is?
head(dm)

# filling in the design matrix
for(i in 1:nrow(bobcat)){ ##for every row
  for(j in 1:15){
    X.array[i, j, dm[j, ] == 1] <-
      c(1, lati[i], long[i], lbyl[i], hike[i],
        1, lati[i], long[i], lbyl[i], hden[i],
        1, lati[i], long[i], lbyl[i], hden[i],
        1, lati[i], long[i], lbyl[i], hike[i],
        1, d5km[i],
        1, hden[i],
        1, hden[i],
        1, d5km[i],
        1, hden[i],
        1, d5km[i])[dm[j, ] == 1]
  }
}

# OBSERVED Y
## function skips over any NA values
y1max <- apply(bobcat, 1, max, na.rm = T)
y2max <- apply(coyote, 1, max, na.rm = T)
y3max <- apply(gryfox, 1, max, na.rm = T)
y4max <- apply(redfox, 1, max, na.rm = T)

# Vectorizing detection / non-detection
Y1 <- Y2 <- Y3 <- Y4 <- trail <- dd <- numeric()
for(i in 1:nrow(bobcat)){
  Y1 <- c(Y1, bobcat[i, 1:cday[i]])
  Y2 <- c(Y2, coyote[i, 1:cday[i]])
  Y3 <- c(Y3, gryfox[i, 1:cday[i]])
  Y4 <- c(Y4, redfox[i, 1:cday[i]])
  trail <- c(trail, rep(psi.cov$Trail[i], cday[i]))
  dd <- c(dd, rep(p.cov$Det_dist[i], cday[i]))
}

# Stan does not support ragged indexing.  This is a work-around
start <- 1
for(i in 2:nrow(bobcat)){
  start <- c(start, sum(cday[1:(i - 1)]) + 1)
}

data <- list(
  K = ncol(X.array[1, , ]),
  L = 3,
  N = nrow(bobcat),
  NJ = length(Y1),
  S = 16,
  obs = cday,
  start = start,
  x = X.array,
  trl = trail,
  dd = scale(dd)[, 1],
  I1 = y1max, I2 = y2max, I3 = y3max, I4 = y4max,
  Y1 = Y1, Y2 = Y2, Y3 = Y3, Y4 = Y4
  )

inits <- function(){
  list(
    a1 = rnorm(3),
    a2 = rnorm(3),
    a3 = rnorm(3),
    a4 = rnorm(3),
    beta = rnorm(ncol(X.array[1, , ])))
}

params <- c('a1', 'a2', 'a3', 'a4', 'beta', 'll')
