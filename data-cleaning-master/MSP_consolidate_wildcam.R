rm(list=ls())
setwd("~/Desktop/Grad School/Post doc_Princeton/Mozambique/Data_Kaitlyn/Camera Trap Data/Processed camera data")
library(lubridate)

## DATA 

# Kaitlyn + UC Berkeley Undergrads
dat1 <- read.csv("recordtable_year1_withbehaviorcount_CLEAN.csv")
names(dat1)[13] <- "Males"

dat2 <- read.csv("recordtable_year2_withbehaviorcount_CLEAN.csv")
dat2$X <- NULL; dat2$Notes <- NULL

dat3 <- rbind(dat1, dat2); #rm(dat1); rm(dat2)
dat3$Classifier <- paste("KG/Undergrads +", dat3$Classifier)

dat3$Orig_Species <- NULL; dat3$Date <- NULL; dat3$Time <- NULL; dat3$delta.time.secs <- NULL; 
dat3$delta.time.days <- NULL; dat3$delta.time.hours <- NULL; dat3$delta.time.mins <- NULL
names(dat3)[c(2:14)] <- c("site", "species", "datetime", "count", "juvenile", "moving", "eating", 
                          "resting", "standing", "interacting", "male", "directory", "filename")
dat3$datetime <- strptime(dat3$datetime, "%m/%d/%y %H:%M")
dat3$date <- substr(dat3$datetime, 1, 10)
dat3$time <- substr(dat3$datetime, 12, 19)
dat3$juvenile_count <- dat3$juvenile
dat3$juvenile <- ifelse(dat3$juvenile > 0, 1, 0)
dat3[dat3$moving %in% c("", "nA"),]$moving <- NA
dat3$moving <- as.numeric(as.character(dat3$moving)); dat3$moving <- ifelse(dat3$moving > 0, 1, 0)
dat3$eating <- as.numeric(as.character(dat3$eating)); dat3$eating <- ifelse(dat3$eating > 0, 1, 0)
dat3$resting <- as.numeric(as.character(dat3$resting)); dat3$resting <- ifelse(dat3$resting > 0, 1, 0)
dat3$standing <- as.numeric(as.character(dat3$standing)); dat3$standing <- ifelse(dat3$standing > 0, 1, 0)
dat3$interacting <- as.numeric(as.character(dat3$interacting)); dat3$interacting <- ifelse(dat3$interacting > 0, 1, 0)
dat3$male_count <- as.numeric(as.character(dat3$male))
dat3$male <- ifelse(dat3$male > 0, 1, 0)

# Zoonvierse 
dat1 <- read.csv("WLD_Year3/WLD_S1_report_consensus_survey.csv")
dat1$zooniverse_url_3 <- NA
dat1$question__horns_visible <- ifelse(dat1$question__horns_visible > 0.5, 1, 0)

dat2 <- read.csv("WLD_Year4/WLD_S2_report_consensus_survey.csv")
dat2$question__horns_visible <- ifelse(dat2$question__horns_count_median > 0, 1, 0)

dat4 <- rbind(dat1, dat2); rm(dat1); rm(dat2)

# - quickly fix weird species (use URLs to check)
dat4[dat4$question__species == "klipspringer",]$question__species <- "duiker"
dat4[dat4$question__species == "hyena",]$question__species <- "civet" #both records
dat4[dat4$question__species == "wilddog",]$question__species[4] <- "bushbuck" #other records good

dat4 <- dat4[c("site", "capture_date_local", "capture_time_local", "capture_id", "subject_id", "question__species", "question__count_median", "question__standing", "question__resting", "question__moving", "question__eating", "question__interacting", "question__young_present", "question__horns_count_median", "question__horns_visible")]
names(dat4)[c(2:3, 6:15)] <- c("date", "time", "species", "count", "standing", "resting", "moving", "eating", "interacting", "juvenile", "male_count", "male")
dat4$datetime <- strptime(paste(dat4$date, dat4$time), "%Y-%m-%d %H:%M:%S")
dat4$filename <- paste("capture_id:", dat4$capture_id)
dat4$directory <- paste("subject_id:", dat4$subject_id)
dat4$subject_id <- NULL; dat4$capture_id <- NULL
dat4$Classifier <- "Zooniverse"

dat4$count <- as.character(dat4$count)
dat4[dat4$count == "11-50",]$count <- "30"
dat4[dat4$count == "",]$count <- NA #fire, flood
dat4$standing <- ifelse(dat4$standing > 0.2, 1, 0)
dat4$resting <- ifelse(dat4$resting > 0.2, 1, 0)
dat4$eating <- ifelse(dat4$eating > 0.2, 1, 0)
dat4$interacting <- ifelse(dat4$interacting > 0.1, 1, 0)
dat4$juvenile <- ifelse(dat4$juvenile > 0.1, 1, 0)
dat4$juvenile_count <- NA

# some invalid timestamps in Zooniverse data that I correct in my data 
dat4 <- dat4[!(dat4$site == "B09" & year(dat4$datetime)  > 2020),]
dat4 <- dat4[!(dat4$site == "I10" & year(dat4$datetime)  > 2020),]
dat4 <- dat4[!(dat4$site == "B07" & year(dat4$date) > 2019),]

# MSP classifications 
dat1 <- read.csv("wildcam_year3_additionaldata.csv")
dat2 <- read.csv("wildcam_year4_additionaldata.csv")
dat5 <- rbind(dat1, dat2)
dat5$DateTimeOriginal <- NULL
names(dat5)[2:4] <- c("directory", "filename", "datetime")
dat5$datetime <- strptime(dat5$datetime, "%m/%d/%y %H:%M")
dat5$date <- substr(dat5$datetime, 1, 10)
dat5$time <- substr(dat5$datetime, 12, 19)

dat6 <- read.csv("wildcam_year4_wilddogdata.csv") #may overlap with Zooniverse data (classified some records independently before citizen scientists finished)
dat6$X <- NULL
dat6$datetime <- paste(dat6$capture_date, dat6$capture_time)
dat6$datetime <- strptime(dat6$datetime, "%m/%d/%y %H:%M")
dat6$date <- substr(dat6$datetime, 1, 10)
dat6$time <- substr(dat6$datetime, 12, 19)
dat6$capture_date <- NULL; dat6$capture_time <- NULL
names(dat6)[1] <- "site"
dat6$directory <- NA; dat6$filename <- NA

dat7 <- rbind(dat5, dat6); rm(dat5); rm(dat6)
dat7$Classifier <- "MSP"

dat7$juvenile_count <- dat7$juvenile
dat7$juvenile <- ifelse(dat7$juvenile > 0, 1, 0)
dat7$moving <- ifelse(dat7$moving > 0, 1, 0)
dat7$male_count <- dat7$male
dat7$male <- ifelse(dat7$male > 0, 1, 0)

# substitute my data for Zooniverse data 
dat4$tag <- paste(dat4$site, dat4$date, dat4$time)
dat7$tag <- paste(dat7$site, dat7$date, dat7$time)
dat4 <- dat4[!dat4$tag %in% unique(dat7$tag),]
dat4$tag <- NULL; dat7$tag <- NULL

# merge into one file that only contains "correct" information 
dat3[] <- lapply(dat3, as.character)
dat4[] <- lapply(dat4, as.character)
dat7[] <- lapply(dat7, as.character)

x <- rbind(dat3, dat4, dat7)

# fix species to be consistent 
x$species <- tolower(x$species)
sort(unique(x$species))
x[x$species %in% c("birdother", "other_bird"),]$species <- "bird_other"
x[x$species %in% c("busbbuck", "bush"),]$species <- "bushbuck"
x[x$species %in% c("wild dog", "wilddog"),]$species <- "wild_dog"
x[x$species %in% c("vervet", "vervet monkey", "vervetmonkey"),]$species <- "vervet_monkey"
x[x$species %in% c("unknown antelope"),]$species <- "unknown_antelope"
x[x$species %in% c("samango", "samangomonkey"),]$species <- "samango_monkey"
x[x$species %in% c("sable", "sableantelope"),]$species <- "sable_antelope"
x[x$species %in% c("reptile", "reptilesamphibians", "lizard", "monitor_lizard"),]$species <- "reptile_amphibian"
x[x$species %in% c("lioncub", "lionfemale", "lionmale"),]$species <- "lion"
x[x$species %in% c("duiler", "duiker_unknown"),]$species <- "duiker"
x[x$species %in% c("mongooe", "mongoose_unknown", "mongoose_other"),]$species <- "mongoose"
x <- x[!x$species == "end date",]
x[x$species %in% c("groundhornbill"),]$species <- "ground_hornbill"
x[x$species %in% c("hippo"),]$species <- "hippopotamus"
x[x$species %in% c("honeybadger"),]$species <- "honey_badger"
x[x$species %in% c("hornbill_ground 2"),]$species <- "hornbill_ground"
x[x$species %in% c("owl"),]$species <- "birdofprey"
x[x$species %in% c("setup"),]$species <- "human"
x[x$species %in% c("black", "white"),]$species <- "invalid"
x <- x[!is.na(x$species),]
x[x$species == "ghost",]$species <- "unknown" 
x[x$species %in% c("no baboon", "no babbons", "no baboons", "no elephant", "no waterbuck", "no image", "no bushbuck", "no impala", "no nyala", "no warthog"),]$species <- "invalid"

# remove invalid images
x <- x[!x$species == "invalid",]

# write csv
write.csv(x, "wildcam_fulldata_2019.csv", row.names=F)

## search effort 
rm(list=ls())
dat1 <- read.csv("Camera_operation_years1and2.csv")
dat1$Notes <- NA

dat2 <- read.csv("Camera_operation_year3.csv")
dat2$X <- NULL; dat2$X.1 <- NULL; dat2$X.2 <- NULL
dat2$Problem2_from <- NA; dat2$Problem2_to <- NA; dat2$Problem3_from <- NA; dat2$Problem3_to <- NA

dat3 <- read.csv("Camera_operation_year4.csv")
dat3$X <- NULL
dat3$Problem3_from <- NA; dat3$Problem3_to <- NA

dat4 <- rbind(dat1, dat2, dat3)

myfun <- function(x) strptime(x, format="%m/%d/%y")
dat4[,c(2:8)] <- lapply(dat4[,c(2:8)], myfun)
dat4 <- dat4[!is.na(dat4$Start),] #one invalid roll 
max(dat4$End) 
dat4[] <- lapply(dat4[], as.character)
dat4[is.na(dat4)] <- ""

# save 
write.csv(dat4, "Camera_operation_year1-4.csv", row.names=F)


## expanding search effort from camtrapR to full 
library(dplyr); library(Hmisc); library(lubridate)

effort <- read.csv("Camera_operation_year1-4.csv") %>% select(-Notes)
datify <- function(x){strptime(x, "%m/%d/%y", tz="Africa/Maputo")}

effort_full <- NULL 
for(i in 1:nrow(effort)){
    sub <- effort[i,]; sub <- sub[!sapply(sub, function(x) all(x == ""))]
    
    if(length(sub) == 3){
        sub.seq <- seq(datify(sub$Start), datify(sub$End), 'days')
    } else if(length(sub) == 5){
        sub.seq <- c(seq(datify(sub$Start), datify(sub$Problem1_from),'days'), 
                     seq(datify(sub$Problem1_to), datify(sub$End),'days'))
    } else if(length(sub) == 7){ 
        sub.seq <- c(seq(datify(sub$Start), datify(sub$Problem1_from), 'days'), 
                     seq(datify(sub$Problem1_to), datify(sub$Problem2_from), 'days'), 
                     seq(datify(sub$Problem2_to), datify(sub$End),'days'))
    } else if(length(sub) == 9){
        sub.seq <- c(seq(datify(sub$Start), datify(sub$Problem1_from), 'days'), 
                     seq(datify(sub$Problem1_to), datify(sub$Problem2_from), 'days'), 
                     seq(datify(sub$Problem2_to), datify(sub$Problem3_from), 'days'),
                     seq(datify(sub$Problem3_to), datify(sub$End), 'days'))
    }
    
    effort.frame <- data.frame(site = as.character(sub$Camera), date = sub.seq)
    effort_full <- rbind(effort_full, effort.frame)
}

write.csv(effort_full, "~/Desktop/wildcam_fulleffort_2019.csv", row.names=F)
