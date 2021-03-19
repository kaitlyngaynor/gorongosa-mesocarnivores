library(tidyverse)

##########################################################################################
# Import individual files & change column names for merging

year12 <- read_csv("operation-files/Camera_operation_years1and2.csv") %>% 
    rename_all( ~ paste0(.x, "_12")) %>% 
    rename(Camera = Camera_12)

year3 <- read_csv("operation-files/Camera_operation_year3.csv") %>% 
    select(-c(X7, X8, X9)) %>%  # delete the weird extra columns
    rename_all( ~ paste0(.x, "_3")) %>% 
    rename(Camera = Camera_3)

year4 <- read_csv("operation-files/Camera_operation_year4.csv") %>% 
    select(-X9) %>% 
    rename_all( ~ paste0(.x, "_4")) %>% 
    rename(Camera = Camera_4)

##########################################################################################
# Join them all together, and add start date based on year 1-2

all_years <- year12 %>% 
    left_join(year3) %>% 
    left_join(year4) %>% 
    mutate(Start = Start_12, # can fill in start from year 1
           Problem2_from_3 = NA, # create year 3 Problem 2 NA columns to be filled in the next section
           Problem2_to_3 = NA) 

##########################################################################################
# For cameras with year 4 data but no year 3 data, move the year 4 over to year 3

for (i in 1:nrow(all_years)) {
    
    # if there are data for year 4 but not year 3
    if(is.na(all_years$Start_3[i]) == TRUE & is.na(all_years$Start_4[i]) == FALSE) {
        
        # move everything from year 4 to year 3
        all_years$Start_3[i] <- all_years$Start_4[i]
        all_years$End_3[i] <- all_years$End_4[i]
        all_years$Problem1_from_3[i] <- all_years$Problem1_from_4[i]
        all_years$Problem1_to_3[i] <- all_years$Problem1_to_4[i]
        all_years$Notes_3[i] <- all_years$Notes_4[i]
        
        # replace year 4 with blanks
        all_years$Start_4[i] <- NA
        all_years$End_4[i] <- NA
        all_years$Problem1_from_4[i] <- NA
        all_years$Problem1_to_4[i] <- NA
        all_years$Notes_4[i] <- NA
        
    }
    
}


##########################################################################################
# Add problems if there are gaps between end of one year and start of next

# make sure that end of year 1-2 is same as start as year 3 - IT IS NOT
all_years %>% 
    filter(End_12 != Start_3)

all_years %>% 
    filter(End_3 != Start_4)

# we see there is a gap in several cameras; need to add problem to account for this
for (i in 1:nrow(all_years)) {
    
    if(is.na(all_years$Start_3[i]) == FALSE & all_years$End_12[i] != all_years$Start_3[i]) {
        
        if(is.na(all_years$Problem1_from_12[i]) == TRUE) {
            
            all_years$Problem1_from_12[i] <- all_years$End_12[i]
            all_years$Problem1_to_12[i] <- all_years$Start_3[i]
            
        } else {
            
            all_years$Problem2_from_12[i] <- all_years$End_12[i]
            all_years$Problem2_to_12[i] <- all_years$Start_3[i]
            
        }

    }
    
    if(is.na(all_years$Start_3[i]) == FALSE & 
       is.na(all_years$Start_4[i]) == FALSE & 
       all_years$End_3[i] != all_years$Start_4[i]) {
        
        if(is.na(all_years$Problem1_from_3[i]) == TRUE) {
            
            all_years$Problem1_from_3[i] <- all_years$End_3[i]
            all_years$Problem1_to_3[i] <- all_years$Start_4[i]
            
        } else {
            
            all_years$Problem2_from_3[i] <- all_years$End_3[i]
            all_years$Problem2_to_3[i] <- all_years$Start_4[i]
            
        }
        
    }
}


##########################################################################################
# Define the End column based on latest end date

all_years$End <- NA
for (i in 1:nrow(all_years)) {
    
    if(is.na(all_years$End_4[i]) == FALSE) {
        all_years$End[i] <- all_years$End_4[i]
    } else if(is.na(all_years$End_3[i]) == FALSE) {
        all_years$End[i] <- all_years$End_3[i]
    } else {
        all_years$End[i] <- all_years$End_12[i]
    }
    
}


##########################################################################################
# Drop the other start and end dates, and rename the problems

sort(names(all_years)) # just to remind me what the names of the problem columns are now

all_years <- all_years %>% 
    rename(Problem1_from = "Problem1_from_12", Problem1_to = "Problem1_to_12",
           Problem2_from = "Problem2_from_12", Problem2_to = "Problem2_to_12",
           Problem3_from = "Problem3_from_12", Problem3_to = "Problem3_to_12",
           Problem4_from = "Problem1_from_3", Problem4_to = "Problem1_to_3",
           Problem5_from = "Problem2_from_3", Problem5_to = "Problem2_to_3",
           Problem6_from = "Problem1_from_4", Problem6_to = "Problem1_to_4",
           Problem7_from = "Problem2_from_4", Problem7_to = "Problem2_to_4"
    ) %>% 
    select(Camera, Start, End,
           Problem1_from, Problem1_to,
           Problem2_from, Problem2_to,
           Problem3_from, Problem3_to,
           Problem4_from, Problem4_to,
           Problem5_from, Problem5_to,
           Problem6_from, Problem6_to,
           Problem7_from, Problem7_to,
           Notes_3, Notes_4)


##########################################################################################
# Sort out the problem order - this basically goes through and shifts all of the problems to the "left"

for (i in 1:nrow(all_years)) {
    
    if( (is.na(all_years$Problem6_from[i]) == TRUE) & (is.na(all_years$Problem7_from[i]) == FALSE)) {
        all_years$Problem6_from[i] <- all_years$Problem7_from[i]
        all_years$Problem6_to[i] <- all_years$Problem7_to[i]
        all_years$Problem7_from[i] <- NA
        all_years$Problem7_to[i] <- NA
    }
    
    if( (is.na(all_years$Problem5_from[i]) == TRUE) & (is.na(all_years$Problem6_from[i]) == FALSE)) {
        all_years$Problem5_from[i] <- all_years$Problem6_from[i]
        all_years$Problem5_to[i] <- all_years$Problem6_to[i]
        all_years$Problem6_from[i] <- NA
        all_years$Problem6_to[i] <- NA
    }
    
    if( (is.na(all_years$Problem4_from[i]) == TRUE) & (is.na(all_years$Problem5_from[i]) == FALSE)) {
        all_years$Problem4_from[i] <- all_years$Problem5_from[i]
        all_years$Problem4_to[i] <- all_years$Problem5_to[i]
        all_years$Problem5_from[i] <- NA
        all_years$Problem5_to[i] <- NA
    }
    
    if( (is.na(all_years$Problem3_from[i]) == TRUE) & (is.na(all_years$Problem4_from[i]) == FALSE)) {
        all_years$Problem3_from[i] <- all_years$Problem4_from[i]
        all_years$Problem3_to[i] <- all_years$Problem4_to[i]
        all_years$Problem4_from[i] <- NA
        all_years$Problem4_to[i] <- NA
    }
    
    if( (is.na(all_years$Problem2_from[i]) == TRUE) & (is.na(all_years$Problem3_from[i]) == FALSE)) {
        all_years$Problem2_from[i] <- all_years$Problem3_from[i]
        all_years$Problem2_to[i] <- all_years$Problem3_to[i]
        all_years$Problem3_from[i] <- NA
        all_years$Problem3_to[i] <- NA
    }
    
    if( (is.na(all_years$Problem1_from[i]) == TRUE) & (is.na(all_years$Problem2_from[i]) == FALSE)) {
        all_years$Problem1_from[i] <- all_years$Problem2_from[i]
        all_years$Problem1_to[i] <- all_years$Problem2_to[i]
        all_years$Problem2_from[i] <- NA
        all_years$Problem2_to[i] <- NA
    }
}

#  need to run that loop 3 times in a row since there are still gaps after running the first two times (no harm in doing it extra)


##########################################################################################
# Drop any columns that are all blank now

not_all_na <- function(x) {!all(is.na(x))} # write little function
all_years <- all_years %>% select_if(not_all_na)


# export!
write.csv(all_years, "operation-files/Camera_operation_years1-4_consolidated.csv", row.names = F)
