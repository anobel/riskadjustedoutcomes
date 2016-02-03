library(plyr)
library(stringr)
library(dplyr)


# ZIP code level RUCAs developed by University of Washington, in collaboration with
# USDA Economic Research Service and HRSA Office of Rural Health Policy 
# http://depts.washington.edu/uwruca/index.php
# ZIP/ZCTA obtained from: https://ruralhealth.und.edu/ruca
ru <- read.csv("rao_originaldata/RUCA/final310.csv", header=T, stringsAsFactors = T)

# load zipcode/ZCTA file
load("rao_workingdata/zcta.rda")

# select the zip code and RUCA30 columns only
ru <- ru %>%
    select(ZIPCODEN, RUCA30)

# rename columns for ocnvenience
colnames(ru) <- c("zip", "ruca")

# left join ZCTA/ZIP crosswalk with RUCA codes
# ZCTA/ZIP crosswalk already limited to only CA codes
ru <- merge(zcta, ru, all.x=T, all.y=F)

# drop all duplicated ZCTAs (merged on ZIP, and multiple ZIPs can make up a ZCTA)
ru <- ru %>% 
  select(-zip) %>%
  distinct(zcta)

saveRDS(ru, file="rao_workingdata/rural.rds")
