library(plyr)
library(stringr)
library(dplyr)

# Zip Code Data from: https://ruralhealth.und.edu/ruca
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

save(ru, file="rao_workingdata/rural.rda")
