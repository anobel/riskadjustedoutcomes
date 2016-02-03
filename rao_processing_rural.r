# Load packages
library(stringr)
library(dplyr)
library(RCurl)

# ZIP code level RUCAs developed by University of Washington, in collaboration with
# USDA Economic Research Service and HRSA Office of Rural Health Policy 
# http://depts.washington.edu/uwruca/index.php
# ZIP/ZCTA obtained from: https://ruralhealth.und.edu/ruca

# Using Local Data
# ru <- read.csv("rao_originaldata/RUCA/final310.csv", header=T, stringsAsFactors = T)

# Using remote data directly
ru <- getURL("https://ruralhealth.und.edu/ruca/final310.csv")
ru <- read.csv(textConnection(ru))

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

rm(zcta)

# drop all duplicated ZCTAs (merged on ZIP, and multiple ZIPs can make up a ZCTA)
ru <- ru %>% 
  select(-zip) %>%
  distinct(zcta)

# Collapse RUCA codes into 3 categories, based on UW RUCA categorizations
# Use categorization B for 3 codes: http://depts.washington.edu/uwruca/ruca-uses.php
# The percentage of the estimated 2004 US population for these groupings are: urban, 81.0%; 
# large rural, 9.6%; small rural, 5.2%; and isolated small rural, 4.2% (55,526,530 rural 
# residents in the US). The advantage of this definition is that it splits urban and rural in 
# approximately the same way as does the OMB Metro definition but at the sub county-level, and 
# it divides rural into three relevant and useful categories. In many studies and programs, it 
# makes sense to separate the large rural cities/towns (say a place of 30,000 population with 
# many medical providers) from those places that have 1,000 population and are isolated from 
# urban places. It is clear that under most circumstances these two types of places differ 
# greatly and should be considered separately. Alternatively, the small rural and isolated small 
# rural categories can be combined to create a single “small” rural category: Categorization B.
# Urban: 1.0, 1.1, 2.0, 2.1, 3.0, 4.1, 5.1, 7.1, 8.1, and 10.1
# Large Rural City/Town: 4.0, 4.2, 5.0, 5.2, 6.0, and 6.1
# Small and Isolated Small Rural Town: 7.0, 7.2, 7.3, 7.4, 8.0, 8.2, 8.3, 8.4, 9.0, 9.1, 9.2, 10.0, 10.2, 10.3, 10.4, 10.5, and 10.6

# The three categories can be aggregated. For instance, the three rural categories can be combined 
# to create one “rural” category (this would approximate the standard Metro definition but at the 
# sub county level: Categorization C.

# Another alternative is to define urban as all places that have 30% or more of their workers 
# going to a Census Bureau defined Urbanized Area (this is the same as “C” but with code 3.0 being 
# moved to the rural group): Categorization D.
# 
# Urban: 1.0, 1.1, 2.0, 2.1, 4.1, 5.1, 7.1, 8.1, and 10.1
# Rural: 3.0, 4.0, 4.2, 5.0, 5.2, 6.0, 6.1, 7.0, 7.2, 7.3, 7.4, 8.0, 8.2, 8.3, 8.4, 9.0, 9.1, 9.2, 10.0, 10.2, 10.3, 10.4, 10.5, and 10.6

# B
# urban <- c(1.0, 1.1, 2.0, 2.1, 3.0, 4.1, 5.1, 7.1, 8.1, 10.1)
# rurallarge <- c(4.0, 4.2, 5.0, 5.2, 6.0, 6.1)
# ruralsmall <- c(7.0, 7.2, 7.3, 7.4, 8.0, 8.2, 8.3, 8.4, 9.0, 9.1, 9.2, 10.0, 10.2, 10.3, 10.4, 10.5, 10.6)
# ru$ru[ru$ruca %in% urban] <- "Urban"
# ru$ru[ru$ruca %in% rurallarge] <- "Large Rural"
# ru$ru[ru$ruca %in% ruralsmall] <- "Small Rural"


# D
urban <- c(1.0, 1.1, 2.0, 2.1, 4.1, 5.1, 7.1, 8.1, 10.1)
rural <- c(3.0, 4.0, 4.2, 5.0, 5.2, 6.0, 6.1, 7.0, 7.2, 7.3, 7.4, 8.0, 8.2, 8.3, 8.4, 9.0, 9.1, 9.2, 10.0, 10.2, 10.3, 10.4, 10.5, 10.6)
ru$ru[ru$ruca %in% urban] <- "Urban"
ru$ru[ru$ruca %in% rural] <- "Rural"

rm(rural, urban)
saveRDS(ru, file="rao_workingdata/rural.rds")
