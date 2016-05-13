# Load necessary libraries
library(stringr)
library(dplyr)
library(RCurl)

# Import ZIP to ZCTA crosswalk
# Obtained from From the Bureau of Primary Health Care
# Part of the Heath Resources and Services Administration
# Part of the Department of Health and Human Services
# Data used from the UDS (Uniform Data System), link to original file:
# http://udsmapper.org/docs/Zip_to_ZCTA_Crosswalk_2011_JSI.xls

zcta <- read.csv("data/raw/zip_zcta/Zip_to_ZCTA_Crosswalk_2011_JSI.csv", stringsAsFactors = T, header=T)
colnames(zcta) <- str_to_lower(colnames(zcta))

# Drop everything except CA zip codes, only need to keep Zip/ZCTA columns
zcta <- zcta %>%
  filter(stateabbr == "CA") %>%
  select(zip, zcta_use)

# Rename columns
colnames(zcta) <- c("zip", "zcta")

# Download ZCTA Lat/Lon from US Census Gazetteer
# file also downlaoded into folder as raw data for backup in case URL changes
# create temp file and download zip file from census
temp <- tempfile()
download.file("http://www2.census.gov/geo/docs/maps-data/data/gazetteer/Gaz_zcta_national.zip", temp)

# import Gazetteer zip file, remove temp file
gaz <- read.csv(unz(temp, "Gaz_zcta_national.txt"), sep = "\t")
unlink(temp); rm(temp)

# Import local backup version (in case original file at URL disappears)
# gaz <- read.csv(file="data/raw/zip_zcta/Gaz_zcta_national.txt", sep="\t", header=T, stringsAsFactors = F)

# select ZCTA and lat/lon, rename columns
gaz <- gaz %>%
  select(zcta = GEOID, lon = INTPTLONG, lat = INTPTLAT)

# select unique ZCTAs
zctalatlon <- data.frame(zcta = unique(zcta$zcta))

# merge ZCTA with gazetteer coordinates data
zctalatlon <- zctalatlon %>%
  left_join(gaz)

rm(gaz)

# Export
write.csv(zcta, file="data/tidy/zcta.csv", row.names = F)
write.csv(zctalatlon, file="data/tidy/zctalatlon.csv", row.names = F)
