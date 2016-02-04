library(stringr)
library(dplyr)
library(RCurl)

# import zip to ZCTA crosswalk
# obtained from From the Bureau of Primary Health Care
# part of the Heath Resources and Services Administration
# part of the Department of Health and Human Services
# data used for the for the UDS (Uniform Data System)
# https://www.google.com/#safe=off&q=site%3Audsmapper.org%2Fdocs%20-filetype%3Apdf

zcta <- read.csv("rao_originaldata/zip_zcta/Zip_to_ZCTA_Crosswalk_2011_JSI.csv", stringsAsFactors = T, header=T)
colnames(zcta) <- str_to_lower(colnames(zcta))

#drop everything except CA zipcodes, only need to keep Zip/ZCTA columns
zcta <- zcta %>%
  filter(stateabbr=="CA") %>%
  select(zip, zcta_use)

colnames(zcta) <- c("zip", "zcta")

# Download ZCTA Lat/Lon from US Census Gazetteer
# create temp file and download zip file from census
temp <- tempfile()
download.file("http://www2.census.gov/geo/docs/maps-data/data/gazetteer/Gaz_zcta_national.zip",temp)

# import Gazetteer zip file, remove temp file
gaz <- read.csv(unz(temp, "Gaz_zcta_national.txt"), sep="\t")
unlink(temp)

# Import local version
# gaz <- read.csv(file="rao_originaldata/zip_zcta/Gaz_zcta_national.txt", sep="\t", header=T, stringsAsFactors = F)

# select ZCTA and lat/lon, rename columns
gaz <- gaz %>%
  select(zcta = GEOID, lon = INTPTLONG, lat = INTPTLAT)

# merge ZCTA with gazetteer coordinates data
zctalatlon <- zcta %>%
  left_join(gaz) %>%
  select(-zip)

rm(gaz)

# Export
saveRDS(zcta, file="rao_workingdata/zcta.rds")
saveRDS(zctalatlon, file="rao_workingdata/zctalatlon.rds")