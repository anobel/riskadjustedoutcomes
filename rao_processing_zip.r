library(reshape2)
library(stringr)
library(dplyr)

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
save(zcta, file="rao_workingdata/zcta.rda")
