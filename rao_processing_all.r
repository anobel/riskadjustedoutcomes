library(stringr)
library(tidyr)
library(dplyr)
library(sp)

# load previously saved data 
# dsh <- readRDS("rao_workingdata/dsh.rds")
# zcta <- readRDS("rao_workingdata/zcta.rds")
# acs <- readRDS("rao_workingdata/acs.rds")
# ru <- readRDS("rao_workingdata/rural.rds")
# load("rao_workingdata/md.rda")
# load("rao_workingdata/arf12.rda")
# oshpdxwalk <- readRDS("rao_workingdata/oshpdxwalk.rds")
# pt <- readRDS("rao_workingdata/pt.rds")
# pt <- readRDS("rao_workingdata/ptlite.rds")
# ptgu <- readRDS("rao_workingdata/ptgu.rds")

# Merging Datasets

# Add Medicare ID to main patient file using OSHPD crosswalk
# Load overall patient data and OSHPD crosswalk
pt <- readRDS("rao_workingdata/pt.rds")
oshpdxwalk <- readRDS("rao_workingdata/oshpdxwalk.rds")

# merge PT data and crosswalk using common column oshpd_id
# this adds the medicare ID / provider ID to the patient data
pt <- pt %>%
  left_join(oshpdxwalk)

# clean up
rm(oshpdxwalk)

# Combine with Medicare DSH data
# Load Medicare DSH Data
dsh <- readRDS("rao_workingdata/dsh.rds")

# DSH dataframe has values per hospital per year
# will average each hospital's values over the data time period (5 years)
dshmeans <- dsh %>%
  group_by(providerid) %>%
  summarise(
    bedsMean = mean(beds),
    adcMean = mean(adc),
    dsh_pctMean = mean(dsh_pct),
    mcr_pctMean = mean(mcr_pct),
    cmiMean = mean(cmi)
    )

# Merge PT and newly created DSH means data
pt <- pt %>%
  left_join(dshmeans)
rm(dsh, dshmeans)

# Merge ACS SES data and Diez Roux Neighborhood score
# Import
acs <- readRDS(file="rao_workingdata/acs.rds")

# Merge pt data with ACS data
pt <- pt %>%
  left_join(acs, by=c("patzcta" = "zcta"))
rm(acs)

# Merge Rural/Urban Classifications
ru <- readRDS("rao_workingdata/rural.rds")

pt <- pt %>%
  left_join(ru, by=c("patzcta" = "zcta"))
rm(ru)

# Merge ZCTA Lat/Lon data, for both patient and hospital ZCTA
# Import same data set, zctalatlon twice, once to use for patient zip, once for hospital zip
ptlatlon <- readRDS("rao_workingdata/zctalatlon.rds")
colnames(ptlatlon) <- c("patzcta", "ptlon", "ptlat")
pt <- pt %>% 
  left_join(ptlatlon)

hosplatlon <- readRDS("rao_workingdata/zctalatlon.rds")
colnames(hosplatlon) <- c("hospzcta", "hosplon", "hosplat")
pt <- pt %>%
  left_join(hosplatlon)

rm(hosplatlon, ptlatlon)

# make a DF with rln + patzcta (to account for patients that moved)
# drop any with missing ZCTA complete.cases
# make distance matrix, using unique combinations of pat/hosp zctas
# merge back to main dataframe
ptzips <- pt %>%
  ungroup() %>%
  filter(!is.na(patzcta) & !is.na(hospzcta)) %>%
  select(
    patzcta, ptlon, ptlat,
    hospzcta, hosplon, hosplat
    ) %>%
  unique()

# Calculate distance between patient and hospital, in km, straight line
crow <- sapply(1:nrow(ptzips),function(x) spDistsN1(as.matrix(ptzips[x,c("ptlon", "ptlat")]),as.matrix(ptzips[x,c("hosplon", "hosplat")]),longlat=T))

# Convert to miles
crow <- crow*0.6214

# cbind distances
ptzips <- ptzips %>%
  select(patzcta, hospzcta) %>%
  cbind(crow)

# merge to main patient data, using hospzcta and patzcta as keys
pt <- pt %>%
  left_join(ptzips)

rm(crow, ptzips)     

# Save as an RDS so its easier to reload into different named objects
saveRDS(pt, file="rao_workingdata/ptcombined.rds")

#### Subsetting
# Make smaller subset of all patients and also GU specific cohort
ptgu <- pt[!is.na(pt$cohort),]
# Drop those with multiple GU surgeries
ptgu <- ptgu[ptgu$cohort!="Multiple GU Sx",]
ptgu <- droplevels(ptgu)
saveRDS(ptgu, file="rao_workingdata/ptgu.rds")

# Make a subsample of 100k and save as a smaller dataset
ptlite <- pt %>% ungroup() %>% sample_n(100000, replace=F) %>% droplevels()
saveRDS(ptlite, file="rao_workingdata/ptlite.rds")
rm(ptlite)
