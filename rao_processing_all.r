library(stringr)
library(tidyr)
library(dplyr)
library(sp)

# load previously saved data 
# dsh <- readRDS("rao_workingdata/dsh.rds")
# zcta <- readRDS("rao_workingdata/zcta.rds")
# acs <- readRDS("rao_workingdata/acs.rds")
# ru <- readRDS("rao_workingdata/rural.rds")
# resident <- readRDS("rao_workingdata/residents.rds")
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
  dplyr::summarise(
    bedsMean = mean(beds, na.rm=T),
    adcMean = mean(adc, na.rm=T),
    dsh_pctMean = mean(dsh_pct, na.rm=T),
    mcr_pctMean = mean(mcr_pct, na.rm=T),
    cmiMean = mean(cmi, na.rm=T),
    dshquintile = median(dshquintile, na.rm=T))

dshmeans <- dsh %>%
  group_by(providerid) %>%
  select(providerid, safetydsh, safetynaph) %>%
  distinct() %>%
  right_join(dshmeans)

# Merge PT and newly created DSH means data
pt <- pt %>%
  left_join(dshmeans)
rm(dsh, dshmeans)

# Import data on resident number, residency status
residents <- readRDS("rao_workingdata/residents.rds")

pt <- pt %>%
  left_join(residents)
rm(residents)

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
  distinct(patzcta, hospzcta)

# Calculate distance between patient and hospital, in km, straight line
crowkm <- sapply(1:nrow(ptzips),function(x) spDistsN1(as.matrix(ptzips[x,c("ptlon", "ptlat")]),as.matrix(ptzips[x,c("hosplon", "hosplat")]),longlat=T))

# cbind distances
ptzips <- ptzips %>%
  select(patzcta, hospzcta) %>%
  cbind(crowkm)

# merge to main patient data, using hospzcta and patzcta as keys
pt <- pt %>%
  left_join(ptzips)

rm(crowkm, ptzips)     

# import driving distances as calculated using ggmap and google map API
# Currently, have only calculated driving distances/times for GU cohort, but will merge here so that
# in the future, when entire cohort is complete, no changes need to be made here
distances <- readRDS(file="rao_workingdata/distances.rds")

# merge with distances DF using pat/hosp zcta as keys
pt <- pt %>%
  left_join(distances)
rm(distances)
# Save as an RDS so its easier to reload into different named objects
saveRDS(pt, file="rao_workingdata/ptcombined.rds")

#### Subsetting
# Make smaller subset of all patients and also GU specific cohort
ptgu <- pt[!is.na(pt$cohort),]
# Drop those with multiple GU surgeries
ptgu <- ptgu[ptgu$cohort!="Multiple GU Sx",]

# drop procedure, diagnosis fields
ptgu <- ptgu %>% select(
      -diag_p, -(odiag1:odiag9),
      -poa_p, -(opoa1:opoa9),
      -proc_p, -(oproc1:oproc9),
      -proc_pdt, -(procdt1:procdt9)
      )

# Consolidate Payer Categories
# not sure what to do with "Other Govt" just yet...
ptgu$pay_cat[ptgu$pay_cat=="Invalid"] <- "Other Payer"
ptgu$pay_cat[ptgu$pay_cat=="Workers Comp"] <- "Other Payer"
ptgu$pay_cat[ptgu$pay_cat=="Other Indigent"] <- "County Indigent"

# Disposition
# remove those ineligible for consideration (AMA, Incarcerated, Dead)
ptgu <- ptgu %>% filter(!disp %in% c("Incarcerated", "Died"))

# Consolidate disposition
ptgu$disp[ptgu$disp=="SNF-Other Facility"] <- "SNF"
# only 1 case transferred to Acute Care at same facility? seems coding error, will combine with other
ptgu$disp[ptgu$disp=="Acute Care"] <- "Other"

# This is a heterogenous group, but small numbers. May look into more detail later.
ptgu$disp[ptgu$disp=="Other Care Level"] <- "Other"
ptgu$disp[ptgu$disp=="Acute-Other Facility"] <- "Other"
ptgu$disp[ptgu$disp=="Other Care Level-Other Facility"] <- "Other"

# drop levels
ptgu <- droplevels(ptgu)


# assign lowest quintile/decile
ptgu$lowestquint <- F
ptgu$lowestquint[ptgu$hoodquint==1] <- T

ptgu$lowestdecile <- F
ptgu$lowestdecile[ptgu$hooddecile==1] <- T

saveRDS(ptgu, file="rao_workingdata/ptgu.rds")
# Make a subsample of 100k and save as a smaller dataset
# ptlite <- pt %>% ungroup() %>% sample_n(100000, replace=F) %>% droplevels()
# saveRDS(ptlite, file="rao_workingdata/ptlite.rds")
# rm(ptlite)
