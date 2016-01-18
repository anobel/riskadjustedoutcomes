library(stringr)
library(tidyr)
library(dplyr)

# load previously saved data 
# load("rao_workingdata/dsh.rda")
# load("rao_workingdata/zcta.rda")
# load("rao_workingdata/acs.rda")
# load("rao_workingdata/rural.rda")
# load("rao_workingdata/md.rda")
# load("rao_workingdata/arf12.rda")
# load("rao_workingdata/ptlite.rda")
# load("rao_workingdata/pt.rda")
# load("rao_workingdata/oshpdxwalk.rda")

# Merging Datasets

# Add Medicare ID to main patient file using OSHPD crosswalk
# Load overall patient data and OSHPD crosswalk
load("rao_workingdata/pt.rda")
load("rao_workingdata/oshpdxwalk.rda")

# merge PT data and crosswalk using common column oshpd_id
# this adds the medicare ID / provider ID to the patient data
pt <- pt %>%
  left_join(oshpdxwalk)

# clean up
rm(oshpdxwalk)

# Combine with Medicare DSH data
# Load Medicare DSH Data
load("rao_workingdata/dsh.rda")

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

# Save as an RDS so its easier to reload into different named objects
saveRDS(pt, file="rao_workingdata/ptcombined.rds")

# Make smaller subset of all patients and also GU specific cohort
ptgu <- pt[!is.na(pt$cohort),]
# Drop those with multiple GU surgeries
ptgu <- ptgu[ptgu$cohort!="Multiple GU Sx",]
ptgu <- droplevels(ptgu)
saveRDS(ptgu, file="rao_workingdata/ptgu.rds")

# Make a subsample of 100k and save as a smaller dataset
pt <- pt %>% ungroup() %>% sample_n(100000, replace=F) %>% droplevels()
saveRDS(pt, file="rao_workingdata/ptlite.rds")