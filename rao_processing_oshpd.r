##################
#### Load Packages
##################

# Data Cleaning Packages
library(stringr)
library(plyr)
library(tidyr)
library(dplyr)
library(lubridate)

# Graphics Packages
library(ggplot2)

# Specialty Packages
library(icd9)

##################
#### Import Data
##################

# Import OSHPD data into a list of dataframes
pt <- apply(data.frame(paste("rao_originaldata/oshpd/",list.files("rao_originaldata/oshpd/"),sep="")), 1, FUN=read.csv, na.strings=c(""), header=TRUE, stringsAsFactors=TRUE)

# row binds all the dataframes in the list into one frame. rbind.fill from plyr
pt <- do.call(rbind.fill, pt)

# place columns in alphabetical order (ignore incorrect numerical ordering)
pt <- pt[,sort(colnames(pt))]

# make a smaller subset to make it easier to work with (random sample of 100k rows)
# In future, can just remove this line for full analysis
# pt <- droplevels(pt[sample(1:nrow(pt), 10^4, replace=F),])

##################
#### Data Cleaning
##################

# Drop if RLN is missing (based on Social Security Number, so patients without SSN have missing RLN)
# Cannot track outcomes for these patients. May introduce bias.
pt <- pt %>% filter(rln!="---------")

# Format all dates to POSIX
# make a vector identifying the date fields
d <- c("admtdate", "bthdate", "dschdate", "proc_pdt", paste("procdt", 1:20, sep=""))

# Convert date fields from factor to character, replace in dataframe
pt[,d] <- as.data.frame(lapply(pt[,d], as.character), stringsAsFactors = F)

# Convert date fields from character to POSIXct using lubridate, replace in dataframe
pt[,d] <- as.data.frame(lapply(pt[,d], parse_date_time, orders="mdY"))

# Clean workspace
rm(d)

# Admission Type
pt$admtype <- factor(pt$admtype, levels=c(1,2,3,4,0), labels=c("Scheduled", "Unscheduled", "Infant", "Unknown", "Blank"))
pt <- pt %>% filter(admtype!="Infant")

# Licensure of Hospital
pt$typcare <- factor(pt$typcare, levels = c(0, 1, 3, 4, 5, 6), labels=c("Blank", "Acute Care", "SNF", "Psychiatric", "Drug Rehab", "Physical Rehab"))

# Disposition
pt$disp <- factor(pt$disp, levels=0:13, labels=c("Invalid", "Home", "Acute Care", "Other Care Level", "SNF", "Acute-Other Facility", "Other Care Level-Other Facility", "SNF-Other Facility", "Residential Care", "Incarcerated", "AMA", "Died", "Home Health", "Other"))
# Exclude patients with the following dispositions: Invalid
pt <- pt %>% filter(disp!="Invalid")

### Demographics
# Sex
pt$sex <- factor(pt$sex, levels=1:4, labels=c("Male", "Female", "Other", "Unknown"))

# Ethnicity (Self-Report)
pt$ethncty <- factor(pt$ethncty, levels=0:3, labels=c("Invalid", "Hispanic", "Non-Hispanic", "Unknown"))

# Race/Ethnicity (Self - Report)
pt$race <- factor(pt$race, levels=0:6, labels=c("Invalid", "White", "Black", "NativeAm", "AsianPI", "Other", "Unknown"))

# Race normalized to include Hispanic ethnicity per OSHPH
pt$race_grp <- factor(pt$race_grp, levels=0:5, labels = c("Unknown", "White", "Black", "Hispanic", "AsianPI", "NativeAm"))

# Language
# Drop two primary language fields: write in and ID, just keep the abbreviation field
pt <- pt %>% select(-pls_id, -pls_wrtin)


# Hospital County (NOT coded as FIPS codes, jsut numerically 1:58)
# load list of California Counties from Area Health Reseource File
load("rao_workingdata/arf12.rda")
county <- arf12[arf12$f00011==6,c(10)]

# Create data frame with index 1:total number of counties, and counties in alphabetical order
county <- data.frame(countycode = 1:length(county), county)

# Copy over county names for hospital county
pt <- pt %>%
  left_join(county, by = c("hplcnty" = "countycode")) %>%
  rename(hospcounty = county) %>%
  select(-hplcnty)

# Copy over county names for patient county
pt <- pt %>%
  left_join(county, by = c("patcnty" = "countycode")) %>%
  rename(patcounty = county) %>%
  select(-patcnty)

# clear workspace
rm(arf12, arf12cols, county)

# Convert all ZIP codes to ZCTAs
load("rao_workingdata/zcta.rda")
# Hospital ZIP to ZCTA
pt <- pt %>%
  left_join(zcta, by = c("hplzip" ="zip")) %>%
  rename(hospzcta = zcta) %>%
  select(-hplzip)

# Patient ZIP to ZCTA
pt$patzip <- as.numeric(as.character(pt$patzip))

pt <- pt %>%
  left_join(zcta, by = c("patzip" ="zip")) %>%
  rename(patzcta = zcta) %>%
  select(-patzip)

# Clean environment
rm(zcta)

# Was Diagnosis Present On Admission?
# Principal Diagnosis Present on Admission
pt$poa_p <- factor(pt$poa_p, levels=c("Y", "N", "E", "U", "W", "0"), labels=c("Yes", "No", "Exempt", "Unknown", "Clinically Undetermined", "Invalid"))

# Other Diagnoses Present on Admission
# create vector for all 24 Other Present on Admission diagnosis fields
opoa <- paste("opoa", 1:24, sep="")
# Relabel Factors
pt[,opoa] <- as.data.frame(lapply(pt[,opoa], factor, levels=c("Y", "N", "E", "U", "W", "0"), labels=c("Yes", "No", "Exempt", "Unknown", "Clinically Undetermined", "Invalid")))
rm(opoa)

# MDC
# Import listing of MDCs (from OSHPD Appendix I v32.0)
mdcindex <- read.csv("rao_originaldata/oshpd_appendix/appendix_mdc/mdc_index.csv", header=T, stringsAsFactors = F)

# Merge with existing data
pt <- pt %>% left_join(mdcindex)
rm(mdcindex)

## Payers
# Payer Category
pt$pay_cat <- factor(pt$pay_cat, levels=0:9, labels=c("Invalid", "Medicare", "Medi-Cal", "Private", "Workers Comp", "County Indigent", "Other Govt", "Other Indigent", "Self Pay", "Other Payer"))

# Payer Names
# Import index of payer codes/names, obtained from OSHPD data dictionary files for each year (2006-2014)
payers <- apply(data.frame(paste("rao_originaldata/oshpd_appendix/appendix_payers/",list.files("rao_originaldata/oshpd_appendix/appendix_payers"),sep="")), 1, FUN=read.csv, header=T, stringsAsFactors=F)
# combine into one dataframe
payers <- do.call(rbind, payers)

# remove white space characters at start and end of payer names
payers$plan_name <- str_trim(payers$plan_name)

# There are 105 unique plans, but 169 unique plan names
# this is due to whitespace, typos, and abbreviations, slight changes in plan names
# not actual changes in plans
# will select the first plan name from each group if there are multiple plan names for each code
payers <- payers %>%
  group_by(plan_code) %>%
  filter(min_rank(plan_name)==1) %>%
  unique() %>%
  ungroup()

# merge with main data 
pt <- pt %>% left_join(payers, by = c("pay_plan" = "plan_code"))

# Clean environment
rm(payers)

# Payer Type
pt$pay_type <- factor(pt$pay_type, levels=c(0, 1, 2, 3), labels=c("Not Applicable", "HMO - KnoxKeene or MCOHS", "HMO - Other", "Traditional Care - FFS"))

## MS DRGs
# MS-DRG Severity Code
pt$sev_code <- factor(pt$sev_code, levels=0:2, labels=c("Not CC/MCC Based", "MCC Based", "CC Based"))

# DRG used for 2007 data, MS-DRG used in 2008+
# Used year-specific DRG/MSDRGs because Grouper changes each year
# DRG/MSDRG fields taken from OSHPD data dictionary appendix

# copy DRG from 2007 into MSDRG column so there's only one column to deal with
pt[is.na(pt$msdrg),"msdrg"] <- pt[is.na(pt$msdrg),"drg"]

# Generate year column for matching up DRGs (based on discharge year)
pt$msdrg_year <- year(pt$dschdate)

# Import DRG/MSDRG (2007/2008+ listings, one file per year
msdrg <- apply(data.frame(paste("rao_originaldata/oshpd_appendix/appendix_msdrg/",list.files("rao_originaldata/oshpd_appendix/appendix_msdrg"),sep="")), 1, FUN=read.csv, header=T, stringsAsFactors=F)
# combine into a single DF
msdrg <-  do.call(rbind, msdrg)

# Convert DRG names to Title Case
msdrg$msdrg_name <- str_to_title(msdrg$msdrg_name)

# select the distinct MSDRG codes for each year
# exporting from OSHPD PDF to excel resulted in lots of duplicates
msdrg <- msdrg %>%
  group_by(msdrg_year, msdrg) %>%
  distinct()

# merge based on DRG and DRG year
pt <- pt %>% left_join(msdrg)

# Clean up environment
rm(msdrg)

# create visitId variable (combining RLN and admission date) for assigning Elixhauser codes
pt$visitId = paste(pt$rln, pt$admtdate, sep="_")


#### Checkpoint 
# save(pt, file="rao_workingdata/pt.rda")
# rm(pt)

###################################
#### Assign Elixhauser Comorbidity
###################################

# create vector listing just the fields with diagnosis codes
diags <- c("diag_p", paste("odiag",1:24,sep=""))
odiags <- c(paste("odiag",1:24,sep=""))
opoas <- c(paste("opoa",1:24,sep=""))

# Calculate the total number of listed ICD9 diagnoses per patient
pt$totaldiags <- apply(pt[,diags], 1, function(x) sum(!is.na(x)))

# Subset the "Other Diagnoses" (everything except the principal diagnosis), and their corresponding POA fields
elix <- pt[,c(odiags, opoas)]

# Convert factors to characters, combine with visitIds 
elix <- as.data.frame(lapply(elix, as.character), stringsAsFactors = F)
elix <- cbind(visitId = pt$visitId, elix)

# Need to drop all "Other Diagnoses" that were NOT Present on Admission
# Convert from wide to long format, identify and drop all diagnoses that were not present on admission
# Then add principal diagnosis and calculate Elixhauser before merging with main data

# Convert wide to long and rename columns
elix <- gather(elix, visitId, value, na.rm=T)
colnames(elix) <- c("visitId", "var", "value")
elix$value <- factor(elix$value)

# Split the odiag1-24 and opoa1-24 columns into two so that I can identify the number associated with opoa==no
colsplit <- rbind(data.frame(var=paste("odiag",1:24, sep=""), var="odiag", number=1:24), data.frame(var=paste("opoa",1:24, sep=""), var="opoa", number=1:24))

# Join elix data with split column names
elix <- elix %>%
  left_join(colsplit) %>%
  select(-var) %>%
  rename(var = var.1)

rm(colsplit)

# Made a DF of just the visitId and numbers associated with diagnoses NOT Present on Admission
temp <- elix[elix$value=="No",c("visitId","number")]

# drop NAs
temp <- temp[!is.na(temp$number),]
# Assign a flag for drops
temp$drop <- TRUE

### Checkpoint
# setwd("/Users/anobel/Documents/code/rao/")
# save(elix, temp, file="rao_workingdata/elix.rda")
load(file="rao_workingdata/elix.rda")

# Merge working list of ICD9s with temp DF to identify rows to drop, drop them, and simplify DF, prep for icd9ComorbidElix()
elix <- elix %>%
  left_join(temp) %>%
  filter(is.na(drop)) %>%
  filter(var=="odiag") %>%
  select(visitId, icd9=value) %>%
  filter(!is.na(icd9))

rm(temp)

# diag_p: bring in primary diagnoses
load(file="rao_workingdata/pt.rda")
diag_p <- pt[,c("visitId", "diag_p")]
colnames(diag_p) <- c("visitId", "icd9")
elix <- rbind(elix, diag_p)
rm(diag_p)

# based on ICD9s for each patient/admission, excluding ICDs NOT Present on Admission,
# make matrix of all 30 Elixhauser categories (T/F)

# checkpoint
# save(pt, elix, file="rao_workingdata/ptelix.rda")
# load(file="rao_workingdata/ptelix.rda")

elix <- as.data.frame(icd9ComorbidElix(elix, visitId="visitId", icd9Field="icd9"))

# add visitId as index and drop rownames
elix$visitId <- rownames(elix)
row.names(elix) <- NULL

# Sum the total number of positive Elixhauser categories per patient, add at end of DF)
elix <- cbind(elix, elixsum = rowSums(elix[-length(elix)]))

# Merge with main data
pt <- left_join(pt, elix)

# Clean Up Environment
rm(elix, diags, odiags, opoas)

# checkpoint

# setwd("/Users/anobel/Documents/code/rao/")
# save(pt, file="rao_workingdata/pt.rda")
# load(file="rao_workingdata/pt.rda")

#####################################
#### Identify Readmissions within 30d
#####################################

# Will do these manipulations using parallel processing enabled by multidplyr package
library(multidplyr)

# set up 8 core cluster
cluster <- create_cluster(8)
set_default_cluster(cluster)

# Limit to the fields necessary for this calculation
# group by rln and arrange in preparation for splitting for parallel
readmit <- pt %>% 
  select(rln, admtype, admtdate, dschdate, disp) %>%
  group_by(rln) %>%
  arrange(rln, admtdate)

# Partition the data into equal sized shards for parallelized calculation
readmit <- partition(readmit, rln)

# Assign Readmissions
# Must have been within 30 days of discharge date
# Exclude if discharge data and admission date the same (transfers)
# Does not count as readmission if it was a scheduled admission
# Exclude admissions from being eligible for readmission if the dispo was:
# AMA, Incarcerated, Died, Acute-Other Facility, Other Care Level-Other Facility
readmit <- readmit %>%
  mutate(readmitdays = difftime(lead(admtdate),dschdate, units="days")) %>%
  mutate(within30d = ifelse(readmitdays<= 30 & readmitdays!=0, T, F)) %>%
  mutate(isreadmit = ifelse(within30d==T & admtype !="Scheduled" & !(disp %in% c("AMA", "Incarcerated", "Died", "Acute-Other Facility", "Other Care Level-Other Facility")), T, F))

# Recombine 
readmit <- collect(readmit)

# Any fields that were not tagged as readmits will be marked FALSE
readmit$within30d[is.na(readmit$within30d)] <- F
readmit$isreadmit[is.na(readmit$isreadmit)] <- F


####


# misc code
# sample the dataset
# t <- pt %>% sample_n(10^4, replace=F)

# to get listing of ICD9 descriptions for each patient
# apply(pt[1,diags], 1, function(x) icd9Explain(x[icd9IsReal(x)]))