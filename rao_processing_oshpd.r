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
pt <- droplevels(pt[sample(1:nrow(pt), 10^4, replace=F),])
# save(pt, file="rao_workingdata/pt.rdata")
# load(file="rao_workingdata/pt.rdata")

##################
#### Data Cleaning
##################

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

##################
#### Clean ICD-9s
##################

# diag_p, odiag1:24, proc_p, oproc1:20

# create vector listing just the fields with diagnosis codes
diags <- c("diag_p", paste("odiag",1:24,sep=""))

# calculate the total number of listed ICD9 diagnoses per patient
pt$totaldiags <- apply(pt[,diags], 1, function(x) sum(!is.na(x)))

# to get listing of ICD9 descriptions for each patient
# apply(pt[1,diags], 1, function(x) icd9Explain(x[icd9IsReal(x)]))

#####
# Assign Elixhauser Comorbidity 

# Subset visitId and all of the diagnoses fields
elix <- pt[,c("visitId", diags)]

# Need to convert ICD9 fields from factor to character, add visitID back
elix <- as.data.frame(lapply(elix[,diags], as.character), stringsAsFactors = F)
elix <- cbind(visitId = pt$visitId, elix)

# convert from wide to long data, which is necessary for icd9ComorbidElix()
# using visitId as key variable
elix <- elix %>% gather(visitId, na.rm=T)

# rename columns
colnames(elix) <- c("visitId", "diag", "icd9")


# based on ICD9s for each patient/admission, make matrix of all 30 Elixhauser categories (T/F)
elix <- as.data.frame(icd9ComorbidElix(elix, visitId="visitId", icd9Field="icd9"))

# add visitId as index and drop rownames
elix$visitId <- rownames(elix)
row.names(elix) <- NULL

# Sum the total number of positive Elixhauser categories per patient, add at end of DF)
elix <- cbind(elix, elixsum = rowSums(elix[-length(elix)]))

# Merge with main data
pt <- left_join(pt, elix)

# Clean Up Environment
rm(elix)


