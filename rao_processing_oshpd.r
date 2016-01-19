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
#### Functions
##################

# can look up listing of ICD9 codes based on id (RLN) and date of admission
icd9detail <- function(df, id, date) {
  require(icd9)
  diags <- c("diag_p", paste("odiag",1:24,sep=""))
  df <- df[,c("admtdate", "rln", diags)]
  df <- df %>%
    filter(as.character(admtdate)==date) %>%
    filter(rln==id) %>%
    select(contains("diag"))
  apply(df, 1, function(x) icd9Explain(x[icd9IsReal(x)]))
}

##################
#### Defined Data
##################
codes <- list()

### Diagnosis Codes
# Bladder Cancer ICD9 Diagnoses Codes
codes$DxBladderCa <- c(icd9Children("188"), "2337", "2339", "2367", "2394")

# Kidney Cancer ICD9 Codes (Miller DC, Gore JL)
codes$DxKidneyCa <- c("189", "1890", "1891", "1898", "1899", 
                      "1580", "1715", 
                      "1940", "1952", "1976", "1980", "1981", "19889", "1991", 
                      "2239", 
                      "2339", "2354", "2369", "23690", "23691", "23699", "2372", "2381", "2388", "2395", "2399")

# Prostate Cancer ICD9 Diagnosis Codes
codes$DxProstateCa <- c("185", "1850")

# Testis Cancer ICD9 Diagnosis Codes (Mossanen M 2014)
codes$DxTestisCa <- c("186", "1860", "1869", "1580", "158","1976", "2118", "2354")

### Procedure Codes
# Radical Nephrectomy Procedure Codes (DCM, JLG)
codes$SxRadNx <- c("5551", "5552", "5554")

# Partial Nephrectomy Procedure Codes (DCM, JLG)
codes$SxPartialNx <- c("5501", "5524", "5531", "5539", "554", "5540", "5581", "5589", "5591", 
                       "5902", "5909", "5921")

# Radical Cystectomy Procedure Codes
codes$SxCystectomy <- c("577", "5771", "5779")

# Radical Prostatectomy Procedure Codes
codes$SxRP <- c("605", "6050")

# RPLND Procedure Codes (Mossanen 2014)
codes$SxRPLND <- c("590", "5900", "5902", "5909", "4029", "403", "4052", "4059")

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

# Drop if admit date is prior to 2006, these must be data entry errors
pt <- pt[year(pt$admtdate)>2005,]

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
# Keep only male/female categories
pt <- pt %>% filter(sex<3)
# factor sex variable
pt$sex <- factor(pt$sex, levels=1:2, labels=c("Male", "Female"))

# Age
# To prep for analysis, set age 18 as the baseline (0), and divide by 10 for ease of interpretation
pt$agyradmcentered <- (pt$agyradm-18)/10

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
table(pt$plan_name)
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

###################################
#### Assign Elixhauser Comorbidity
###################################

# create vector listing just the fields with diagnosis codes
diags <- c("diag_p", paste("odiag",1:24,sep=""))
odiags <- c(paste("odiag",1:24,sep=""))
opoas <- c(paste("opoa",1:24,sep=""))

# Calculate the total number of listed ICD9 diagnoses per patient
pt$totaldx <- apply(pt[,diags], 1, function(x) sum(!is.na(x)))

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

# Merge working list of ICD9s with temp DF to identify rows to drop, drop them, and simplify DF, prep for icd9ComorbidElix()
elix <- elix %>%
  left_join(temp) %>%
  filter(is.na(drop)) %>%
  filter(var=="odiag") %>%
  select(visitId, icd9=value) %>%
  filter(!is.na(icd9))

rm(temp)

# diag_p: bring in primary diagnoses
diag_p <- pt[,c("visitId", "diag_p")]
colnames(diag_p) <- c("visitId", "icd9")
elix <- rbind(elix, diag_p)
rm(diag_p)

# based on ICD9s for each patient/admission, excluding ICDs NOT Present on Admission,
# make matrix of all 30 Elixhauser categories (T/F)

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

#####################################
#### Identify Readmissions
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
# Must have been within 30/90 days of discharge date
# Exclude if discharge data and admission date the same (transfers)
# Does not count as readmission if it was a scheduled admission
# Exclude admissions from being eligible for readmission if the dispo was:
# AMA, Incarcerated, Died, Acute-Other Facility, Other Care Level-Other Facility

readmit <- readmit %>%
  mutate(los = difftime(dschdate, admtdate, units="days")) %>%
  mutate(readmitdaysadm = difftime(lead(admtdate),admtdate, units="days")) %>%
  mutate(readmitdaysdc = difftime(lead(admtdate),dschdate, units="days")) %>%
  mutate(within30adm = ifelse(readmitdaysadm<= 30 & readmitdaysadm!=0, T, F)) %>%
  mutate(within30dc = ifelse(readmitdaysdc<= 30 & readmitdaysdc!=0, T, F)) %>%
  mutate(within90dc = ifelse(readmitdaysdc<= 90 & readmitdaysdc!=0, T, F)) %>%
  mutate(isreadmit30adm = ifelse(within30adm==T & lead(admtype) !="Scheduled" & !(disp %in% c("AMA", "Incarcerated", "Died", "Acute-Other Facility", "Other Care Level-Other Facility")), T, F)) %>%
  mutate(isreadmit30dc = ifelse(within30dc==T & lead(admtype) !="Scheduled" & !(disp %in% c("AMA", "Incarcerated", "Died", "Acute-Other Facility", "Other Care Level-Other Facility")), T, F)) %>%
  mutate(isreadmit90dc = ifelse(within90dc==T & lead(admtype) !="Scheduled" & !(disp %in% c("AMA", "Incarcerated", "Died", "Acute-Other Facility", "Other Care Level-Other Facility")), T, F))

# Recombine 
readmit <- collect(readmit)
rm(cluster)

# Any fields that were not tagged as readmits will be marked FALSE
readmit$within30adm[is.na(readmit$within30adm)] <- F
readmit$isreadmit30adm[is.na(readmit$isreadmit30adm)] <- F
readmit$within30dc[is.na(readmit$within30dc)] <- F
readmit$isreadmit30dc[is.na(readmit$isreadmit30dc)] <- F
readmit$within90dc[is.na(readmit$within90dc)] <- F
readmit$isreadmit90dc[is.na(readmit$isreadmit90dc)] <- F

# Merge readmit assignments back to main patient data
pt <- readmit %>%
  select(rln, admtdate, los, readmitdaysadm, readmitdaysdc, within30adm, within30dc, within90dc, isreadmit30adm, isreadmit30dc, isreadmit90dc) %>%
  right_join(pt)

rm(readmit)

#####################################
#### Procedure Specific Cohorts
#####################################
# Create vectors for future use
diags <- c("diag_p", paste("odiag",1:24,sep=""))
procs <- c("proc_p", paste("oproc",1:20,sep=""))

# Create empty DF with RLNs to populate
cohort <- as.data.frame(pt$rln)

# Assign Diagnoses
# Use lapply across all diagnosis fields (not just principal diagnosis)
# Assign 1 if matches something in codes vector, and then add up rowsums
cohort$DxBladderCa <- rowSums(as.data.frame(lapply(select(pt, one_of(c(diags))), function(x) x %in% codes$DxBladderCa)))
cohort$DxKidneyCa <- rowSums(as.data.frame(lapply(select(pt, one_of(c(diags))), function(x) x %in% codes$DxKidneyCa)))
cohort$DxProstateCa <- rowSums(as.data.frame(lapply(select(pt, one_of(c(diags))), function(x) x %in% codes$DxProstateCa)))
cohort$DxTestisCa <- rowSums(as.data.frame(lapply(select(pt, one_of(c(diags))), function(x) x %in% codes$DxTestisCa & pt$sex=="Male")))

# Use lapply across all procedure fields (not just principal procedure)
# Assign 1 if matches something in codes vector, and then add up rowsums
cohort$SxCystectomy <- rowSums(as.data.frame(lapply(select(pt, one_of(c(procs))), function(x) x %in% codes$SxCystectomy)))
cohort$SxRadNx <- rowSums(as.data.frame(lapply(select(pt, one_of(c(procs))), function(x) x %in% codes$SxRadNx)))
cohort$SxPartialNx <- rowSums(as.data.frame(lapply(select(pt, one_of(c(procs))), function(x) x %in% codes$SxPartialNx)))
cohort$SxRP <- rowSums(as.data.frame(lapply(select(pt, one_of(c(procs))), function(x) x %in% codes$SxRP)))
cohort$SxRPLND <- rowSums(as.data.frame(lapply(select(pt, one_of(c(procs))), function(x) x %in% codes$SxRPLND)))

# Assign cohorts in main patient dataframe
# Each patient can only be in one cohort (for now)
# Can create possibilities for multiple cohorts if necessary for other analyses
pt$cohort <- NA
pt$cohort[cohort$DxKidneyCa>0 & cohort$SxRadNx>0] <- "RadNx"
pt$cohort[cohort$DxKidneyCa>0 & cohort$SxPartialNx>0] <- "PartialNx"
pt$cohort[cohort$DxProstateCa>0 & cohort$SxRP>0 & pt$sex=="Male"] <- "RP"

# Assign cystectomy AFTER prostatectomy
# often cystoprostatectomy is coded as cystectomy AND prostatectomy, this way its assigned to cystectomy cohort
pt$cohort[cohort$DxBladderCa>0 & cohort$SxCystectomy>0] <- "Cystectomy"

# Limiting RPLND to Males only. Although it should be obvious from Dx codes for testis cancer,
# 198.82 "Secondary malignant neoplasm of genital organs" is often used for gyn malignancies (>1300 cases)
pt$cohort[cohort$DxTestisCa>0 & cohort$SxRPLND>0 & pt$sex=="Male"] <- "RPLND"

# Exclude if patients have the following combinations of surgeries AND diagnoses
# Cystectomy and RPLND, Bladder cancer AND Testis Cancer
pt$cohort[cohort$SxCystectomy>0 & cohort$DxBladderCa>0 & cohort$SxRPLND>0 & cohort$DxTestisCa>0] <- "Multiple GU Sx"

# Cystectomy AND Radical or Partial Nx, Bladder and Kidney Cancer
pt$cohort[cohort$SxCystectomy>0 & cohort$DxBladderCa>0 & cohort$DxKidneyCa>0 & (cohort$SxRadNx>0 |cohort$SxPartialNx>0)] <- "Multiple GU Sx"

# Prostate and Kidney cancer, RP and Radical or Partial Nx
pt$cohort[cohort$SxRP>0 & cohort$DxProstateCa>0 & cohort$DxKidneyCa>0 & (cohort$SxRadNx>0 |cohort$SxPartialNx>0)] <- "Multiple GU Sx"

# Testis and Kidney cancer, RPLND and Radical or Partial Nx
pt$cohort[cohort$SxRPLND>0 & cohort$DxTestisCa>0 & cohort$DxKidneyCa>0 & (cohort$SxRadNx>0 |cohort$SxPartialNx>0) & pt$sex!="Male"] <- "Multiple GU Sx"

# Calculate mean annual number of procedures per hospital for each category, and assign quintiles
# generate years variable, which counts number of years of data included
# It is 5, but in case we include additional data in future
years <- as.numeric(difftime(max(pt$dschdate), min(pt$dschdate))/365)
pt <- pt %>%
  group_by(oshpd_id, cohort) %>%
  summarise(volume = n()/years) %>%
  filter(!is.na(cohort)) %>%
  group_by(cohort) %>%
  mutate(volumequint = ntile(volume, 5)
  ) %>%
  right_join(pt)

# save quintiles as factor variable
pt$volumequint <- factor(pt$volumequint)
rm(years)  

# Checkpoint
# setwd("/Users/anobel/Documents/code/rao/")
saveRDS(pt, file="rao_workingdata/pt.rds")
saveRDS(cohort, file="rao_workingdata/cohort.rds")
rm(cohort, codes, diags, procs, icd9detail)
# Once complete, run rao_processing_all.r to combine with other datasets

# Misc
# to get listing of ICD9 descriptions for each patient
# apply(pt[4,diags], 1, function(x) icd9Explain(x[icd9IsReal(x)]))
