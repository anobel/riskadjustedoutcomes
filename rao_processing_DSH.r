library(stringr)
library(ggmap)
library(dplyr)

# Import Medicare DSH data into separate dataframes
# did this as individual calls because the column names/numbers are different, so cant do regular rbind
# Import DSH data into a list of dataframes
dsh <- apply(data.frame(paste("rao_originaldata/medicare_dsh/",list.files("rao_originaldata/medicare_dsh/"),sep="")), 1, FUN=read.csv, header=T, stringsAsFactors=F)

# This is a list of variables I want to keep 
dvars <- c("Provider.Number", "Name", "BEDS", "ADC", "Average.Daily.Census", "DSHPCT", "URGEO", "DSHOPG", "DSHCPG", "MCR_PCT", "^CMI")
dvars <- paste(dvars, collapse="|")

# lets subset and just keep the columns of interest, including BOTH CMI versions
dsh <- lapply(dsh, function(x) x[,grep(paste(dvars, collapse="|"), colnames(x))])

# Now we need to subset and drop the older of the two CMI versions
# extract the number after CMI and get make a list of the index position of the larger number
cmis <- lapply(dsh, function(x) str_match(colnames(x), "[0-9]{2}"))
cmis <- lapply(cmis, function(x) which(x==max(x, na.rm=TRUE)))

# re list the variables I want to keep
dvars <- c("Provider.Number", "Name", "BEDS", "ADC", "Average.Daily.Census", "DSHPCT", "URGEO", "DSHOPG", "DSHCPG", "MCR_PCT")
dvars <- paste(dvars, collapse="|")

# Make a list of index values for the columns of interest, then combine it with the list of CMI index positions and convert to matrix
dshcols <- lapply(dsh, function(x) grep(dvars,colnames(x)))
dshcols <- mapply(c, dshcols, cmis, SIMPLIFY = FALSE)
dshcols <- as.matrix(do.call(rbind,dshcols))

# use this matrix of column indexes to subset the dsh databases
dsh <- lapply(seq_along(dsh), function(i) dsh[[i]][dshcols[i,]])

# Add the year variable
dsh[[1]]$year <- 2007
dsh[[2]]$year <- 2008
dsh[[3]]$year <- 2009
dsh[[4]]$year <- 2010
dsh[[5]]$year <- 2011

# standardize column labels
# this function will iterate through each dataframe contained in the list and rename columns based on grep match and desired output
ChangeNames <- function(x,y,z) {
  names(x)[grep(y,names(x))] <- z
  return(x)
}

# change all CMIxx variants into just CMI columns
dsh <- lapply(dsh, ChangeNames, "CMI", "CMI")

# replace columns that uce average daily census title to ADC
dsh <- lapply(dsh, ChangeNames, "Average.Daily.Census", "ADC")

# reorder all variables into the same order
dvars <- c("Provider.Number", "Name", "year", "URGEO", "BEDS", "ADC", "DSHPCT", "DSHOPG", "DSHCPG", "MCR_PCT", "CMI")

#reorganize and rename all the columns in the same way
dsh <- lapply(dsh, function(x) x[,match(dvars,colnames(x))])
dvars <- c("providerid", "name", "year", "urban", "beds", "adc", "dsh_pct", "dsh_opg", "dsh_cpg", "mcr_pct","cmi")
dsh <- lapply(dsh, setNames, dvars)

#combine into a single dataframe, remove unwanted dataframes
dsh <- do.call(rbind,dsh)

# In the medicare 2010 data set, there are 61 records that only have one value worth of columns, but nothing else, not even provider id
# drop these errant entries with missing data
dsh <- dsh[!is.na(dsh$providerid),]

# Keep only CA hospitals (first 2 digits in provider number code in for state, and CA is 50)
dsh <- dsh[dsh$providerid>49999 & dsh$providerid<51000,]

#convert column types
dsh$urban <- as.factor(dsh$urban)
dsh$adc <- as.numeric(dsh$adc)
dsh$mcr_pct <- as.numeric(dsh$mcr_pct)

# Clean up environemnt
rm(dshcols, dvars, cmis, ChangeNames)
#######################################
# Now import/prep Hospital Info data
# Load general hospital info from Medicare
hospinfo <- read.csv("rao_originaldata/medicare/Hospital_General_Information.csv", header=TRUE, stringsAsFactors = FALSE)

# Keep only CA hospitals
hospinfo <- hospinfo[grep("^05", hospinfo$Provider.ID),]

# Remove leading zero to match DSH data format
hospinfo$Provider.ID <- as.character(substr(hospinfo$Provider.ID, 2, length(hospinfo$Provider.ID)))

# Merge DSH and Hospital Info data
dsh <- merge(dsh, hospinfo, by.x="providerid", by.y="Provider.ID", all.x=TRUE)

# Need to find info for hospitals that are missing addresses/contact info
# How many hospitals have missing addresses?
dsh %>% 
  filter(is.na(Hospital.Name)) %>% 
  summarise (missing = n_distinct(providerid))

# make dataframe of hospitals with missing addresses
missing <- dsh %>% 
  filter(is.na(Hospital.Name)) %>%
  filter(!name=="") %>%
  distinct(providerid) %>%
  select(providerid, name, Address, City, State, ZIP.Code, County.Name)


# save the missing hospitals to a csv to manually find addresses
write.csv(missing, "rao_workingdata/missing.csv", row.names = F)
# re-import the DF once I've put addresses in manually
missing <- read.csv("rao_workingdata/missing_completed.csv")
# Geocode using ggmap to get lat/lon, combine with missing df
missingcoords <- geocode(paste(missing$Address, missing$City, missing$ZIP.Code))
missing <- cbind(missing, missingcoords)

# make a new Location column in the missing DF, combination of all address fields in one
missing$Location <- paste(missing$Address, missing$City, missing$State, missing$ZIP.Code, sep=" ")

# make a vector identifying the matches, so we only copy over the matched results and not the NAs, so we dont overwrite the original
vec <- !is.na(missing[match(dsh$providerid,missing$providerid),"ZIP.Code"])

# copy data from missing DF into original DSH DF
dsh$Address[vec] <- missing[match(dsh$providerid, missing$providerid),"Address"][vec]
dsh$City[vec] <- missing[match(dsh$providerid, missing$providerid),"City"][vec]
dsh$State[vec] <- missing[match(dsh$providerid, missing$providerid),"State"][vec]
dsh$ZIP.Code[vec] <- missing[match(dsh$providerid, missing$providerid),"ZIP.Code"][vec]
dsh$lon <- missing[match(dsh$providerid, missing$providerid),"lon"]
dsh$lat <- missing[match(dsh$providerid, missing$providerid),"lat"]

rm(missing, missingcoords, vec, hospinfo)

# extract the coordinates from the $location column
dsh$latnew <- as.numeric(str_extract(dsh$Location, "[0-9]{2}\\.[0-9]+"))
dsh$lonnew <- as.numeric(str_extract(dsh$Location, "-[0-9]{3}\\.[0-9]+"))

# I had separately geocoded certain addresses that were manually added, so lets copy those over 
dsh$lon[is.na(dsh$lon)] <- dsh$lonnew[is.na(dsh$lon)] 
dsh$lat[is.na(dsh$lat)] <- dsh$latnew[is.na(dsh$lat)]
dsh <- select(dsh, -lonnew, -latnew)

# there are 46 records with providerid and some data, but no names/addresses, all 2007-2008
# facilities that no longer exist. will drop them
dsh<- dsh[!dsh$name=="",]

titlecase <- c("name", "Hospital.Name", "Address", "City", "County.Name")
dsh[,colnames(dsh) %in% titlecase] <- lapply(dsh[,colnames(dsh) %in% titlecase], FUN=str_to_title)
rm(titlecase)

# Scale percentages to 100
dsh$dsh_pct <- dsh$dsh_pct*100
dsh$mcr_pct <- dsh$mcr_pct*100

# Assign hospitals to quintiles
dsh <- dsh %>%
  group_by(providerid) %>%
  dplyr::summarise(
    dshMean = mean(dsh_pct, na.rm=T)
  ) %>%
  mutate(
    dshquintile = ntile(dshMean, 5)
  ) %>%
  select(-dshMean) %>%
  right_join(dsh)

# classify safety net hospitals as top quintile of DSH percentage
dsh$safetydsh <- ifelse(dsh$dshquintile==5, T, F)

# Assign Safety Net status based on NAPH membership
# assign all hospitals as "no", then tag specific hospitals as YES
dsh$safetynaph <- F

# Listing of safety net list from California Association of Public Hospitals and Health Systems
# http://caph.org/caphmemberhospitals/memberdirectory/
# Cross referenced with list from California Health Care Safety Net Institute
# http://safetynetinstitute.org/californias-public-hospitals-and-clinics/
safetynaph <- c(50320, 50211, 50276, 50315, 50373, 50376, 50040, 50717, 50262, 50248, 50348, 50292, 50599,
                50245, 50025, 50228, 50668, 50454, 50167, 50113, 50038, 50159)

dsh$safetynaph[dsh$providerid %in% safetynaph] <- T
rm(safetynaph)

saveRDS(dsh, file="rao_workingdata/dsh.rds")
