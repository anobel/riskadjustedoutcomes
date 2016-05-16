library(data.table)
library(RCurl)
library(dplyr)

# Download Medicare Provider of Service files, extract number of residents per hospital
# Data from: https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/Provider-of-Services/index.html

# List of URLs for data per year. if we need extra years, just add the URL to this list
# in 2011, column names changed and are descriptive, do nOt match 2007-2010
# also, in 2011, number of FTE residents is not included in data set, so will only use 2007-2010 data
# download all years together, rbind 2007-2010 and then add 2011
mp <- list()
mp$year2007 <- c("https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/Provider-of-Services/Downloads/DEC07_OTHER_CSV.zip")
mp$year2008 <- c("https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/Provider-of-Services/Downloads/DEC08_OTHER_CSV.zip")
mp$year2009 <- c("https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/Provider-of-Services/Downloads/DEC09_OTHER_CSV.zip")
mp$year2010 <- c("https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/Provider-of-Services/Downloads/DEC10_OTHER_CSV.zip")

# Create folder structure for the data that will be downloaded
lapply(1:length(mp), function(x) dir.create(paste("data/raw/medicarepos", names(mp)[x], sep="/"), showWarnings = F))

# Download Medicare data
lapply(1:length(mp), function(x) download.file(mp[[x]], paste("data/raw/medicarepos/", names(mp)[x], "/", names(mp)[x], ".zip", sep = ""), mode = "wb"))

# get list of the zip files, then use that list to identify the csv files within them, save names into vector
filenames <- list.files("data/raw/medicarepos/", recursive = T)
filenames <- lapply(paste("data/raw/medicarepos/", filenames, sep = ""), function(x) unzip(x, list=T))

filenames <- unlist(filenames)
filenames <- filenames[grepl("csv", filenames)]

# Unzip each of the files and import the data into a list using fread
# of note, the second line of some files is corrupt, so have to separately import column names 
# and then skip the first two lines to import the rest of the data
# Import column names
mpnames <- lapply(1:length(mp), 
                  function(x) fread(
                    unzip(
                      paste("data/raw/medicarepos/",list.files("data/raw/medicarepos", recursive = T)[x], sep=""),
                      filenames[x]
                    ),
                    nrows = 0)
                  )

# Import data
mpdata <- lapply(1:length(mp), 
            function(x) fread(
                          unzip(
                            paste("data/raw/medicarepos/",list.files("data/raw/medicarepos", recursive = T)[x], sep=""),
                            filenames[x]
                            )
                          , skip=2)
            )

# Apply column names from each file to the respective data frame within the list
lapply(1:length(mp), function (x) colnames(mpdata[[x]]) <- colnames(mpnames[[x]]))

# combine them into a single data frame, using rbind_all to match up column names
mpdata <- rbind_all(mpdata)

# Clean environment
rm(mp, filenames, mpnames)

# select variables of interest
mpdata <- mpdata %>%
  select(facilityname = PROV0475,
         providerid = PROV1680,
         state = PROV3230,
         residents = PROV1165
         ) %>%
filter(state == "CA")

# select the number of residents for each facility, create flag if residency exists
mpdata <- mpdata %>%
  group_by(providerid) %>%
  dplyr::summarise(residentnum = max(residents, na.rm=T)) %>%
  mutate(residency = ifelse(residentnum>0, T, F))

mpdata$providerid <- as.numeric(mpdata$providerid)

mpdata <- mpdata %>%
  filter(!is.na(providerid)) %>%
  ungroup()

# Import data listing which programs have urology residencies
residencygu <- read.csv("data/tidy/residentsGU.csv", header=T, stringsAsFactors = F)
residencygu <- select(residencygu, -fac_name)

# Merge with Medicare residency data
mpdata <- mpdata %>%
  left_join(residencygu)

# if NA, assign it as No
mpdata$residencygu[mpdata$residencygu == "Yes"] <- T
mpdata$residencygu[is.na(mpdata$residencygu)] <- F
mpdata$residencygu <- as.logical(mpdata$residencygu)

# Save data
write.csv(mpdata, file="data/tidy/residents.csv", row.names = F)
