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
pt <- apply(data.frame(paste("rao_originaldata/oshpd/",list.files("rao_originaldata/oshpd/"),sep="")), 1, FUN=read.csv, header=TRUE, stringsAsFactors=TRUE)

# row binds all the dataframes in the list into one frame. rbind.fill from plyr
pt <- do.call(rbind.fill, pt)

# place columns in alphabetical order (ignore incorrect numerical ordering)
pt <- pt[,sort(colnames(pt))]

# make a smaller subset to make it easier to work with (random sample of 100k rows)
# In future, can just remove this line for full analysis
pt <- droplevels(pt[sample(1:nrow(pt), 10^5, replace=F),])


##################
#### Data Cleaning
##################

# Format all dates to POSIX
# make a vector identifying the date fields
d <- c("admtdate", "bthdate", "proc_pdt", paste("procdt", 1:20, sep=""))

# Convert date fields from factor to character, replace in dataframe
pt[,d] <- as.data.frame(lapply(pt[,d], as.character), stringsAsFactors = F)

# Convert date fields from character to POSIXct using lubridate, replace in dataframe
pt[,d] <- as.data.frame(lapply(pt[,d], parse_date_time, orders="mdY"))


