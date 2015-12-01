# Data Cleaning Packages
library(stringr)
library(plyr)
library(tidyr)
library(dplyr)

# Graphics Packages
library(ggplot2)

# Specialty Packages
library(icd9)

# Import OSHPD data into a list of dataframes
pt <- apply(data.frame(paste("rao_originaldata/oshpd/",list.files("rao_originaldata/oshpd/"),sep="")), 1, FUN=read.csv, header=TRUE, stringsAsFactors=TRUE)

# row binds all the dataframes in the list into one frame. rbind.fill from plyr
pt <- do.call(rbind.fill, pt)

# standardizes column index numbers 
colnames(pt) <- sub("([a-z])([0-9])$","\\10\\2",colnames(pt))

# row binds all the dataframes in the list into one gbdframe
pt <- do.call(rbind.fill, pt)

# make a smaller subset to make it easier to work with (random sample of 100k rows)
# In future, can just remove this line for full analysis
pt <- droplevels(pt[sample(1:nrow(pt), 10^5, replace=F),])

