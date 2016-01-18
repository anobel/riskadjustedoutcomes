library(reshape2)
library(stringr)
library(acs)
library(dplyr)

# to use the ACS package to directly download ACS data, need to sign up for API key and install it (once)
#api.key.install("d5481ffa6d9e8cb41a08e565fbd4d6baa46cf350")
load("rao_workingdata/zcta.rda")

# define the geographic unit (ZCTAs) for data
# this cannot be combined with any other options, so get all ZCTAs then drop non-CA
geo_zip = geo.make(zip.code = "*")

# Download the data for desired tables directly from the American Community Survey
acsd <- NULL
# B01003 Total Population
acsd$pop <- acs.fetch(geo_zip, table.number = "B01003", endyear=2011, span=5, col.names = "pretty")
# Keep only California ZCTAs
acsd$pop <- acsd$pop[geography(acsd$pop)$zipcodetabulationarea %in% zcta$zcta]

# B03002 - Ethnicity Data
acsd$ethnicity <- acs.fetch(geo_zip, table.number = "B03002", endyear=2011, span=5, col.names = "pretty")
acsd$ethnicity <- acsd$ethnicity[geography(acsd$ethnicity)$zipcodetabulationarea %in% zcta$zcta]

acsd$ethwhite <- divide.acs(num=acsd$ethnicity[,3], den=acsd$ethnicity[,1], method="proportion")
acsd$ethblack <- divide.acs(num=acsd$ethnicity[,4], den=acsd$ethnicity[,1], method="proportion")
acsd$ethlatin <- divide.acs(num=acsd$ethnicity[,12], den=acsd$ethnicity[,1], method="proportion")
acsd$ethasian <- divide.acs(num=acsd$ethnicity[,6], den=acsd$ethnicity[,1], method="proportion")
acsd$ethother <- apply(X=acsd$ethnicity[,c(5,7:11)], 2, FUN=sum, agg.term="ethother") 
acsd$ethother <- divide.acs(num=acsd$ethother, den=acsd$ethnicity[,1], method="proportion")

# B19013 - Median Household Income
acsd$medianincome <- acs.fetch(geo_zip, table.number = "B19013", endyear=2011, span=5, col.names = "pretty")
# Keep only California ZCTAs
acsd$medianincome <- acsd$medianincome[geography(acsd$medianincome)$zipcodetabulationarea %in% zcta$zcta]

# B19001 - Household Income Groupings
acsd$incomegroups <- acs.fetch(geo_zip, table.number = "B19001", endyear=2011, span=5, col.names = "pretty")
# Keep only California ZCTAs
acsd$incomegroups <- acsd$incomegroups[geography(acsd$incomegroups)$zipcodetabulationarea %in% zcta$zcta]

# for income under 25k, combine these groups then divide by total to create proportion
# Household Income     Less than $10,000
# Household Income    $10,000 to $14,999
# Household Income    $15,000 to $19,999
# Household Income    $20,000 to $24,999
acsd$incomeunder25k <- apply(X=acsd$incomegroups[,c(2:5)], 2, FUN=sum, agg.term="incomeunder25k") 
acsd$incomeunder25k <- divide.acs(num=acsd$incomeunder25k, den=acsd$incomegroups[,1], method="proportion")

# for income over 100k, combine these groups then divide by total to create proportion
# Household Income  $100,000 to $124,999
# Household Income  $125,000 to $149,999
# Household Income  $150,000 to $199,999
# Household Income      $200,000 or more
acsd$incomeover100k <- apply(X=acsd$incomegroups[,c(14:17)], 2, FUN=sum, agg.term="incomeover100k") 
acsd$incomeover100k <- divide.acs(num=acsd$incomeover100k, den=acsd$incomegroups[,1], method="proportion")

# B19054 - Interest, Dividends, or Net Rental Income for Households 
acsd$capincome <- acs.fetch(geo_zip, table.number = "B19054", endyear=2011, span=5, col.names = "pretty")
# Keep only California ZCTAs
acsd$capincome <- acsd$capincome[geography(acsd$capincome)$zipcodetabulationarea %in% zcta$zcta]

# Calculate the proportion of households that have interest, dividend, or rental income
# used divide method in ACS package to also generate appropriate standard errors
acsd$capincome <- divide.acs(num=acsd$capincome[,2], den=acsd$capincome[,1], method="proportion")

# B25077 - Median Value (Dollars) for Owner-Occupied Housing Units
acsd$housevalue <- acs.fetch(geo_zip, table.number = "B25077", endyear=2011, span=5, col.names = "pretty")
# Keep only California ZCTAs
acsd$housevalue <- acsd$housevalue[geography(acsd$housevalue)$zipcodetabulationarea %in% zcta$zcta]

# B15002  Sex by Educational Attainment for the Population 25 Years and over
acsd$edu <- acs.fetch(geo_zip, table.number="B15002", endyear=2011, span=5, col.names = "pretty")
acsd$edu <- acsd$edu[geography(acsd$edu)$zipcodetabulationarea %in% zcta$zcta]
# Need to combine columns to get the total number of people (males and females) that did not complete HS
# combined the following columns
# Sex by Educational Attainment for the Population 25 Years and over: No schooling completed 
# Sex by Educational Attainment for the Population 25 Years and over: Nursery to 4th grade 
# Sex by Educational Attainment for the Population 25 Years and over: 5th and 6th grade 
# Sex by Educational Attainment for the Population 25 Years and over: 7th and 8th grade 
# Sex by Educational Attainment for the Population 25 Years and over: 9th grade 
# Sex by Educational Attainment for the Population 25 Years and over: 10th grade 
# Sex by Educational Attainment for the Population 25 Years and over: 11th grade 
# Sex by Educational Attainment for the Population 25 Years and over: 12th grade, no diploma 
acsd$edunohs <- apply(X=acsd$edu[,c(3:10, 20:27)], 2, FUN=sum, agg.term="nohs") 
# divide by total ZCTA population (using ACS functions) to get proportion
acsd$edunohs <- divide.acs(num=acsd$edunohs, den=acsd$edu[,1], method="proportion")

# need to combine columns to get the total number of people (males and females) that have completed HS
# combined these columns:
# Sex by Educational Attainment for the Population 25 Years and over: High school graduate, GED, or alternative
# Sex by Educational Attainment for the Population 25 Years and over: Some college, less than 1 year
# Sex by Educational Attainment for the Population 25 Years and over: Some college, 1 or more years, no degree
# Sex by Educational Attainment for the Population 25 Years and over: Associate's degree
# Sex by Educational Attainment for the Population 25 Years and over: Bachelor's degree
# Sex by Educational Attainment for the Population 25 Years and over: Master's degree
# Sex by Educational Attainment for the Population 25 Years and over: Professional school degree
# Sex by Educational Attainment for the Population 25 Years and over: Doctorate degree

acsd$eduhs <- apply(X=acsd$edu[,c(11:18,28:35)], 2, FUN=sum, agg.term="hs") 
# divide by total ZCTA population (using ACS functions) to get proportion
acsd$eduhs <- divide.acs(num=acsd$eduhs, den=acsd$edu[,1], method="proportion")

# For numbers that completed college, combined the last 4 columns above
acsd$educollege <- apply(X=acsd$edu[,c(15:18,32:35)], 2, FUN=sum, agg.term="college") 
# divide by total ZCTA population (using ACS functions)
acsd$educollege <- divide.acs(num=acsd$educollege, den=acsd$edu[,1], method="proportion")

# C24010 Sex by Occupation for the Civilian Employed Population 16 years and over
acsd$emp <- acs.fetch(geo_zip, table.number="C24010", endyear=2011, span=5, col.names = "pretty")
# Keep only California ZCTAs
acsd$emp <- acsd$emp[geography(acsd$emp)$zipcodetabulationarea %in% zcta$zcta]

# Add up columns for "Management, business, science, and arts occupations", male and female
acsd$empexec <- apply(X=acsd$emp[,c(3, 39)], 2, FUN=sum, agg.term = "exec")
acsd$empexec <- divide.acs(num=acsd$empexec, den=acsd$pop, method="proportion")

# B19083 Gini Index Of Income Inequality
acsd$gini <- acs.fetch(geo_zip, table.number="B19083", endyear=2011, span=5, col.names = "pretty")
# Keep only California ZCTAs
acsd$gini <- acsd$gini[geography(acsd$gini)$zipcodetabulationarea %in% zcta$zcta]

acs <- data.frame(
  zcta = as.numeric(geography(acsd$pop)$zipcodetabulationarea),
  pop = as.numeric(estimate(acsd$pop)),
  ethwhite = as.numeric(estimate(acsd$ethwhite))*100,
  ethblack = as.numeric(estimate(acsd$ethblack))*100,
  ethlatin = as.numeric(estimate(acsd$ethlatin))*100,
  ethasian = as.numeric(estimate(acsd$ethasian))*100,
  ethother = as.numeric(estimate(acsd$ethother))*100,
  medianincome = as.numeric(estimate(acsd$medianincome)),
  medianincomelog = log(as.numeric(estimate(acsd$medianincome))),
  incomeunder25k = as.numeric(estimate(acsd$incomeunder25k))*100,
  incomeover100k = as.numeric(estimate(acsd$incomeover100k))*100,
  capincome = as.numeric(estimate(acsd$capincome))*100,
  housevalue = as.numeric(estimate(acsd$housevalue)),
  housevaluelog = log(as.numeric(estimate(acsd$housevalue))),
  edunohs = as.numeric(estimate(acsd$edunohs))*100,
  eduhs = as.numeric(estimate(acsd$eduhs))*100,
  educollege = as.numeric(estimate(acsd$educollege))*100,
  empexec = as.numeric(estimate(acsd$empexec))*100,
  gini = as.numeric(estimate(acsd$gini))
  )

# Clean up
rm(zcta, geo_zip)

# all the NaN were generated by me dividing by zero for population
# because these were supposed to proportions of population, if 0 population, these are by definition 0
# replace NaN with 0, but is.nan only works on matrices, not dataframes, so a simple function to fix that
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
acs[is.nan(acs)] <- 0

acs$iceinc <- (acs$incomeover100k - acs$incomeunder25k)/100
acs$iceedu <- (acs$educollege - acs$edunohs)/100
acs$iceeth <- (acs$ethwhite - acs$ethblack)/100

# clean up environment
rm(is.nan.data.frame, acsd)

# convert the dataframe from values into z-scores
acsz<- as.data.frame(cbind(zcta=acs[,1],scale(acs[,c(2:length(acs))], center=T, scale=T)))

# Calculate combined neighbordhood Z-score
# Used rowSums after binding columns of interest into a matrix so I could ignore NA's
acs$hoodzscore <- rowSums(cbind(acsz$medianincomelog, acsz$housevalue, acsz$capincome, acsz$eduhs, acsz$educollege, acsz$empexec), na.rm=T)

# Assign quintiles
acs$hoodquint <- ntile(acs$hoodzscore, 5)

# Create centered and scaled variables, useful for regression interpretation
# Zip code population for every 10,000 people
acs$pop10k <- acs$pop/10000

# Median household income, for every $1,000
acs$medianincome1k <- acs$medianincome/1000
# For now, will not center median income (description of interpretation will be confusing
# "For each $1k increase in median income over the mean)
# acs$medianincome1kcenter <- scale(acs$medianincome, center=T, scale=F)/1000

# Median Home Value, foe every $100,000
acs$housevalue100k <- acs$housevalue/100000

acs <- acs %>%
  select(zcta, pop, pop10k, 
         ethwhite, ethblack, ethlatin, ethasian, ethother, 
         medianincome, medianincome1k, incomeunder25k, incomeover100k, capincome, 
         housevalue, housevalue100k,
         edunohs, eduhs, educollege, empexec,
         gini, iceinc, iceedu, iceeth, hoodzscore, hoodquint)

# Save into RDS
saveRDS(acs, file="rao_workingdata/acs.rds")



