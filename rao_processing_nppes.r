library(reshape2)
library(stringr)
library(tidyr)
library(dplyr)
library(sp)
library(rgeos)
library(rgdal)
library(leaflet)
library(DT)

# Import NPI 2011 and 2013 data
load("rao_workingdata/nppes_workingdata/npi11geo.rda")
load("rao_workingdata/nppes_workingdata/npi12geo.rda")
load("rao_workingdata/nppes_workingdata/npi13geo.rda")
load("rao_workingdata/zcta.rda")

# add year variable
npi11$year = 2011
npi12$year = 2012
npi13$year = 2013

# combine NPI data sets from each year, filter california only, drop unwanted fields, and correct zip code
npi <- bind_rows(npi11, npi12, npi13) %>% 
  filter(state=="CA") %>%
  filter(zip>90000) %>%
  select(-enumeration, -update) %>%
  mutate(zip = as.numeric(str_sub(zip, 1, 5)))

# merge NPI to add ZCTAs, drip Zip
npi <- left_join(npi, zcta) %>%
  select(-zip)

# clean up environment
rm(npi11, npi12, npi13, zcta)

md <- npi %>%
  group_by(zcta, year, spec) %>%
  summarise(count = n()) %>%
  spread(spec, count)

md$total <- rowSums(md[3:5], na.rm=T)

# any zip codes that have no physicians in a category come up as NA, make these structural 0
md[is.na(md)] <- 0


# calculate the mean number of providers in each ZCTA across all years available
md <- md %>%
  group_by(zcta) %>%
  summarise(
    meangu = mean(Urology),
    meanpmd = mean(PMD),
    meanothermd = mean(Other),
    meantotal = mean(total)
  ) %>%
  filter(zcta>0)

save(npi, md, file="rao_workingdata/md.rda")
