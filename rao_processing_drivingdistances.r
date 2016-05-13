# load packages
library(ggmap)
library(dplyr)

# load the dataframe of addresses for which we want to calculate driving distances
# For now, because google maps API limit is 2500 requests per day, will only do the urology cohort
# which is about 20k combinations of to/from zip codes
# pt <- readRDS("data/patient/pt.rds")
ptgu <- readRDS("data/patient/ptgu.rds")

# make dataframe of unique zip combinations and drop obs with mizzing zips
ptzips <- ptgu %>%
  ungroup () %>%
  filter(!is.na(patzcta) & !is.na(hospzcta)) %>%
  select(
    patzcta, hospzcta
  ) %>%
  mutate(
    patzcta = as.character(patzcta),
    hospzcta = as.character(hospzcta)) %>%
  distinct(patzcta, hospzcta)

# calculate distances using mapdist() in ggmap package
# but due to google API limit of 2500 reuests per day, have to do it in a series of chunks and combine them
driving <- lapply(1:2500,
                  function (x) {
                    mapdist(as.character(ptzips[x, "patzcta"]), as.character(ptzips[x, "hospzcta"]),
                      mode="driving",
                      output="simple")
                  })

# drop any zip code pairs with missing/incomplete data
driving <- driving[sapply(driving,length) == 8]
 
# make an empty data frame the first time this is done, and then bind to the prior data
# drivingdf <- list()
drivingdf <- rbind(drivingdf, do.call(rbind, driving))

driving <- drivingdf %>%
 mutate(
   patzcta = as.numeric(from),
   hospzcta = as.numeric(to),
 ) %>%
 select(
    patzcta, hospzcta,
    km,
    hours
 )

driving$patzcta <- as.numeric(driving$patzcta)
driving$hospzcta <- as.numeric(driving$hospzcta)

write.csv(driving, file="data/tidy/distances.csv", row.names = F)