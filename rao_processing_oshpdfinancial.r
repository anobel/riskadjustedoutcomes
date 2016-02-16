library(plyr)
library(stringr)
library(tidyr)
library(dplyr)

# Load OHSPD Hospital Data to use as Crosswalk to Medicare ID
# From http://www.oshpd.ca.gov/hid/Products/Hospitals/AnnFinanData/SubSets/SelectedData/default.asp
# Load data from 2007-2011 (in case there are new faciliteis or old ones removed)

# Import OSHPD data into a list of dataframes
oshpdxwalk <- apply(data.frame(paste("rao_originaldata/oshpd_financialdata/",list.files("rao_originaldata/oshpd_financialdata/"),sep="")), 1, FUN=read.csv, na.strings=c(""), header=T, stringsAsFactors=F)

# Row binds all the dataframes in the list into one frame. rbind.fill from plyr
oshpdxwalk <- do.call(rbind.fill, oshpdxwalk)

colnames(oshpdxwalk) <- str_to_lower(names(oshpdxwalk))

# select just the facility number (OSHPD ID), name, and Medicare Provider ID
oshpdxwalk <- oshpdxwalk %>%
  select(oshpd_id = fac_no, fac_name, providerid = mcar_pro.) %>%
  # remove the first 3 numbers from all fields, all are 106 and not part of OSHPD ID
  mutate(oshpd_id = as.numeric(str_sub(oshpd_id,4,9))) %>% 
  # remove intervening - from medicare ID
  mutate(providerid = as.numeric(str_replace(providerid, "-",""))) %>%
  group_by(oshpd_id) %>%
  # select just the distinct OSHPD IDs
  distinct(oshpd_id) %>%
  # Drop fields with OSHPD ID or Medicare provider id listed as NA
  filter(!is.na(oshpd_id)) %>%
  as.data.frame()

# some Provider IDs are missing in the OSHPD data
# write.csv(oshpdxwalk %>% filter(is.na(providerid)), file="rao_workingdata/oshpdidmedicareblank.csv", row.names = F)
# most of them are Mental Health Facilities and Substance Rehab facilities
# Will have to manually enter Medicare IDs for certain hospitals (some Kaisers, and Scripps)
oshpdxwalk[oshpdxwalk$oshpd_id==74097, c("fac_name", "providerid")] <- c("KAISER FOUNDATION HOSPITAL - ANTIOCH", 50760)
oshpdxwalk[oshpdxwalk$oshpd_id==10858, c("fac_name", "providerid")] <- c("KAISER FOUNDATION HOSPITAL - FREMONT", 50512)
oshpdxwalk[oshpdxwalk$oshpd_id==410806, c("fac_name", "providerid")] <- c("KAISER FOUNDATION HOSPITAL - SOUTH SAN FRANCISCO", 50070)
oshpdxwalk[oshpdxwalk$oshpd_id==484044, c("fac_name", "providerid")] <- c("KAISER FOUNDATION HOSPITAL - VACAVILLE", 50767)
oshpdxwalk[oshpdxwalk$oshpd_id==480989, c("fac_name", "providerid")] <- c("KAISER FOUNDATION HOSPITAL AND REHAB CENTER", 50073)
oshpdxwalk[oshpdxwalk$oshpd_id==371256, c("fac_name", "providerid")] <- c("SCRIPPS GREEN HOSPITAL", 50424)

oshpdxwalk <- oshpdxwalk %>%
  filter(!is.na(providerid))

oshpdxwalk$providerid <- as.numeric(oshpdxwalk$providerid)

saveRDS(oshpdxwalk, file="rao_workingdata/oshpdxwalk.rds")
rm(oshpdxwalk)