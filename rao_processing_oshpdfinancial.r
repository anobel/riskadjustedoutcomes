library(stringr)
library(tidyr)
library(dplyr)

# Load OHSPD Hospital Data to use as Crosswalk to Medicare ID
# From http://www.oshpd.ca.gov/hid/Products/Hospitals/AnnFinanData/SubSets/SelectedData/default.asp

# Load data from 2007-2011 (in case there are new faciliteis or old ones removed)

# Import OSHPD data into a list of dataframes
oshpdfinance <- apply(data.frame(paste("rao_originaldata/oshpd_financialdata/",list.files("rao_originaldata/oshpd_financialdata/"),sep="")), 1, FUN=read.csv, na.strings=c(""), header=T, stringsAsFactors=F)

# row binds all the dataframes in the list into one frame. rbind.fill from plyr
oshpdfinance <- do.call(rbind.fill, oshpdfinance)
colnames(oshpdfinance) <- str_to_lower(names(oshpdfinance))

oshpdxwalk <- oshpdfinance %>%
  select(oshpd_id = fac_no, fac_name, providerid = mcar_pro.) %>%
  mutate(oshpd_id = str_sub(oshpd_id,4,9)) %>%
  group_by(oshpd_id) %>%
  distinct(oshpd_id)

save(oshpdxwalk, file="rao_workingdata/oshpdxwalk.rda")
rm(oshpdfinance, oshpdxwalk)