###################################
#### ICD-9CM Procedure Codes
###################################
# import and save, keep for future use

# Import procedure codes for 2011, use them as field labels
# Files originally available from: http://www.cdc.gov/nchs/icd/icd9cm.htm
icd9cm <- read.fwf(file = "data/raw/medicare_icd9cm/v29.y2011.icd9cm.txt", header = F, widths = c(4, 1000), stringsAsFactors = F)
write.csv(icd9cm, file="data/tidy/icd9cm.csv", row.names = F)
rm(icd9cm)
