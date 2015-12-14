###################################
#### ICD-9CM Procedure Codes
###################################
# import and save, keep for future use

# Import procedure codes for 2011, use them as field labels
icd9cm <- read.fwf(file="rao_originaldata/medicare_icd9cm/v29.y2011.icd9cm.txt", header=F, widths=c(4,1000), stringsAsFactors=F)
save(icd9cm, file="rao_workingdata/icd9cm.rda")
rm(icd9cm)
