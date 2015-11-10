
# analyze survey data for free (http://asdfree.com) with the r language
# area resource file




# set your working directory.
# all ARF files will be stored here
# after downloading and importing it.
# use forward slashes instead of back slashes

# uncomment this line by removing the `#` at the front..

setwd("/Users/anobel/Documents/Dropbox/Research/Risk Adjusted Outcomes/RAO_analysis/rao_originaldata/arf/arf12/")
# ..in order to set your current working directory

############################################
# no need to edit anything below this line #

# # # # # # # # #
# program start #
# # # # # # # # #


# load necessary libraries
library(RSQLite) 	# load RSQLite package (creates database files in R)
library(SAScii) 	# load the SAScii package (imports ascii data with a SAS script)
library(descr) 		# load the descr package (converts fixed-width files to delimited files)
library(foreign) 	# load foreign package (converts data files into R)
library(downloader)	# downloads and then runs the source() function on scripts from github


# load the read.SAScii.sqlite function (a variant of read.SAScii that creates a database directly)
source_url( "https://raw.github.com/ajdamico/asdfree/master/SQLite/read.SAScii.sqlite.R" , prompt = FALSE )


# create a temporary database file and another temporary file
temp.db <- tempfile()
tf <- tempfile()


########## THIS Section was original, for downloading ARF file from web
## If already on disk, skip this section
# download the most current ARF file
# and save it as the temporary file
#download.file( "http://datawarehouse.hrsa.gov/DataDownload/ARF/AHRF_2014-2015.zip" , tf , mode = 'wb' )
# unzip all of the files in the downloaded .zip file into the current working directory
# then save all of their unzipped locations into a character vector called 'files'
#files <- unzip( tf , exdir = getwd() )

## This generates the vector of files necessary, use this line if using files already on disk instead of the download process
files <- list.files(getwd())
# note that this saves *all* of the files contained in the .zip
# into the current working directory -- including the ARF documentation 

# identify ascii file on your locak disk
fn <- files[ grep( '\\.asc' , files ) ]


# identify sas (read-in) import instructions
sas_ri <- files[ grep( '\\.sas' , files ) ]


# create and connect to a temporary SQLite database
db <- dbConnect( SQLite() , temp.db )


# parse through the ARF without touching RAM #
read.SAScii.sqlite( 
  fn = fn ,
  sas_ri = sas_ri ,
  tl = TRUE ,			# convert all column names to lowercase?
  tablename = 'arf' ,
  conn = db
)


# read the ARF into RAM
arf <- dbReadTable( db , 'arf' )


# disconnect from the temporary SQLite database
dbDisconnect( db )

# and delete it
file.remove( temp.db )

## This code generates a vector for the column names from the .sas file
arfcols <- readLines(sas_ri)
arfcols <- arfcols[ grep( "@(.*)/\\*" , arfcols ) ]
arfcols <- str_trim(gsub( "(.*)/\\*(.*)\\*/" , "\\2" , arfcols ))


# save the arf data table as an R data file (.rda)
# (for quick loading later)
save(arf15, arf15cols, file = "arf15.rda" )


# uncomment this line to export the arf data table as a csv file
# write.csv( arf , file.path( getwd() , "arf2014.csv" ) )


# uncomment this line to export the arf data table as a stata file
# Recode blanks to NA, first. You get an "empty string is not valid in Stata's documented format" message otherwise.
# arf[arf == ""] <- NA
# write.dta( arf , file.path( getwd() , "arf2014.dta" ) )


# delete the ARF table from RAM
rm( arf )

# clear up RAM
gc()


# print a reminder: set the directory you just saved everything to as read-only!
message( paste( "all done.  you should set" , getwd() , "read-only so you don't accidentally alter these files." ) )

# for more details on how to work with data in r
# check out my two minute tutorial video site
# http://www.twotorials.com/

# dear everyone: please contribute your script.
# have you written syntax that precisely matches an official publication?
message( "if others might benefit, send your code to ajdamico@gmail.com" )
# http://asdfree.com needs more user contributions

# let's play the which one of these things doesn't belong game:
# "only you can prevent forest fires" -smokey bear
# "take a bite out of crime" -mcgruff the crime pooch
# "plz gimme your statistical programming" -anthony damico