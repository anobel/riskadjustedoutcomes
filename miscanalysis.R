############ MISC GEOCODING CODE I DONT NEED ANYMORE FOR NOW
# Now to geocode all the hospitals from medicare DSH database that are combined in a messy string
# will run it through google's geocoder instead of manually destringing
# dsh$locnew[str_locate(dsh$location,"\\(")] <- substr(dsh$Location,1,str_locate(dsh$Location, "\\(")-2)
#dsh$locnew <- str_replace_all(dsh$locnew,"\\\n",", ")

# geocoding
# add<- geocode(dsh$locnew, output="more")

# Put addresses back into dsf dataframe
#dsh$Address <- str_to_title(paste(add$streetNo, add$street))
#dsh$City <- str_to_title(add$locality)
#dsh$State <- "CA"
#dsh$ZIP.Code <- add$postal_code
#dsh$County.Name <- str_to_title(substring(add$administrative_area_level_2,0,str_length(add$administrative_area_level_2)-7))

# find the missing values for manual arbitration (city, zipcode, county)
#dsh[is.na(dsh$City),]
#dsh$City[dsh$providerid==50078] <- "San Pedro"
#dsh[is.na(dsh$ZIP.Code),]
#dsh$ZIP.Code[dsh$providerid==50280] <- 96001
#dsh[is.na(dsh$County.Name),]
bg <- "#FFFFFF"
main1 <- "#ca0020"
acc1 <- "#f4a582"
main2 <- "#92c5de"
acc2 <- "#0571b0"

diag1 <- DiagrammeR("
  graph LR;
           A(Patient Clinical Factors)-->X(Outcome Quality)
           B(Hospital)-->X(Outcome Quality)
           
           style A fill:#92c5de, stroke-width:0px;
           style B fill:#92c5de, stroke-width:0px;
           style X fill:#F4A582, stroke-width:0px;
           ")

diag2 <- DiagrammeR("
  graph LR;
                    A(Patient Clinical Factors)-->X(Outcome Quality)
                    B(Hospital)-->X(Outcome Quality)
                    C(Doctor)-->X(Outcome Quality)
                    
                    style A fill:#92c5de, stroke-width:0px;
                    style B fill:#92c5de, stroke-width:0px;
                    style C fill:#92c5de, stroke-width:0px;
                    style X fill:F4A582, stroke-width:0px;
                    ")


diag3 <- DiagrammeR("
  graph LR;
          A(Patient Clinical Factors)-->X(Quality)
          B(Patient SES Factors)-->X(Quality)
          C(Access to Care)-->X(Quality);
          D(Hospital)-->X(Quality)
          E(Doctor)-->X(Quality)
           
          style A fill:#92c5de, stroke-width:0px;
          style B fill:#92c5de, stroke-width:0px;
          style C fill:#92c5de, stroke-width:0px;
          style D fill:#92c5de, stroke-width:0px;
          style E fill:#92c5de, stroke-width:0px;
          style X stroke:#0F4A582, stroke-width:1px;
           ")


saveWidget(diag, file="diag_quality1.html")


