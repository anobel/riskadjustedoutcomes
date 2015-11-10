library(reshape2)
library(stringr)
library(dplyr)
library(sp)
library(rgeos)
library(ggplot2)
library(ggthemes)
library(ggmap)
library(rgdal)
library(jsonlite)
library(leaflet)

bg <- "#FCFCFA"
main1 <- "#ca0020"
acc1 <- "#f4a582"
main2 <- "#92c5de"
acc2 <- "#0571b0"

# load data to add to map
load("rao_workingdata/acs.rda")
load("rao_workingdata/rural.rda")

# import 2010 TIGER/Line ZCTA map for CA only
map <- readOGR("rao_originaldata/shp/zcta_2010_simple/zcta2010simple.shp", layer="zcta2010simple")
mapsimpler <- readOGR("rao_originaldata/shp/zcta_2010_simpler/zcta2010simpler.shp", layer="zcta2010simpler")

# drop ZCTA in TIGER/Line map that do not exist in my data (only 6 that were not within CA)
map <- map[map@data$ZCTA5CE10 %in% ru$zcta,]
mapsimpler <- mapsimpler[mapsimpler@data$ZCTA5CE10 %in% ru$zcta,]

map@data <- droplevels(map@data)
mapsimpler@data <- droplevels(mapsimpler@data)

# merge RUCA codes and acs data into spatial object
map <- sp::merge(map, ru, by.x="ZCTA5CE10", by.y="zcta")
map <- sp::merge(map, acs, by.x="ZCTA5CE10", by.y="zcta")

mapsimpler <- sp::merge(mapsimpler, ru, by.x="ZCTA5CE10", by.y="zcta")
mapsimpler <- sp::merge(mapsimpler, acs, by.x="ZCTA5CE10", by.y="zcta")

#########################################################################################
# Map of Zip Code Borders
maps$borderzip <- leaflet(mapsimpler) %>%
  addTiles('http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png') %>%
  # Data Layers
  addPolygons(group="Borders", stroke=T, smoothFactor=.1, weight=0.9, color="grey", opacity=0.7, fillOpacity=0)

setwd("/Users/anobel/Documents/Dropbox/Research/Risk Adjusted Outcomes/RAO_analysis/RAOPresentation/externalfigs")
saveWidget(maps$borderzip, file="diag_borderzipmap.html", selfcontained = FALSE, libdir="dependencies")
setwd("/Users/anobel/Documents/Dropbox/Research/Risk Adjusted Outcomes/RAO_analysis")

#########################################################################################
# SES Factor map
# generate palettes for each variable
qpal.medianincome <- colorQuantile("Blues", map@data$medianincome, n = 6)
qpal.pop <- colorQuantile("Blues", map@data$pop, n=6)
qpal.incomeunder25k <- colorQuantile("Blues", map@data$incomeunder25k, n=6)
qpal.incomeover100k <- colorQuantile("Blues", map@data$incomeover100k, n=6)
qpal.capincome <- colorQuantile("Blues", map@data$capincome, n=6)
qpal.housevalue <- colorQuantile("Blues", map@data$housevalue, n=6)
qpal.edunohs <- colorQuantile("Blues", map@data$edunohs, n=6)
qpal.eduhs <- colorQuantile("Blues", map@data$eduhs, n=6)
qpal.educollege <- colorQuantile("Blues", map@data$educollege, n=6)
qpal.empexec <- colorQuantile("Blues", map@data$empexec, n=6)
qpal.gini <- colorQuantile("Blues", map@data$gini, n=6)
qpal.iceinc <- colorQuantile("RdBu", map@data$iceinc, n=5)
qpal.iceedu <- colorQuantile("RdBu", map@data$iceedu, n=5)
qpal.iceeth <- colorQuantile("RdBu", map@data$iceeth, n=5)


#make pop-up
popup <- data.frame(
  zcta=paste("<b>",map@data$ZCTA5CE10,"</b>"),
  pop = paste("Population:", prettyNum(map@data$pop, big.mark=",")),
  medianincome = paste("Median Income: $",prettyNum(map@data$medianincome, big.mark=","), sep=""),
  incomeunder25k = paste("Households with income < $25k: ",prettyNum(map@data$incomeunder25k, digits=3),"%", sep=""),
  incomeover100k = paste("Households with income > $100k: ",prettyNum(map@data$incomeover100k, digits=3),"%", sep=""),
  capincome = paste("Households with capital income: ", prettyNum(map@data$capincome, digits=3),"%", sep=""),
  housevalue = paste("Median Home Value: $",prettyNum(exp(map@data$housevalue), big.mark =","), sep=""),
  edunohs = paste("No HS: ", prettyNum(map@data$edunohs, digits=3), "%", sep=""),
  eduhs = paste("HS Only: ", prettyNum(map@data$eduhs, digits=3), "%", sep=""),
  educollege = paste("College degree: ", prettyNum(map@data$educollege, digits=3), "%", sep=""),
  empexec = paste("Executive/Management Employment: ",prettyNum(map@data$empexec, digits=3),"%",sep=""),
  gini = paste("Gini:", prettyNum(map@data$gini, digits=3)),
  iceinc = paste("ICE Income: ", prettyNum(map@data$iceinc, digits=3)),
  iceedu = paste("ICE Education: ", prettyNum(map@data$iceedu, digits=3)),
  iceeth = paste("ICE Ethnicity: ", prettyNum(map@data$iceeth, digits=3))
  )
popup <- apply(popup, 1, FUN=paste, collapse="</br>")

maps <- NULL
maps$overall <- leaflet(mapsimpler) %>%
  addTiles('http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png') %>%
  # Data Layers
  addPolygons(group="Borders", stroke=T, smoothFactor=.1, weight=0.1, color="grey", opacity=0.7, fillOpacity=0) %>%
  addPolygons(group="Population", fillOpacity = 0.7, fillColor=qpal.pop(map@data$pop), smoothFactor = .1, stroke=T, weight=.1, color="grey", opacity=0.7, popup=popup) %>%
  addPolygons(group="Income", fillOpacity = 0.7, fillColor=qpal.medianincome(map@data$medianincome), smoothFactor = .1, stroke=T, weight=.1, color="grey", opacity=0.5, popup=popup) %>%
  addPolygons(group="Income < $25k", fillOpacity = 0.7, fillColor=qpal.incomeunder25k(map@data$incomeunder25k), smoothFactor = .1, stroke=T, weight=.1, color="grey", opacity=0.5, popup=popup) %>%
  addPolygons(group="Income > $100k", fillOpacity = 0.7, fillColor=qpal.incomeover100k(map@data$incomeover100k), smoothFactor = .1, stroke=T, weight=.1, color="grey", opacity=0.5, popup=popup) %>%
  addPolygons(group="Capital Income", fillOpacity = 0.7, fillColor=qpal.capincome(map@data$capincome), smoothFactor = .1, stroke=T, weight=.1, color="grey", opacity=0.5, popup=popup) %>%
  addPolygons(group="Median House Value", fillOpacity = 0.7, fillColor=qpal.housevalue(map@data$housevalue), smoothFactor = .1, stroke=T, weight=.1, color="grey", opacity=0.5, popup=popup) %>%
  addPolygons(group="Education: No HS", fillOpacity = 0.7, fillColor=qpal.edunohs(map@data$edunohs), smoothFactor = .1, stroke=T, weight=.1, color="grey", opacity=0.5, popup=popup) %>%
  addPolygons(group="Education: HS Only", fillOpacity = 0.7, fillColor=qpal.eduhs(map@data$eduhs), smoothFactor = .1, stroke=T, weight=.1, color="grey", opacity=0.5, popup=popup) %>%
  addPolygons(group="Education: College Degree", fillOpacity = 0.7, fillColor=qpal.educollege(map@data$educollege), smoothFactor = .1, stroke=T, weight=.1, color="grey", opacity=0.5, popup=popup) %>%
  addPolygons(group="Employment: Execs", fillOpacity = 0.7, fillColor=qpal.empexec(map@data$empexec), smoothFactor = .1, stroke=T, weight=.1, color="grey", opacity=0.5, popup=popup) %>%
  addPolygons(group="Gini", fillOpacity = 0.7, fillColor=qpal.gini(map@data$gini), smoothFactor = .1, stroke=T, weight=.1, color="grey", opacity=0.5, popup=popup) %>%
  addPolygons(group="ICE Income", fillOpacity = 0.7, fillColor=qpal.iceinc(map@data$iceinc), smoothFactor = .1, stroke=T, weight=.1, color="grey", opacity=0.5, popup=popup) %>%
  addPolygons(group="ICE Education", fillOpacity = 0.7, fillColor=qpal.iceedu(map@data$iceedu), smoothFactor = .1, stroke=T, weight=.1, color="grey", opacity=0.5, popup=popup) %>%
  addPolygons(group="ICE Ethnicity", fillOpacity = 0.7, fillColor=qpal.iceeth(map@data$iceeth), smoothFactor = .1, stroke=T, weight=.1, color="grey", opacity=0.5, popup=popup) %>%
  # Layers Control
  addLayersControl(baseGroups=c("Population", "Income", "Income < $25k", "Income > $100k", "Capital Income", "Median House Value", "Education: No HS", "Education: HS Only", "Education: College Degree", "Employment: Execs", "Gini", "ICE Income", "ICE Education", "ICE Ethnicity"), options = layersControlOptions(collapsed=FALSE)) %>%
  hideGroup(c("Population", "Income", "Income < $25k", "Income > $100k", "Capital Income", "Median House Value", "Education: No HS", "Education: HS Only", "Education: College Degree", "Employment: Execs", "Gini", "ICE Income", "ICE Education", "ICE Ethnicity"))

# SAVE the widget for later use
setwd("/Users/anobel/Documents/Dropbox/Research/Risk Adjusted Outcomes/RAO_analysis/RAOPresentation/externalfigs")
saveWidget(maps$overall, file="diag_overallmap.html", selfcontained = FALSE, libdir="dependencies")
setwd("/Users/anobel/Documents/Dropbox/Research/Risk Adjusted Outcomes/RAO_analysis")

#########################################################################################
# map density map of physician density
load("rao_workingdata/md.rda")

# keep only data for 2011
npi11 <- npi %>%
  filter(year==2011)

basemapca <- get_map(location=c(lon=-120, lat=38), maptype="terrain", color="bw", source="google", zoom=6)

maps$allmds <- ggmap(basemapca) + 
  stat_density2d(aes(x = lon, y = lat,  fill = ..level.., alpha = ..level..), size = 0.1, bins = 200, geom = 'polygon', data=npi11) + 
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range=c(.1, .5), guide = FALSE) +
  theme_few() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank()) + 
  theme(plot.background = element_rect(fill=bg), legend.background= element_rect(fill=bg), legend.title=element_blank())

densitymap <- left_join(acs, md) %>%
  select(zcta, pop, starts_with("mean"))

# any zip codes that have no physicians in a category come up as NA, make these structural 0
densitymap[is.na(densitymap)] <- 0

# Calculate MD Density for all categories (Urologist, PMD, other)
densitymap$gudens <- densitymap$meangu/densitymap$pop*100000
densitymap$pmddens <- densitymap$meanpmd/densitymap$pop*100000
densitymap$othermddens <- densitymap$meanothermd/densitymap$pop*100000
densitymap$totalmddens <- densitymap$meantotal/densitymap$pop*100000

densitymap[is.na(densitymap)] <- 0

mapdensity <- sp::merge(map, densitymap, by.x="ZCTA5CE10", by.y="zcta")

qpal.totalmddensity <- colorQuantile("Blues", mapdensity@data$totalmddens[mapdensity@data$totalmddens>0], n=5, na.color = "white")
qpal.gudensity <- colorQuantile("Blues", mapdensity@data$gudens[mapdensity@data$gudens>0], n=5, na.color = "white")
qpal.pmddensity <- colorQuantile("Blues", mapdensity@data$pmddens[mapdensity@data$pmddens>0], n=5, na.color = "white")
qpal.othermddensity <- colorQuantile("Blues", mapdensity@data$othermddens[mapdensity@data$othermddens>0], n=5, na.color = "white")

maps$density <- leaflet(mapsimpler) %>%
  addTiles('http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png') %>%
  # Data Layers
  addPolygons(group="Borders", stroke=T, smoothFactor=.1, weight=0.1, color="grey", opacity=0.7, fillOpacity=0) %>%
  addPolygons(group="Total MD Density", fillOpacity = 0.7, fillColor=qpal.totalmddensity(mapdensity@data$totalmddens), smoothFactor = .1, stroke=T, weight=.1, color="grey", opacity=0.7) %>%
  addPolygons(group="Urologist Density", fillOpacity = 0.7, fillColor=qpal.gudensity(mapdensity@data$gudens), smoothFactor = .1, stroke=T, weight=.1, color="grey", opacity=0.7) %>%
  addPolygons(group="PMD Density", fillOpacity = 0.7, fillColor=qpal.pmddensity(mapdensity@data$pmddens), smoothFactor = .1, stroke=T, weight=.1, color="grey", opacity=0.7) %>%
  addPolygons(group="Other MD Density", fillOpacity = 0.7, fillColor=qpal.othermddensity(mapdensity@data$othermddens), smoothFactor = .1, stroke=T, weight=.1, color="grey", opacity=0.7) %>%
  # Layers Control
  addLayersControl(baseGroups=c("Total MD Density", "Urologist Density", "PMD Density", "Other MD Density"), options = layersControlOptions(collapsed=FALSE)) %>%
  hideGroup(c("Total MD Density", "Urologist Density", "PMD Density", "Other MD Density"))

# SAVE the widget for later use
setwd("/Users/anobel/Documents/Dropbox/Research/Risk Adjusted Outcomes/RAO_analysis/RAOPresentation/externalfigs")
saveWidget(maps$density, file="diag_mddensitymap.html", selfcontained = FALSE, libdir="dependencies")
setwd("/Users/anobel/Documents/Dropbox/Research/Risk Adjusted Outcomes/RAO_analysis/RAOPresentation")


#########################################################################################
## Hospital Cluster Map
load("rao_workingdata/dsh.rda")
dsh2011 <- filter(dsh, year=="2011")

popup <- data.frame(
  title=paste("<b>",str_to_title(dsh2011$name),"</b>"),
  beds = paste("Beds:", dsh2011$beds),
  adc = paste("Average Daily Census:", dsh2011$adc),
  dsh_pct = paste("Medicare DSH:", round(dsh2011$dsh_pct,2), "%"),
  mcr_pct = paste("Medicare Patients:", round(dsh2011$mcr_pct,2), "%")
)
popup <- apply(popup, 1, FUN=paste, collapse="</br>")

maps$hospitalcluster <-  leaflet(dsh2011) %>%
  addTiles('http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png') %>%
  addCircleMarkers(lat=dsh2011$lat, lng=dsh2011$lon, color=main1,clusterOptions = markerClusterOptions(), popup=popup)

setwd("/Users/anobel/Documents/Dropbox/Research/Risk Adjusted Outcomes/RAO_analysis/RAOPresentation/externalfigs")
saveWidget(maps$hospitalcluster, file="diag_hospitalclustermap.html", selfcontained = FALSE, libdir="dependencies")
setwd("/Users/anobel/Documents/Dropbox/Research/Risk Adjusted Outcomes/RAO_analysis")

##############################
save(map, maps, file="rao_workingdata/spatial.rda")
