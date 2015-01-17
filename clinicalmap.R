# Load libraries
library(XML)
library(plyr)
library(RCurl)
library(utils)
library(rMaps)
library(geomapdata)
library(rgdal)
library(RgoogleMaps)
library(httr)
library(rjson)

# Define search term
keyword      <- "tuberculosis"
start_date   <- "1/1/2009"
stop_date    <- "12/31/2014"
trial_status <- "Closed"


# Construct search term and link
keyword <- as.character(gsub(" ", "+", as.character(keyword)))

download.file(paste(
  "http://clinicaltrials.gov/search?term=", keyword,"&recr=", trial_status ,"&rcv_s=", start_date, "&rcv_e=", stop_date, "&studyxml=true", sep=''), destfile="search_result.zip", mode="wb")

# Unzip & remove zip file
unzip("search_result.zip")
file.remove("search_result.zip")

file.list <- list.files(pattern = ".xml")

location_tab <- NULL


for (i in 1:length(file.list))
{
  adat <- xmlTreeParse(file.list[i], useInternalNodes = TRUE)
  
  location_tab <- rbind(location_tab, 
                        if(ncol(xmlToDataFrame(getNodeSet(adat, "//address//country"))) > 0)
                        {
                          cbind(
                            as.character(xmlToDataFrame(getNodeSet(adat, "//nct_id"))[1,]),
                            if(ncol(xmlToDataFrame(getNodeSet(adat, "//city"))) > 0)
                            {
                              as.character(xmlToDataFrame(getNodeSet(adat, "//city"))[,1])
                            } else {
                              NULL},
                            as.character(xmlToDataFrame(getNodeSet(adat, "//address//country"))[,1]))
                        } else {
                          NULL
                        })
}

location_tab           <- as.data.frame(location_tab)
colnames(location_tab) <- c("trialId", "city", "country")

# Tried this API service looks like it cannot access the other countries locations
# Get longitude and latitude coordinates
# location_tab$address<-paste(location_tab$city,",",location_tab$country)
# data <- paste0("[",paste(paste0("\"",location_tab$address,"\""),collapse=","),"]")
# url  <- "http://www.datasciencetoolkit.org/street2coordinates"
# response <- POST(url,body=data)
# json     <- fromJSON(content(response,type="text"))
# geocode  <- do.call(rbind,sapply(json,function(x) c(long=x$longitude,lat=x$latitude)))


get_geocode         <- geocode(as.character(location_tab[,2]))
location_tab$long   <- as.numeric(get_geocode[,1])
location_tab$lat    <- as.numeric(get_geocode[,2])

# Using rMaps()
map <- Leaflet$new()
map$setView(c(33.320731,-14.033990), zoom = 2)
map$tileLayer(provider = 'Stamen.Toner')
map$set(width = 1600, height =800)
for (i in 1:nrow(location_tab))
{
  if(is.na(location_tab$lat[i]) | is.na(location_tab$lon[i])) {next}
  map$marker(
    c(location_tab$lat[i], location_tab$lon[i]),
    bindPopup = location_tab$trialId[i])
  
}  

map
sink("outfile.txt")
map$show('iframesrc',cdn=TRUE)
sink()
