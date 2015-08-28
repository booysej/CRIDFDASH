 library(jsonlite)
 library(leaflet)  
 library(sp)
 library(rgdal)
 library(data.table)
 library(RJSONIO)  
 library(RSQLite)
 af = readOGR(dsn="www/africa.geojson",layer = 'OGRGeoJSON') 
 thebb = bbox(af)
 geojson <- readLines("www/africa.geojson", warn = FALSE) %>%
   paste(collapse = "\n") %>%
   fromJSON(simplifyVector = FALSE)
 geojson$style = list(weight = 1,color = "#000000",opacity = 1,fillOpacity = 0) 
 gistranslate = list("Angola"="Angola","Botswana"="Botswana","Bur"="Burundi",
                     "Congo"="Congo-Brazzaville","Democratic Republic of Congo"="Congo DRC",
                     "Equatorial Guinea"="Equatorial Guinea",
                     "Gabon"="Gabon","Kenya"="Kenya","Lesotho"="Lesotho",
                     "Malawi"="Malawi","Mozambique"="Mozambique","Namibia"="Namibia",                     
                     "Rwanda"="Rwanda",  #"South Afirca"="South Africa",
                     "South Africa"="South Africa"
                     ,"Swaziland"="Swaziland","Tanzania"="Tanzania",
                     "Uganda"="Uganda","Zambia"="Zambia","Zimbabwe"="Zimbabwe")  

dt = readOGR(dsn="www/thedata.geojson",layer = 'OGRGeoJSON') 
dtfrom = dt@data[,c("CODE","COUNTRY")]
names(dtfrom) = c("source","producing.country")
dtto = dt@data[,c("CODE","COUNTRY")]
names(dtto) = c("target","consuming.country")  

load("data/sample.Rdata")
series1 = df; # get series names masterdata
series1[series1$unit=="MWyr",]$value = series1[series1$unit=="MWyr",]$value * 8.76581277 
series1[series1$unit=="MWyr",]$unit = "GWh"

#load("data/run.masterdata.rdata")
load("data/run.masterdata_11_12.rdata") 
#runMasterdata = as.data.table(runMasterdata)

nodes <- subset(ref_objects, show=='yes')

uniqueAndSorted <- function(col){
  u <- unique(as.character(col))      
  sort(u) 
}

fix.status <- function(col){
  col <- as.character(col)
  col[col=="N/A"] <- "Active"
  col[col=="UC"] <-  "Active"
  col
}

countries <- uniqueAndSorted(nodes$country.name)
level <- uniqueAndSorted(nodes$level)
nodes$status <- fix.status(nodes$status)

#idedata = read.csv("data/20150821_input_explorer_dataset.csv")
#ideseries = read.csv("data/20150821_masterdata_series.csv")
#ideobjects = read.csv("data/20150821_masterdata_objects.csv")