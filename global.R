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

load("data/rdata/1.Rdata")
series1 = df; # get series names masterdata

load("data/run.masterdata.rdata")
runMasterdata = as.data.table(runMasterdata)


idedata = read.csv("data/20150821_input_explorer_dataset.csv")
ideseries = read.csv("data/20150821_masterdata_series.csv")
ideobjects = read.csv("data/20150821_masterdata_objects.csv")



