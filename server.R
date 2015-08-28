#install.packages("../src/DiagrammeR",repos=NULL,type="source")
#library(DiagrammeR) #devtools::install_github("booysej/DiagrammeR")
library(shiny)
#library(shinyAce)   #devtools::install_github("shinyAce", "trestletech")
library(shinyBS)
library(R.cache)
library(RSQLite)
# install.packages("/home/jacques/shiny-server/apps/src/leaflet",repos=NULL,type="source")
# install.packages("/home/jacques/shiny-server/apps/src/rpivotTable",repos=NULL,type="source")
# install.packages("/home/jacques/shiny-server/apps/src/shinyTree",repos=NULL,type="source")
library(leaflet)    #devtools::install_github("booysej/leaflet")
library(shinyTree)  #devtools::install_github("booysej/shinyTree")
library(jsonlite)
#library(DT);
library(rCharts)
library(R.cache)
library(memoise)
# devtools::install_github(c("ramnathv/htmlwidgets", "smartinsightsfromdata/rpivotTable"))
library(rpivotTable);
#library(diagram)
#fileData <- reactiveFileReader(1000, session, 'data.csv', read.csv)

shinyServer(function(input, output, session) {
  #addResourcePath('tiles', system.file('legacy/www/tiles', package='leaflet'))    
  #addResourcePath('tiles2', system.file('legacy/www/tiles2', package='leaflet'))
  values <- reactiveValues(startyear=2011,
                           endyear=2040,
                           map1suspended=TRUE,
                           map2suspended=FALSE,
                           selectavail="NONE",
                           country="All",                                                      
                           geojson=geojson, 
                           selectedfeat=NULL,                           
                           abehave="showonclick",
                           lockedbasepolicy="NONE",
                           lockedscenpolicy="NONE",
                           #lockedbasepolicy="Assume High Water Availability",
                           #lockedscenpolicy="Assume Low Water Availability",
                           createdpolicy=list(list(name="NONE",thewater=0,thecoaluclf=0,
                                                   thetxuclf=0,varyload=TRUE,load=0,withoutinga=FALSE),
                                              list(name="Assume Low Water Availability",
                                                   thewater=60,thecoaluclf=80,thetxuclf=50,varyload=TRUE,load=100,withoutinga=FALSE),
                                              list(name="Assume High Water Availability",
                                                   thewater=140,thecoaluclf=100,thetxuclf=100,varyload=TRUE,load=100,withoutinga=TRUE)
                                              ),
                           availablepolicy=list(list(name="NONE",thewater=0,thecoaluclf=0,thetxuclf=0,varyload=TRUE,load=0,withoutinga=FALSE))
                           #availablepolicy=as.character(unique(txoutput$policy))[!grepl("unconstraint",
                          #                                                              as.character(unique(txoutput$policy)))]
  )  
  ######## DASH Boards #############
  # Initial Map Step 1 setup
  output$d1m1 <- renderLeaflet({
    isolate({
      abe = "showall";
      thewater = input$d1water    
      theuclf = input$d1uclf
      theuclf2 = input$d1uclf2
      thecountry = values$country
      theyear = input$d1year    
      exclGI = input$withoutGrandInga
      varyload=TRUE
      load = input$d1cons
      if (!is.null(thewater) && !is.null(theuclf) && !is.null(theuclf2) && !is.null(thecountry) && !is.null(theyear)  ) {
        showarrowsunconstraint(thewater,theuclf,theuclf2,thecountry, theyear,"TransmissionOutput",FALSE,"d1m1",exclGI,varyload,load);
      }
    });
    isolate({
      values$map1suspended = FALSE;
    });
    
    leaflet(height=600) %>%
#         showArrows(mapname="d1m1",
#                 jsonfile="thedata.geojson",
#                 linkfile="linksd1m1.csv",    
#                 behaviour=abe,    
#                 showid="") %>%        
      addGeoJSON(geojson,layerId="main") %>%
      addTiles(options=tileOptions(minZoom = 4, maxZoom = 6),attribution="Enerweb EOH")   %>% 
      setView(22.8731,-22.9992,5) %>% hideArrows(mapname="d1m1",behaviour="hideall");
  })  
  
  # Initial Map Step 3 setup
  output$d3m1 <- renderLeaflet({
          isolate({
            values$map2suspended = FALSE;
          });
          leaflet(height=600) %>%
            addGeoJSON(geojson,layerId="main") %>%
            addTiles(options=tileOptions(minZoom = 4, maxZoom = 6),attribution="Enerweb EOH")   %>% 
            setView(22.8731,-22.9992,5) %>% hideArrows(mapname="d3m1",behaviour="hideall");
        })  
  
  # click on sea map ( show all)
  observe({
    if(!is.null(input$d1m1_click)) {   
      isolate({
      values$geojson$features <- isolate(lapply(values$geojson$features, function(feat) {              
        feat$properties$style <- list(fillOpacity = 0,color="green")
        values$selectedfeat = NULL;
        feat
      }))    
      proxy = leafletProxy("d1m1") # %>% removeGeoJSON(geojson[[2]]$properties$name)             
      proxy %>% addGeoJSON(isolate(values$geojson),layerId="main") 
      values$country <- 'All';
      });
    }
  },priority=1000)
  # click on country s1
  observe({
    theclick = NULL;    
    if(!is.null(input$d1m1_geojson_click)) {     
      theclick = input$d1m1_geojson_click      
    }    
        
    isolate({    
    if(!is.null(theclick)) {                 
      tcountry <- names(gistranslate[gistranslate==theclick$properties$COUNTRY])              
      if(!is.null(tcountry)) {   
        isolate({
          if(nchar(tcountry[1])>0) {
            gist = as.character(unlist(gistranslate[gsub(" $","",strsplit(tcountry,"\\(")[[1]][1])]))        
            if( (length(gist)>0) && (nchar(gist)>0) ) {    
              
              values$geojson$features <- isolate(lapply(values$geojson$features, function(feat) {              
                if (feat$properties$COUNTRY==gist) {
                  #print(gist)
                  feat$properties$style <- list(fillOpacity = 0.4,color="green")   
                  values$selectedfeat = feat;
                } else {
                  feat$properties$style <- list(fillOpacity = 0,color="green")
                }       
                feat
              }))              
            }
          }
        });      
      }
      proxy = leafletProxy("d1m1") # %>% removeGeoJSON(geojson[[2]]$properties$name)             
      proxy %>% addGeoJSON(isolate(values$geojson),layerId="main")  # %>% addTiles( "tiles2/{z}/{x}/{y}.png")                   
                  
      values$country <- tcountry
    }
    });
    
  },priority=1000)  
  # click on sea map ( show all)
  observe({
    if(!is.null(input$d3m1_click)) { 
      isolate({
      values$geojson$features <- isolate(lapply(values$geojson$features, function(feat) {              
        feat$properties$style <- list(fillOpacity = 0,color="green")
        values$selectedfeat = NULL;
        feat
      }))    
      proxy = leafletProxy("d3m1") # %>% removeGeoJSON(geojson[[2]]$properties$name)             
      proxy %>% addGeoJSON(isolate(values$geojson),layerId="main") 
      values$country <- 'All';
      });
    }
  },priority=1000)
  # click on country s3
  observe({
    theclick = NULL;        
    if(!is.null(input$d3m1_geojson_click)) {     
      theclick = input$d3m1_geojson_click      
    }
    
    isolate({
    if(!is.null(theclick)) {                 
      tcountry <- names(gistranslate[gistranslate==theclick$properties$COUNTRY])              
      if(!is.null(tcountry)) {   
        isolate({
          if(nchar(tcountry[1])>0) {
            gist = as.character(unlist(gistranslate[gsub(" $","",strsplit(tcountry,"\\(")[[1]][1])]))        
            if( (length(gist)>0) && (nchar(gist)>0) ) {    
              
              values$geojson$features <- isolate(lapply(values$geojson$features, function(feat) {              
                if (feat$properties$COUNTRY==gist) {
                  #print(gist)
                  feat$properties$style <- list(fillOpacity = 0.4,color="green")   
                  values$selectedfeat = feat;
                } else {
                  feat$properties$style <- list(fillOpacity = 0,color="green")
                }       
                feat
              }))              
            }
          }
        });      
      }      
      proxy = leafletProxy("d3m1") # %>% removeGeoJSON(geojson[[2]]$properties$name)             
      proxy %>% addGeoJSON(isolate(values$geojson),layerId="main")  # %>% addTiles( "tiles2/{z}/{x}/{y}.png")                   
      
      values$country <- tcountry
    }
    });
  },priority=1000)  

  showarrowsunconstraint <-function(thewater,thecoaluclf,thetxuclf,thecountry="All", theyear=2015,theseries="TransmissionOutput",
                        proxy=TRUE, mapname="", exclGI=FALSE,adjcons=FALSE,cons=100) {
    
    
    td = getunconstraint(thewater/100, thecoaluclf/100,thetxuclf/100, exclGI,adjcons,cons/100)
    if(length(td[,1])==0) {return(NULL);}
    tfinal = subset(td, series == theseries)  
    units = as.character(tfinal$unit[1])
    if (thecountry!="All") {
      #tfinal = subset(tfinal, country.name == thecountry)          
    }
    
    tfinal = merge(tfinal,dtfrom,by="producing.country")
    tfinal= merge(tfinal,dtto,by="consuming.country") 
    
    txoutputfinal = subset(tfinal, time == theyear)          
    t = txoutputfinal[,c("source","target","value"),with=FALSE]
    t$text=paste("%flow ",units,sep="")
    setnames(t,names(t),c("source","target","flow","text"))
    t$flow = round(as.numeric(as.character(t$flow)))
    t = t[t$flow>0,]      
    t = t[, lapply(.SD, sum), by = c("source","target","text")]
    write.csv(t,file=paste("www/links",mapname,".csv",sep=""),row.names = FALSE)
    abe = "showall";
    if(proxy) {
      proxy2 = leafletProxy(mapname);     
      proxy2  %>% showArrows(mapname=mapname,
                             jsonfile="thedata.geojson",
                             linkfile=paste("links",mapname,".csv",sep=""),    
                             behaviour=abe,    
                             showid="")  %>% hideArrows(mapname=mapname,behaviour="hideall");
    }
  }  
  
  showarrowsconstraint <-function(designwater,designcoaluclf,designtxuclf,designexclGI,designcons,thewater,thecoaluclf,thetxuclf,exclGI,cons,
                                  thecountry="All", 
                                  theyear=2015,theseries="TransmissionOutput",
                                  proxy=TRUE, mapname="") {
     
    td = getconstraint(designwater/100,designcoaluclf/100,designtxuclf/100,designexclGI,designcons/100, 
                              thewater/100, thecoaluclf/100,thetxuclf/100,exclGI,cons/100)
    
    if(length(td)==0) {
        createAlert(session, "globalalert", "ga", title = "",
                    content = "No data found for selected combination", append = FALSE)
      return(NULL);
    } else {closeAlert(session, "ga")}
    
    
    tfinal = subset(td, series == theseries)  
    units = as.character(tfinal$unit[1])
    if (thecountry!="All") {
      #tfinal = subset(tfinal, country.name == thecountry)          
    }
    
    tfinal = merge(tfinal,dtfrom,by="producing.country")
    tfinal= merge(tfinal,dtto,by="consuming.country") 
    
    txoutputfinal = subset(tfinal, time == theyear)          
    t = txoutputfinal[,c("source","target","value"),with=FALSE]
    t$text=paste("%flow ",units,sep="")
    setnames(t,names(t),c("source","target","flow","text"))
    t$flow = round(as.numeric(as.character(t$flow)))
    t = t[t$flow>0,]      
    t = t[, lapply(.SD, sum), by = c("source","target","text")]
    write.csv(t,file=paste("www/links",mapname,".csv",sep=""),row.names = FALSE)
    abe = "showall";
    if(proxy) {
      proxy2 = leafletProxy(mapname);     
      proxy2  %>% showArrows(mapname=mapname,
                             jsonfile="thedata.geojson",
                             linkfile=paste("links",mapname,".csv",sep=""),    
                             behaviour=abe,    
                             showid="")  %>% hideArrows(mapname=mapname,behaviour="hideall");
    }
  }  
  
  # Update Arrows Step1
  observe({
    observe({
    thewater = input$d1water    
    theuclf = input$d1uclf
    theuclf2 = input$d1uclf2
    thecountry = values$country
    theyear = input$d1year
    exclGI = input$withoutGrandInga
    load = input$d1cons
    
    varyload = TRUE
    
    if (!is.null(thewater) & !is.null(theuclf) & !is.null(theuclf2) & !is.null(thecountry) & !is.null(theyear)  ) {
      showarrowsunconstraint(thewater,theuclf,theuclf2,thecountry, theyear,"TransmissionOutput",TRUE,"d1m1",exclGI,varyload,load);
    }
    
    
  },suspended=values$map1suspended)
  });  
    
  # Update Arrows Step3
  observe({
    observe({    
    if( !is.null(values$lockedbasepolicy) && !is.null(values$lockedscenpolicy)) {    
      if( (values$lockedbasepolicy!='NONE') && (values$lockedscenpolicy!='NONE')) {    
        
        thewater = input$d3water    
        theuclf = input$d3uclf
        theuclf2 = input$d3uclf2
        thecountry = values$country
        theyear = input$d3year
        fixedyear = 2020 #input$d3fixedyear
        exclGI = input$d3withoutGrandInga
        cons = input$d3cons
        flowtype = input$d3txflowtype
        
        
        isolate({
          if(is.null(flowtype)) {
            flowtype = "baseline";
          }
          if(is.null(fixedyear)) {
            fixedyear = 2020
          }
          
          abe = "showall";
          if (!is.null(thewater) && !is.null(theuclf) && !is.null(thecountry) && !is.null(theyear) && !is.null(flowtype) ) {
            thepolicies = do.call(c,lapply(values$availablepolicy,function(x) {x$name}))
            if(length(thepolicies)>1) {
              
              if(flowtype=="baseline") {
                r = values$availablepolicy[thepolicies==isolate(values$lockedbasepolicy)][[1]]
              } else if(flowtype=="scenario") {
                r = values$availablepolicy[thepolicies==isolate(values$lockedscenpolicy)][[1]]
              }
              designwater = unlist(r[2])
              designcoaluclf = unlist(r[3])
              designtxuclf = unlist(r[4])
              designexclGI = unlist(r[7])
              varyload=TRUE
              designcons = unlist(r[6])
              
             # print("HERE")
              
              showarrowsconstraint(designwater,designcoaluclf,designtxuclf,
                                   designexclGI,designcons,
                                   thewater,
                                   theuclf,
                                   theuclf2,exclGI,cons,
                                              thecountry, 
                                              theyear,"TransmissionOutput",
                                              TRUE, "d3m1")
              
              #showarrowsconstraint(designwater,designcoaluclf,designtxuclf,fixedyear,
              #                     thewater,theuclf,100,thecountry, theyear,"TransmissionOutput",TRUE,"d3m1",exclGI,varyload,load,100);
            }
          } 
          
        });

        
      }
    }    
  },suspended=values$map2suspended)
  });
  
  observe({
    values$startyear <- as.numeric(strsplit(as.character(input$daterange[1]),"-")[[1]][1])
    values$endyear <- as.numeric(strsplit(as.character(input$daterange[2]),"-")[[1]][1])
  });
  
  #output$queryText <- renderText({
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query$demo)) {
      if(query$demo==1) {
        updateTabsetPanel(session,"nav","STEP 1")
        values$country="Namibia"
        updateSliderInput(session, "daterange",value=c(2011,2050))
        
        updateCollapse(session,id="story",open="DEMO1: New Capacity for 2 Water Availability Scenarios")
        updateSliderInput(session, "d1water", value = 100)
        updateSliderInput(session, "d1uclf", value = 100)
        updateSliderInput(session, "d1uclf2", value = 100)
        updateCheckboxInput(session, "withoutGrandInga", value = FALSE)
        updateSliderInput(session, "d1cons", value = 100)
        
        return(query$demo)
      }
      if(query$demo==2) {
        updateTabsetPanel(session,"nav","STEP 1")
        values$country="All"
        updateCollapse(session,id="story",open="DEMO2: Difference in Average Price per Country for 2 Consumption Scenarios")
        updateSliderInput(session, "d1water", value = 100)
        updateSliderInput(session, "d1uclf", value = 100)
        updateSliderInput(session, "d1uclf2", value = 100)
        updateCheckboxInput(session, "withoutGrandInga", value = FALSE)
        updateSliderInput(session, "d1cons", value = 100)
        
        return(query$demo)
      }
      if(query$demo==3) {
        return(query$demo)
      }
      if(query$demo==4) {
        return(query$demo)
      }
      if(query$demo==5) {
        return(query$demo)
      }
    } else {
      updateTabsetPanel(session,"nav","STEP 1")
      updateCollapse(session,id="story",open="EVALUATE: Flows, Map View (Unconstrained) - click on country to filter")
    }
  })
  
  demo1 <- function(thewater,thecoaluclf,thetxuclf,thecountry, thedom="",stext="",thelevel="All",startyear=2011,endyear=2040,
                    exclGI=FALSE,adjcons=FALSE,cons=0) {
    
    
    td = getunconstraint(thewater/100, thecoaluclf/100,thetxuclf/100, exclGI,adjcons,cons/100)
    if(length(td[,1])==0) {return(NULL);}
    
    seriesname = "New Capacity"
    tfinal = subset(td, series == seriesname)  
    units = as.character(tfinal$unit[1])
    
    if (thecountry!="All") {
      tfinal = subset(tfinal, country.name == thecountry)          
    }
    if (thelevel!="All") {
      tfinal = subset(tfinal, level == thelevel)          
    }
    tfinal = subset(tfinal, time %in% (seq(startyear,endyear,1)))          
    
    
    #x = unique(tfinal$time)
    #x = x[order(x)]
    
    if(nrow(tfinal)>0) {
      
      tfinal2 = tfinal[, c("time","value","energy.source"),with=F]
      tfinal3 = tfinal2[, lapply(.SD, sum), by = c("time","energy.source")]     
      tfinal3 = tfinal3[(tfinal3$time>2010) & (tfinal3$time<2050) ,]
      
      tdat = as.data.frame(t(reshape(tfinal3,idvar=c("energy.source"),direction="wide")),stringsAsFactors=F)
      colnames(tdat) = as.character(unlist(tdat[1,]))
      tdat = tdat[-1,]
      rownames(tdat) = gsub("value\\.","",rownames(tdat))      
      
      x = as.data.frame(apply(tdat,2,as.numeric),stringsAsFactors=F)
      rownames(x) = rownames(tdat)
    }
    
    h1 <- rCharts:::Highcharts$new()
    h1$chart(type = "area",marginLeft=50,height=300)
    h1$title(text = paste("New Capacity (",thecountry,")",sep=""))
    h1$subtitle(text = paste(stext,sep=""))
    
    if(nrow(tfinal)>0) {
      h1$xAxis(categories = paste("",rownames(x),sep="") )
      h1$yAxis(title = list(text = units),stackLabels= list(enabled=T))
      h1$data(x)      
      # Print chart
    }
    
    
    h1$legend(symbolWidth = 10)
    h1$set(dom = thedom)
    h1$plotOptions(animation=FALSE,
                   area=list(
                     stacking= 'normal',
                     animation=FALSE,
                     events=list(
                       legendItemClick = paste("#! function() {
                          console.log(this);
                          Shiny.onInputChange(\'",thedom,"LegendItemClick\', {
                              name: this.name,
                              visible: this.visible      
                          })
                         } !#",sep="")
                       #legendItemClick = "#! function() {alert(this.name);  } !#"
                     )
                   )
    )
    h1$exporting(enabled = T)    
    
    return(h1)       
  }  
  
  demo2 <- function(thewater,thecoaluclf,thetxuclf,thecountry, thedom="",stext="",thelevel="All",startyear=2011,endyear=2040,
                              exclGI=FALSE,adjcons=FALSE,cons=0) {
    
    td = getunconstraint(thewater/100, thecoaluclf/100,thetxuclf/100, exclGI,adjcons,100/100)
    td1 = getunconstraint(thewater/100, thecoaluclf/100,thetxuclf/100, exclGI,adjcons,80/100)
    td2 = getunconstraint(thewater/100, thecoaluclf/100,thetxuclf/100, exclGI,adjcons,120/100)
    if(length(td[,1])==0) {return(NULL);}
    if(length(td1[,1])==0) {return(NULL);}
    if(length(td2[,1])==0) {return(NULL);}
    
    seriesname = "Avg Price"
    tfinal = subset(td, series == seriesname)
    tfinal1 = subset(td1, series == seriesname)  
    tfinal2 = subset(td2, series == seriesname)  
    units = "Difference in Average Price"
    if (thecountry!="All") {
      tfinal = subset(tfinal, country.name == thecountry)          
      tfinal1 = subset(tfinal1, country.name == thecountry)
      tfinal2 = subset(tfinal2, country.name == thecountry)
    }
    if (thelevel!="All") {
      tfinal = subset(tfinal, level == thelevel)
      tfinal1 = subset(tfinal1, level == thelevel)          
      tfinal2 = subset(tfinal2, level == thelevel)          
    }
    tfinal = subset(tfinal, time %in% (seq(startyear,endyear,1)))          
    tfinal1 = subset(tfinal1, time %in% (seq(startyear,endyear,1)))          
    tfinal2 = subset(tfinal2, time %in% (seq(startyear,endyear,1)))          
    
    if(nrow(tfinal)>0) {
      tfinala = tfinal[, c("time","value","country.name"),with=F]
      tfinalb = tfinala[, lapply(.SD, mean), by = c("country.name")]     # Mean of AVG Price
      tfinalb = tfinalb[(tfinalb$time>2010) & (tfinalb$time<2050) ,c("country.name","value"),with=F]
    }
    if(nrow(tfinal1)>0) {
      tfinal1a = tfinal1[, c("time","value","country.name"),with=F]
      tfinal1b = tfinal1a[, lapply(.SD, mean), by = c("country.name")]     # Mean of AVG Price
      tfinal1b = tfinal1b[(tfinal1b$time>2010) & (tfinal1b$time<2050) ,c("country.name","value"),with=F]
    }
    if(nrow(tfinal2)>0) {
      tfinal2a = tfinal2[, c("time","value","country.name"),with=F]
      tfinal2b = tfinal2a[, lapply(.SD, mean), by = c("country.name")]     # Mean of AVG Price
      tfinal2b = tfinal2b[(tfinal2b$time>2010) & (tfinal2b$time<2050) ,c("country.name","value"),with=F]
    }
    
    # tfinal1b$value - tfinalb$value # 20% less consumption
    # tfinal2b$value - tfinalb$value # 20% more consumption
    
    
    h1 <- rCharts:::Highcharts$new()
    h1$chart(type = "column",marginLeft=50,height=500)
    h1$title(text = paste("Difference in Average Price (",thecountry,")",sep=""))
    h1$subtitle(text = paste(stext,sep=""))
    
    if(nrow(tfinal)>0) {
      h1$xAxis(categories = as.character(tfinalb$country.name) )
      h1$yAxis(title = list(text = units))
      h1$series(list( list(name="20% less consumption",data=(tfinal1b$value - tfinalb$value)),
                    list(name="20% more consumption",data=(tfinal2b$value - tfinalb$value))
                    ))      
      # Print chart
    }
    
    
    h1$legend(symbolWidth = 10)
    h1$set(dom = thedom)
    h1$plotOptions(animation=FALSE,
                   column=list(
                     animation=FALSE,
                     events=list(
                       legendItemClick = paste("#! function() {
                          console.log(this);
                          Shiny.onInputChange(\'",thedom,"LegendItemClick\', {
                              name: this.name,
                              visible: this.visible      
                          })
                         } !#",sep="")
                       #legendItemClick = "#! function() {alert(this.name);  } !#"
                     )
                   )
    )
    h1$exporting(enabled = T)    
    
    return(h1)       
  }  
  
  
  
  barunconstraint <- function(thewater,thecoaluclf,thetxuclf,thecountry, thedom="",stext="",thelevel="All",startyear=2011,endyear=2040,
                              exclGI=FALSE,adjcons=FALSE,cons=0) {
    
    
    td = getunconstraint(thewater/100, thecoaluclf/100,thetxuclf/100, exclGI,adjcons,cons/100)
    if(length(td[,1])==0) {return(NULL);}
    
    seriesname = "New Capacity"
    tfinal = subset(td, series == seriesname)  
    units = as.character(tfinal$unit[1])
    
    if (thecountry!="All") {
      tfinal = subset(tfinal, country.name == thecountry)          
    }
    if (thelevel!="All") {
      tfinal = subset(tfinal, level == thelevel)          
    }
    tfinal = subset(tfinal, time %in% (seq(startyear,endyear,1)))          
    
    
    #x = unique(tfinal$time)
    #x = x[order(x)]
    
    if(nrow(tfinal)>0) {
      
      tfinal2 = tfinal[, c("time","value","energy.source"),with=F]
      tfinal3 = tfinal2[, lapply(.SD, sum), by = c("time","energy.source")]     
      tfinal3 = tfinal3[(tfinal3$time>2010) & (tfinal3$time<2050) ,]
      
      tdat = as.data.frame(t(reshape(tfinal3,idvar=c("energy.source"),direction="wide")),stringsAsFactors=F)
      colnames(tdat) = as.character(unlist(tdat[1,]))
      tdat = tdat[-1,]
      rownames(tdat) = gsub("value\\.","",rownames(tdat))      
      
      x = as.data.frame(apply(tdat,2,as.numeric),stringsAsFactors=F)
      rownames(x) = rownames(tdat)
    }
    
    h1 <- rCharts:::Highcharts$new()
    h1$chart(type = "column",marginLeft=50,height=300)
    h1$title(text = paste("New Capacity (",thecountry,")",sep=""))
    h1$subtitle(text = paste(stext,sep=""))
    
    if(nrow(tfinal)>0) {
      h1$xAxis(categories = paste("",rownames(x),sep="") )
      h1$yAxis(title = list(text = units),stackLabels= list(enabled=T))
      h1$data(x)      
      # Print chart
    }
    
    
    h1$legend(symbolWidth = 10)
    h1$set(dom = thedom)
    h1$plotOptions(animation=FALSE,
                   column=list(
                      stacking= 'normal',
                      animation=FALSE,
                      events=list(
                        legendItemClick = paste("#! function() {
                          console.log(this);
                          Shiny.onInputChange(\'",thedom,"LegendItemClick\', {
                              name: this.name,
                              visible: this.visible      
                          })
                         } !#",sep="")
                        #legendItemClick = "#! function() {alert(this.name);  } !#"
                       )
                   )
    )
    h1$exporting(enabled = T)    
    
    return(h1)       
  }  
  
  barconstraint <- function(designwater,designcoaluclf,designtxuclf,fixyear,thewater,thecoaluclf,thetxuclf,thecountry, 
                            thedom="",stext="",thelevel="All",startyear=2011,endyear=2040) {
    
    td = getconstraint(designwater,designcoaluclf,designtxuclf,fixyear,thewater/100, thecoaluclf/100,thetxuclf/100)
    if(length(td[,1])==0) {return(NULL);}
    
    seriesname = "New Capacity"
    tfinal = subset(td, series == seriesname)  
    units = as.character(tfinal$unit[1])
    
    if (thecountry!="All") {
      tfinal = subset(tfinal, country.name == thecountry)          
    }
    if (thelevel!="All") {
      tfinal = subset(tfinal, level == thelevel)          
    }
    tfinal = subset(tfinal, time %in% (seq(startyear,endyear,1)))          
    
    
    #x = unique(tfinal$time)
    #x = x[order(x)]
    
    if(nrow(tfinal)>0) {
      
      tfinal2 = tfinal[, c("time","value","energy.source"),with=F]
      tfinal3 = tfinal2[, lapply(.SD, sum), by = c("time","energy.source")]     
      tfinal3 = tfinal3[(tfinal3$time>2010) & (tfinal3$time<2050) ,]
      
      tdat = as.data.frame(t(reshape(tfinal3,idvar=c("energy.source"),direction="wide")),stringsAsFactors=F)
      colnames(tdat) = as.character(unlist(tdat[1,]))
      tdat = tdat[-1,]
      rownames(tdat) = gsub("value\\.","",rownames(tdat))      
      
      x = as.data.frame(apply(tdat,2,as.numeric),stringsAsFactors=F)
      rownames(x) = rownames(tdat)
    }
    
    h1 <- rCharts:::Highcharts$new()
    h1$chart(type = "column",marginLeft=50,height=300)
    h1$title(text = paste("New Capacity (",thecountry,")",sep=""))
    h1$subtitle(text = paste(stext,sep=""))
    
    if(nrow(tfinal)>0) {
      h1$xAxis(categories = paste("",rownames(x),sep="") )
      h1$yAxis(title = list(text = units),stackLabels= list(enabled=T))
      h1$data(x)      
      # Print chart
    }
    
    
    h1$legend(symbolWidth = 10)
    h1$set(dom = thedom)
    h1$plotOptions(animation=FALSE,column=list(stacking= 'normal',animation=FALSE))
    h1$exporting(enabled = T)    
    
    return(h1)       
  }  
  
  # used for unconstraint
  makeMs <- function(crit){
    l <- lapply(crit$run_id, function(run_id){
      load(sprintf("data/rdata/%s.Rdata", run_id))
      #if(!is.null(timeseries)) { df = df[df$series %in% timeseries,] }
      df
    })
    ms <- as.data.table(do.call(rbind,l))
    ms[ms$unit=="MWyr",]$value = ms[ms$unit=="MWyr",]$value * 8.76581277 
    ms[ms$unit=="MWyr",]$unit = "GWh"
    return(ms)
  }
  makeMs2 <- memoise(makeMs)
  
  # used for constrant
  makeMs3 <- function(crit){
    l <- lapply(crit$run_id, function(run_id){
      load(sprintf("data/rdata/%s.Rdata", run_id))
      df
    })
    ms <- as.data.table(do.call(rbind,l))
    ms[ms$unit=="MWyr",]$value = ms[ms$unit=="MWyr",]$value * 8.76581277 
    ms[ms$unit=="MWyr",]$unit = "GWh"
    return(ms)
  }
  makeMs4 <- function(crit) { return(memoizedCall(makeMs3,crit)) }
  
  
  getunconstraint <- function(water,coaluclf,txuclf,
                              exclGI=FALSE,adjcons=FALSE,cons=1) {
  
    crit <- subset(runMasterdata, policy_id==11 & 
                      water.availability==water &
                      coal.uclf==coaluclf & 
                      transmission.uclf==txuclf &
                      grand.inga.out==as.integer(exclGI) &
                      consumption.adjustment==cons
                     )
    
    ms <- makeMs2(crit)
    ms <- subset(ms,show=='yes')
    
    return(ms)
  }
  
  getconstraint <- function(designwater,designcoaluclf,designtxuclf,designexclGI,designcons, 
                            water, coaluclf,txuclf,exclGI,cons,
                            fixwater=TRUE,fixcoaluclf=TRUE,fixtxuclf=TRUE,fixexclGI=TRUE,fixcons=TRUE) {
    
    if(!fixwater) {
      crit <- subset(runMasterdata, policy_id==12 & 
                       coal.uclf==coaluclf & 
                       transmission.uclf==txuclf & 
                       consumption.adjustment==cons &
                       grand.inga.out==as.integer(exclGI) & 
                       design.water.availability == designwater &
                       design.coal.uclf == designcoaluclf & 
                       design.consumption.adjustment == designcons & 
                       design.grand.inga.out==as.integer(designexclGI) & 
                       design.transmission.uclf == designtxuclf
      )
    } else if(!fixcoaluclf) {
      crit <- subset(runMasterdata, policy_id==12 & 
                       water.availability==water &
                       transmission.uclf==txuclf & 
                       consumption.adjustment==cons &
                       grand.inga.out==as.integer(exclGI) & 
                       design.water.availability == designwater &
                       design.coal.uclf == designcoaluclf & 
                       design.consumption.adjustment == designcons & 
                       design.grand.inga.out==as.integer(designexclGI) & 
                       design.transmission.uclf == designtxuclf
      )
    } else if(!fixtxuclf) {
      crit <- subset(runMasterdata, policy_id==12 & 
                       water.availability==water &
                       coal.uclf==coaluclf & 
                       consumption.adjustment==cons &
                       grand.inga.out==as.integer(exclGI) & 
                       design.water.availability == designwater &
                       design.coal.uclf == designcoaluclf & 
                       design.consumption.adjustment == designcons & 
                       design.grand.inga.out==as.integer(designexclGI) & 
                       design.transmission.uclf == designtxuclf
      )
    } else if(!fixcons) {
      crit <- subset(runMasterdata, policy_id==12 & 
                       water.availability==water &
                       coal.uclf==coaluclf & 
                       transmission.uclf==txuclf & 
                       grand.inga.out==as.integer(exclGI) & 
                       design.water.availability == designwater &
                       design.coal.uclf == designcoaluclf & 
                       design.consumption.adjustment == designcons & 
                       design.grand.inga.out==as.integer(designexclGI) & 
                       design.transmission.uclf == designtxuclf
      )
    } else if(!fixexclGI) {      
      crit <- subset(runMasterdata, policy_id==12 & 
                       water.availability==water &
                       coal.uclf==coaluclf & 
                       transmission.uclf==txuclf & 
                       consumption.adjustment==cons &
                       design.water.availability == designwater &
                       design.coal.uclf == designcoaluclf & 
                       design.consumption.adjustment == designcons & 
                       design.grand.inga.out==as.integer(designexclGI) & 
                       design.transmission.uclf == designtxuclf
      )
    } else {
      crit <- subset(runMasterdata, policy_id==12 & 
                       water.availability==water &
                       coal.uclf==coaluclf & 
                       transmission.uclf==txuclf & 
                       consumption.adjustment==cons &
                       grand.inga.out==as.integer(exclGI) & 
                       design.water.availability == designwater &
                       design.coal.uclf == designcoaluclf & 
                       design.consumption.adjustment == designcons & 
                       design.grand.inga.out==as.integer(designexclGI) & 
                       design.transmission.uclf == designtxuclf
      )
    }
      
    ms <- makeMs4(crit) 
    ms <- subset(ms,show=='yes')
    return(ms)
  }
  
  
  observe({
    print(input$d1t1LegendItemClick$name);
    
  })
  
  # EVALUATE: New Capacity (Unconstrained)
  output$d1t1 <- renderChart({      
    thewater = input$d1water    
    theuclf = input$d1uclf
    theuclf2 = input$d1uclf2
    thecountry = values$country
    thepolicy = "unconstraint"    
    exclGI = input$withoutGrandInga
    load = input$d1cons
    varyload=TRUE
    
      if (!is.null(thewater) & !is.null(theuclf) & !is.null(theuclf2) & !is.null(thecountry)   ) {
        return(barunconstraint(thewater,theuclf,theuclf2,thecountry, thedom="d1t1","Unconstraint","All",
                               values$startyear,values$endyear,exclGI,varyload,load));                
      }
  });
  
  output$demo1a <- renderChart({      
    thewater = input$d1water    
    theuclf = input$d1uclf
    theuclf2 = input$d1uclf2
    thecountry = values$country
    thepolicy = "unconstraint"    
    exclGI = input$withoutGrandInga
    load = input$d1cons
    varyload=TRUE
    
    if (!is.null(thewater) & !is.null(theuclf) & !is.null(theuclf2) & !is.null(thecountry)   ) {
      return(demo1(80,theuclf,theuclf2,thecountry, thedom="demo1a","Assume 20% Less Water","All",
                             values$startyear,values$endyear,exclGI,varyload,load));                
    }
  });
  output$demo1b <- renderChart({      
    thewater = input$d1water    
    theuclf = input$d1uclf
    theuclf2 = input$d1uclf2
    thecountry = values$country
    thepolicy = "unconstraint"    
    exclGI = input$withoutGrandInga
    load = input$d1cons
    varyload=TRUE
    
    if (!is.null(thewater) & !is.null(theuclf) & !is.null(theuclf2) & !is.null(thecountry)   ) {
      return(demo1(120,theuclf,theuclf2,thecountry, thedom="demo1b","Assume 20% More Water","All",
                   values$startyear,values$endyear,exclGI,varyload,load));                
    }
  });
  
  output$demo2 <- renderChart({      
    thewater = input$d1water    
    theuclf = input$d1uclf
    theuclf2 = input$d1uclf2
    thecountry = values$country
    thepolicy = "unconstraint"    
    exclGI = input$withoutGrandInga
    load = input$d1cons
    varyload=TRUE
    
    if (!is.null(thewater) & !is.null(theuclf) & !is.null(theuclf2) & !is.null(thecountry)   ) {
      return(demo2(thewater,theuclf,theuclf2,thecountry, thedom="demo2","","All",
                   values$startyear,values$endyear,exclGI,varyload,load));                
    }
  });
  
  
  

  infounconstraint <- function(thewater,thecoaluclf,thetxuclf,thecountry,theseries="Demand", thedom="d1t2a", exclGI=FALSE,adjcons=FALSE,cons=0) {
    
    if (!is.null(thewater) && !is.null(thecoaluclf) && !is.null(thetxuclf) ) {    
      td = getunconstraint(thewater/100, thecoaluclf/100,thetxuclf/100, exclGI,adjcons,cons/100)  
      
      tfinal = subset(td, series %in% theseries)  
      units = as.character(tfinal$unit[1])
      if (thecountry!="All") {
        tfinal = subset(tfinal, country.name == thecountry)          
      }
      
      #if(nrow(tfinal)>0) {        
      tfinal2 = tfinal[, c("time","value"),with=F]
      tfinal3 = tfinal2[, lapply(.SD, sum), by = c("time")]             
      tfinal3 = tfinal3[(tfinal3$time>2010) & (tfinal3$time<2050) ,]
      x = tfinal3;
      #}
      
      
      
      h1 <- rCharts:::Highcharts$new()
      h1$chart(type = "spline",marginLeft=80,height=300)
      h1$title(text = paste(paste(theseries,collapse=",") ," (",thecountry[1],")",sep="") )
      
      #if(nrow(tfinal)>0) {
      h1$xAxis(categories = x$time,labels=list(enabled=TRUE))
      h1$yAxis(title = list(text = units),min=0)        
      h1$series( data = x$value, type="spline", name=paste(theseries,sep="")  )
      
      #}
      
      h1$legend(symbolWidth = 80,enabled=FALSE)
      h1$set(dom = thedom)
      h1$plotOptions(animation=FALSE,spline=list(animation=FALSE))
      h1$exporting(enabled = F)          
      #print(h1)
      return(h1)         
      
    }        
  }
  
  output$d1t2a <- renderChart({
    thewater = input$d1water    
    theuclf = input$d1uclf
    theuclf2 = input$d1uclf2
    thecountry = values$country
    exclGI = input$withoutGrandInga
    varyload=TRUE
    load = input$d1cons
    
    return(infounconstraint(thewater,theuclf,theuclf2,thecountry,theseries="Demand", thedom="d1t2a",exclGI,varyload,load))
  });
  output$d1t2b <- renderChart({
    thewater = input$d1water    
    theuclf = input$d1uclf
    theuclf2 = input$d1uclf2
    thecountry = values$country
    exclGI = input$withoutGrandInga
    varyload=TRUE
    load = input$d1cons
    return(infounconstraint(thewater,theuclf,theuclf2,thecountry,theseries="Generation", thedom="d1t2b",exclGI,varyload,load))
  });
  output$d1t2c <- renderChart({
    thewater = input$d1water    
    theuclf = input$d1uclf
    theuclf2 = input$d1uclf2
    thecountry = values$country
    exclGI = input$withoutGrandInga
    varyload = TRUE
    load = input$d1cons
    return(infounconstraint(thewater,theuclf,theuclf2,thecountry,
                            theseries=c("Export revenue","Import cost","TransmissionInvestment",
                                        "Investment","Annual Investment","Fuel Cost","O&M Costs"), thedom="d1t2c",exclGI,varyload,load))
  });
  output$d1t2d <- renderChart({
    thewater = input$d1water    
    theuclf = input$d1uclf
    theuclf2 = input$d1uclf2
    thecountry = values$country
    exclGI = input$withoutGrandInga
    varyload=TRUE
    load = input$d1cons
    return(infounconstraint(thewater,theuclf,theuclf2,thecountry,theseries="Import Capacity", thedom="d1t2d",exclGI,varyload,load))
  });
  
  output$createlistui <- renderUI({
    thepolicies = do.call(c,lapply(values$createdpolicy,function(x) {x$name}))
    if(length(thepolicies)>1) {
      thepolicies = thepolicies[thepolicies!="NONE"]  
    }
    selectInput("createdpolicies","Created Policies:",
                as.list(thepolicies)
                ,size=5,selectize=FALSE)
  })
  
  
  output$s1info <- renderUI({
    thepolicies = do.call(c,lapply(values$createdpolicy,function(x) {x$name}))
    if(length(thepolicies)>1) {
      
    
    if(!is.null(input$createdpolicies)) {
      r = values$createdpolicy[thepolicies==input$createdpolicies][[1]]
      
    tags$table(border=1,spacing=1,
      tags$tr(
        tags$th("Assumption"),
        tags$th("Value")
      ),
      tags$tr(
        tags$td("Water Availability"),
        tags$td(r[2])
      ),
      tags$tr(
        tags$td("Coal UCLF"),
        tags$td(r[3])
      ),  
      tags$tr(
        tags$td("Transmission UCLF"),
        tags$td(r[4])
      ),  
      tags$tr(
        tags$td("Include Grand Inga"),
        tags$td( ifelse(r[7],"No","Yes") )
      ),  
      tags$tr(
        tags$td("Adjust Consumption"),
        tags$td( ifelse(r[5],"Yes","No") )
      ),  
      tags$tr(
        tags$td("Consumption"),
        tags$td( r[6] )
      )
    )
    }
      
    }
  })
  output$s2info <- renderUI({
    thepolicies = do.call(c,lapply(values$availablepolicy,function(x) {x$name}))
    if(length(thepolicies)>1) {
      
      
      if(!is.null(input$availablepolicies)) {
      r = values$availablepolicy[thepolicies==input$availablepolicies][[1]]
      
      tags$table(border=1,spacing=1,
                 tags$tr(
                   tags$th("Assumption"),
                   tags$th("Value")
                 ),
                 tags$tr(
                   tags$td("Water Availability"),
                   tags$td(r[2])
                 ),
                 tags$tr(
                   tags$td("Coal UCLF"),
                   tags$td(r[3])
                 ),  
                 tags$tr(
                   tags$td("Transmission UCLF"),
                   tags$td(r[4])
                 ),  
                 tags$tr(
                   tags$td("Include Grand Inga"),
                   tags$td( ifelse(r[7],"No","Yes") )
                 ),  
                 tags$tr(
                   tags$td("Adjust Consumption"),
                   tags$td( ifelse(r[5],"Yes","No") )
                 ),  
                 tags$tr(
                   tags$td("Consumption"),
                   tags$td( r[6] )
                 )
      )
      }
    }
  })
  
  
  
  output$availlistui <- renderUI({
    thepolicies = do.call(c,lapply(values$availablepolicy,function(x) {x$name}))
    if(length(thepolicies)>1) {
      thepolicies = thepolicies[thepolicies!="NONE"]  
    }
    
    selectInput("availablepolicies","AVAILABLE Policy Assuptions: (SELECT)",as.list(thepolicies),
                size=5,selectize=FALSE)#selected=values$selectavail)
  })    
  output$lockedlistui <- renderUI({
    tags$span(
      selectInput("lockedbaseline","LOCKED Baseline Policy:",as.list(values$lockedbasepolicy),size=1,selectize=FALSE),
      selectInput("lockedscenario","LOCKED Scenario Policy:",as.list(values$lockedscenpolicy),size=1,selectize=FALSE)
    )
  })
  
  output$d3m1type <- renderUI({
    
    a = list("Baseline" = "baseline",
             "Scenario" = "scenario")    
    names(a) = c(paste("Baseline: ",values$lockedbasepolicy,sep=""), paste("Scenario: ",values$lockedscenpolicy,sep=""))
    
    radioButtons("d3txflowtype", "Scenario vs Baseline",a)    
  })
  
  output$d2t1 <- renderChart({
    if(!is.null(input$availablepolicies)) {
      if(input$availablepolicies!="NONE") {
      
    thepolicies = do.call(c,lapply(values$availablepolicy,function(x) {x$name}))
    if(length(thepolicies)>1) {
      r = values$availablepolicy[thepolicies==input$availablepolicies][[1]]
      
      thewater = unlist(r[2])
      theuclf = unlist(r[3])
      theuclf2 = unlist(r[4])
      thecountry = values$country
      exclGI = unlist(r[7])
      varyload=TRUE
      load = unlist(r[6])
      if (!is.null(thewater) && !is.null(theuclf) && !is.null(theuclf2)  ) {
        return(barunconstraint(thewater,theuclf,theuclf2,thecountry, thedom="d2t1",r[1],"All",
                             values$startyear,values$endyear,exclGI,varyload,load));                
      }
    }
    } else {
     
      h1 <- rCharts:::Highcharts$new()
      h1$chart(type = "column",marginLeft=50,height=300)
      h1$title(text = paste("New Capacity",sep=""))
      h1$subtitle(text = paste("",sep=""))
      
  
      
      
      h1$legend(symbolWidth = 10)
      h1$set(dom = "d2t1")
      h1$plotOptions(animation=FALSE,column=list(stacking= 'normal',animation=FALSE))
      h1$exporting(enabled = T)    
      
      return(h1)  
    }
    }
  });
    
  # Create new policy
  observe({
    if(input$s1createpolicy>0) {
      isolate({
      if (!is.null(isolate(input$createdpolicies))) {
        if(!is.null(input$s1policyname)) {
          if(nchar(input$s1policyname)>5) {
            #values$createdpolicy <- isolate(values$createdpolicy[!grepl("NONE",values$createdpolicy)])
            
            thepolicies = do.call(c,lapply(values$createdpolicy,function(x) {x$name}))
            if (!input$s1policyname %in% thepolicies) {
              
              exclGI = input$withoutGrandInga
              varyload=TRUE
              load = input$d1cons
              
               values$createdpolicy[[isolate(length(values$createdpolicy))+1]] <- isolate(
                                                 list(name=input$s1policyname,
                                                      thewater=input$d1water,
                                                      thecoaluclf=input$d1uclf,
                                                      thetxuclf=input$d1uclf2,
                                                      varyload=varyload,load=load,withoutinga=exclGI)
                                                 )
               #cp = values$createdpolicy;
               #save(cp,file = "/tmp/cp.rdata");
               #values$availablepolicy <- isolate(values$createdpolicy)
            }
            closeAlert(session, "s1a")
          } else {
            createAlert(session, "s1alert", "s1a", title = "Step1: Error",
                        content = "Policy Name need to be at least 6 characters long!", append = FALSE)
          }
        }
      }
      });
    } 
    
    values$availablepolicy <- isolate(values$createdpolicy)
  });
  
  # Delete a Policy
  observe({
    if(input$s1deletepolicy>0) {
      isolate({
        if (!is.null(isolate(input$createdpolicies))) {
            if(nchar(input$createdpolicies)>0) {
              
              if(input$createdpolicies!="NONE") {
                if(length(values$createdpolicy)>1) {
                  values$createdpolicy <- isolate(values$createdpolicy[
                  do.call(c, lapply(values$createdpolicy,function(x) {x$name}))!=input$createdpolicies])
                }
              }
              
              #if(length(values$createdpolicy)==1) {
#                 if (input$createdpolicies==values$createdpolicy$name) {
              #     values$createdpolicy <- list(list(name="NONE",thewater=0,thecoaluclf=0,thetxuclf=0))
#             #    }
              # }
              
              #values$createdpolicy <- isolate(values$createdpolicy[values$createdpolicy!=input$createdpolicies])
              #if(length(isolate(values$createdpolicy))==0) {
              #  values$createdpolicy <- list(name="NONE",thewater=0,thecoaluclf=0,thetxuclf=0)
              #}
            }
        }
      });
    }     
    values$availablepolicy <- isolate(values$createdpolicy)
  });  
  
  # Show Policy
  observe({
    if(input$s1loadpolicy>0) {
      isolate({
        if (!is.null(isolate(input$createdpolicies))) {
            if(length(values$createdpolicy)>1) {
              
              if(input$createdpolicies!="NONE") {
                thepolicies = do.call(c,lapply(values$createdpolicy,function(x) {x$name}))
                if(length(thepolicies)>1) {
                  sel <- isolate(values$createdpolicy[thepolicies==input$createdpolicies])
                  updateSliderInput(session, "d1water", value = sel[[1]]$thewater)
                  updateSliderInput(session, "d1uclf", value = sel[[1]]$thecoaluclf)
                  updateSliderInput(session, "d1uclf2", value = sel[[1]]$thetxuclf)
                  updateCheckboxInput(session, "withoutGrandInga", value = sel[[1]]$withoutinga)
                  #updateCheckboxInput(session, "varyload", value = sel[[1]]$varyload)
                  updateSliderInput(session, "d1cons", value = sel[[1]]$load)
                }
              }

            }
        }
      })
    }
  })
  
  # LOCK base
  observe({
    if(input$plockbase>0) {
      if (!is.null(isolate(input$availablepolicies))) {
        if(isolate(input$lockedbaseline=='NONE')) {
          isolate({
           closeAlert(session, "s2a")
            if(isolate(values$lockedscenpolicy)!=isolate(input$availablepolicies)) {
                values$lockedbasepolicy <- isolate(input$availablepolicies);
            }
            thepolicies = do.call(c,lapply(values$availablepolicy,function(x) {x$name}))
            if(length(thepolicies)>=1) {
              #values$availablepolicy  = values$availablepolicy[thepolicies!=as.character(input$availablepolicies)]  
              #values$selectavail = values$availablepolicy[thepolicies!=as.character(input$availablepolicies)][[1]]$name
            }
          });
          
        }
      }
    }      
  });
  # LOCK scen
  observe({
    if(input$plockscen>0) {
      if (!is.null(isolate(input$availablepolicies))) {
        if(isolate(input$lockedscenario=='NONE')) {
          isolate({
            closeAlert(session, "s2a")
            if(isolate(values$lockedbasepolicy)!=isolate(input$availablepolicies)) {
              values$lockedscenpolicy <- isolate(input$availablepolicies);
            }
             thepolicies = do.call(c,lapply(values$availablepolicy,function(x) {x$name}))
             if(length(thepolicies)>=1) {
               #values$availablepolicy  = values$availablepolicy[thepolicies!=as.character(input$availablepolicies)]  
               #values$selectavail = values$availablepolicy[thepolicies!=as.character(input$availablepolicies)][[1]]$name
               #values$selectavail = values$availablepolicy[thepolicies!=as.character(input$availablepolicies)][1]
             }
          });
          
        }
      }
    }      
  });  
  # UNLOCK base
  observe({
    if(input$punlockbase>0) {      
      if(isolate(input$lockedbaseline!='NONE')) {
        isolate({
        closeAlert(session, "s2a")
        values$lockedbasepolicy <- 'NONE';
        values$availablepolicy <- isolate(values$createdpolicy) 
        thepolicies = do.call(c,lapply(values$availablepolicy,function(x) {x$name}))
        #values$availablepolicy  = values$availablepolicy[thepolicies!=as.character(input$lockedscenario)]  
        });
      }
    }      
  });
  # UNLOCK scen
  observe({
    if(input$punlockscen>0) {      
      if(isolate(input$lockedscenario!='NONE')) {
        isolate({
        closeAlert(session, "s2a")
        values$lockedscenpolicy <- 'NONE';
        values$availablepolicy <- isolate(values$createdpolicy)
        thepolicies = do.call(c,lapply(values$availablepolicy,function(x) {x$name}))
        #values$availablepolicy  = values$availablepolicy[thepolicies!=as.character(input$lockedbaseline)]    
        });
      }
    }      
  });  

  output$d2pivot <-renderRpivotTable({  
    
    thepolicies = do.call(c,lapply(values$availablepolicy,function(x) {x$name}))
    if(length(thepolicies)>1) {
      r = values$availablepolicy[thepolicies==input$availablepolicies][[1]]
      
      thewater = unlist(r[2])
      theuclf = unlist(r[3])
      theuclf2 = unlist(r[4])
      thecountry = values$country
      exclGI = unlist(r[7])
      varyload=TRUE
      load = unlist(r[6])
      
      td = getunconstraint(thewater/100, theuclf/100,theuclf2/100, exclGI,varyload,load/100)  
      tfinal = subset(td, series == input$d2pivotts)  
      units = as.character(tfinal$unit[1])
      if (thecountry!="All") {
        tfinal = subset(tfinal, country.name == thecountry)          
      }
      fs = subset(tfinal, time %in% c(values$startyear:values$endyear))     
      if(nrow(fs)>0) {
        rpivotTable(data=fs,rows=c("series","energy\\.source"),cols=c("time"),aggregatorName="Sum",rendererName="Stacked Bar Chart",vals="value"
                    ,hiddenAttributes=c("consuming\\.country","producing\\.country","policy_name","policy_id","run_id",
                                        "design\\.coal\\.uclf",
                                        "design\\.fix\\.year",
                                        "design\\.consumption\\.adjustment",
                                        "design\\.grand\\.inga\\.out",
                                        "design\\.transmission\\.uclf",
                                        "design\\.water\\.availability",
                                        "coal\\.uclf",
                                        "fix\\.year",
                                        "consumption\\.adjustment",
                                        "grand\\.inga\\.out",
                                        "transmission\\.uclf",
                                        "water\\.availability"
                                        ))    
      }
    }

  })

  observe({    
    if( !is.null(input$lockedbaseline) || !is.null(input$lockedbaseline)) {    
      if( (input$lockedbaseline=='NONE') || (input$lockedbaseline=='NONE')) {
        createAlert(session, "s3alert", "s3a", title = "Step2: Error",
                    content = "Please Lock>> both a BASELINE and SCENARIO policy before proceeding to Step3!", append = FALSE)
      } else {
        closeAlert(session, "s3a")
      }    
    }
  })
  
  GetBaseScen <- function(seriesnames,
                          water, coaluclf,txuclf,exclGI,cons,
                          fixwater=TRUE,fixcoaluclf=TRUE,fixtxuclf=TRUE,fixexclGI=TRUE,fixcons=TRUE) {
    
    thepolicies = do.call(c,lapply(values$availablepolicy,function(x) {x$name}))
    if(length(thepolicies)>1) {
      
      thecountry = values$country

     
      ## Get BASELINE
      r = values$availablepolicy[thepolicies==isolate(values$lockedbasepolicy)][[1]]
      designwater = unlist(r[2])
      designcoaluclf = unlist(r[3])
      designtxuclf = unlist(r[4])
      designexclGI = unlist(r[7])
      varyload=TRUE
      designcons = unlist(r[6])
      
      
      td = getconstraint(designwater/100,designcoaluclf/100,designtxuclf/100,designexclGI,designcons/100, 
                                water/100, coaluclf/100,txuclf/100,exclGI,cons/100,
                                fixwater,fixcoaluclf,fixtxuclf,fixexclGI,fixcons)
      
      #td = getconstraint(designwater/100,designcoaluclf/100,designtxuclf/100,fixyear,thewater/100, thecoaluclf/100,thetxuclf/100, 
      #                   exclGI,varyload,load/100,cons/100,fixwater,fixcoaluclf,fixtxuclf,fixcons)
      
      if(length(td)==0) {
        createAlert(session, "globalalert", "ga", title = "",
                    content = "No data found for selected combination", append = FALSE)
        return(NULL);
      } else {closeAlert(session, "ga")}
      
      #td = getunconstraint(thewater/100, theuclf/100,theuclf2/100, exclGI,varyload,load/100)  
      tfinal = subset(td, series %in% seriesnames)  
      units = as.character(tfinal$unit[1])
      if (thecountry!="All") {
        tfinal = subset(tfinal, country.name == thecountry)          
      }
      fs = subset(tfinal, time %in% c(values$startyear:values$endyear))
      fs$resulttype = "Baseline" 
      fs$lockedname = values$lockedbasepolicy
      
      # Get SCENARIO
      r = values$availablepolicy[thepolicies==isolate(values$lockedscenpolicy)][[1]]
      designwater = unlist(r[2])
      designcoaluclf = unlist(r[3])
      designtxuclf = unlist(r[4])
      designexclGI = unlist(r[7])
      varyload=TRUE
      designcons = unlist(r[6])

      
      td = getconstraint(designwater/100,designcoaluclf/100,designtxuclf/100,designexclGI,designcons/100, 
                         water/100, coaluclf/100,txuclf/100,exclGI,cons/100,
                         fixwater,fixcoaluclf,fixtxuclf,fixexclGI,fixcons)
      
      #td = getconstraint(designwater/100,designcoaluclf/100,designtxuclf/100,fixyear,thewater/100, thecoaluclf/100,thetxuclf/100, 
      #                   exclGI,varyload,load/100,cons/100,fixwater,fixcoaluclf,fixtxuclf,fixcons)
      
      if(length(td)==0) {
        createAlert(session, "globalalert", "ga", title = "",
                    content = "No data found for selected combination", append = FALSE)
        return(NULL);
      } else {closeAlert(session, "ga")}
      
      #td = getunconstraint(thewater/100, theuclf/100,theuclf2/100, exclGI,varyload,load/100)  
      tfinal = subset(td, series %in% seriesnames)  
      units = as.character(tfinal$unit[1])
      if (thecountry!="All") {
        tfinal = subset(tfinal, country.name == thecountry)          
      }
      fs2 = subset(tfinal, time %in% c(values$startyear:values$endyear))
      fs2$resulttype = "Scenario"
      fs2$lockedname = values$lockedscenpolicy
      
      fs3 = rbind(fs,fs2,fill=TRUE)
      return(fs3)
    }
  }
 
  
  output$d3pivot1 <-renderRpivotTable({ 
      
      water = input$d3water    
      coaluclf = input$d3uclf
      txuclf = input$d3uclf2
      thecountry = values$country
      fixedyear = 2020 
      exclGI = input$d3withoutGrandInga
      cons = input$d3cons
      
      if (!is.null(water) && !is.null(coaluclf) && !is.null(txuclf) && !is.null(thecountry) && !is.null(cons) && !is.null(exclGI) ) {
      
        withProgress(message = 'Loading Pivot',
                     detail = '', value = 20, {
                       
              
      
      fs3 = GetBaseScen(c("Export revenue","Import cost","TransmissionInvestment","Investment",
                          "Annual Investment","Fuel Cost","O&M Costs"),
                              water, coaluclf,txuclf,exclGI,cons,
                              fixwater=FALSE,fixcoaluclf=TRUE,fixtxuclf=TRUE,fixexclGI=TRUE,fixcons=TRUE);
      
  
      if(nrow(fs3)>0) {
        rpivotTable(data=fs3,rows=c("resulttype","coal\\.uclf"),cols=c("water\\.availability"),aggregatorName="Sum",rendererName="Line Chart",vals="value"
                    ,hiddenAttributes=c("consuming\\.country","producing\\.country","policy_name","policy_id","run_id",
                                        "design\\.coal\\.uclf",
                                        "design\\.fix\\.year",
                                        "design\\.consumption\\.adjustment",
                                        "design\\.grand\\.inga\\.out",
                                        "design\\.transmission\\.uclf",
                                        "design\\.water\\.availability",
                                        #"coal\\.uclf",
                                        "fix\\.year"
                                        #"consumption\\.adjustment"
                                        #"grand\\.inga\\.out",
                                        #"transmission\\.uclf",
                                        #"water\\.availability"
                    ))    
      
      }
      
                     })
      }
  })

  output$d3pivot2 <-renderRpivotTable({ 
    
    water = input$d3water    
    coaluclf = input$d3uclf
    txuclf = input$d3uclf2
    thecountry = values$country
    fixedyear = 2020 
    exclGI = input$d3withoutGrandInga
    cons = input$d3cons
    
    if (!is.null(water) && !is.null(coaluclf) && !is.null(txuclf) && !is.null(thecountry) && !is.null(cons) && !is.null(exclGI) ) {
    
    withProgress(message = 'Loading Pivot',
                 detail = '', value = 20, {
    
    
      fs3 = GetBaseScen(c("Fuel Cost"),
                              water, coaluclf,txuclf,exclGI,cons,
                              fixwater=FALSE,fixcoaluclf=TRUE,fixtxuclf=TRUE,fixexclGI=TRUE,fixcons=TRUE);
    
    if(nrow(fs3)>0) {
      rpivotTable(data=fs3,rows=c("coal\\.uclf","energy\\.source"),cols=c("resulttype","water\\.availability"),aggregatorName="Sum",rendererName="Table Barchart",vals="value"
                  ,hiddenAttributes=c("consuming\\.country","producing\\.country","policy_name","policy_id","run_id",
                                      "design\\.coal\\.uclf",
                                      "design\\.fix\\.year",
                                      "design\\.consumption\\.adjustment",
                                      "design\\.grand\\.inga\\.out",
                                      "design\\.transmission\\.uclf",
                                      "design\\.water\\.availability",
                                      #"coal\\.uclf",
                                      "fix\\.year"
                                      #"consumption\\.adjustment"
                                      #"grand\\.inga\\.out",
                                      #"transmission\\.uclf",
                                      #"water\\.availability"
                  ))    
      
    }
                 });
    }
  })
  
  output$d3pivot3 <-renderRpivotTable({ 
    
    water = input$d3water    
    coaluclf = input$d3uclf
    txuclf = input$d3uclf2
    thecountry = values$country
    fixedyear = 2020 
    exclGI = input$d3withoutGrandInga
    cons = input$d3cons
    
    if (!is.null(water) && !is.null(coaluclf) && !is.null(txuclf) && !is.null(thecountry) && !is.null(cons) && !is.null(exclGI) ) {
    
    withProgress(message = 'Loading Pivot',
                 detail = '', value = 20, {
    
    if(!is.null(water)) {
      
      
      fs3 = GetBaseScen(c("Export revenue","Import cost","TransmissionInvestment","Investment",
                          "Annual Investment","Fuel Cost","O&M Costs"),
                        water, coaluclf,txuclf,exclGI,cons,
                        fixwater=TRUE,fixcoaluclf=FALSE,fixtxuclf=TRUE,fixexclGI=TRUE,fixcons=TRUE);
      
      
      if(nrow(fs3)>0) {
        rpivotTable(data=fs3,rows=c("resulttype","water\\.availability"),cols=c("coal\\.uclf"),aggregatorName="Sum",rendererName="Line Chart",vals="value"
                    ,hiddenAttributes=c("consuming\\.country","producing\\.country","policy_name","policy_id","run_id",
                                        "design\\.coal\\.uclf",
                                        "design\\.fix\\.year",
                                        "design\\.consumption\\.adjustment",
                                        "design\\.grand\\.inga\\.out",
                                        "design\\.transmission\\.uclf",
                                        "design\\.water\\.availability",
                                        #"coal\\.uclf",
                                        "fix\\.year"
                                        #"consumption\\.adjustment"
                                        #"grand\\.inga\\.out",
                                        #"transmission\\.uclf",
                                        #"water\\.availability"
                    ))    
        
      }
    }
                 });
    }
  })

  output$d3pivot4 <-renderRpivotTable({ 
    
    water = input$d3water    
    coaluclf = input$d3uclf
    txuclf = input$d3uclf2
    thecountry = values$country
    fixedyear = 2020 
    exclGI = input$d3withoutGrandInga
    cons = input$d3cons
    
    if (!is.null(water) && !is.null(coaluclf) && !is.null(txuclf) && !is.null(thecountry) && !is.null(cons) && !is.null(exclGI) ) {
      
    
    withProgress(message = 'Loading Pivot',
                 detail = '', value = 20, {
    
    
    if(!is.null(water)) {
      
      
      fs3 = GetBaseScen(c("New Capacity"),
                        water, coaluclf,txuclf,exclGI,cons,
                        fixwater=TRUE,fixcoaluclf=TRUE,fixtxuclf=TRUE,fixexclGI=TRUE,fixcons=TRUE);
      
      
      if(nrow(fs3)>0) {
        rpivotTable(data=fs3,rows=c("energy\\.source"),cols=c("time","resulttype"),aggregatorName="Sum",rendererName="Stacked Bar Chart",vals="value"
                    ,hiddenAttributes=c("consuming\\.country","producing\\.country","policy_name","policy_id","run_id",
                                        "design\\.coal\\.uclf",
                                        "design\\.fix\\.year",
                                        "design\\.consumption\\.adjustment",
                                        "design\\.grand\\.inga\\.out",
                                        "design\\.transmission\\.uclf",
                                        "design\\.water\\.availability",
                                        #"coal\\.uclf",
                                        "fix\\.year"
                                        #"consumption\\.adjustment"
                                        #"grand\\.inga\\.out",
                                        #"transmission\\.uclf",
                                        #"water\\.availability"
                    ))    
        
      }
    }
                 });
      
    }
  })
  
    
  output$aggregate_inflows <- renderText({
    paste("Aggregate_inflows: ",input$d1m1_arrownode$properties$aggregate_inflows,sep="")
  })
  
  output$aggregate_outflows <- renderText({
    paste("Aggregate_outflows: ",input$d1m1_arrownode$properties$aggregate_outflows,sep="")    
  })
  
  observe({
    if(!is.null(input$d1m1_arrownode)) {
      print(input$d1m1_arrownode$properties$aggregate_inflows)
      print(input$d1m1_arrownode$properties$aggregate_outflows)
    }    
  });
  

  
  # Draw: Energy chain TREE (Hierarchy)
  output$tree <- renderTree({
    
    #- Country
    #- Level
    #- Energy. Source
    #- Status
    #- Name
    
    withProgress(message = 'Loading hierarchy....',
                 detail = 'Please wait', value = 0, {
              
                   tree <- lapply(countries, function(country){
                     level.list <- lapply(level[c(4,1,3,5,2)], function(alevel){
                       energy.sources <- uniqueAndSorted(subset(nodes,country.name == country & level==alevel)$energy.source)
                       energy.sources.list <- lapply(energy.sources, function(asource){
                         status <- uniqueAndSorted(subset(nodes,country.name == country & level==alevel & energy.source == asource)$status)
                         status.list <- lapply(status, function(astatus){
                           #names <- uniqueAndSorted(subset(nodes,country.name == country & level==alevel & energy.source == asource & status == astatus)$name)        
                           tech <- uniqueAndSorted(subset(nodes,country.name == country & level==alevel & energy.source == asource & status == astatus)$technology)        
                           lnames <- as.list(tech)
                           names(lnames) <- tech
                           return(lnames)
                         })
                         names(status.list) <- status
                         return(status.list)
                       })
                       names(energy.sources.list) <- energy.sources
                       return(energy.sources.list)
                     })
                     names(level.list) <- level[c(4,1,3,5,2)]
                     return(level.list)
                   })
                   names(tree) <- countries

                 });
    
    return(tree)
  })
  
  output$treesel <- renderText({
    tree <- input$tree
    
   
      t = get_selected(tree)
      
     
      if(length(t)>0) {
        ans = attr(t[[1]],"ancestry")
        if(length(ans)>0) {
          country = ans[1]
        } else {
          country = t[[1]]
        }
        
        if (length(ans)==4) {          
          paste(country,t[[1]],sep=",");
        } else {
          ""
        }
        
        
        
        
      } else {
        ""
      }
      
        
#         if (length(ans)==5) {          
#           lk = unlist(lapply(strsplit(ans,"\\("),function(x) { gsub(" $","",x[1]) } ))          
#           eff = as.character(enodes[enodes$name==lk[3] & 
#                                       enodes$level==lk[2] & 
#                                       enodes$country==lk[1],]$energyform.full) 
#           
#           myt = gsub("\\)","",strsplit(t[[1]][1],"\\(")[[1]][2] )
#           
#           values$leafnode = paste(myt, lk[4] ,eff,lk[1],lk[2],lk[3], sep=":")
#           #values$leafnode = paste(t[[1]][1], lk[4] ,eff,lk[1],lk[2],lk[3], sep=":")
#           values$country = lk[1]
#           
#         } else {
#           values$leafnode = "None"
#           values$leaftypes=data.frame()
#           values$leafdata=data.frame()
#           if(!is.na(country)) {                      
#             values$country=country
#           } else {            
#             values$country=t[[1]][1]                    
#           }
#         }
       
         
       
    
    
  })
  
  
  observe({
    if(!is.null(input$nav) && !is.null(input$s2s3) ) {
      if((input$nav=="STEP 3") && (input$s2s3<1)  ) {
        updateTabsetPanel(session, "nav", selected = "STEP 2")
      }
    }    
  })
  
  observe({
    if(!is.null(input$nav) && !is.null(input$s1s2) ) {
      if((input$nav=="STEP 2") && (input$s1s2<1)  ) {
        
        updateTabsetPanel(session, "nav", selected = "STEP 1")
      }
    }    
  })
  
  
  ### NEXT/Back
  observe({    
    if(input$s0s1>0) {      
      updateTabsetPanel(session, "nav", selected = "STEP 1")
    }    
  })
  observe({    
    if(input$s1s2>0) {      
      isolate({
      thepolicies = do.call(c,lapply(values$createdpolicy,function(x) {x$name}))
      if(length(thepolicies)>1) {
        thepolicies = thepolicies[thepolicies!="NONE"]  
      }
      if (length(thepolicies)>=2) {
        updateTabsetPanel(session, "nav", selected = "STEP 2")
      } else {
        createAlert(session, "s1alert", "s1a", title = "Step1: Error",
                    content = "Create at least 2 Policies!", append = FALSE);
      }
      });
    }    
  })
  observe({    
    if(input$s2s1>0) {      
      updateTabsetPanel(session, "nav", selected = "STEP 1")
    }    
  })
  observe({    
   
    if(input$s2s3>0) {      
       if( (isolate(input$lockedscenario)!='NONE') && (isolate(input$lockedbaseline)!='NONE')  ) {       
          updateTabsetPanel(session, "nav", selected = "STEP 3")
        } else {
          createAlert(session, "s2alert", "s2a", title = "Step2: Error",
                      content = "Please Lock>> both a BASELINE and SCENARIO policy before proceeding to Step3!", append = FALSE)
        }        
    }
    
  })
  observe({    
    if(!is.null(input$s3s2)) {
     if(input$s3s2>0) {      
       updateTabsetPanel(session, "nav", selected = "STEP 2")
     }    
    }
  })
  observe({    
    if(!is.null(input$s3s4)) {
     if(input$s3s4>0) {      
      updateTabsetPanel(session, "nav", selected = "HELP")
     }    
    }
  })
#   observe({    
#     if(input$s4s3>0) {      
#       updateTabsetPanel(session, "nav", selected = "STEP 3")
#     }    
#   })
  
})




