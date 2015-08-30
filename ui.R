#library(DiagrammeR)
library(shiny)
#library(shinyAce)
library(leaflet)   # devtools::install_github("booysej/leaflet-shiny")
library(shinyTree) # devtools::install_github("booysej/shinyTree")
library(shinyBS)
library(shinythemes)
library(RSQLite)
#library(DT)
library(rCharts)
library(rpivotTable);

shinyUI(fluidPage( theme = shinytheme("spacelab"),
      
  tags$head(
    #includeCSS("style.css"),
    #includeScript("busy.js"),
    includeCSS("styles.css"),        
    includeScript("gomap.js"),        
    tags$style(type="text/css", "select.pvtAggregator {width: 55px;}"),
    #tags$style(type="text/css", "select.pvtRenderer {width: 120px;}"),  
    tags$style(type="text/css", "select.pvtAttrDropdown {width: 100px;}"),  
    tags$style(type="text/css", "table#locktable { border-collapse:separate; border-spacing:0 5px;}") ,
    tags$style(type="text/css", ".navbar {margin-bottom: 0px;}")    
  ),    
  
  fluidRow(column(5,navbarPage("CRIDF SAPP Scenario Planning Tool: ",position="static-top")),
           column(1,tags$br(),"Date Range (View):"),
                       column(4,
                              sliderInput("daterange", label = "Select", min = 2011, 
                                          max = 2050, value = c(2015, 2025),ticks=FALSE,width="600px")
                              ),
           column(2,
             bsAlert("globalalert")
           )
  ),
  tabsetPanel(id="nav",  type="pills",   selected="STEP 1",                        
              ########################### STEP0 ##########################
              tabPanel("STEP 0",
                       tabsetPanel(id="overview",     
                            tabPanel("Photo",
                                     fluidRow(
                                       column(6,
                                              tags$h4("STEP 0 - Overview")),                                                                                              
                                       column(6,bsButton("s0s1","Next (Step1) >>",style="primary"),verbatimTextOutput("queryText"))
                                     ),
                                    tags$img(src="overview.gif")
                            ),
                            tabPanel("Text",
                                     fluidRow(
                                       column(6,tags$h4("STEP 0 - Overview")),                                                                                              
                                       column(6,bsButton("s0s1","Next (Step1) >>",style="primary"))
                                     ),
                              tags$h1("Executive Summary"),
                        tags$p("The Climate Resilient Infrastructure Development Facility (CRIDF) has identified the need to deliver a 
                       user friendly, graphically driven, SADC regionalised long-term energy planning and scenario 
                       tool to determine climatic and economic impacts in the region over the coming 30 years (2015-2045).
                       Until now, individual country, as well as regionally co-ordinated integrated resource and master 
                       planning studies have been undertaken, e.g. IRENA SPLAT, or may be currently in progress.  It is not 
                       the intention of this project to duplicate any of these projects but rather to build and enhance on the 
                       outputs of these studies. The proposed model will be unique in that it will include the effects of 
                       sustainable water utilisation, long term temperature changes as well as economic feedback (e.g. lower 
                       energy prices will stimulate economic growth), into the future projections of energy and demand."),
                       tags$p("Enerweb-EOH is proposing a three phase approach to delivering a visually interactive optimisation 
                       tool, allowing economic and climatic scenarios to be investigated. In the first phase, the tool will use 
                       the already established local and regional generation and transmission expansion plans as input data.  
                       After the first phase of the project, a demonstration solution will be shown at the 45th SAPP 
                       Conference, after which a dynamic optimiser, which includes climate and economic feedback 
                       modules, will be delivered as a second phase. During the third phase, it is proposed to deliver two 
                       detailed case studies, to ensure transfer, training and embedding of the tool in the SAPP participants."),
                       tags$p("The successful delivery of this project will see a scenario analysis type of tool being made available to 
                       country and regional electricity planners, financiers, developers, government and non-government 
                       bodies.  It will specifically enable and facilitate a visually interactive experience, containing a GIS 
                       graphical type interface, and require minimal training. The user will be able to gain valuable insights 
                       from comparisons and differences between different future scenarios around climatic and economic 
                       influences. This will empower decision makers and financiers to make better informed decisions which 
                       include the influences of climatic sustainability and economic dynamics.")
                            )
                       )
              ),
              ########################### STEP1 ##########################
             tabPanel("STEP 1",
                      #uiOutput("step1ui1"),
                      fluidRow(
                        column(3,tags$h5("STEP 1 - EVALUATE/CREATE POLICIES"),
                               "Create policies based on assuptions by changing the slider positions (Generation Expantion Plan runs UNCONSTRAINT), then when happy enter a 'Policy Name' and then click 'Create Policy'."
                               ),
                        column(2,sliderInput("d1water", "Water Availability % (Assumption)", 
                                             min(unique(runMasterdata[runMasterdata$policy_id==14,]$water.availability))*100, 
                                             max(unique(runMasterdata[runMasterdata$policy_id==14,]$water.availability))*100, 
                                             mean(unique(runMasterdata[runMasterdata$policy_id==14,]$water.availability))*100,
                                             10,
                                             animate=FALSE,ticks=FALSE,width = "100%")),
                        column(2,sliderInput("d1uclf", "Coal UCLF % (Assumption)", 
                                             min(unique(runMasterdata[runMasterdata$policy_id==14,]$coal.uclf))*100, 
                                             max(unique(runMasterdata[runMasterdata$policy_id==14,]$coal.uclf))*100, 
                                             max(unique(runMasterdata[runMasterdata$policy_id==14,]$coal.uclf))*100,
                                             10,
                                             animate=FALSE,ticks=FALSE)),
                        column(2,
                               sliderInput("d1uclf2", "Transmission UCLF % (Assumption)", 
                                             min(unique(runMasterdata[runMasterdata$policy_id==14,]$transmission.uclf))*100, 
                                             max(unique(runMasterdata[runMasterdata$policy_id==14,]$transmission.uclf))*100, 
                                             max(unique(runMasterdata[runMasterdata$policy_id==14,]$transmission.uclf))*100,
                                             10,
                                             animate=FALSE,ticks=FALSE) 
                        ),
                        column(1,checkboxInput("withoutGrandInga", "Without Grand Inga", FALSE)), 
                        column(2,
                               sliderInput("d1cons", "Consumption % Adjustment", 
                                           min(unique(runMasterdata[runMasterdata$policy_id==14,]$consumption.adjustment))*100, 
                                           max(unique(runMasterdata[runMasterdata$policy_id==14,]$consumption.adjustment))*100, 
                                           mean(unique(runMasterdata[runMasterdata$policy_id==14,]$consumption.adjustment))*100,
                                           10,
                                           animate=FALSE,ticks=FALSE)) 
                      ),
                      fluidRow(
                        column(3,
                               textInput("s1policyname", "Descriptive Policy Name for Assuptions:"),
                               bsAlert("s1alert"),
                               bsButton("s1createpolicy","Create Policy",style="primary"),
                               uiOutput("createlistui"),
                               fluidRow(
                                 column(8,uiOutput("s1info")),
                                 column(4,
                                    "Show/Delete selected Policy", tags$br(),   
                                    bsButton("s1loadpolicy","Show >",style="default")
                                  ,tags$br(),tags$br(),
                                  bsButton("s1deletepolicy","Delete",style="warning")
                                 )
                               ),
                               tags$h5("Create at least 2 Policies before proceeding to STEP2."),
                               bsButton("s1s2","Next (Step2) >>",style="primary")
                        ),
                        column(9,
                      bsCollapse(id="story",  open=c(#"EVALUATE: Flows, Map View (Unconstrained) - click on country to filter"
                        #"EVALUATE: New Capacity (Unconstrained)"
                                                  ),multiple=T,                                          
                          
                      bsCollapsePanel("EVALUATE: Flows, Map View (Unconstrained) - click on country to filter",style="info",                      
                                      
                                      fluidRow(
                                        column(12,                                                        
                                               conditionalPanel(
                                                 condition = "output.d1m1==null ",                                    
                                                 div(class = "busy",
                                                     p("Loading Map ..."),
                                                     img(src="ajaxloaderq.gif")
                                                 )
                                               ),
                                               leafletOutput("d1m1", width="90%",height=600),                               
                                               tags$div(style=" position: absolute;left: 70px;top: 500px;",
                                                        sliderInput("d1year", " Tx Flows Year", 2010, 2030, 2015,1,animate=list(loop=TRUE),ticks=FALSE)
                                               )
                                        )
                                        
                                      )                             
                      ),   
                      bsCollapsePanel("EVALUATE: New Capacity (Unconstrained)",  style="info",                                                              
                                      fluidRow(                                            
                                        column(12,     
                                               div(class='wrapper',tags$style(".highcharts{height: 100px, width: 300px}"),
                                                   showOutput("d1t1", "highcharts"))
                                        )
                                      )
                                      
                                      
                      ),
                     bsCollapsePanel("1.1) New Capacity (2 Water Availability Scenarios)",  style="info",                                                              
                                     fluidRow(                                            
                                       column(12,     
                                              div(class='wrapper',tags$style(".highcharts{height: 100px, width: 300px}"),
                                                  showOutput("demo1a", "highcharts"))
                                       ),
                                       column(12,     
                                              div(class='wrapper',tags$style(".highcharts{height: 100px, width: 300px}"),
                                                  showOutput("demo1b", "highcharts"))
                                       )
                                     )
                     ),
                     bsCollapsePanel("2.1) Average Price Difference",  style="info",
                                     fluidRow(      
                                       column(12,     
                                              div(class='wrapper',tags$style(".highcharts{height: 100px, width: 300px}"),
                                                  showOutput("demo2", "highcharts"))
                                       )
                                     )
                     ),
                     bsCollapsePanel("4.1) Fuel Cost vs Consumption (Contraint until 2020 with default design)",  style="info",                                                              
                                     fluidRow(                                            
                                       column(12,     
                                              div(class='wrapper',tags$style(".highcharts{height: 100px, width: 300px}"),
                                                  showOutput("demo3", "highcharts"))
                                       )
                                     )
                     ),
                     bsCollapsePanel("4.2) Cost vs Sensitivity (Contraint until 2020 with default design)",  style="info",                                                              
                                     fluidRow(                                            
                                       column(12,     
                                              div(class='wrapper',tags$style(".highcharts{height: 100px, width: 300px}"),
                                                  showOutput("demo4", "highcharts"))
                                       )
                                     )
                     ),
                     bsCollapsePanel("5.1) Average Price vs Water Availability (Contraint until 2020 with default design)",  style="info",                                                              
                                     fluidRow(                                            
                                       column(12,     
                                              div(class='wrapper',tags$style(".highcharts{height: 100px, width: 300px}"),
                                                  showOutput("demo5", "highcharts"))
                                       )
                                     )
                     ),
                     bsCollapsePanel("EVALUATE: Timeseries",style="info",                           
                                 showOutput("d1t2a", "highcharts"),
                                 showOutput("d1t2b", "highcharts"),
                                 showOutput("d1t2c", "highcharts"),
                                 showOutput("d1t2d", "highcharts")
                          )
                      ) 
                        )# col
                      )
                      
             ),
             ########################### STEP2 ##########################
             tabPanel("STEP 2",                        
                       fluidRow(
                        column(6,tags$h5("STEP 2 - SELECT/LOCK POLICY"),                                                                 
                               tags$h5("")),                                                                                                                      
                        column(6,
                               bsButton("s2s1","<< Back (Step1)",style="primary"),
                               bsButton("s2s3","Next (Step3) >>",style="primary")                               
                               )                        
                      ),
                      fluidRow(
                        column(5,
                               fluidRow(
                                 column(8,
                                   uiOutput("availlistui")
                                 ),
                                 column(4,
                                   uiOutput("s2info")
                                 )
                               )),
                        column(4,tags$span(tags$br(),
                                           bsButton("punlockbase", "<<UNLOCK",style="danger"),
                                           bsButton("plockbase", "LOCK as Baseline>>",style="warning"),
                                           tags$br(),tags$br(),
                                           bsButton("punlockscen", "<<UNLOCK",style="danger"),
                                           bsButton("plockscen", "LOCK as Scenario>>",style="warning")
                                           )),
                        column(3, uiOutput("lockedlistui"))
                      ),
                      bsAlert("s2alert"),
                      conditionalPanel(
                        #condition = "(input.lockedbaseline=='NONE') || (input.lockedscenario=='NONE') ",  
                        condition = "(1==1)",  
                      #################################                                        
                      tabsetPanel(id="story2",  selected="New Capacity (Constrained) for Selected",    type="pills",                      
                        
                        tabPanel("New Capacity (Constrained) for Selected", 
                                        fluidRow(                                            
                                          column(12,     
                                                 div(class='wrapper',tags$style(".highcharts{height: 100px, width: 300px}"),
                                                     showOutput("d2t1", "highcharts"))                                                              
                                          )
                                        )
#                                         fluidRow(                                            
#                                           column(4,sliderInput("d2water", "Water Availability (Constraint until Fixed Year)", 
#                                                                min(unique(runMasterdata[runMasterdata$policy_id==3,]$water.availability))*100, 
#                                                                max(unique(runMasterdata[runMasterdata$policy_id==3,]$water.availability))*100, 
#                                                                min(unique(runMasterdata[runMasterdata$policy_id==3,]$water.availability))*100,
#                                                                20,
#                                                                animate=FALSE,ticks=FALSE)),
#                                           column(4,sliderInput("d2uclf", "Coal UCLF (Constraint until Fixed Year)", 
#                                                                min(unique(runMasterdata[runMasterdata$policy_id==3,]$coal.uclf))*100, 
#                                                                max(unique(runMasterdata[runMasterdata$policy_id==3,]$coal.uclf))*100, 
#                                                                min(unique(runMasterdata[runMasterdata$policy_id==3,]$coal.uclf))*100,
#                                                                20,
#                                                                animate=FALSE,ticks=FALSE)),
#                                           column(4,sliderInput("d2uclf2", "Transmission UCLF (Constraint until Fixed Year)", 
#                                                                min(unique(runMasterdata[runMasterdata$policy_id==3,]$transmission.uclf))*100, 
#                                                                max(unique(runMasterdata[runMasterdata$policy_id==3,]$transmission.uclf))*100, 
#                                                                min(unique(runMasterdata[runMasterdata$policy_id==3,]$transmission.uclf))*100,
#                                                                20,
#                                                                animate=FALSE,ticks=FALSE))
#                                         )
                        ),
                        tabPanel("Pivot Table for Selected (Policy)", 
                          fluidRow(
                            column(4,
                                selectInput("d2pivotts", "Filter Time Series:",
                                    as.character(unique(series1$series))
                                    ,multiple=T,selected="New Capacity")     ),                            
                            column(4,
                                ""),                          
                            column(4,
                                "")
                          ),                                                  
                          fluidRow(
                            column(12,     
                              conditionalPanel(
                                condition = "output.d2pivot==null ",                                    
                                div(class = "busy",
                                  p("Loading Pivot ..."),
                                  img(src="ajaxloaderq.gif")
                                )
                            ),
                          rpivotTableOutput("d2pivot",width="50%")  
                  
                          ) 
                        )     
                      )

                      ) 
                      #####
                    ) # Conditional
                      
                      
             ),
             ########################### STEP3 ##########################
             tabPanel("STEP 3",
                      fluidRow(
                        column(2,
                               tags$h5("STEP 3 - CHECK SENSITIVITIES"),
                               bsButton("s3s2","<< Back (Step2)",style="primary"),
                               bsButton("s3s4","Help",style="primary")
                               
                        ),
                        #column(1, "Fixed Year: 2020 (Keep Centralized Generation Fixed until this year)"),
                        column(2,sliderInput("d3water", "Water Availability % (Assumption)", 
                                             min(unique(runMasterdata[runMasterdata$policy_id==15,]$water.availability))*100, 
                                             max(unique(runMasterdata[runMasterdata$policy_id==15,]$water.availability))*100, 
                                             mean(unique(runMasterdata[runMasterdata$policy_id==15,]$water.availability))*100,
                                             10,
                                             animate=FALSE,ticks=FALSE,width = "100%")),
                        column(2,sliderInput("d3uclf", "Coal UCLF % (Assumption)", 
                                             min(unique(runMasterdata[runMasterdata$policy_id==15,]$coal.uclf))*100, 
                                             max(unique(runMasterdata[runMasterdata$policy_id==15,]$coal.uclf))*100, 
                                             max(unique(runMasterdata[runMasterdata$policy_id==15,]$coal.uclf))*100,
                                             10,
                                             animate=FALSE,ticks=FALSE)),
                        column(2,
                               sliderInput("d3uclf2", "Transmission UCLF % (Assumption)", 
                                           min(unique(runMasterdata[runMasterdata$policy_id==15,]$transmission.uclf))*100, 
                                           max(unique(runMasterdata[runMasterdata$policy_id==15,]$transmission.uclf))*100, 
                                           max(unique(runMasterdata[runMasterdata$policy_id==15,]$transmission.uclf))*100,
                                           10,
                                           animate=FALSE,ticks=FALSE) 
                        ),
                        column(2,checkboxInput("d3withoutGrandInga", "Without Grand Inga", FALSE)), 
                        column(2,
                               sliderInput("d3cons", "Consumption % Adjustment", 
                                           min(unique(runMasterdata[runMasterdata$policy_id==15,]$consumption.adjustment))*100, 
                                           max(unique(runMasterdata[runMasterdata$policy_id==15,]$consumption.adjustment))*100, 
                                           mean(unique(runMasterdata[runMasterdata$policy_id==15,]$consumption.adjustment))*100,
                                           10,
                                           animate=FALSE,ticks=FALSE)
                               )
                        
                        
                        
                      ),
                      tags$span("Test Sensitivities on Baseline and Scenario, if we keep centralized generation CONSTRAINT using the Original selected expansion plans (Step 1 and 2) up until the year 2020, 
                                    whereafter the model runs again. "),
                      bsCollapse(id="story3",  open=c("CHECK: Map View and Tx Energy Flows - click on country to select"),
                        #"CHECK: Sensitivity"),
                        multiple=T,                                 
                        bsCollapsePanel("CHECK: Map View and Tx Energy Flows - click on country to select",style="info",                                                              
                                        fluidRow(
                                          column(12,                                                        
                                                 conditionalPanel(
                                                   condition = "output.d3m1==null ",                                    
                                                   div(class = "busy",
                                                       p("Loading Map ..."),
                                                       img(src="ajaxloaderq.gif")
                                                   )
                                                 ),
                                                 leafletOutput("d3m1", width="80%",height=600),                               
                                                 tags$div(style=" position: absolute;left: 70px;top: 350px;",
                                                          sliderInput("d3year", " Tx Flows Year", 2010, 2030, 2015,1,animate=list(loop=TRUE),ticks=FALSE),
                                                          uiOutput("d3m1type")
                                                          
                                                 )
                                          )                                          
                                        )

                        ),   
                        
                        bsCollapsePanel("CHECK: Sensitivity",style="info",  
                                        tabsetPanel(id="sensnav",  type="pills", 
                                        
                                            tabPanel("Total Cost to variability in Actual Water",        
                                              conditionalPanel(
                                                condition = "output.d3pivot1==null ",                                    
                                                div(class = "busy",
                                                   p("Loading Pivot ..."),
                                                   img(src="ajaxloaderq.gif")
                                                )
                                              ),
                                              rpivotTableOutput("d3pivot1",width="50%")
                                            ),
                                            
                                            tabPanel("Fuel Cost per Fuel Type in Actual Water",
                                                     conditionalPanel(
                                                       condition = "output.d3pivot2==null ",                                    
                                                       div(class = "busy",
                                                           p("Loading Pivot ..."),
                                                           img(src="ajaxloaderq.gif")
                                                       )
                                                     ),
                                                     rpivotTableOutput("d3pivot2",width="50%")
                                            ),
                                            tabPanel("Total Cost to variability in Actual Coal UCLF",
                                                     conditionalPanel(
                                                       condition = "output.d3pivot3==null ",                                    
                                                       div(class = "busy",
                                                           p("Loading Pivot ..."),
                                                           img(src="ajaxloaderq.gif")
                                                       )
                                                     ),
                                                     rpivotTableOutput("d3pivot3",width="50%")
                                                     ),
                                            tabPanel("Compare New Capacity",
                                                     conditionalPanel(
                                                       condition = "output.d3pivot4==null ",                                    
                                                       div(class = "busy",
                                                           p("Loading Pivot ..."),
                                                           img(src="ajaxloaderq.gif")
                                                       )
                                                     ),
                                                     rpivotTableOutput("d3pivot4",width="50%")
                                            )
                                            
                                        )
                        )
                    
                        
              
                        #bsAlert("s3alert")
#                         bsCollapsePanel("PIVOT",  style="info",                                                              
#                                                 
#                                         fluidRow(
#                                           column(12,
#                                                  bsAlert("s3alert"),
#                                       ######################################           
#                                       tabsetPanel(id="story3",  selected="New Capacity",    type="pills",                      
#                                           tabPanel("New Capacity", 
#                                                  
#                                           )
#                                       )       
#                                       #####################################           
#                                           ) 
#                                         )
#                         )
                        
                      )  
             ),
#             ########################### STEP4 ##########################
              tabPanel("HELP",                        
                      
                       fluidRow(
                         column(12,"")
                       )
              ),
              tabPanel("Input Data Explorer",                        
                       sidebarPanel(
                         fluidRow(
                           column(12,
                                  conditionalPanel(
                                    condition = "output.tree==null ",                                    
                                    div(class = "busy",
                                        p("Drawing Hierarchy..."),
                                        img(src="ajaxloaderq.gif")
                                    )
                                  ),      
                                  verbatimTextOutput("treesel"),
                                  shinyTree("tree",search=T)
                           )
                         ),
                         fluidRow(
                           column(6,"Drill down and select a technology.")                                                                          
                         )                                
                        
                         ,width=4
                       ),
                       mainPanel(
                         
                         fluidRow(
                           column(4,
                                  leafletOutput("map1", width="100%", height="300px"),
                                  conditionalPanel(
                                    condition = "$('html').hasClass('shiny-busy')",    
                                    div(class = "busy",
                                        p("Calculation in progress.."),
                                        img(src="ajaxloaderq.gif")
                                    )
                                  )
                           ),
                           column(8,  
                                  
                                  DT::dataTableOutput('x1')
                           )
                         ),
                
                         fluidRow(
                           
                           column(12,
                                  showOutput("timeseries", "highcharts")
                           )                                                     
                         ),
                         fluidRow(
                           column(12,
                                  DT::dataTableOutput('x5')
                           )                                                     
                         )
                         
                         ,width=8)
                
              )

  ) # navbar
  
  
  
))
