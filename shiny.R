suppressMessages({
   require(shiny)
   require(shinyjs)
   require(shinydashboard)
   require(leaflet)
})
source("./main.R")
height <- c("600px","248px","165px")
sliderWidth <- c("100%","125%")[2]
epsgList <- 3571:3576
'plotEmpty' <- function(desc="Simulation has not been done yet") {
   plot(0,0,type="n",axes=FALSE,xlab="",ylab="")
   text(0,0,desc)
}
'shapefile' <- function(fname) {
   if (isZip <- length(grep("\\.zip$",basename(fname)))>0) {
      list1 <- unzip(fname)
      ret <- sf::st_read(list1[grep("\\.shp$",list1)],quiet=TRUE)
      file.remove(list1)
   }
   else
      ret <- sf::st_read(fname,quiet=TRUE)
   if (FALSE)
      return(ret)
   sf::st_zm(ret)
}
'get_iucn_map' <- function() {
   pb <- shapefile("IUCN_pb_subpopulations.shp.zip")[,c("POPID","POP")]
   pb <- subset(pb,POPID>0)
   iucn <- readxl::read_excel("IUCN_pb_subpopulations.xlsx")
   iucn <- iucn[sample(nrow(iucn)),]
   ind <- match(iucn$POPID,pb$POPID)
   sf::st_geometry(iucn) <- sf::st_geometry(pb)[ind]
   sf::st_transform(iucn,4326)
}
iucn_map <- get_iucn_map()
'polarmap' <- function(epsg,centered=TRUE) {
   if (is.character(epsg))
      epsg <- as.integer(epsg)
   extent <- 11000000 + 9036842.762 + 667
   origin <- c(-extent, extent)
   maxResolution <- 2*extent/256
   bounds <- list(c(-extent,extent),c(extent,-extent))
   resolutions <- purrr::map_dbl(0:18,function(x) maxResolution/(2^x))
   crsArtic <- leafletCRS(crsClass="L.Proj.CRS",code=paste0("EPSG:",epsg)
                         ,proj4def=sf::st_crs(epsg)$proj4string
                         ,resolutions=resolutions,origin=origin,bounds=bounds)
   m <- leaflet(options=leafletOptions(crs=crsArtic,minZoom=3,maxZoom=9))
   if (centered) {
      if (epsg==3575)
         m <- setView(m,-110,80,4)
      else if (epsg==3576)
         m <- setView(m,-100,82,4)
      else if (epsg==3574)
         m <- setView(m,-40,87,4)
      else if (epsg==3573)
         m <- setView(m,-100,77,4)
      else if (epsg==3572)
         m <- setView(m,-78,78,4)
      else if (epsg==3571)
         m <- setView(m,-120,80,4)
      else if (TRUE)
         m <- setView(m,0,90,4)
        # m <- setView(m,12.57,55.687,12) ## Kopenhagen
   }
   m <- addTiles(m,urlTemplate=paste0("http://{s}.tiles.arcticconnect.ca/osm_"
                                     ,epsg,"/{z}/{x}/{y}.png")
                ,attribution="Map: © ArcticConnect. Data: © OpenStreetMap contributors"
                ,options=tileOptions(subdomains="abc"
                                    ,noWrap=TRUE,continuousWorld=FALSE)) 
  # if (!is.null(data))
  #    m <- addPolygons(m,data=data,weight=0.5)
  # m <- addGraticule(m,interval=10)
   m
}
ui <- dashboardPage(skin = "blue"
   ,dashboardHeader(title="Polar Bear Demography",disable=TRUE,titleWidth=350)
   ,dashboardSidebar(NULL
      ,collapsed=!TRUE
      ,disable=TRUE
     # ,width = 350
      ,sidebarMenu(id="tabs"
        # ,HTML("<center>"),h4("Polar Bear Demography"),HTML("</center>")
        # ,menuItem("Input", tabName = "tabInput", icon = icon("align-left"))
        # ,actionLink("reset", "Reset inputs",icon=icon("angle-double-right"))
         ##~ ,actionLink("randomize1","Randomize scenario")
         ##~ ,actionLink("randomize2","Randomize simulation")
         ##~ ,actionLink("simulate", "Simulate",icon=icon("angle-double-down"))
        # ,menuItem("Output", tabName = "summary", icon = icon("table"))
        # ,actionLink("itemize", "Itemize",icon=icon("angle-double-down"))
        # ,menuItem("Projection", tabName = "conclusion", icon = icon("table"))
         ,menuItem("Main", tabName = "main", icon = icon("table"))
         ,menuItem("About", tabName = "about2", icon = icon("info-circle"))
         ,menuItem("Source code", icon = icon("file-code-o"), 
                   href = "https://github.com/nplatonov/demography/")
        # ,menuItem("Predefined", tabName = "predefined", icon = icon("align-justify")
        #          ,startExpanded = FALSE
        #    ,menuSubItem("Barents Sea", "BS", icon = NULL)
        #    ,menuSubItem("Chukchi Sea", "CS", icon = NULL)
        # )
        # ,actionButton("close", "Close")
         ,br()
         ,br()
         ,br()
         ,br()
         ,HTML("<center>")
         ,img(src="http://www.sevin.ru/menues1/institute_logo.gif",width=20)
         ,HTML("</center>")
      )
   )
   ,dashboardBody(id = "resetable"
     # ,tags$script(HTML("$('body').addClass('sidebar-mini');"))
      ,tabItems(
         tabItem(tabName="about2"
            ,actionLink("gotomainTop", "Simulation",icon=icon("angle-double-left"))
            ,br()
            ,includeMarkdown("about.md")
            ,br()
            ,actionLink("gotomainBottom", "Simulation",icon=icon("angle-double-left"))
         )
         ,tabItem(tabName="main"
            ,tags$head(NULL
               ,HTML(
                 '<link href="https://fonts.googleapis.com/css?family=Roboto" rel="stylesheet"/>
                  <link href="https://fonts.googleapis.com/css?family=Raleway" rel="stylesheet"/>
                  <link rel="icon" type="image/png" sizes="32x32" href="/favicon-32x32.png">
                  <link rel="icon" type="image/png" sizes="16x16" href="/favicon-16x16.png">
                  <style>
                     .main-header .logo {
                        font-family: "Roboto","Raleway", Times, "Times New Roman", serif;
                       /* font-weight: bold; */
                        font-size: 18px;
                     }
                     .skin-blue .main-header .logo {
                        background-color: #3c8dbc;
                     }
                     .skin-blue .main-header .logo:hover {
                        background-color: #3c8dbc;
                     }                
                     aside, .main-sidebar {
                        background-color: #f00;
                     }
                     .action-button {
                        font-size: 1.1em;
                        margin-left: 0px;
                        margin-right: 0px;
                     }
                     .simulate-button {
                       /* font-size: 1.3em; */
                        border: 0px solid green;
                        background-color: #428BCAAF;
                        color: #fff;
                        padding: 3px;
                        border-radius: 0.3em !important;
                     }
                     .simulate-button:hover {
                        color: #428BCA;
                        background-color: #fff;
                     }
                     #menuhead {
                        width:600px;
                     }
                     .zzzemptymargin {
                        font-size: 0.3em;
                     }
                     .nav > li > a {
                        padding: 8px 12px;
                     }
                  </style>'
               )
            )
            ,fluidRow(NULL
              # ,column(1
              #    ,img(src="http://www.sevin.ru/menues1/institute_logo.gif",width=20)
              # )
              # ,box(NULL,id="menuhead",width=10
                  ,column(1
                    # ,img(src="http://www.sevin.ru/menues1/institute_logo.gif"
                    #     ,width=16)
                    # ,actionLink("about3",""
                    #            ,icon=icon("info-circle"))
                  )
                  ,column(2
                    # ,actionLink("randomize1","Randomize scenario"
                    #            ,icon=icon("angle-double-down"))
                  )
                  ,column(2
                    # ,actionLink("simulate", "Simulate"
                    #            ,icon=icon("angle-double-right")
                    #            ,class="simulate-button"
                    #            )
                  )
                  ,column(2
                    # ,actionLink("randomize2","Randomize simulation"
                    #            ,icon=icon("angle-down"))
                  )
                  ,column(2
                    # ,actionLink("sensitive","Improve"
                    #            ,icon=icon("angle-right"))
                  )
              # )
            )
           # ,fluidRow(NULL,class="emptymargin"
           #    ,br()
           # )
            ,fluidRow(NULL
               ,tabBox(width=12,title = "",id = "tabset1", selected=c("Inputs","Map")[2]
                  ,tabPanel(title="",value="Map",icon=icon("globe")
                     ,fluidRow(NULL
                        ,column(2)
                        ,column(8
                           ,uiOutput("iucn")
                        )
                        ,column(2
                           ,selectInput("epsg","Projection (EPSG code)",epsgList
                                       ,selected=3573)#sample(epsgList,1))
                           ,selectInput("iucn_prm","Parameter"
                                       ,grep(attr(iucn_map,"sf_column")
                                            ,colnames(iucn_map)
                                            ,value=TRUE,invert=TRUE)
                                       ,selected=grep(c("Ecoregion","(COY.*litter|litter.*COY)")[1]
                                                     ,colnames(iucn_map),ignore.case=TRUE,value=TRUE))
                        )
                     )
                  )
                  ,tabPanel(title="",value="Inputs",icon=icon("edit")
                     # width=4,
                     ,fluidRow(NULL
                        ,column(3
                          # ,plotOutput("plotPreAnalysis",height=height[2])
                           ,uiOutput("uiPreAnalysis",height=height[2])#,inline=TRUE)
                           ,fluidRow(NULL
                              ,column(8
                                 ,actionLink("simulate", "Simulate"
                                            ,icon=icon("angle-double-right")
                                            ,class="simulate-button"
                                            )
                              )
                              ,column(2)
                           )
                           ,fluidRow(NULL
                              ,column(12,"Random seeds:")
                           )
                           ,fluidRow(NULL
                              ,column(4
                                 ,actionLink("randomize1","Scenario's")
                                 ,numericInput("seed1"
                                    ,"",min=100,max=999
                                    ,value=init$seed1,step=1,width="80px"
                                 )
                              )
                              ##~ ,column(2
                                 ##~ ,textOutput("seed1")
                              ##~ )
                              ,column(4
                                 ,actionLink("randomize2","Simulation's")
                                 ,numericInput("seed2"
                                    ,"",min=100,max=999
                                    ,value=init$seed2,step=1,width="80px"
                                 )
                              )
                              ,column(4
                                 ,""
                                 ,actionLink("randomize2","Perturbation's")
                                 ,numericInput("seed3"
                                    ,"",min=100,max=999
                                    ,value=-1,step=1,width="80px"
                                 )
                              )
                           )
                          # ,br()
                          # ,actionLink("analysis6", "Analyze"
                          #              ,icon=icon("angle-double-right"))
                           ,sliderInput("max.age", "Max age, years"
                                       ,width=sliderWidth
                                       ,min=25, max=40, value=init$max.age,step=1
                                       )
                        )
                        ,column(3
                           ,sliderInput("sexratio", "COY sex ratio (% of females)"
                                       ,width=sliderWidth
                                       ,min=20, max=80,value=init$sexratio,step=0.1
                                       )
                           ,sliderInput("init.den", "Initial number of dens"
                                       ,width=sliderWidth
                                       ,min=10, max=200,value=init$init.den,step=1,sep=""
                                       )
                           ,sliderInput("fertility", "Age specific fertility"
                                       ,width=sliderWidth
                                       ,min=0, max=1,value=init$fertility,step=0.01,sep=""
                                       )
                           ,sliderInput("removal.age","Age specific human-caused removal"
                                       ,width=sliderWidth
                                       ,min=0,max=1,value=init$removal.age,step=0.01
                                       )
                           ,sliderInput("broken.C1", "Broken yearling families"
                                       ,width=sliderWidth
                                       ,min=0, max=1
                                       ,value=init$indep.fraction[2],step=0.01
                                       )
                          # ,textOutput("indep.C1")
                        )
                        ,column(3
                           ,sliderInput("removal.rate","2M/1F human-caused removal rate"
                                       ,width=sliderWidth
                                       ,min=0,max=0.1,value=init$removal.rate,step=0.001
                                       )
                           ,sliderInput("litter", "Litter size"
                                       ,width=sliderWidth
                                       ,min=1.0, max=2.5,value=init$litter,step=0.01
                                       )
                           ,fluidRow(NULL
                              ,column(10,"Litter size proportions:")
                           )
                           ,fluidRow(NULL
                              ,column(4,numericInput("C1", "1C",init$litterF[1]
                                                    ,step=0.001,width="80px"))
                              ,column(4,numericInput("C2", "2C",init$litterF[2]
                                                    ,step=0.001,width="80px"))
                              ,column(4,numericInput("C3", "3C",init$litterF[3]
                                                    ,step=0.001,width="80px"))
                              ,column(4)
                           )
                           ,textOutput("mort.C1.depend")
                           ,textOutput("mort.C1.indep")
                           ,textOutput("mort.C2.depend")
                           ,textOutput("mort.C2.indep")
                           ,br()
                           ,sliderInput("pregnant", "Birth success"
                                       ,width=sliderWidth
                                       ,min=0.2, max=1.0,value=init$pregnant
                                       ,step=0.01
                                       )
                        )
                        ,column(3
                           ,sliderInput("mortality.cub", "Dependent COY mortality"
                                       ,width=sliderWidth
                                       ,min=0.2, max=0.5,value=init$mortality.cub
                                       ,step=0.01
                                       )
                           ,sliderInput("mortality.adult", "Adult mortality",
                                       ,width=sliderWidth
                                       ,min=0.01, max=0.15,value=init$mortality.adult
                                       ,step=0.005
                                       )
                           ,sliderInput("k1d", "Mortality rate of dependent young"
                                       ,width=sliderWidth
                                       ,min=1,max=20,value=init$k1d,step=0.1
                                       )
                           ,sliderInput("k1i", "Mortality rate of independent youngs",
                                       ,width=sliderWidth
                                       ,min=1,max=20, value=init$k1i,step=0.1
                                       )
                           ,sliderInput("k2", "Mortality rate of aging"
                                       ,width=sliderWidth
                                       ,min=1, max=20, value=init$k2,step=0.1
                                       )
                        )
                     )
                     ##~ ,fluidRow(
                        ##~ column(6,
                           ##~ sliderInput("adult", "Adult age range (DEPRECATED)",
                                       ##~ min=3, max=40, value=c(6,25),step=1
                                      ##~ )
                        ##~ ),
                     ##~ ),

                  ) ## tabPanel
                  ,tabPanel(title="",value="Check",icon=icon("eye")
                    # width=4,
                    # status = "warning", solidHeader = !TRUE,
                    # collapsible = !TRUE,collapsed=!FALSE,
                    # height = "350px",
                     ,fluidRow(NULL
                        ,column(2
                             # ,actionLink("simulate3","Simulate"
                             #            ,icon=icon("angle-double-right"))
                        )
                        ,column(4,
                           plotOutput("curve.lin",height=height[2])
                        )
                        ,column(4,
                           plotOutput("curve.log",height=height[2])
                        )
                        ,column(2)
                     )
                     ,fluidRow(NULL
                        ,column(2)
                        ,column(4
                           ,plotOutput("curve.fert",height=height[2])
                        )
                        ,column(4
                           ,plotOutput("curve.removal",height=height[2])
                        )
                        ,column(2)
                     )
                  )
                  ,tabPanel(title="",value="Results",icon=icon("signal")
                     ,fluidRow(NULL
                        ,column(3
                           ,plotOutput("plotPopSize",height=height[2])
                           ,plotOutput("plotAgeStructure",height=height[2])
                        )
                        ,column(9
                           ,fluidRow(NULL
                              ,column(4
                                 ,plotOutput("plotCubs",height=height[2])
                              )
                              ,column(3
                                 ,plotOutput("plotDens",height=height[2])
                              )
                              ,column(3
                                 ,plotOutput("plotAdults",height=height[2])
                              )
                              ,column(2
                                 ,plotOutput("plotInterbirth",height=height[2])
                              )
                           )
                           ,fluidRow(NULL
                              ,column(4
                                 ,plotOutput("plotSurvival",height=height[2])
                              )
                              ,column(3
                                 ,plotOutput("plotLitterSize",height=height[2])
                              )
                              ,column(3
                                 ,plotOutput("plotLitterProduction",height=height[2])
                              )
                           )
                        )
                     )
                  )
                  ##~ ,tabPanel("",value="Verbatim",icon=icon("terminal")
                     ##~ ,verbatimTextOutput("interim")
                  ##~ )
                  ,tabPanel(title="",value="Sensitive",icon=icon("cogs")
                     ,fluidRow(NULL
                        ,column(12
                           ,fluidRow(NULL
                              ,column(3
                                 ,plotOutput("plotSensitivity1",height=height[2])
                                 ,plotOutput("plotSensitivity2",height=height[2])
                              )
                              ,column(3
                                 ,plotOutput("plotSensitivity3",height=height[2])
                                 ,plotOutput("plotSensitivity4",height=height[2])
                              )
                              ,column(3
                                 ,plotOutput("plotSensitivity5",height=height[2])
                                 ,plotOutput("plotSensitivity6",height=height[2])
                              )
                           )
                        )
                     )
                  )
                  ,tabPanel(title="",value="Interpretation",icon=icon("desktop")
                     ,fluidRow(NULL 
                       # ,box(width=12
                           ,column(2)
                           ,column(8
                              ,uiOutput("markdown")
                              ,br()
                              ,uiOutput("downloadHTML")
                              ,uiOutput("downloadPDF")
                              ,uiOutput("downloadNote")
                           )
                           ,column(2)
                       # )
                     )
                  )
                  ,tabPanel(title="",value="About",icon=icon("info-circle")
                     ,includeMarkdown("about.md")
                  )
                 # ,tabPanel(title="",value="about",icon=icon("circle-info")
                     #,actionLink("gotomainTop", "Simulation",icon=icon("angle-double-left"))
                     #,br()
                 #    ,includeMarkdown("about.md")
                     #,br()
                     #,actionLink("gotomainBottom", "Simulation",icon=icon("angle-double-left"))
                 # )
               ) ## tabBox
            ) ## fluidRow
         )
      )
   ) ## dashboardBody
)

server <- function(input, session, output) {
   session$onSessionEnded(stopApp)
   observeEvent(input$close, {
      js$closeWindow()
      stopApp()
   })
   output$seed1 <- renderText(input$seed1)
   output$seed2 <- renderText(input$seed2)
   observeEvent(input$litter, {
     # message("*** 'litter' observe")
      p <- litterFraction(input$litter)
     # print(litter)
      updateNumericInput(session,"C1",value=sprintf("%.3f",p[1]))
      updateNumericInput(session,"C2",value=sprintf("%.3f",p[2]))
      updateNumericInput(session,"C3",value=sprintf("%.3f",p[3]))
   })
   observeEvent(input$init.den.text, {
      message("text init.den")
      opW <- options(warn=0)
      s <- as.integer(input$init.den.text)
      options(opW)
      s[is.na(s)] <- 100
      s[s<10] <- 10
      s[s>400] <- 400
      updateNumericInput(session,"init.den.text",value=s)
   })
   output$mort.C1.depend <- renderText({
      m <- params()$mortality
      sprintf("Dependent yearling mortality: %.3f",m[2])
   })
   output$mort.C2.depend <- renderText({
      m <- params()$mortality
      sprintf("Dependent sub-adult mortality: %.3f",m[3])
   })
   output$mort.C1.indep <- renderText({
      m <- params()$indep.mortality
      sprintf("Independent yearling mortality: %.3f",m[2])
   })
   output$mort.C2.indep <- renderText({
      m <- params()$indep.mortality
      sprintf("Independent sub-adult mortality: %.3f\n",m[3])
   })
   output$indep.C1 <- renderText({
     # m <- params()$
      sprintf("Independent yearling ratio: %.3f\n",runif(1))
   })
   observeEvent(input$C1, {
     # message("*** 'C1' observe")
      s <- as.numeric(c(input$C1,input$C2,input$C3))
      s[is.na(s)] <- 0
      s[s<0] <- 0
      s[s>0.9] <- 0.9
      if (all(s==0))
         s <- c(0.6,0.4,0)
      s[2] <- 1-s[3]-s[1]
      updateSliderInput(session,"litter",value=round(sum(seq(3)*s),3))
      s <- round(s,3)
      updateNumericInput(session,"C1",value=s[1])
      updateNumericInput(session,"C2",value=s[2])
      updateNumericInput(session,"C3",value=s[3])
   })
   observeEvent(input$C2, {
     # message("*** 'C2' observe")
      s <- as.numeric(c(input$C1,input$C2,input$C3))
      s[is.na(s)] <- 0
      s[s<0] <- 0
      s[s>0.9] <- 0.9
      if (all(s==0))
         s <- c(0.6,0.4,0)
      s[1] <- 1-s[3]-s[2]
      updateSliderInput(session,"litter",value=round(sum(seq(3)*s),3))
      s <- round(s,3)
      updateNumericInput(session,"C1",value=s[1])
      updateNumericInput(session,"C2",value=s[2])
      updateNumericInput(session,"C3",value=s[3])
   })
   observeEvent(input$C3, {
     # message("*** 'C3' observe")
      s <- as.numeric(c(input$C1,input$C2,input$C3))
      s[is.na(s)] <- 0
      s[s<0] <- 0
      s[s>0.9] <- 0.9
      if (all(s==0))
         s <- c(0.6,0.4,0)
      ind <- c(1,2)
      s[ind] <- s[ind]/sum(s[ind])
      s[ind] <- s[ind]*(1-s[-ind])
      updateSliderInput(session,"litter",value=round(sum(seq(3)*s),3))
      s <- round(s,3)
      updateNumericInput(session,"C1",value=s[1])
      updateNumericInput(session,"C2",value=s[2])
      updateNumericInput(session,"C3",value=s[3])
   })
   params <- reactive({
      message("*** reactive *** ")
      ret <- curveInputs(indep.mortality=input$indep.mortality
                        ,mortality.cub=input$mortality.cub
                        ,mortality.adult=input$mortality.adult
                        ,init.den=input$init.den
                        ,litter=input$litter
                        ,broken.C1=input$broken.C1
                        ,max.age=input$max.age
                        ,pregnant=input$pregnant
                        ,sexratio=input$sexratio
                        ,seed1=input$seed1
                        ,seed2=input$seed2
                        ,fertility=input$fertility
                        ,removal.rate=input$removal.rate
                        ,removal.age=input$removal.age
                        ,k1d=input$k1d
                        ,k1i=input$k1i
                        ,k2=input$k2
                        )
      ret
   })
  # observeEvent(input$randomize1, {
  #   # resetAnalysis()
  #    updateTabsetPanel(session,"tabset1",selected="Inputs")
  #   # updateTabItems(session,"tabs","tabInput")
  # })
  # observeEvent(input$randomize2, {
  #    updateTabsetPanel(session,"tabset1",selected="Inputs")
  #   # updateTabItems(session,"tabs","tabInput")
  # })
  # observeEvent(input$simulate, {
  #   # message("*** Simulate (observe), goto results")
  #   # updateTabItems(session,"tabs","main")
  #    updateTabsetPanel(session,"tabset1",selected="Inputs")
  #   # updateTabsetPanel(session,"tabset1",selected="Results")
  # })
   observeEvent(input$simulate3, {
      updateTabsetPanel(session,"tabset1",selected="Inputs")
   })
   result.dev <- eventReactive(input$simulate,{
  # result <- reactive({
      message("*** Simulate (reactive)")
     # updateTabsetPanel(session,"tabset1",selected="Results") ## NOT WORKING
      ##~ res <- with(rv,simulate(max.age=max.age
                             ##~ ,litter=litter
                             ##~ ,sexratio=sexratio
                             ##~ ,init.den=init.den
                             ##~ ,pregnant=pregnant
                             ##~ ,mortality.cub=mortality.cub
                             ##~ ,mortality.adult=mortality.adult
                             ##~ ,indep.fraction=indep.fraction
                             ##~ ,fertility=fertility
                             ##~ ,removal.rate=removal.rate
                             ##~ ,removal.age=removal.age
                             ##~ ,k1d=k1d
                             ##~ ,k1i=k1i
                             ##~ ,k2=k2
                             ##~ ,seed1=seed1
                             ##~ ,seed2=seed2
                             ##~ ))
      rv <- params()
     # print(c(rv=rv$seed2,input=input$seed2))
      res <- do.call("simulate",rv)
     # updateTabsetPanel(session,"restab",selected="Verbatim") ## NOT WORKING
      res
   })
   result.anytimecalled <- reactive({
      ret <- do.call("simulate",params())
      ret
   })
   result <- eventReactive(input$simulate,{
      message("*** Simulate (reactive)")
      do.call("simulate",params())
   })
  # resultUpdate <- reactive({
  #    message("*** result update ***")
  #    result()
  # })
   observeEvent(input$seed1, {
      message("*** seed1")
      res <- randomize(seed1=input$seed1,seed2=NA)
      updateNumericInput(session,"seed1",value=res$seed1)
      updateNumericInput(session,"seed2",value=res$seed2)
      updateSliderInput(session,"max.age",value=res$max.age)
      updateSliderInput(session,"litter",value=res$litter)
      updateSliderInput(session,"sexratio",value=res$sexratio)
      updateSliderInput(session,"init.den",value=res$init.den)
      updateSliderInput(session,"pregnant",value=res$pregnant)
      updateSliderInput(session,"broken.C1",value=res$indep.fraction[2])
      updateSliderInput(session,"mortality.cub",value=res$mortality.cub)
      updateSliderInput(session,"mortality.adult",value=res$mortality.adult)
      updateSliderInput(session,"fertility",value=res$fertility)
      updateSliderInput(session,"removal.rate",value=res$removal.rate)
      updateSliderInput(session,"removal.age",value=res$removal.age)
      updateSliderInput(session,"k1d",value=res$k1d)
      updateSliderInput(session,"k1i",value=res$k1i)
      updateSliderInput(session,"k2",value=res$k2)
   })
   observeEvent(input$randomize1, {
      showNotification(closeButton=FALSE,duration=1
                      ,paste("Previous scenario:",input$seed1))
      message("*** Randomize scenario")
      res <- randomize(seed1=NA,seed2=NA)
      updateNumericInput(session,"seed1",value=res$seed1)
      updateNumericInput(session,"seed2",value=res$seed2)
      updateSliderInput(session,"max.age",value=res$max.age)
      updateSliderInput(session,"litter",value=res$litter)
      updateSliderInput(session,"sexratio",value=res$sexratio)
      updateSliderInput(session,"init.den",value=res$init.den)
      updateSliderInput(session,"pregnant",value=res$pregnant)
      updateSliderInput(session,"broken.C1",value=res$indep.fraction[2])
      updateSliderInput(session,"mortality.cub",value=res$mortality.cub)
      updateSliderInput(session,"mortality.adult",value=res$mortality.adult)
      updateSliderInput(session,"fertility",value=res$fertility)
      updateSliderInput(session,"removal.rate",value=res$removal.rate)
      updateSliderInput(session,"removal.age",value=res$removal.age)
      updateSliderInput(session,"k1d",value=res$k1d)
      updateSliderInput(session,"k1i",value=res$k1i)
      updateSliderInput(session,"k2",value=res$k2)
     # removeNotification(id="seed1")
   })
   observeEvent(input$randomize2, {
      showNotification(closeButton=FALSE,duration=1
                      ,paste("Previous simulation:",input$seed2))
      message("*** Randomize simulation")
      res <- randomize(seed1=input$seed1,seed2=NA)
      updateNumericInput(session,"seed2",value=res$seed2)
      removeModal()
   })
   observeEvent(input$about2, {
      updateTabItems(session,"tabs","about")
   })
   observeEvent(input$gotomainTop, {
      updateTabsetPanel(session,"tabset1",selected="Inputs")
      updateTabItems(session,"tabs","main")
   })
   observeEvent(input$gotomainBottom, {
      updateTabsetPanel(session,"tabset1",selected="Inputs")
      updateTabItems(session,"tabs","main")
   })
  # observeEvent(input$fertility, {
  #    updateTabsetPanel(session,"tabset1",selected="Check")
  # })
   output$markdown <- renderUI({
      analysis()
      showModal(modalDialog(title = "Formatting in progress","Please wait"
                       ,size="s",easyClose = TRUE,footer = NULL))
      if (!TRUE) {
         a1 <- knitr::knit('interpretation.Rmd',quiet=FALSE)
         a2 <- markdown::markdownToHTML(a1,fragment.only=TRUE)
         str(a2)
         ret <- HTML(a2)
      }
      else {
         a1 <- "./res1.html" # "./res1.html" #tempfile()
         rmarkdown::render('interpretation.Rmd'
                          ,output_format=rmarkdown::html_fragment()
                         # ,output_format=rmarkdown::html_vignette(css=NULL)
                          ,output_file=a1,quiet=TRUE
                          ,params=list(prm=analysis(),kind=1L)
                          )
         a2 <- scan(a1,what=character(),encoding="UTF-8",quiet=TRUE)
        # file.remove(a1)
         ret <- HTML(a2)
      }
      removeModal()
      ret
   })
   output$download_pdf <- downloadHandler(
      filename = "report.pdf",
      content = function(file) {
         fname <- "interpretation.Rmd"
        # tempReport <- file.path(tempdir(),fname)
        # file.copy(fname,tempReport,overwrite=TRUE)
         showModal(modalDialog(title = "Compiling in progress","Please wait"
                          ,size="s",easyClose = TRUE,footer = NULL))
         rmarkdown::render(fname
                          ,output_file=file
                          ,output_format=bookdown::pdf_document2(toc=FALSE
                             ,number_sections=FALSE
                          )
                          ,params=list(prm=analysis(),kind=2L)
                          ,envir=new.env(parent=globalenv()))
         removeModal()
      }
   )
   output$download_html <- downloadHandler(
      filename = "report.html",
      content = function(file) {
         fname <- "interpretation.Rmd"
         showModal(modalDialog(title = "Knitting in progress","Please wait"
                          ,size="s",easyClose = TRUE,footer = NULL))
         rmarkdown::render(fname
                          ,output_file=file
                          ,output_format=bookdown::html_document2(toc=FALSE
                             ,base_format=rmarkdown::html_vignette
                             ,css="https://nplatonov.github.io/html_vignette.css"
                             ,number_sections=FALSE
                          )
                          ,params=list(prm=analysis(),kind=2L)
                          ,envir=new.env(parent=globalenv()))
         removeModal()
      }
   )
   output$downloadPDF <- renderUI({
      req(analysis())
      downloadLink("download_pdf","Download PDF")
   })
   output$downloadNote <- renderUI({
      req(analysis())
      helpText("Note 2019-02-08: PDF compilation on shinyapps server can be failed")
   })
   output$downloadHTML <- renderUI({
      req(analysis())
      downloadLink("download_html","Download HTML")
   })
   output$interim <- renderPrint({
     # notification <- showNotification("Please wait",duration=1e6)
      res <- result()$output
     # saveRDS(res,"lifestory-app.rds")
      str(res)
     # removeNotification(notification)
     # Sys.sleep(0.5)
     # showNotification("Ready",duration=2)
      invisible(NULL)
   })
   output$value <- renderPrint({
      rv <- params()
      length(rv$mortality)
   })
   output$curve.lin <- renderPlot({
      params()$tube.lin
     # ggplotly(params()$tube.lin+theme(legend.position="none"))
   })
   output$curve.log <- renderPlot({
      params()$tube.log
     # ggplotly(params()$tube.log+theme(legend.position="none"))
   })
   output$curve.fert <- renderPlot({
      params()$tube.fert
     # ggplotly(params()$tube.fert)
   })
   output$curve.removal <- renderPlot({
      params()$tube.removal
     # ggplotly(params()$tube.removal)
   })
   analysis <- reactive({
      lifestory <- result()
     # updateTabsetPanel(session,"tabset1",selected="Results")
      ret <- analyze(lifestory)
      showNotification(closeButton=FALSE,duration=2,"Rendering page..."
                      ,type="warning")
      ret
   })
   sensitivity <- reactive({
      message("*** sensitivity reactive ***")
      lifestory <- result()
      ret <- perturb(lifestory)
      msg <- ifelse(all(!sapply(ret,inherits,"ggplot"))
                   ,"Nothing to render","Rendering page...")
      showNotification(closeButton=FALSE,duration=2,msg,type="warning")
      ret
   })
   preAnalysis <- reactive({
      message("*** Preanalysis")
      lifestory <- result()
      ret <- analyze(lifestory,options="popsize")
      ret
   })
   resetAnalysis <- reactive({
      message("*** Reset analysis")
      analyze()
   })
   output$plotAgeStructure <- renderPlot({
      analysis()$p6
   })
   output$uiPreAnalysis <- renderUI({
      message("*** UI preAnalysis")
     # if (!input$simulate)
     #    return(textOutput("printReminder"))
     #    return(notSimulatedPlot())
      plotOutput("plotPreAnalysis",height=height[2])
   })
   output$printReminder <- renderPrint({
      cat("There is no data for visulation. Click 'Simulate'.")
   })
   output$plotPreAnalysis <- renderPlot({
      if (!input$simulate)
         return(plotEmpty("There is no data for visulation.\nClick 'Simulate'."))
      preAnalysis()$p5
     # ggplotly(preAnalysis()$p5+theme(legend.position="none"))
     # NULL  
   })
   output$plotPopSize <- renderPlot({
      if (!input$simulate)
         return(plotEmpty())
      analysis()$p5
     # ggplotly(analysis()$p5+theme(legend.position="none"))
   })
   output$plotInterbirth <- renderPlot({
      if (!input$simulate)
         return(plotEmpty())
      analysis()$p1
   })
   output$plotDens <- renderPlot({
      if (!input$simulate)
         return(plotEmpty())
      analysis()$p2
   })
   output$plotCubs <- renderPlot({
      if (!input$simulate)
         return(plotEmpty())
      analysis()$p3
   })
   output$plotAdults <- renderPlot({
      if (!input$simulate)
         return(plotEmpty())
      analysis()$p4
   })
   output$plotP7a <- renderPlot({
      if (!input$simulate)
         return(plotEmpty())
      res <- analysis()
      res$p7+facet_grid(age~.)+res$p0
   })
   output$plotP7b <- renderPlot({
      if (!input$simulate)
         return(plotEmpty())
      res <- analysis()
      res$p7+facet_grid(.~age)+res$p0
   })
   output$plotLitterSize <- renderPlot({
      if (!input$simulate)
         return(plotEmpty())
      res <- analysis()
      res$p8 #+facet_grid(.~age)+res$p0
   })
   output$plotP8b <- renderPlot({
      if (!input$simulate)
         return(plotEmpty())
      res <- analysis()
      res$p8 #+facet_grid(age~.)+res$p0
   })
   output$plotSurvival <- renderPlot({
      if (!input$simulate)
         return(plotEmpty())
      analysis()$p9
   })
   output$plotLitterProduction <- renderPlot({
      if (!input$simulate)
         return(plotEmpty())
      analysis()$p10
   })
   output$plotSensitivity1 <- renderPlot({
      if (!input$simulate)
         return(plotEmpty())
      sensitivity()[[1]]
   })
   output$plotSensitivity2 <- renderPlot({
      if (!input$simulate)
         return(plotEmpty())
      sensitivity()[[2]]
   })
   output$plotSensitivity3 <- renderPlot({
      if (!input$simulate)
         return(plotEmpty())
      sensitivity()[[3]]
   })
   output$plotSensitivity4 <- renderPlot({
      if (!input$simulate)
         return(plotEmpty())
      sensitivity()[[4]]
   })
   output$plotSensitivity5 <- renderPlot({
      if (!input$simulate)
         return(plotEmpty())
      sensitivity()[[5]]
   })
   output$plotSensitivity6 <- renderPlot({
      if (!input$simulate)
         return(plotEmpty())
      sensitivity()[[6]]
   })
   output$iucn <- renderUI({
     # if (T & input$rpath==editName) {
     #    editModUI("editor",height=height)
     # }
     # else {
     #   # leafletOutput("viewerMapview",height="500px")
     #    leafletOutput("viewerLeaflet",height=height)
     # }
      leafletOutput("viewerLeaflet",height="500px")
   })
   output$viewerLeaflet <- renderLeaflet({
      epsg <- as.integer(input$epsg)
      prm <- input$iucn_prm
      m <- polarmap(epsg)
      ##~ pal <- colorNumeric(palette="viridis",domain=freq3$sum)
      if (T)
         m <- addPolygons(m,data=iucn_map
                        ,opacity=0
                        ,fillOpacity=0
                        ,label=iucn_map[["Abbrev"]]
                        ,labelOptions=labelOptions(noHide=T,textOnly=T)
                        )
      if (T)
         m <- addPolygons(m,data=iucn_map
                        # ,color=~pal(sum)
                        # ,weight=0
                         ,popup=~as.character(POP)
                        # ,label=~as.character(POPID)
                        # ,label=paste0(iucn_map[["Abbrev"]],": ",iucn_map[[prm]])
                         ,label=iucn_map[[prm]]
                         ,stroke=TRUE
                         ,weight=1
                         ,fillOpacity=0.1
                         ,highlightOptions=highlightOptions(fillOpacity=0.3)
                        # ,labelOptions=labelOptions(noHide=T,textOnly=T)
                         )
      m
   })
}
if (.argv0.()=="shiny.R") {
   runApp(shinyApp(ui=ui,server=server),launch.browser=TRUE)
}
