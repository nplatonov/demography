suppressMessages(require(shinydashboard))
uiDashboard <- dashboardPage(skin = "blue"
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
                     .col-sm-2, .col-sm-3, .col-sm-4, .col-sm-5, .col-sm-6,
                     .col-sm-7, .col-sm-8, .col-sm-9, .col-sm-10, .col-sm-12, {
                        padding-right: 5px !important;
                        padding-left: 5px !important;
                        border: 1px solid red !important;
                     }
                     #menuhead {
                        width:600px;
                     }
                     .zzzemptymargin {
                        font-size: 0.3em;
                     }
                     .nav > li > a {
                        padding: 8px 2.4vw; /* 8px 12px */
                     }
                     .modebar-group {
                        padding-left: 0px !important;
                        background-color: transparent !important;
                     }
                     .modebar-btn {
                        opacity: 0.1;
                     }
                     .modebar-btn:hover {
                        opacity: 1;
                     }
                     .skin-blue #reminder {
                        color: lightskyblue;
                     }
                     @media screen {
                     .skin-blue #viewerLeaflet,
                     .skin-blue #viewerMapview,
                     .skin-blue #editor-map {
                        height: 83vh !important;
                     }}
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
               ,tabBox(width=12,title = "",id = "tabset1", selected=c("Inputs","Map","Check")[1]
                  ,tabPanel(title="",value="Map",icon=icon("globe")
                     ,fluidRow(NULL
                        ,column(2)
                        ,column(8
                           ,uiOutput("iucn")
                          # ,DT::DTOutput("tbl")
                        )
                        ,column(2
                           ,selectInput("epsg","Projection (EPSG code)",epsgList
                                       ,selected=3571) #sample(epsgList,1)) 
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
                              ,column(5
                                 ,actionLink("simulate", "Simulate"
                                            ,icon=icon("angle-double-right")
                                            ,class="simulate-button"
                                            )
                              )
                              ,column(7
                                 ,textOutput("reminder")
                              )
                             # ,column(4)
                           )
                           ,fluidRow(NULL
                              ,column(12,"Random seeds:")
                           )
                           ,fluidRow(NULL
                              ,column(4
                                 ,actionLink("randomize1","Scenario's")
                                 ,numericInput("seed1"
                                    ,"",min=100,max=999
                                    ,value=c(init$seed1,570)[1]
                                    ,step=1,width=spinWidth[1]
                                 )
                              )
                              ##~ ,column(2
                                 ##~ ,textOutput("seed1")
                              ##~ )
                              ,column(4
                                 ,actionLink("randomize2","Simulation's")
                                 ,numericInput("seed2"
                                    ,"",min=100,max=999
                                    ,value=init$seed2,step=1,width=spinWidth[1]
                                 )
                              )
                              ,column(4
                                 ,""
                                 ,actionLink("randomize3","Control's")
                                 ,numericInput("seed3"
                                    ,"",min=100,max=999
                                    ,value=-1,step=1,width=spinWidth[1]
                                 )
                              )
                           )
                          # ,br()
                          # ,actionLink("analysis6", "Analyze"
                          #              ,icon=icon("angle-double-right"))
                           ,with(initRange$max.age,sliderInput("max.age"
                              ,"Max age, years"
                              ,width=sliderWidth,min=from,max=to,step=by
                              ,value=init$max.age))
                        )
                        ,column(3
                           ,with(initRange$sexratio,sliderInput("sexratio"
                              ,"COY sex ratio (% of females)"
                              ,width=sliderWidth,min=from,max=to,step=by
                              ,value=init$sexratio))
                           ,with(initRange$init.den,sliderInput("init.den"
                              ,"Initial number of dens"
                              ,width=sliderWidth,min=from,max=to,step=by,sep=""
                              ,value=init$init.den))
                           ,with(initRange$fertility,sliderInput("fertility"
                              ,"Age specific fertility"
                              ,width=sliderWidth,min=from,max=to,step=by,sep=""
                              ,value=init$fertility))
                           ,with(initRange$removal.age,sliderInput("removal.age"
                              ,"Age specific human-caused removal"
                              ,width=sliderWidth,min=from,max=to,step=by,sep=""
                              ,value=init$removal.age))
                           ,with(initRange$removal.rate,sliderInput("removal.rate"
                              ,"2M/1F human-caused removal rate"
                              ,width=sliderWidth,min=from,max=to,step=by
                              ,value=init$removal.rate))
                        )
                        ,column(3
                           ,fluidRow(NULL
                              ,column(9
                                 ,with(initRange$broken.C1,sliderInput("broken.C1"
                                    ,"Weaning C1 fraction"
                                   # ,width=sliderWidth
                                    ,min=from,max=to,step=by
                                    ,value=init$indep.fraction[2]))
                              )
                              ,column(3
                                 ,with(initRange$broken.C2,sliderInput("broken.C2"
                                    ,"...C2"
                                   # ,width=sliderWidth
                                    ,min=from,max=to,step=by
                                    ,value=init$indep.fraction[3]))
                              )
                           )
                           ,with(initRange$litter,sliderInput("litter"
                              ,"Litter size C0"
                              ,width=sliderWidth,min=from,max=to,step=by
                              ,value=init$litter))
                           ,fluidRow(NULL
                              ,column(10,"Litter size proportions C0:")
                           )
                           ,fluidRow(NULL
                              ,column(4,numericInput("C1", "1C",init$litterF[1]
                                                    ,step=0.001,width=spinWidth[2]))
                              ,column(4,numericInput("C2", "2C",init$litterF[2]
                                                    ,step=0.001,width=spinWidth[2]))
                              ,column(4,numericInput("C3", "3C",init$litterF[3]
                                                    ,step=0.001,width=spinWidth[2]))
                              ,column(4)
                           )
                           ,textOutput("mort.C1.depend")
                           ,textOutput("mort.C1.indep")
                           ,textOutput("mort.C2.depend")
                           ,textOutput("mort.C2.indep")
                           ,br()
                           ,with(initRange$pregnant,sliderInput("pregnant"
                              ,"Birth success"
                              ,width=sliderWidth,min=from,max=to,step=by
                              ,value=init$pregnant))
                        )
                        ,column(3
                           ,with(initRange$mortality.cub,sliderInput("mortality.cub"
                              ,"Dependent COY mortality"
                              ,width=sliderWidth,min=from,max=to,step=by
                              ,value=init$mortality.cub))
                           ,with(initRange$mortality.adult,sliderInput("mortality.adult"
                              ,"Adult mortality",
                              ,width=sliderWidth,min=from,max=to,step=by
                              ,value=init$mortality.adult))
                           ,with(initRange$k1d,sliderInput("k1d"
                              ,"Mortality slope of dependent young"
                              ,width=sliderWidth,min=1,max=20,step=by
                              ,value=init$k1d))
                           ,with(initRange$k1i,sliderInput("k1i"
                              ,"Mortality slope of independent youngs",
                              ,width=sliderWidth,min=1,max=20,step=by
                              ,value=init$k1i))
                           ,with(initRange$k2,sliderInput("k2"
                              ,"Mortality slope of aging"
                              ,width=sliderWidth,min=1,max=20,step=by
                              ,value=init$k2))
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
                        ,column(4
                           ,uiOutput("curve.surv")
                        )
                        ,column(4
                           ,uiOutput("curve.lin")
                        )
                        ,column(4
                           ,uiOutput("curve.log")
                        )
                     )
                     ,fluidRow(NULL
                        ,column(2)
                        ,column(4
                           ,uiOutput("curve.fert")
                        )
                        ,column(4
                           ,uiOutput("curve.removal")
                        )
                        ,column(2)
                     )
                  )
                  ,tabPanel(title="",value="Results",icon=icon("signal")
                     ,fluidRow(NULL
                        ,column(3
                           ,uiOutput("plotPopSize")
                           ,uiOutput("plotPieAge")
                        )
                        ,column(9
                           ,fluidRow(NULL
                              ,column(4
                                 ,uiOutput("plotDens")
                                 ,uiOutput("plotCubs")
                                 ,uiOutput("plotAdults")
                                 ,uiOutput("plotInterbirth")
                              )
                              ,column(4
                                 ,uiOutput("plotSurvival")
                                 ,uiOutput("plotLitterSize")
                              )
                              ,column(4
                                 ,uiOutput("plotLitterProduction")
                                 ,uiOutput("plotAgeStructure")
                              )
                             # ,column(3
                             # )
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
                              ,column(4
                                 ,uiOutput("plotSensitivity1")
                                 ,uiOutput("plotSensitivity2")
                                 ,uiOutput("plotSensitivity3")
                              )
                              ,column(4
                                 ,uiOutput("plotSensitivity4")
                                 ,uiOutput("plotSensitivity5")
                                 ,uiOutput("plotSensitivity6")
                              )
                              ,column(4
                                 ,uiOutput("plotSensitivity7")
                                 ,uiOutput("plotSensitivity8")
                                 ,uiOutput("plotSensitivity9")
                              )
                              ##~ ,column(3
                              ##~ )
                           )
                        )
                     )
                  )
                  ,tabPanel(title="",value="Advanced",icon=icon("flask")
                     ,fluidRow(NULL
                        ,column(12
                           ,fluidRow(NULL
                              ,column(4
                                 ,uiOutput("plotSensitivity10")
                              )
                              ,column(4
                              )
                              ,column(4
                              )
                              ##~ ,column(3
                              ##~ )
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
