suppressMessages({
   require(shiny)
   require(shinyjs)
   require(shinydashboard)
})
source("./main.R")
height <- c("600px","248px","165px")
ui <- dashboardPage(skin = "purple"
   ,dashboardHeader(title = "Polar Bear Demography",disable=TRUE,titleWidth = 350)
   ,dashboardSidebar(NULL
      ,collapsed=TRUE
      ,disable=!TRUE
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
         ,menuItem("About", tabName = "about", icon = icon("info-circle"))
         ,menuItem("Source code", icon = icon("file-code-o"), 
                   href = "https://github.com/nplatonov/ursa/")
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
         tabItem(tabName="about"
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
                        font-family: "Roboto","Raleway", Times, "T   imes New Roman", serif;
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
                        background-color: #FFFFFF7F;
                        padding: 3px;
                     }
                     #menuhead {
                        width:600px;
                     }
                     .zzzemptymargin {
                        font-size: 0.3em;
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
                     ,actionLink("about2",""
                                ,icon=icon("info-circle"))
                  )
                  ,column(2
                     ,actionLink("randomize1","Randomize scenario"
                                ,icon=icon("angle-double-down"))
                  )
                  ,column(2
                     ,actionLink("simulate", "Simulate"
                                ,icon=icon("angle-double-right")
                                ,class="simulate-button"
                                )
                  )
                  ,column(2
                     ,actionLink("randomize2","Randomize simulation"
                                ,icon=icon("angle-down"))
                  )
              # )
            )
            ,fluidRow(NULL,class="emptymargin"
               ,br()
            )
            ,fluidRow(NULL
               ,tabBox(width=12,title = "",id = "tabset1", selected="Inputs"
                  ,tabPanel("Input",value="Inputs"#,icon=icon("align-left")
                     # width=4,
                     ,fluidRow(NULL
                        ,column(3
                           ,sliderInput("max.age", "Max age, years"
                                       ,min=25, max=40, value=init$max.age,step=1
                                       )
                        )
                        ,column(3
                           ,sliderInput("sexratio", "COY sex ratio (% of females)"
                                       ,min=20, max=80,value=init$sexratio,step=0.1
                                       )
                        )
                        ,column(3
                           ,sliderInput("init.den", "Initial number of dens"
                                       ,min=10, max=200,value=init$init.den,step=1,sep=""
                                       )
                        )
                        ,column(3
                           ,sliderInput("fertility", "Age specific fertility"
                                       ,min=0, max=1,value=init$fertility,step=0.01,sep=""
                                       )
                        )
                     )
                     ,fluidRow(NULL
                        ,column(3,
                           sliderInput("litter", "Litter size",
                                       min=1.0, max=2.5, value=init$litter,step=0.01
                                      )
                        )
                        ,column(3
                           ,fluidRow(NULL
                              ,"Random seed:"
                           )
                           ,fluidRow(NULL
                              ,column(6
                                 ,numericInput("seed1"
                                    ,"Scenario's",min=100,max=999
                                    ,value=init$seed1,step=1
                                 )
                              )
                              ##~ ,column(2
                                 ##~ ,textOutput("seed1")
                              ##~ )
                              ,column(6
                                 ,numericInput("seed2"
                                    ,"Simulation's"
                                    ,min=100,max=999
                                    ,value=init$seed2,step=1
                                 )
                              )
                              ##~ ,column(2
                                 ##~ ,textOutput("seed2")
                              ##~ )
                           )
                        )
                        ,column(2
                           ,fluidRow(NULL
                              ,textOutput("scenario")
                             # ,actionLink("randomize1","Randomize scenario")
                             # ,actionLink("randomize2","Randomize simulation")
                           )
                           ,fluidRow(NULL
                             # ,actionLink("simulate", "Simulate"
                             #            ,icon=icon("angle-double-right"))
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
                     ,fluidRow(NULL
                        ,column(3,
                           sliderInput("mortality.cub", "Dependent COY mortality",
                                       min=0.2, max=0.5,value=init$mortality.cub,
                                       step=0.01
                                      )
                        )
                        ,column(3
                           ,textOutput("mort.C1.depend")
                           ,textOutput("mort.C1.indep")
                           ,textOutput("mort.C2.depend")
                           ,textOutput("mort.C2.indep")
                           ##~ ,sliderInput("mortality.yearling", "Yearling mortality",
                                       ##~ min=0.1, max=0.5,value=mortality.yearling,
                                       ##~ step=0.001
                                      ##~ )
                        )
                        ,column(3,
                           sliderInput("mortality.adult", "Adult mortality",
                                       min=0.01, max=0.15,value=init$mortality.adult,
                                       step=0.005
                                      )
                        )
                        ,column(3,
                           sliderInput("pregnant", "Birth success",
                                       min=0.2, max=1.0,value=init$pregnant,
                                       step=0.01
                                      )
                        )
                     )
                     ,fluidRow(NULL
                        ,column(4
                           ,fluidRow(
                              "Litter size proportions:"
                           )
                           ,fluidRow(NULL
                              ,column(4,numericInput("C1", "1C",init$litterF[1],step=0.001))
                              ,column(4,numericInput("C2", "2C",init$litterF[2],step=0.001))
                              ,column(4,numericInput("C3", "3C",init$litterF[3],step=0.001))
                           )
                           ##~ ,fluidRow(NULL
                              ##~ ,column(4,textInput("C1", "1C",litterF[1]))
                              ##~ ,column(4,textInput("C2", "2C",litterF[2]))
                              ##~ ,column(4,textInput("C3", "3C",litterF[3]))
                           ##~ )
                        )
                     )
                    # ,"Parameters of demography tube:"
                     ,fluidRow(NULL
                        ,column(3,
                           sliderInput("k1d", "Mortality rate of dependent young",
                                       min=1, max=20, value=init$k1d,step=0.1
                                      )
                        )
                        ,column(3,
                           sliderInput("k1i", "Mortality rate of independent youngs",
                                       min=1, max=20, value=init$k1i,step=0.1
                                      )
                        )
                        ,column(3,
                           sliderInput("k2", "Mortality rate of aging",
                                       min=1, max=20, value=init$k2,step=0.1
                                      )
                        )
                        ,column(3,
                           sliderInput("indep.C1", "Broken yearling families",
                                       min=0, max=1
                                       ,value=init$indep.fraction[2],step=0.01
                                      )
                        )
                     )
                     ,fluidRow(NULL
                        ,column(4
                          # ,checkboxInput("equilibrium"
                          #              ,"Population is stable"
                          #              ,value=equilibrium)
                        )
                        ,column(4
                        )
                        ,column(3
                         # ,actionButton("simulate", "Simulate"
                         #              ,icon=icon("angle-double-right"))
                        )
                        ,column(1
                             # ,submitButton("Submit",shiny::icon("refresh"))
                        )
                     )
                  ) ## tabPanel
                  ,tabPanel(title="Check",value="Check"#,icon=icon("eye")
                    # width=4,
                    # status = "warning", solidHeader = !TRUE,
                    # collapsible = !TRUE,collapsed=!FALSE,
                    # height = "350px",
                     ,fluidRow(NULL
                        ,column(4,
                           plotOutput("curve.lin")
                        )
                        ,column(4,
                           plotOutput("curve.log")
                        )
                        ,column(4,
                           plotOutput("curve.fert")
                        )
                     )
                  )
                  ,tabPanel(title="Results",value="Results"#,icon=icon("signal")
                     ,fluidRow(NULL
                        ,column(3
                           ,plotOutput("plotPopSize",height=height[2])
                           ,plotOutput("plotAgeStructure",height=height[2])
                        )
                        ,column(9
                           ,fluidRow(NULL
                              ,column(4
                                 ,plotOutput("plotCubs",height=height[3])
                              )
                              ,column(3
                                 ,plotOutput("plotDens",height=height[3])
                              )
                              ,column(3
                                 ,plotOutput("plotAdults",height=height[3])
                              )
                              ,column(2
                                 ,plotOutput("plotInterbirth",height=height[3])
                              )
                           )
                           ,fluidRow(NULL
                              ,column(4
                                 ,plotOutput("plotSurvival",height=height[3])
                              )
                              ,column(3
                                 ,plotOutput("plotLitterSize",height=height[3])
                              )
                              ,column(3
                                 ,plotOutput("plotLitterProduction",height=height[3])
                              )
                           )
                           ,fluidRow(NULL
                           )
                        )
                     )
                  )
                  ##~ ,tabPanel("",value="Verbatim",icon=icon("terminal")
                     ##~ ,verbatimTextOutput("interim")
                  ##~ )
                  ,tabPanel(title="Details",value="Interpretation" #,icon=icon("list")
                     ,fluidRow(NULL 
                       # ,box(width=12
                           ,column(2)
                           ,column(8
                              ,uiOutput("markdown")
                              ,br()
                              ,uiOutput("downloadPDF")
                              ,uiOutput("downloadHTML")
                           )
                           ,column(2)
                       # )
                     )
                  )
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
      sprintf("Independent sub-adult mortality: %.3f",m[3])
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
      max.age <- input$max.age
      k1d <- input$k1d
      k1i <- input$k1i
      k2 <- input$k2
      mortality.cub <- input$mortality.cub
      mortality.adult <- input$mortality.adult
      ## isolate({}) doubt; so many dependencies
      mortality <- mortalityTube(max.age=max.age,mortality.cub=mortality.cub
                                ,mortality.adult=mortality.adult
                                ,k1d=k1d,k1i=k1i,k2=k2)
      tube <- mortalityTubePlot(mortality)
      indep.mortality <- mortality$indep
      mortality <- mortality$depend
      age <- seq(max.age)
      init.den <- input$init.den
      indep.fraction <- init$indep.fraction
      indep.fraction[2] <- input$indep.C1
      list(mortality=mortality
          ,indep.mortality=indep.mortality
          ,mortality.cub=input$mortality.cub
          ,mortality.adult=input$mortality.adult
          ,age=age
          ,tube.lin=tube
          ,tube.log=tube+scale_y_log10()
          ,tube.fert=fertilityCurve(age=age,u=input$fertility,plot=TRUE)
          ,init.den=init.den
          ,litter=input$litter
          ,indep.fraction=indep.fraction
          ,max.age=input$max.age
          ,pregnant=input$pregnant
          ,sexratio=input$sexratio
          ,seed1=input$seed1
          ,seed2=input$seed2
          ,fertility=input$fertility
          ,k1d=k1d
          ,k1i=k1i
          ,k2=k2
          )
   })
   observeEvent(input$randomize1, {
      updateTabsetPanel(session,"tabset1",selected="Inputs")
     # updateTabItems(session,"tabs","tabInput")
   })
   observeEvent(input$randomize2, {
      updateTabsetPanel(session,"tabset1",selected="Inputs")
     # updateTabItems(session,"tabs","tabInput")
   })
   observeEvent(input$simulate, {
     # message("*** Simulate (observe), goto results")
     # updateTabItems(session,"tabs","main")
      updateTabsetPanel(session,"tabset1",selected="Inputs")
      updateTabsetPanel(session,"tabset1",selected="Results")
   })
   result <- eventReactive(input$simulate,{
      message("*** Simulate (reactive)")
     # updateTabsetPanel(session,"tabset1",selected="Results") ## NOT WORKING
      rv <- params()
      showModal(modalDialog(title = "Simulation in progress","Please wait"
                       ,sise="s",easyClose = TRUE,footer = NULL))
      res <- with(rv,simulate(max.age=max.age
                             ,litter=litter
                             ,sexratio=sexratio
                             ,init.den=init.den
                             ,pregnant=pregnant
                             ,mortality.cub=mortality.cub
                             ,mortality.adult=mortality.adult
                             ,indep.fraction=indep.fraction
                             ,fertility=input$fertility
                             ,k1d=input$k1d
                             ,k1i=input$k1i
                             ,k2=input$k2
                             ,seed1=input$seed1
                             ,seed2=input$seed2
                             ))
     # updateTabsetPanel(session,"restab",selected="Verbatim") ## NOT WORKING
      removeModal()
      res
   })
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
      updateSliderInput(session,"indep.C1",value=res$indep.C1)
      updateSliderInput(session,"mortality.cub",value=res$mortality.cub)
      updateSliderInput(session,"mortality.adult",value=res$mortality.adult)
      updateSliderInput(session,"fertility",value=res$fertility)
      updateSliderInput(session,"k1d",value=res$k1d)
      updateSliderInput(session,"k1i",value=res$k1i)
      updateSliderInput(session,"k2",value=res$k2)
   })
   observeEvent(input$randomize1, {
      showNotification(closeButton=FALSE,duration=3
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
      updateSliderInput(session,"indep.C1",value=res$indep.fraction[2])
      updateSliderInput(session,"mortality.cub",value=res$mortality.cub)
      updateSliderInput(session,"mortality.adult",value=res$mortality.adult)
      updateSliderInput(session,"fertility",value=res$fertility)
      updateSliderInput(session,"k1d",value=res$k1d)
      updateSliderInput(session,"k1i",value=res$k1i)
      updateSliderInput(session,"k2",value=res$k2)
     # removeNotification(id="seed1")
   })
   observeEvent(input$randomize2, {
      showNotification(closeButton=FALSE,duration=3
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
   output$markdown <- renderUI({
      analysis()
      if (!TRUE) {
         a1 <- knitr::knit('interpretation.Rmd',quiet=FALSE)
         a2 <- markdown::markdownToHTML(a1,fragment.only=TRUE)
         str(a2)
         HTML(a2)
      }
      else {
         a1 <- tempfile()
         rmarkdown::render('interpretation.Rmd'
                          ,output_format=rmarkdown::html_fragment()
                         # ,output_format=rmarkdown::html_vignette(css=NULL)
                          ,output_file=a1,quiet=TRUE
                          ,params=list(prm=analysis(),kind=1L)
                          )
         a2 <- scan(a1,what=character(),encoding="UTF-8",quiet=TRUE)
        # file.remove(a1)
         HTML(a2)
      }
   })
   output$download_pdf <- downloadHandler(
      filename = "report.pdf",
      content = function(file) {
         fname <- "interpretation.Rmd"
        # tempReport <- file.path(tempdir(),fname)
        # file.copy(fname,tempReport,overwrite=TRUE)
         rmarkdown::render(fname
                          ,output_file=file
                          ,output_format=bookdown::pdf_document2(
                             toc=FALSE
                             ,number_sections=FALSE
                          )
                          ,params=list(prm=analysis(),kind=2L)
                          ,envir=new.env(parent=globalenv()))
      }
   )
   output$download_html <- downloadHandler(
      filename = "report.html",
      content = function(file) {
         fname <- "interpretation.Rmd"
         rmarkdown::render(fname
                          ,output_file=file
                          ,output_format=bookdown::html_document2(
                             toc=FALSE
                             ,number_sections=FALSE
                          )
                          ,params=list(prm=analysis(),kind=2L)
                          ,envir=new.env(parent=globalenv()))
      }
   )
   output$downloadPDF <- renderUI({
      req(analysis())
      downloadLink("download_pdf","Download PDF")
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
     # with(params(),plot(age,mortality,type="b"))
      params()$tube.lin
   })
   output$curve.log <- renderPlot({
      params()$tube.log
   })
   output$curve.fert <- renderPlot({
      params()$tube.fert
   })
   analysis <- reactive({
      lifestory <- result()
      updateTabsetPanel(session,"tabset1",selected="Results")
      showModal(modalDialog(title = "Analysis in progress","Please wait"
                       ,sise="s",easyClose = TRUE,footer = NULL))
      ret <- analyze(lifestory)
      removeModal()
      ret
   })
   output$plotAgeStructure <- renderPlot({
      analysis()$p6
   })
   output$plotPopSize <- renderPlot({
      analysis()$p5
   })
   output$plotInterbirth <- renderPlot({
      analysis()$p1
   })
   output$plotDens <- renderPlot({
      analysis()$p2
   })
   output$plotCubs <- renderPlot({
      analysis()$p3
   })
   output$plotAdults <- renderPlot({
      analysis()$p4
   })
   output$plotP7a <- renderPlot({
      res <- analysis()
      res$p7+facet_grid(age~.)+res$p0
   })
   output$plotP7b <- renderPlot({
      res <- analysis()
      res$p7+facet_grid(.~age)+res$p0
   })
   output$plotLitterSize <- renderPlot({
      res <- analysis()
      res$p8+facet_grid(.~age)+res$p0
   })
   output$plotP8b <- renderPlot({
      res <- analysis()
      res$p8+facet_grid(age~.)+res$p0
   })
   output$plotSurvival <- renderPlot({
      analysis()$p9
   })
   output$plotLitterProduction <- renderPlot({
      analysis()$p10
   })
}
