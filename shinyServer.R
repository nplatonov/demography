serverDashboard <- function(input, session, output) {
   if (devel)
      session$onSessionEnded(stopApp)
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
      sprintf("Dependent yearling (C1) mortality: %.3f",m[2])
   })
   output$mort.C2.depend <- renderText({
      m <- params()$mortality
      sprintf("Dependent juvenile (C2) mortality: %.3f",m[3])
   })
   output$mort.C1.indep <- renderText({
      m <- params()$indep.mortality
      sprintf("Independent yearling (C1) mortality: %.3f",m[2])
   })
   output$mort.C2.indep <- renderText({
      m <- params()$indep.mortality
      sprintf("Independent juvenile (C2) mortality: %.3f\n",m[3])
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
      message("*** reactive -> params() ***")
      ret <- curveInputs(indep.mortality=input$indep.mortality
                        ,mortality.cub=input$mortality.cub
                        ,mortality.adult=input$mortality.adult
                        ,init.den=input$init.den
                        ,litter=input$litter
                        ,broken.C1=input$broken.C1
                        ,broken.C2=input$broken.C2
                        ,max.age=input$max.age
                        ,pregnant=input$pregnant
                        ,sexratio=input$sexratio
                        ,seed1=input$seed1
                        ,seed2=input$seed2
                        ,seed3=input$seed3
                        ,fertility=input$fertility
                        ,removal.rate=input$removal.rate
                        ,removal.age=input$removal.age
                        ,k1d=input$k1d
                        ,k1i=input$k1i
                        ,k2=input$k2
                        ,simulate=input$simulate
                        )
      indTube <- grep("^tube",names(ret))
      if (input$seed1>0) {
         check1 <- randomize(seed1=input$seed1,seed2=input$seed2,seed3=input$seed3)
         check2 <- ret[-indTube]
         cmp <- comparePrm(check1,check2)
         if (length(cmp)==1) {
            ret$seed1 <- -1L
            updateNumericInput(session,"seed1",value=ret$seed1)
            updateNumericInput(session,"seed2",value=ret$seed2)
            updateNumericInput(session,"seed3",value=ret$seed3)
         }
      }
      if (dir.exists(logpath)) {
         saveRDS(ret[indTube],file.path(logpath,"curveInput.rds"))
         saveRDS(ret[-indTube],file.path(logpath,"inputs.rds"))
      }
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
      message("*** simulate3 pressed")
      updateTabsetPanel(session,"tabset1",selected="Inputs")
   })
   result.dev <- eventReactive(input$simulate,{
  # result <- reactive({
      message("*** Simulate (reactive) -> result.dev")
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
      message("*** Simulate (eventReactive) -> results()")
      rv <- params()
     # rv <- simulate(seed1=703,seed2=702,seed3=701,check=TRUE)
     # saveRDS(rv,"curveInput1.rds")
     # rv$tube.fert <- rv$tube.lin <- rv$tube.removal <- rv$tube.log <-
     #    rv$mortality <- rv$indep.mortality <- NULL
     # do.call(simulate,rv)
      ret <- do.call("simulate",params())
      if (dir.exists(logpath))
         saveRDS(ret,file.path(logpath,"simulation.rds"))
      ret
   })
  # resultUpdate <- reactive({
  #    message("*** result update ***")
  #    result()
  # })
   observeEvent(input$seed1, {
      message("*** seed1")
      if (input$seed1<0) {
         if (input$seed3<0)
            updateNumericInput(session,"seed3",value=init$seed2)
         return(NULL)
      }
      res <- randomize(seed1=input$seed1,seed2=NA)
      updateNumericInput(session,"seed1",value=res$seed1)
      updateNumericInput(session,"seed2",value=res$seed2)
      updateNumericInput(session,"seed3",value=res$seed3)
      updateSliderInput(session,"max.age",value=res$max.age)
      updateSliderInput(session,"litter",value=res$litter)
      updateSliderInput(session,"sexratio",value=res$sexratio)
      updateSliderInput(session,"init.den",value=res$init.den)
      updateSliderInput(session,"pregnant",value=res$pregnant)
      updateSliderInput(session,"broken.C1",value=res$indep.fraction[2])
      updateSliderInput(session,"broken.C2",value=res$indep.fraction[3])
      updateSliderInput(session,"mortality.cub",value=res$mortality.cub)
      updateSliderInput(session,"mortality.adult",value=res$mortality.adult)
      updateSliderInput(session,"fertility",value=res$fertility)
      updateSliderInput(session,"removal.rate",value=res$removal.rate)
      updateSliderInput(session,"removal.age",value=res$removal.age)
      updateSliderInput(session,"k1d",value=res$k1d)
      updateSliderInput(session,"k1i",value=res$k1i)
      updateSliderInput(session,"k2",value=res$k2)
     # updateActionButton(session,"simulate",label="Simulate")
   })
   observeEvent(input$randomize1, {
      showNotification(closeButton=FALSE,duration=1
                      ,paste("Previous scenario:",input$seed1))
      message("*** Randomize scenario")
      res <- randomize(seed1=NA,seed2=NA)
      updateNumericInput(session,"seed1",value=res$seed1)
      ##~ updateNumericInput(session,"seed2",value=res$seed2)
      ##~ updateNumericInput(session,"seed3",value=res$seed3)
      ##~ updateSliderInput(session,"max.age",value=res$max.age)
      ##~ updateSliderInput(session,"litter",value=res$litter)
      ##~ updateSliderInput(session,"sexratio",value=res$sexratio)
      ##~ updateSliderInput(session,"init.den",value=res$init.den)
      ##~ updateSliderInput(session,"pregnant",value=res$pregnant)
      ##~ updateSliderInput(session,"broken.C1",value=res$indep.fraction[2])
      ##~ updateSliderInput(session,"broken.C2",value=res$indep.fraction[3])
      ##~ updateSliderInput(session,"mortality.cub",value=res$mortality.cub)
      ##~ updateSliderInput(session,"mortality.adult",value=res$mortality.adult)
      ##~ updateSliderInput(session,"fertility",value=res$fertility)
      ##~ updateSliderInput(session,"removal.rate",value=res$removal.rate)
      ##~ updateSliderInput(session,"removal.age",value=res$removal.age)
      ##~ updateSliderInput(session,"k1d",value=res$k1d)
      ##~ updateSliderInput(session,"k1i",value=res$k1i)
      ##~ updateSliderInput(session,"k2",value=res$k2)
     # updateActionButton(session,"simulate",label="Simulate")
     # removeNotification(id="seed1")
   })
   observeEvent(input$randomize2, {
      showNotification(closeButton=FALSE,duration=1
                      ,paste("Previous simulation's:",input$seed2))
      message("*** Randomize simulation")
      res <- randomize(seed1=input$seed1,seed2=NA)
      updateNumericInput(session,"seed2",value=res$seed2)
      updateNumericInput(session,"seed3",value=res$seed3)
     # updateActionButton(session,"simulate",label="Simulate")
      removeModal()
   })
   observeEvent(input$randomize3, {
      showNotification(closeButton=FALSE,duration=1
                      ,paste("Previous perturbation's:",input$seed2))
      message("*** Randomize perturbation")
      res <- randomize(seed1=input$seed1,seed2=NA)
      updateNumericInput(session,"seed3",value=res$seed3)
     # updateActionButton(session,"simulate",label="Simulate")
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
     # helpText("Note 2019-02-08: PDF compilation on shinyapps server can be failed")
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
   'selectivePlot' <- function(obj,ncol=3,now=FALSE,empty="Not simulated yet") {
      h <- height[ncol]
      if (is.character(ncol)) {
         print(ncol)
         print(h)
      }
      id <- basename(tempfile(pattern=""))
      if ((!now)&&(!input$simulate)) {
        # return("Simulation is not ready")
         output[[id]] <- renderPlot(plotEmpty(empty))
         return(plotOutput(id,height=h))
      }
      if (is.character(obj))
         s <- eval(parse(text=obj))$plot
      else
         s <- obj$plot
      if (inherits(s,"ggplot")) {
         output[[id]] <- renderPlot(s)
         return(plotOutput(id,height=h))
      }
      if (inherits(s,"plotly")) {
         if ((!now)&&(reminder())) {
            s <- s %>% add_annotations(x=0.5,y=0.5,xref="paper",yref="paper"
                                      ,showarrow=F
                                      ,font=list(color="#0000000F",size=16)
                                      ,text="inputs are updated")
         }
         output[[id]] <- renderPlotly(s)
         return(plotlyOutput(id,height=h))
      }
      if (inherits(s,"highchart")) {
         output[[id]] <- renderHighchart(s)
         return(highchartOutput(id,height=h))
      }
      output[[id]] <- renderPlot(plotEmpty(""))
      return(plotOutput(id,height=h))
   }
   output$curve.surv <- renderUI({
      selectivePlot(params()$tube.surv,2,now=TRUE)
   })
   output$curve.lin <- renderUI({
     # params()$tube.lin
     # ggplotly(params()$tube.lin+theme(legend.position="none"))
      selectivePlot(params()$tube.lin,2,now=TRUE)
   })
   output$curve.log <- renderUI({
     # params()$tube.log
     # ggplotly(params()$tube.log+theme(legend.position="none"))
      selectivePlot(params()$tube.log,2,now=TRUE)
   })
   output$curve.fert <- renderUI({
     # params()$tube.fert
     # ggplotly(params()$tube.fert)
      selectivePlot(params()$tube.fert,2,now=TRUE)
   })
   output$curve.removal <- renderUI({
     # params()$tube.removal
     # ggplotly(params()$tube.removal)
      selectivePlot(params()$tube.removal,2,now=TRUE)
   })
   analysis <- reactive({
     # if (reminder())
     #    return(NULL)
      message("*** analysis reactive ***")
      lifestory <- result()
     # updateTabsetPanel(session,"tabset1",selected="Results")
      ret <- analyze(lifestory)
      showNotification(closeButton=FALSE,duration=2,"Rendering page..."
                      ,type="warning")
      if (dir.exists(logpath))
         saveRDS(ret,file.path(logpath,"analysis.rds"))
      ret
   })
   sensitivity <- reactive({
     # if (reminder())
     #    return(NULL)
      message("*** sensitivity reactive ***")
      lifestory <- result()
      ret <- perturb(lifestory)
      if (dir.exists(logpath))
         saveRDS(ret,file.path(logpath,"perturb-basic.rds"))
      msg <- ifelse(all(!sapply(ret,function(x) inherits(x$plot,c("ggplot","plotly"))))
                   ,"Nothing to render","Rendering page...")
      showNotification(closeButton=FALSE,duration=2,msg,type="warning")
      ret
   })
   sensitivityAdv <- reactive({
     # if (reminder())
     #    return(NULL)
      message("*** sensitivityAdv reactive ***")
      lifestory <- result()
      ret <- perturb(lifestory,set="advanced")
      if (dir.exists(logpath))
         saveRDS(ret,file.path(logpath,"perturb-advanced.rds"))
      msg <- ifelse(all(!sapply(ret,function(x) inherits(x$plot,c("ggplot","plotly"))))
                   ,"Nothing to render","Rendering page...")
      showNotification(closeButton=FALSE,duration=2,msg,type="warning")
      ret
   })
   preAnalysis <- reactive({
      message("*** Preanalysis")
      lifestory <- result()
      ret <- analyze(lifestory,options="popsize")
     # updateActionButton(session,"simulate",label="See results")
      ret
   })
   resetAnalysis <- reactive({
      message("*** Reset analysis")
      analyze()
   })
   output$printReminder <- renderPrint({
      cat("There is no data for visulation. Click 'Simulate'.")
   })
   ##~ output$plotPreAnalysis <- renderPlot({
      ##~ if (!input$simulate)
         ##~ return(plotEmpty("There is no data for visulation.\nClick 'Simulate'."))
      ##~ preAnalysis()$p5
     ##~ # ggplotly(preAnalysis()$p5+theme(legend.position="none"))
     ##~ # NULL  
   ##~ })
   output$uiPreAnalysis <- renderUI({
      message("*** UI preAnalysis")
     # if (!input$simulate)
     #    return(textOutput("printReminder"))
     #    return(notSimulatedPlot())
     # plotOutput("plotPreAnalysis",height=height[2])
      selectivePlot(preAnalysis()$p5,2
                   ,empty="There is no data for visulation.\nClick 'Simulate'.")
   })
   output$plotPopSize <- renderUI({
     # if (!input$simulate)
     #    return(plotEmpty())
     # analysis()$p5
     # ggplotly(analysis()$p5+theme(legend.position="none"))
      selectivePlot(analysis()$p5,2)
   })
   output$plotAgeStructure <- renderUI({
     # analysis()$p6
      selectivePlot(analysis()$p6,"1/3")
   })
   output$plotInterbirth <- renderUI({
      ##~ if (!input$simulate)
         ##~ return(plotEmpty())
      ##~ analysis()$p1
      selectivePlot(analysis()$p1,4)
   })
   output$plotDens <- renderUI({
      ##~ if (!input$simulate)
         ##~ return(plotEmpty())
      ##~ analysis()$p2
      selectivePlot(analysis()$p2,4)
   })
   output$plotCubs <- renderUI({
      ##~ if (!input$simulate)
         ##~ return(plotEmpty())
      ##~ analysis()$p3
      selectivePlot(analysis()$p3,4)
   })
   output$plotAdults <- renderUI({
      ##~ if (!input$simulate)
         ##~ return(plotEmpty())
      ##~ analysis()$p4
      selectivePlot(analysis()$p4,4)
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
   output$plotLitterSize <- renderUI({
      ##~ if (!input$simulate)
         ##~ return(plotEmpty())
      ##~ res <- analysis()
      ##~ res$p8 #+facet_grid(.~age)+res$p0
      selectivePlot(analysis()$p8,2)
   })
   output$plotPieAge <- renderUI({
      selectivePlot(analysis()$p11,"1/2")
   })
   output$plotP8b <- renderPlot({
      if (!input$simulate)
         return(plotEmpty())
      res <- analysis()
      res$p8 #+facet_grid(age~.)+res$p0
   })
   output$plotSurvival <- renderUI({
      ##~ if (!input$simulate)
         ##~ return(plotEmpty())
      ##~ analysis()$p9
      selectivePlot(analysis()$p9,2)
   })
   output$plotLitterProduction <- renderUI({
      ##~ if (!input$simulate)
         ##~ return(plotEmpty())
      ##~ analysis()$p10
      selectivePlot(analysis()$p10,"1/3")
   })
   output$plotSensitivity1 <- renderUI(selectivePlot(sensitivity()[[1]],3))
   output$plotSensitivity2 <- renderUI(selectivePlot(sensitivity()[[2]],3))
   output$plotSensitivity3 <- renderUI(selectivePlot(sensitivity()[[3]],3))
   output$plotSensitivity4 <- renderUI(selectivePlot(sensitivity()[[4]],3))
   output$plotSensitivity5 <- renderUI(selectivePlot(sensitivity()[[5]],3))
   output$plotSensitivity6 <- renderUI(selectivePlot(sensitivity()[[6]],3))
   output$plotSensitivity7 <- renderUI(selectivePlot(sensitivity()[[7]],3))
   output$plotSensitivity8 <- renderUI(selectivePlot(sensitivity()[[8]],3))
   output$plotSensitivity9 <- renderUI(selectivePlot(sensitivity()[[9]],3))
   output$plotSensitivity10 <- renderUI(selectivePlot(sensitivityAdv()[[1]],3))
   output$plotSensitivity11 <- renderUI(selectivePlot(sensitivityAdv()[[2]],3))
   output$plotSensitivity12 <- renderUI(selectivePlot(sensitivityAdv()[[3]],3))
   output$plotSensitivity13 <- renderUI(selectivePlot(sensitivityAdv()[[4]],3))
   output$plotSensitivity14 <- renderUI(selectivePlot(sensitivityAdv()[[5]],3))
   output$plotSensitivity15 <- renderUI(selectivePlot(sensitivityAdv()[[6]],3))
   output$plotSensitivity16 <- renderUI(selectivePlot(sensitivityAdv()[[7]],3))
   output$plotSensitivity17 <- renderUI(selectivePlot(sensitivityAdv()[[8]],3))
   output$plotSensitivity18 <- renderUI(selectivePlot(sensitivityAdv()[[9]],3))
   output$iucn <- renderUI({
     # if (T & input$rpath==editName) {
     #    editModUI("editor",height=height)
     # }
     # else {
     #   # leafletOutput("viewerMapview",height="500px")
     #    leafletOutput("viewerLeaflet",height=height)
     # }
      leafletOutput("viewerLeaflet",height=c("500px","auto")[1])
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
   output$tbl <- DT::renderDT(
      iris,options = list(lengthChange = FALSE,scrollX = TRUE)
   )
   'reminder2' <- function() {
      message("*** reminder fun (deprecated)")
      if (!input$simulate)
         return(-1L)
      done <- result()$input
      res <- 0L
      for (aname in names(done)) {
         v1 <- input[[aname]]
         if (is.null(v1))
            next
         if ((aname=="sexratio")&&(v1>1))
            v1 <- 0.01*v1
         v2 <- done[[aname]]
         res <- res+as.integer(!identical(v1,v2))
      }
      res
   }
   reminder <- reactive({
      message("*** reminder reactive")
      if (!input$simulate) {
         return(-1L)
      }
      done <- result()$input
      res <- 0L
      for (aname in names(done)) {
         if (aname=="indep.fraction")
            v1 <- c(input[["broken.C1"]],input[["broken.C2"]])
         else
            v1 <- input[[aname]]
         if (is.null(v1))
            next
         if ((aname=="sexratio")&&(v1>1))
            v1 <- 0.01*v1
         v2 <- done[[aname]]
         if (aname=="indep.fraction")
            v2 <- v2[2:3]
         res <- res+as.integer(!identical(v1,v2))
      }
      attr(res,"message") <- result()$message
      res
   })
   output$reminder <- renderText({
      res <- reminder()
      if (res<0)
         return("Not simulated yet")
      msg <- attr(res,"message")
      if (res>0)
         return("Inputs have been changed")
      if (nchar(msg))
         return(msg)
      "Ready for analysis"
   })
}
