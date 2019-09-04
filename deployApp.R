## ?rsconnect::deployApp
opW <- options(warn=10)
require(rsconnect)
options(rsconnect.http=c("rcurl","curl","internal")[1]
       ,rsconnect.check.certificate=FALSE
       ,rsconnect.http.verbose=FALSE)
account <- c("nplatonov","wwf")[1]
opShiny <- getOption(switch(account[1],nplatonov="rsconnect",wwf="rsconnectWWF"
                           ,"rsconnectDummy"))
if (is.null(opShiny)) {
   message("Expected record of 'rsconnect' option (example):")
   opShiny <- list(name="yourname"
                  ,token=paste(sample(c(0:9,LETTERS[seq(6)]),32,rep=TRUE),collapse="")
                  ,secret=paste(sample(c(0:9,LETTERS,letters,"+"),40,rep=TRUE),collapse="")
                  )
   str(opShiny)
   opShiny <- NULL
   stop("Authentification data are not receieved")
}
appname <- c("demography","openday")[2]
appfiles <- c("about.md","demography.bib","interpretation.Rmd",".Renviron"
             ,"IUCN_pb_subpopulations.xlsx","IUCN_pb_subpopulations.shp.zip"
             ,"main.R","resources.R","simulate.R","analyze.R","perturb.R"
             ,"app.R","shinyInit.R","shinyUI.R","shinyServer.R"
             ,"www"
             )
appfiles
with(opShiny,setAccountInfo(name=name,token=token,secret=secret))
deployApp(appName=appname,appFiles=appfiles,account=opShiny$name)
options(opW)
warnings()
