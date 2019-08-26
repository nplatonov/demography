if (F)
   suppressMessages(require(highcharter))
if (T)
   suppressMessages(require(plotly))
require(ggplot2)
source("simulate.R")
source("analyze.R")
source("resources.R")
isShiny <- ("shiny" %in% loadedNamespaces())
init <- randomize()
options(stringsAsFactors=FALSE
       ,show.error.messages=TRUE
       ,warn=ifelse(isShiny,1,10)
       )
