#+ include=F
source("./main.R")

#+ include=F
invisible({
})

#+
#res <- readRDS("./ongoing/analysis.rds")
#res$p5$plot

#+
#res <- readRDS("./ongoing/simulation.rds")
#str(res$input)
#str(res$output)

#+
res <- readRDS("./ongoing/inputs.rds")
#LS <- simulate(res)
LS <- do.call("simulate",res)
