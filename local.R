#+ code=readLines("./main.R"), echo=F

#+
source("./main.R")

#+
#ls()
#str(colorScheme)

#+ include=F, eval=F
#res <- readRDS("./data/analysis.rds")
#res$p5$plot

#+
LS <- readRDS("./data/simulation.rds")
#str(LS$input)
#str(LS$output)
s <- perturb(LS,pattern="broken.C2")
s1 <- s[[1]] ## $data, $plot
#str(s1$data)

#+ results='asis'
ursa:::browse(s1$plot)

#+
#res <- readRDS("./data/inputs.rds")
#LS <- simulate(res)
#LS <- do.call("simulate",res)
