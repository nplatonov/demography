suppressMessages({
   require(shiny)
   require(shinyjs)
   require(leaflet)
   require(webshot)
})
source("./main.R")
height <- c('1/1'="600px",'1/2'="273px",'1/3'="182px"
           ,'1/4'="136px",'2/3'="364px") ## 600 248 165 (Chrome: 1374x736)
sliderWidth <- c("100%","125%","115%")[2]
spinWidth <- c("80px","80px")
devel <- length(grep("^[A-Z]\\:/platt",Sys.getenv("USER")))>0 & !interactive() &
   length(grep("vanilla",commandArgs(FALSE)))==0
logpath <- "./data"
if (dir.exists(logpath))
   invisible(file.remove(dir(path=logpath,patter="\\.rds$",full.names=TRUE)))
sessionid <- paste0("-",paste(sample(letters,4,rep=TRUE),collapse=""))
if (!devel) {
  # options(shiny.launch.browser=TRUE)
} 
epsgList <- 3571:3576
'plotEmpty' <- function(desc="Simulation has not been done yet") {
   op <- par(mar=rep(1,4))
   plot(0,0,type="n",axes=FALSE,xlab="",ylab="")
   text(0,0,desc)
   box()
   par(op)
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
   m <- addTiles(m,urlTemplate=paste0("https://{s}.tiles.arcticconnect.ca/osm_"
                                     ,epsg,"/{z}/{x}/{y}.png")
                ,attribution="Map: © ArcticConnect. Data: © OpenStreetMap contributors"
                ,options=tileOptions(subdomains="abc"
                                    ,noWrap=TRUE,continuousWorld=FALSE)) 
  # if (!is.null(data))
  #    m <- addPolygons(m,data=data,weight=0.5)
  # m <- addGraticule(m,interval=10)
   m
}
