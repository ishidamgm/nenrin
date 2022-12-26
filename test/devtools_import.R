#detach("package:nenrin", unload = TRUE)


library(devtools)
devtools::install_github("ishidamgm/nenrin")
devtools::install_github("ishidamgm/GISsource") ; library(GISsource)



library(nenrin) ;help(package="nenrin")

setwd("~/Dropbox/00D/00/ring/Abies_mariesii/277_h600")
ReadShapefile_AnnualRingPoints("points277_h600",id.tag="id",ring.tag="ring")


shapefiles::read.shapefile("points277_h600")


library(shapefiles)
read.shapefile("line155_h120")
read.shapefile("points155_h120")
read.shapefile("points277_h600")
library(sf)
d<-st_read("points155_h120.shp")
plot(d)
st_write(d,"!points155_h120.shp")
st_read("line155_h120.shp")



dir()
nenrin::ReadShapefile_AnnualRingPoints("points277_h600",id.tag="id",ring.tag="ring")

shapefiles::read.shapefile("points277_h600")
Lplot(L)

shapefiles::read.shapefile("points277_h600")
?importFrom
