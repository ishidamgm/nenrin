#detach("package:nenrin", unload = TRUE)


library(devtools)
devtools::install_github("ishidamgm/nenrin")
help(package="nenrin")

library(nenrin)
setwd("~/Dropbox/00D/00/ring/Abies_mariesii/277_h600")
ReadShapefile_AnnualRingPoints("points277_h600",id.tag="id",ring.tag="ring")


shapefiles::read.shapefile("points277_h600")


library(shapefiles)
read.shapefile("points277_h600")

dir()
nenrin::ReadShapefile_AnnualRingPoints("points277_h600",id.tag="id",ring.tag="ring")

shapefiles::read.shapefile("points277_h600")
Lplot(L)

shapefiles::read.shapefile("points277_h600")
?importFrom
