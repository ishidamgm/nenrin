# nenrin_source.R
# library(shapefiles)
#' setwd("../../Abies_mariesii/277_h600") #dir("../../Abies/277_h600")
#' source('C:/Script.R', encoding = 'UTF-8')
#' dir("../R_/")
#' setwd("C:/Users/ishid/Dropbox/00D/00/ring/_R/nenrin/R")
#' rm(list=ls()) ; ls()





# if(0) ####

if(0){
  source("nenrin_source.R")
  source("setClass.R")
  source("plot.R")
  source("calculation.R")
  source("ShapeFile.R")
  source("TreeRingsInterpolation.R")
  source("IncompleteTreeRingFix.R")
  source("TreeRingGrowth.R")

}


if(0){
  P<-ReadShapefile_AnnualRingPoints("points277_h600",id.tag="id",ring.tag="ring")
  save(TR,iTR,L,P,P00,ya,file="Abies277_h600.RData")
  load("Abies277_h600.RData")



  trw<-TreeRingWidthMatrix_ID(P,idn=8)
  plot(trw[[1]],type="l")
  for (i in 2:8)lines(trw[[i]],col=i)
}
