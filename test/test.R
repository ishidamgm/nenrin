# shapefileの読み込み.R
library(shapefiles)
library(sf)
hd<-getwd()
setwd("B:/2021asahara/155_h120")

read.shapefile("points155_h120")
d<-st_read("points155_h120.shp")
str(st_coordinates(d))
edit(datad)

months
LETTERS
letters

mon<-c("J","F","M","A","M","J","J","A","S","O","N","D")
length(mon)


met

cor2yr


takahashi<-function(ii){
  (diskname<-names(YA[ii]))
  ya<-YA[[ii]]$Year_AnnualRingArea
  idx<-TreeRingIndex(ya)$idx
  r12<-cor2yr(idx,met,y1=1950,y2=2018)

  #Takahashi2011
  mon<-c("J","F","M","A","M","J","J","A","S","O","N","D")
  m<-c(mon[5:12],mon[1:10])
  rr<-r12[5:22]
  barplot(rr,names=m,main=diskname)
  abline(v=c(7.3,13.3),lty=2,lwd=3)
  mtext(c("PGS","DS","CGS"),side=3,at=c(3,10,16.5))
}

# disk number
windows()
par(mfrow=c(3,4))
for(i in 1:length(YA))takahashi(i)


