# sf.R
library(sf)
st_read( "../155_h120/points155_h120.shp")


ReadShapefile_AnnualRingPoints <-function(filename="points277_h600",id.tag="id",ring.tag="ring"){
  # d<-read.shapefile(filename)　
  # shp<-d$shp$shp;
  # dbf<-d$dbf$dbf;	names(dbf)
  # id<-as.numeric(as.vector(dbf[,id.tag]));
  # #print(unique(id))
  # #print(table(id))
  # yr<-as.numeric(as.vector(dbf[,ring.tag]))	#### データフレームの作成
  # d<-data.frame(x=shp[,2],y=shp[,3],id,yr)
  # head(d)
  # return(d)
  #
  d<-st_read(paste0(P_filename,".shp"))　# 2022/12/18  str(d)
  xy<-st_coordinates(d)
  i<-(unique(xy[,3]))
  d.<-data.frame(d)[i,c(P_id_colname,P_ring_colname)]
  d<-data.frame(x=xy[,1],y=xy[,2],id=d.[,1],yr=d.[,2])
  d<-d[order(d$id,d$yr), ]
  return(d)
}
