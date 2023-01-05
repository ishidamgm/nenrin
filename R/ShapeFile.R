# ShapeFile.R ####

#' WriteShapefile_AnnualRings
#'
#' @param L2 is as list of annual ring polygons (X, Y)
#' @param filename is a file name of shape file written to disk.
#' The extension (.shp) is unnecessary.
#'
#' @return data of Shapefile
#' @export
#'
#' @examples
#' 補完推定した年輪のshapefile書き出し ####
#' WriteShapefile_AnnualRings (L, "test")
#'
WriteShapefile_AnnualRings <-function(L2=L,filename="test"){
  id.<-c();shp.<-c()
  yn.<-length(L2)
  yr.<-names(L2)
  for(ii in 1:yn.){
    shp.<-rbind(shp.,L2[[ii]])
    id.<-c(id.,rep((ii-1),nrow(L2[[ii]])))
  }
  dd <- data.frame(Id=id.,X=shp.[,1],Y=shp.[,2])
  ddTable <- data.frame(Id= 0:(yn.-1),ring= yr. )###=c("1","2","3","4","5")
  ddShapefile <- convert.to.shapefile(dd, ddTable, "Id", 3)
  shapefiles::write.shapefile(ddShapefile, filename, arcgis=T)
  return(ddShapefile)
}


#' ReadShapefile_AnnualRingPoints
#'
#' @param filename is a file name of annual ring points (shape file )
#' The extension (.shp) is unnecessary.
#'
#' @param   id  string, column name of id (attribute table)
#'
#'  @param ring string, column name of ring years  (0 is cambium layer)
#'
#' @return a data frame of AnnualRingPoints (radial input and correction points)
#' @export
#'
#' @examples
#' #年輪計測点の読み込み ####
#' ReadShapefile_AnnualRingPoints("../155_h120/points155_h120",id.tag="id",ring.tag="ring")
#'
ReadShapefile_AnnualRingPoints <-function(filename="points277_h600",id.tag="id",ring.tag="ring"){
    d<-st_read(paste0(filename,".shp"))　# 2022/12/18  str(d)
    xy<-st_coordinates(d)
    if(ncol(xy)==3){i<-(unique(xy[,3]))}else{i<-1:nrow(xy)}
    d.<-data.frame(d)[i,c(id.tag,ring.tag)]
    d<-data.frame(x=xy[,1],y=xy[,2],id=d.[,1],yr=d.[,2])
    d<-d[order(d$id,d$yr), ]
    return(d)
  }


#' return x,y coordinates of a tree ring center point (P00) from shape file of tree ring points
#'
#' @param filename is a file name of annual ring points (shape file )
#' The extension (.shp) is unnecessary.
#'
#' @param   id  string, column name of id (attribute table)
#'
#'  @param ring string, column name of ring years  (0 is cambium layer)
#'
#' @return numeric : x,y coordinates of a tree ring center point (P00)
#' @export
#'
#' @examples
#' #年輪計測点の読み込み ####
#' ReadShapefile_P00("points277_h600",id.tag="id",ring.tag="ring")
#'
ReadShapefile_P00 <-function(filename="points277_h600",id.tag="id",ring.tag="ring"){
  d<-ReadShapefile_AnnualRingPoints(filename,id.tag,ring.tag)
  i<-which(d$id==0); (P00<-c(d$x[i],d$y[i]))		###　原点 ■P00
  return(P00)
}

#' return x,y coordinates of a tree ring center point (P00) from shape file of tree ring points
#'
#' @param filename is a file name of annual ring points (shape file )
#' The extension (.shp) is unnecessary.
#'
#' @param   id  string, column name of id (attribute table)
#'
#'  @param ring string, column name of ring years  (0 is cambium layer)
#'
#' @return numeric : x,y coordinates of a tree ring center point (P00)
#' @export
#'
#' @examples
#' #年輪計測点の読み込み ####
#' d<-ReadShapefile_IncompleteTreeRingsPoints("277_h600_IncompleteRingPoints_shp",year_tag="yr",year_ref.tag="yr.ref")
#' d
ReadShapefile_IncompleteTreeRingsPoints <-function(filename,year_tag,year_ref.tag){
  # filename="277_h600_IncompleteRingPoints_shp"; year_tag="yr" ;year_ref.tag="yr.ref"
  #'
  d<-ReadShapefile_AnnualRingPoints(filename,id.tag=year_tag,ring.tag=year_ref.tag)
  names(d)[3:4]<-c("yr","yr.ref")
  d<-d[order(d$yr.ref),]
  return(d)
}


#' ReadShapefile_AnnualRings
#'
#' @param filename is a file name of shape file written to disk.
#' The extension (.shp) is unnecessary.
#'
#' @return a data frame of AnnualRingPoints (radial input and correction points)
#' @export
#'
#' @examples
#' #年輪計測点の読み込み ####
#' Ltest <- ReadShapefile_AnnualRings("line277_h600",ring.tag="id")
#' ReadShapefile_AnnualRings("L.comp",ring.tag="ring")
#' Lplot(Ltest)
ReadShapefile_AnnualRings <-function(filename="line277_h600",ring.tag="id"){
  d<-shapefiles::read.shapefile(filename)  	### ライン・データの読み込み
  ring<-as.numeric(as.vector(d$dbf$dbf[,ring.tag]))		###　年輪ポリゴン年数  #!!! 朝原さん用 $id

  # 入力順に格納されているのを小さい順に直す　jjjはその順番 #
  jjj <- order(ring)
  (ln<-length(d$shp$shp))
  L<-c()
  for (i in 1:ln)L<-c(L,list(as.matrix(d$shp$shp[[jjj[i]]]$points))) #### Lの追加

  (YR_L<-c(as.numeric(as.vector(d$dbf$dbf[,ring.tag])))[jjj])

  names(L)<-YR_L #　年輪数の名前を追加 #
  return(L)
}

#' return a list of coordinates(x,y) of cracks from shape file
#'
#' @param filename shape file name of cracks
#' @param ring.tag column name of year
#'
#' @return a list of coordinates(x,y) of cracks
#' @export
#'
#' @examples
#' ware <- ReadShapefile_Cracks(filename="loss277_h600_2",ring.tag="year")
#' Lplot(Lmove(TR@L2,TR@P00),col="gray")
#' sapply(ware,polygon,col="red")
#' sapply(ware,area)
#' plot(ya)
ReadShapefile_Cracks <-function(filename="loss277_h600_2",ring.tag="year")
{
  d<-shapefiles::read.shapefile(filename)  	### ライン・データの読み込み
  ring<-as.numeric(as.vector(d$dbf$dbf[,ring.tag]))		###　年輪ポリゴン年数  #!!! 朝原さん用 $id

  # 入力順に格納されているのを小さい順に直す　jjjはその順番 #
  jjj <- order(ring)
  (ln<-length(d$shp$shp))
  ware<-c()
  for (i in 1:ln)ware<-c(ware,list(as.matrix(d$shp$shp[[jjj[i]]]$points))) #### Lの追加

  (YR_L<-c(as.numeric(as.vector(d$dbf$dbf[,ring.tag])))[jjj])

  names(ware)<-YR_L #　年輪数の名前を追加 #
  return(ware)
}
