# nenrin_source.R
# setwd("../Abies_mariesii/277_h600") #dir("../../Abies/277_h600")
#rm(list=ls()) ; ls()

# library    library(shapefiles)


degree<-pi/180  #### ラジアン定数


# setClass  class_DiskInfo ####
setClass (
  "class_DiskInfo",
  slots=c(
    id="character",
    code="character",
    species="character",
    place="character",
    latitude="numeric",
    longitude="numeric",
    altitude="numeric",
    year_Pith="numeric",
    year_OutmostRing="numeric",
    DiskSamplingDate="character",
    DiskSampler = "character",
    analyst="character",
    memo="character"
  )
)

# Analysis ####
# setClass  class_YearArea ####
setClass (
  "class_YearArea",
  slots=c(
    wd="character",
    L2="list",
    L3="list",
    Year_Area0="data.frame",
    Year_CrackArea="data.frame",
    Year_Area="data.frame"
  )
)

#' relationships year and tree(annual) ring area
#'
#' @param YA
#'
#' @return
#' @export
#'
#' @examples
#' L2<-TR@L2
#' YearArea(L2,yr_end=2018)
#'
YearArea <- function(L2,yr_end=2018){
  a<-rev(sapply(L2,area))
  yr_a<-(yr_end-length(a)+1):yr_end
  Year_DiskArea <- data.frame(year=yr_a,DiskArea=a)
  plot(Year_DiskArea)

  windows()
  Year_TreeRingArea <- data.frame(year=yr_a[-1],TreeRingArea=diff(a))
  plot(Year_TreeRingArea,type="b")
  abline(v = seq(floor(min(yr_a)/10)*10, max(yr_a),10 ), col = "gray")

  return(YA)
}


setClass (
  "class_disk",
  #slots=c(wd="character",L_filename="character"), #""class_DiskInfo",
  #contains=c( "class_TreeRingsInterpolate","class_TreeRingsInterpolate ") #
  contains=c( "class_DiskInfo", "class_Analysis")
)


# setClass_class_TreeRingsInterpolate ####

setClass (
  "class_TreeRingsInterpolate ",                         # クラス名

  slots=c(                   # スロットの型指定
    P_filename ="character",
    P_id.tag="character",
    P_ring.tag="character",

    P    = "data.frame", #放射計測点:データフレーム　x,y,id,yr,r,deg
    P00  = "numeric", #  原点:c(px00,py00) 髄の中心　通常　0,0 の近く　放射計測点.shp　id==0の点に格納
    n_id = "numeric",	#　放射計測線本数　	:計測線本数　length(unique(P$id)) ただし原点(髄)id0を除いたP
    YR_P = "numeric", #　総年輪		　	:ベクトル一番外側の形成層も含む推定後の年輪の年　=　unique(P$ring)　
    n_YR = "numeric", # 総年輪数			:整数　一番外側の形成層も含む推定後の総年輪数　 = unique(P$yr), length(L2)

    L_filename ="character",
    L_id.tag="character",
    L_ring.tag="character",
    L = "list",	      #　年輪ポリゴン　		:リスト$年数-マトリクスx,y
    L_= "data.frame", #データフレーム　x,y,r,rad,deg
    YR_L= "numeric",  #ベクトル　(外側から)　年輪ポリゴンの外から数えた年数　=dbf$ring,　names(L)
    ln= "numeric",    #ン数		:整数　length(L)
    L2= "list",        # list : tree ring polygons interpolated
    L2_filename ="character"
    #maLr= "numeric",		:整数　n_YR-1
    #PL= "numeric",	####　放射計測点		:マトリクス年数(1年刻み),ライン番号
  )
)


# setClass_class_TreeRingsInterpolate ####

setClass(
  "class_IncompleteTreeRingsFix",                         # クラス名

  slots=c(                   # スロットの型指定
    P00="numeric",     #x,y coordinates of a center point of tree rings
    L_filename ="character",
    L_id.tag="character",
    L_ring.tag="character",
    L = "list",	      #　年輪ポリゴン　		:リスト$年数-マトリクスx,y
    L_= "data.frame", #データフレーム　x,y,r,rad,deg
    yr = "numeric",  #ベクトル　(外側から)　年輪ポリゴンの外から数えた年数　=dbf$ring,　names(L)
    rn = "numeric",  #ベクトル　(外側から)　年輪ポリゴンの外から数えた参照番号
    inc.ring = "data.frame", #同一点を相互に有する年輪番号。不完全年輪修正にかかわる。
    inc_filename ="character",    #  inc.ring の保存ファイル名(.csv)
    pn ="table",
    inc_edit="data.frame",
    ln= "numeric",    #入力年輪ポリゴン数		:整数　length(L)
    L2= "list",        # list : tree ring polygons fixed
    L2_filename ="character" #  file name of L2
  )
)






# calculation ####

#### 多角形の面積を求める関数　
#' area return a  area from polygon xy coordinates
#'
#' @param xy a atrix or data frame of xy coordinates
#'
#' @return a vector of polygon area
#' @export
#'
#' @examples
#' xy<-data.frame(x=c(0,1,2,1),y=c(1,2,1,0))
#' plot(xy,type="b") ; polygon(xy)
#' area(xy)
#'
area <- function(xy){
  x <- xy[,1];y <- xy[,2]
  x2 <- c(x[2:length(x)], x[1])
  y2 <- c(y[2:length(y)], y[1])

  abs(sum((x2-x)*(y+y2)/2))
}

####　点間の距離を求める関数　
#' dstpp returns vector distance between adjacent two points
#'
#' @param x vector of x coordinates
#' @param y vector of y coordinates
#'
#' @return  vector distance between adjacent two points
#' @export
#'
#' @examples
#' x<-l.[,1] ;y<-l.[,2]
#' dstpp(x,y)
dstpp<-function(x,y){
  x2<-c(rev(x)[1],x[-length(x)])
  y2<-c(rev(y)[1],y[-length(y)])
  dxy<-sqrt((x-x2)^2+(y-y2)^2)
  return(dxy)
}

####　点間の距離を求める関数
#' dst return a vector of distances from original a point (0,0)
#' from a matrix or data frame of xy coordinates
#'
#' @param xy a matrix or data frame of xy coordinates
#'
#' @return a vector of distances from original a point
#' @export
#'
#' @examples
#' plot(L[[1]])
#' plot(dst(L[[1]]))
dst <- function(xy){#xy<-L[[19]]
  x<-xy[,1] ; y <-xy[,2]
  sqrt(diff(x)^2+diff(y)^2)
}

#### 内側年輪の最近角点番号の取得　数は外側年輪の打点数と同じ

#' nstP return a vector of row numbers of  points that have nearest  center angle
#'
#'
#' @param z1 a data frame or a matrix of  xy coordinates of a tree ring  (usualy inner ring)
#' @param z2  a data frame or a matrix of  xy coordinates of a tree ring  (usualy outer ring)
#'
#' @return a vector of row numbers of z2, the length is nrow(z1)
#' @export
#'
#' @examples
#' L_out<-L[[1]];L_in<-L[[30]]
#' np<-nstP(L_out,L_in)
#' plot(L_out,col="red"); points(L_in)
#' segments(L_out[,1],L_out[,2],L_in[np,1],L_in[np,2],col="blue")
#'
nstP<-function(z1,z2){
  a1<-atan2(z1[,2],z1[,1]) ; a2<-atan2(z2[,2],z2[,1])
  np<-c()
  for (i in 1:nrow(z1)){
    np<-c(np,which.min((cos(a1[i])-cos(a2))^2+(sin(a1[i])-sin(a2))^2))
  }
  return(np)
}




#' seq.deg return vector of sequence of angles between start and end angle
#'  0 to pi -pi to 0
#'
#' @param deg1 start angle
#' @param deg2 end angle
#' @param deg.by  step of sequence
#'
#' @return vector of sequence of angles between start and end angle
#' @export
#'
#' @examples
#' seq.deg(170,-170,.5)
#'
seq.deg <- function(deg1,deg2,deg.by=1){
  if((deg1<=0 & deg2<=0) | (deg1>=0 & deg2>=0))return(seq(deg1,deg2,deg.by))
  if(deg1>=0 & deg2<=0) return(c(seq(deg1,180,deg.by),seq(-180,deg2,deg.by)))
  if(deg1<=0 & deg2>=0) return(c(seq(deg1,0,deg.by),seq(0,deg2,deg.by)))
}




#' rdst return relative distance between two representative tree rings
#'
#' @param L_ is data frame of tree ring
#' @param yr_out is a integer of year of outer ring
#' @param yr_in  is a integer of year of inner ring
#' @param x is a value.  x coordinate of a target point
#' @param y is a value.  y coordinate of a target point
#'
#' @return is  a data frame with relative distance and center angle
#' @export
#'
#' @examples
#' rdst.<-rdst(L,P,73)
#' plot(rdst.)
#' spline<-smooth.spline(rdst.$rad,rdst.$rdst, spar =0.0002)
#'　lines(predict(spline,seq(-pi,pi,0.01)),col="red")

rdst <-function(L,P,yr){
  i<-which(P$yr==yr)
  x<-P$x[i] ; y<-P$y[i]

  yr.<-as.numeric(names(L))
  i<-tail(which(yr.<yr),1)
  yr1<-yr.[i];yr2<-yr.[i+1]
  L1<-L[[i]] ; L2<-L[[i+1]]
  ##  plot(L2,type="l",col="red");lines(L1,col="blue"); lines(x,y,type="b")
  r <-sqrt( x^2+ y^2)
  rad <- atan2(y,x)
  r1<-sqrt(L1[,1]^2+L1[,2]^2)
  r2<-sqrt(L2[,1]^2+L2[,2]^2)

  r_in <-  r1[nstP(cbind(x,y),L1)]
  r_out <- r2[nstP(cbind(x,y),L2)]
  rdst. <- (r_out-r) / (r_out-r_in)
  rdst.[rdst.==Inf] <-0
  i<-order(rad)
  return(data.frame(rad,rdst=rdst.)[i,])
}



#' rdst_MerginePlus return relative distance between two representative tree rings
#'
#' @param L_ is data frame of tree ring
#' @param yr_out is a integer of year of outer ring
#' @param yr_in  is a integer of year of inner ring
#' @param x is a value.  x coordinate of a target point
#' @param y is a value.  y coordinate of a target point
#'
#' @return is  a data frame with relative distance and center angle(degree)
#' with mergine (-90 - 0 - 360 - 90)
#' @export
#'
#' @examples
#' year.<-73
#' rdst.<-rdst_MerginePlus(L,P,year.)
#' plot(rdst.,xlim=c(-200,200),main=year.)
#' spline<-smooth.spline(rdst.$deg,rdst.$rdst, spar =0.0002)
#'　lines(predict(spline,seq(-202,220,1)),col="red")
#'　
#'　
rdst_MerginePlus <- function(L,P,yr){ #,pi/2
   ddeg<-180
   df <- rdst(L,P,yr) #nrow(df)
   deg.<- df$rad/degree
   rdst.<- df$rdst
   # front
   i<--ddeg <deg. & deg. < 0
  deg.pre<-deg.[i]+360 ; rdst.pre<-rdst.[i]
  # back
  i<- 0 <deg. & deg. < ddeg
  deg.pos<-deg.[ i ]-360 ;rdst.pos<-rdst.[ i ]
  deg.<- c(deg.pre,deg.,deg.pos)
  rdst.<- c(rdst.pre,rdst.,rdst.pos)
   return(data.frame(deg=deg., rdst=rdst.))

  }
sn_RingSegmentsInsert <- function(sn,InsertPoints,RingSegments){
  j<-c(1:InsertPoints[1],RingSegments[[1]])
  for(i in 2:length(InsertPoints)){
    j<-(c(j,(InsertPoints[i-1]+1):InsertPoints[i],RingSegments[[i]]))
  }
  if(rev(sn)[1]!=rev(InsertPoints)[1]){
    j<-c(j,(rev(InsertPoints)[1]+1):rev(sn)[1])}
  return(j)
}

#' against_numbers
#'
#' @param n1  is  vector of integer sequence number
#' @param n2  is  vector of integer sequence number
#'
#' @return This function retunr a vector n2 eliminate n1
#' @export
#'
#' @examples
#' against_numbers(1:5,1:100)
against_numbers <- function(n1=1:5,n2=1:100){
  return(n2[!is.element(n2,n1)])
}


#' rotate_numbers returns rotate number sequence (for circle data)
#' @param j   all number
#' @param j1  start number
#' @param j2  end number
#'
#' @return rotate number sequence
#' @export
#'
#' @examples
#' j<-1:100
#' through_numbers(j,5,20)
#' through_numbers(j,95,20)
#' against_numbers(rotate_numbers(j,95,20),j)
#'
rotate_numbers<-function(j,j1,j2){
  if(j1<j2){i12<-j1:j2}else{i12<-c(j1:length(j),1:j2)}
  return(i12)
}




#IncompleteAnnualRings_RingSegmentsInsert.R

#' Lmove move the tree rings coordinates based on P00 (x,y movement coordinates).
#'
#' @param L is a list of tree rings(x,y coordinates).
#' @param P00 x, y coordinates of a center point (usually a pith).
#'
#' @return
#' @export
#'
#' @examples
#' Lplot(L)
#' sapply(Lmove(L,P00),lines,col="red")
Lmove <- function(L,P00){
  # 座標修正　髄の中心P00と【0,0】の差分もどす #
  for (i in 1:length(L))L[[i]]<-t(t(L[[i]])+P00)
  return(L)
}

#' Llist2dataframe  convert from a list of tree rings polygons (L)
#' to a data frame with no.,year,x,y,r(radius),radian(center angle),degree.
#' The data frame is sorted by degree(0 to 360).
#'
#' @param L
#'
#' @return data fraeme
#' @export
#'
#' @examples
#' L_ <- Llist2dataframe(L)
#' head(L_) ; tail(L_)
Llist2dataframe <-function(L){
  L_<-c()
  for (i in 1:length(L)){
    x<-L[[i]][,1];y<- L[[i]][,2]
    Lr   <- sqrt(x^2+y^2)
    Lrad <- atan2(y,x)
    #Ldeg <- (Lrad/degree+360)%%360
    Ldeg <- Lrad/degree
    L.df <- data.frame(i,yr=names(L)[i],x,y,r=Lr,rad=Lrad,deg=Ldeg)
    #!!! 角度順にL座標ソート　#
    #　(部分的に逆方向に入力してしまったような場合　クマハギなどの巻き込みの場合は使用不可)

    L.df <- L.df[order(L.df$deg),]
    L_<-rbind(L_,L.df)
  }
  return(L_)
}



#' return a ring number of tree ring polygons list (L) from year
#'
#' @param L tree ring polygons list (L)
#' @param yr years (or rings)
#'
#' @return a ring number of tree ring polygons list (L)
#' @export
#'
#' @examples
#' Lrn(L,8)
Lrn <- function(L,yr){
  return(which(names(L)==yr))
}


#' Lsort
#'
#' @param l.  x,y coordinates matrix (ncol=2) or data.frame of an annual ring.
#'
#' @return    ordered with center angle of each point
#' @export
#'
#' @examples
#' save(l.,file="l..RData")
#' plot(l.,type="l")
#' plot(Lsort(l.),type="l")
#'
Lsort <- function(l.){#
  x<-l.[,1] ; y<- l.[,2]
  #Ldeg <- (atan2(y,x)/degree+360)%%360
  Ldeg <- atan2(y,x)/degree
  return(l.[order(Ldeg),])

}

#' Lsort_all
#'
#' @param L  a list of annual ring polygons (x,y)
#'
#' @return   a list of annual ring polygons (x,y) ordered with center angle of each point
#' @export
#'
#' @examples
#' Lsort_all(L)
Lsort_all <- function(L){
  L000 <- c()
  for(i in 1:length(L)){
    L000 <- c(L000,list(Lsort(L[[i]])))
  }
  names(L000) <- names(L)
  return(L000)
}


#' Ldeg360 return a vector of center angle 0 to 360(degree) for x y coordinate vector
#'
#' @param x is a vector of x coordinates
#' @param y is a vector of y coordinates
#'
#' @return a vector of center angle 0 to 360(degree) for x y coordinate vector
#' @examples
#'  xy <- L[[1]]
#'  Ldeg360(xy[,1],xy[2])
Ldeg360 <- function(x,y){
  return((atan2(y,x)/degree+360)%%360)
}

# plot ####

#' Lplot plots a graphics of tree rings
#'
#' @param L is a list of tree rings polygon coordinates (X,Y)
#' @param rn vector of ring number of list (L), default 1:length(L)
#' @export
#'
#' @examples
#' Lplot(L)
#' Lplot(L,rn=1:50,type="l")
Lplot<-function(L,rn=1:length(L),...){
  plot(L[[1]],type="l",col="red")
  for(i in rn)lines(L[[i]],...)
}

#' Lplot2 plots a graphics of tree rings by 1 ring (3*3 in a screen)
#'
#' @param L is a list of tree rings polygon coordinates (X,Y)
#' @param nrow
#' @param ncol
#'
#' @export
#'
#' @examples
#' load("L.RData")
#' Lplot2(L,type="b")
#' load("L_001.RData")
#' Lplot2(L,type="b")
#' Lplot2(L,i.ring=1:9, type="b")
#' Lplot2(L,i.ring=1:9, nrow=1,ncol=1,type="b")
Lplot2<-function(L,i.ring=1:length(L), nrow=3,ncol=3,ask="FALSE",...){
  par(mfrow=c(nrow,ncol))
  for (i in i.ring)  plot(L[[i]],main=paste(i,":",names(L)[i],"yr"),...)
  par(mfrow=c(1,1))

}

#' Lrad.plot check center angle of points to input order
#'
#' @param L
#' @param i.ring
#'
#' @return
#' @export
#'
#' @examples
#' load("L.RData")
#' Lrad.plot(L,i.ring)
#' load("L_001.RData")
#' Lrad.plot(L,i.ring)
#'
Lrad.plot<-function(L,i.ring=1:length(L)){
  par(mfrow=c(3,3))
  for (i in i.ring) {
    x<-L[[i]][,1] ;  y<-L[[i]][,2] ; rad <- atan2(y,x)
    plot(rad,main=paste(i,":",names(L)[i],"year"),ylim=c(-pi-.1,pi+.1))
    abline(h=c(-pi,0,pi),col="red")

  }
  par(mfrow=c(1,1))
}

#' plot_AnnualRing
#' This function draws annual rings of a disk from x, y list(x,y) with name of year.
#' @param L   list(x,y) of annual ring coordinates with name of year
#' @param year_label name of  column of annual ring year (0(cambium),1,2,....,n(pith))
#'
#' @return
#' @export
#'
#' @examples
#' plot_AnnualRing(L.,year=8)
#' plot_AnnualRing(L,year="0",type="b",col="red")
#
plot_AnnualRing<-function(L=L,year=0,...){
  xy<-L[[which(names(L)==year)]]
  plot(xy,...)
}

#' plot_AnnualRings_df
#' This function draws annual rings of a disk from data frame(x,y,year).
#' @param df  name of a data frame
#' @param year_label name of  column of annual ring year (0(cambium),1,2,....,n(pith))
#'
#' @return
#' @export
#'
#' @examples
#' plot_AnnualRings_df(L_)
#'
plot_AnnualRings_df<-function(df=L_,year_label="yr"){
  plot(df[df[,year_label]==0,c("x","y")],type="l")
  for (i in  uniue(df[,year_label])) lines(df[df[,year_label]==i,c("x","y")])
}

#' plot_AnnualRing_df
#' This function draws a annual ring of a disk from data frame(x,y,year).
#' @param df  name of a data frame
#' @param year_label name of  column of annual ring year (0(cambium),1,2,....,n(pith))
#'
#' @return
#' @export
#'
#' @examples
#' plot_AnnualRing_df(L_,1)
#'
plot_AnnualRing_df<-function(df=L_,year=0,year_label="yr"){
  plot(df[df[,year_label]==year,c("x","y")],type="l")
}

#' plot and return data frame of  year_disk area and year_annual ring area
#'
#' @param L2
#' @param yr_end
#'
#' @return
#' @export
#'
#' @examples
#' L2<-TR@L2
#' plot_year_RingArea(L2,2018)
plot_year_RingArea<-function(L2,yr_end=2018){
  a<-rev(sapply(L2,area))
  yr_a<-(yr_end-length(a)+1):yr_end
  Year_DiskArea<-data.frame(Year=yr_a,Disk_Area=a)
  plot(Year_DiskArea)

  windows()
  Year_AnnualRingArea<-data.frame(Year=yr_a[-1],Annual_Ring_Area=diff(a))
  plot(Year_AnnualRingArea,type="b")

  abline(v = seq(round(min(yr_a)/10)*10, max(yr_a), 10), col = "gray")

  return(list(Year_DiskArea=Year_DiskArea,Year_AnnualRingArea=Year_AnnualRingArea))

}


# shape file read write ####

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
  write.shapefile(ddShapefile, filename, arcgis=T)
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
#' ReadShapefile_AnnualRingPoints("points277_h600",id.tag="id",ring.tag="ring")
#'
ReadShapefile_AnnualRingPoints <-function(filename="points277_h600",id.tag="id",ring.tag="ring"){
  d<-read.shapefile(filename)　
  shp<-d$shp$shp;
  dbf<-d$dbf$dbf;	names(dbf)
  id<-as.numeric(as.vector(dbf[,id.tag]));
  #print(unique(id))
  #print(table(id))
  yr<-as.numeric(as.vector(dbf[,ring.tag]))	#### データフレームの作成
  d<-data.frame(x=shp[,2],y=shp[,3],id,yr)
  head(d)
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
  d<-read.shapefile(filename)  	### ライン・データの読み込み
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

# Incomplete tree ring fix  ####

# setClass_class_IncompleteTreeRingsFix ####



#' sets class for IncompleteTreeRingsFix, should run this function before start IncompleteTreeRingsFix
#'
#' @param L_filename
#' @param L_id.tag
#' @param L_ring.tag
#'
#' @return class　of IncompleteTreeRingsFix
#' @export
#'
#' @examples
#' iTR<-IncompleteTreeRingsFix_setClass(L_filename="line277_h600_2",L_id.tag="",L_ring.tag="id")
#' iTR<-IncompleteTreeRingsFix_RingList(iTR)
#' P00<-ReadShapefile_P00("points277_h600",id.tag="id",ring.tag="ring")
#' iTR@P00 <-P00
#' L  <-Lsort_all( Lmove(iTR@L,-P00))
#' Lplot(L)
#' L_ <- Llist2dataframe(L)
#' # manual correct
#' slotNames(iTR)
#' d<-iTR@inc.ring
#' idst<-c() ; irn<-c() ; n<-c()
#' for(i in iTR@yr[-1]){
#' n<-c(n,sum(L_$yr==i))
#'  idst<-c(idst,list(dst(d[d$yr==i,c("x","y")])))
#'  irn<-c(irn,list(d$pn[d$yr==i]))
#' }
#'
#' names(irn) <- names(idst) <- iTR@yr[-1]
#' n<-data.frame(yr=iTR@yr[-1],n)
#' idst ; irn ;n
#'if(0){
#' i<-d[,c("yr1","yr2","yr3")]==2
#' d[,c("pn1","pn2","pn3")][i]
#'}
#'
#' edit(iTR@inc.ring)
#' #' iTR@pn
#' L2<-L
#' Lplot2(L2,1:5)
#' 可能性として
#' #8yr ref 9yr, 9yr earlier
#' (d <- iTR@inc_edit)
#' (d<-d[-1,])  # outer ring omited
#' #edit(d)
#' 8年は9年を参照している
#' (d<-d[c(1:7,9,8,11:nrow(d)),]) # 8年9年交代
#' d$rn.ref[which(d$yr==9)]<-8 #9年は7年 rn=8 を参照する
#' d$rn.ref[which(d$yr==8)]<-10 #8年は9年 rn=10を参照する
#'
#'
#'
#' #

#' edit(d)
#' #######
#'
#' L2<-L
#' Lplot2(L2,d$rn)
#' #'
#' L2<-L
#' i12<-1:nrow(d) #;i12<-1:9
#' for( i in i12){#nrow(d)
#' j<-d$rn[i]
#'   L2[[j]]<-IncompleteTreeRingsBind(L2,j,d$rn.ref[i])
#' }
#'
#' Lplot2(L2,d$rn[i12],nrow=3,ncol=3,type="b",col="red")
#'
#' #WriteShapefile_AnnualRings (Lmove(L2,P00), "test20221112")
#'
#' plot(IncompleteTreeRingsBind(L2,9,10,plot.yn=T))
#'plot(L2[[3]])
#' d
IncompleteTreeRingsFix_setClass <-function(L_filename="line277_h600_2",L_id.tag="id",L_ring.tag="ring")
  {
  #  L_filename="line277_h600_2"; L_ring.tag="id"
  iTR<-new("class_IncompleteTreeRingsFix")
  iTR@L_filename=L_filename
  iTR@L_ring.tag=L_ring.tag
  # L_filename <- iTR@L_filename ; L<-iTR@L ; L_ring.tag<-iTR@L_ring.tag
  #　■L　年輪ポリゴンデータの追加と完成　■YR_L■ln #
  d<-read.shapefile( L_filename)  	### ライン・データの読み込み
  ring<-as.numeric(as.vector(d$dbf$dbf[,L_ring.tag]))
  # 入力順に格納されているのを小さい順に直す　jjjはその順番 #
  jjj <- order(ring)
  ln<-length(d$shp$shp)
  L<-c()
  for (i in 1:ln)L<-c(L,list(as.matrix(d$shp$shp[[jjj[i]]]$points))) # Lの追加
  L<-Lsort_all(L)
  YR_L<-c(as.numeric(as.vector(d$dbf$dbf[,L_ring.tag])))[jjj]
  names(L)<-YR_L #　年輪数の名前を追加 #

  #　リストデータのデーターフレーム化 #
  L_ <- Llist2dataframe(L)
  iTR@L=L ; iTR@L_ = L_ ; iTR@ln = ln ;
 #str(iTR)
  return(iTR)
}



#' return data frame of points list duplicated,
#'
#' @param iTR
#'
#' @return iTR
#'
#' @export
#'
#' @examples
#' iTR<-IncompleteTreeRingsFix_RingList(iTR)
#' iTR@pn
IncompleteTreeRingsFix_RingList <- function(iTR){
  L = iTR@L ; L_ = iTR@L_
  # 重複同一点の検出 #
  i<-duplicated(L_[,c("x","y")]) #
  rn<-which(i)
  inc.ring <- L_[i,]
  inc.ring$pn<-which(i) #

  #
  pn.<-c() ; yr. <- c()
  inc.ring<-data.frame(inc.ring,pn1=0,pn2=0,pn3=0,yr1=0,yr2=0,yr3=0)#,pn1=0,pn2=0,)
  for(i in 1:nrow(inc.ring)){#i=3
    j<-which(L_$x==inc.ring$x[i] & L_$y==inc.ring$y[i])
    pn. <-c( pn.,list(j))
    yr. <-c( yr.,list(L_$yr[j]))
    inc.ring[i,9:(9+length(j)-1)]<-j
    inc.ring[i,12:(12+length(j)-1)]<-L_$yr[j]
  }
  #pn. ;  yr.
  yr.<-sort(as.numeric(c(inc.ring$yr1,inc.ring$yr2,inc.ring$yr3)))
  pn. <-table(yr.)
  yr.<-sort(unique(yr.))

  rn.<-match(yr.,names(L))
  inc_edit <-data.frame(yr=yr.,rn=rn.,rn.ref=rn.-1)

  iTR@inc.ring = inc.ring;iTR@yr = yr.;iTR@pn = pn.
  iTR@rn = rn. ;iTR@inc_edit=inc_edit
  #slotNames(iTR)
  iTR@inc_filename=paste0(iTR@L_filename,"_IncompleteRing.csv")
  write.csv(inc.ring,file=iTR@inc_filename)
  #edit(iTR@inc.ring)

  return(iTR)

}

#' return reference number of tree ring polygons (L) with missing range determined median magnification
#'
#' @param L
#' @param mag.med  median magnification, threshold for decision as missing ring range
#'
#' @return numeric vector of L list reference number
#' @export
#'
#' @examples
#' i<-IncompleteTreeRings_MissingRings(L,mag.med=200)
#' length(i)
#' Lplot2(L,i)
#'
IncompleteTreeRings_MissingRings<-function(L,mag.med=30){
rn<-c()
 for(i in 1:length(L))  {#1<-1
   l.<-L[[i]] ;  l. <- rbind(l.,l.[1,])
   dst.<-dst(l.)
   med<-median(dst.)
   rn<-c(rn,max(dst.)>mag.med*med)
 }
return(which(rn))
}



#' IncompleteTreeRingsBind bind complete tree ring line to incomplete tree ring part
#'
#' @param rn     reference number (tree ring list ex. L) of   incomplete tree ring
#' @param rn0    reference number (tree ring list ex. L) of adjacent complete tree ring
#'
#' @return      data frame of x y coordinates compensated incomplete tree ring part
#' @export
#'
#' @examples
#'
#' #load("L_000.RData");rn=2;rn0=1 #
#' #rn=3;rn0=2 # rn=4;rn0=3 # rn=17;rn0=18
#'
#' load("L_001.RData")
#' rn=20;rn0=19
#' par(mfrow=c(1,3)) # par(mfrow=c(1,1))#'
#' plot(L[[rn]],type="b")
#' plot(L[[rn0]],type="b")
#' lnew<-IncompleteTreeRingsBind(L,rn,rn0,plot.yn=T)
#' plot(lnew,type="l",col="red")
#' par(mfrow=c(1,1))
#' plot(L[[rn0]],type="l") ; lines(lnew,type="l",col="red")
#'
#'
#' #
#' iTR<-IncompleteTreeRingsFix_setClass(L_filename="line277_h600_2",L_id.tag="",L_ring.tag="id")
#' iTR<-IncompleteTreeRingsFix_RingList(iTR)
#' P00<-ReadShapefile_P00("points277_h600",id.tag="id",ring.tag="ring")
#' iTR@P00 <-P00
#' L  <-Lsort_all( Lmove(iTR@L,-P00))
#' Lplot(L)                        # plot tree lings
#' L_ <- Llist2dataframe(L)
#' # manual correct
#' slotNames(iTR)
#' d<-iTR@inc.ring
#' idst<-c() ; irn<-c() ; n<-c()
#' for(i in iTR@yr[-1]){
#' n<-c(n,sum(L_$yr==i))
#'  idst<-c(idst,list(dst(d[d$yr==i,c("x","y")])))
#'  irn<-c(irn,list(d$pn[d$yr==i]))
#' }
#'
#' names(irn) <- names(idst) <- iTR@yr[-1]
#' n<-data.frame(yr=iTR@yr[-1],n)
#' idst ; irn ;n
#'if(0){
#' i<-d[,c("yr1","yr2","yr3")]==2
#' d[,c("pn1","pn2","pn3")][i]
#'}
#'
#' #edit(iTR@inc.ring)
#' iTR@pn  # 年輪合流点確認　(Qgisのsnap)
#' L2<-L  #　年輪修正用のコピー
#' (d <- iTR@inc_edit)　#作業行程ファイル　不完全年輪と参照年輪
#' (d<-d[-1,])  # outer ring omited 形成層は外す
#' #edit(d)
#' 8年は9年を参照している　9年も不完全年輪で7年を参照している
#' (d<-d[c(1:7,9,8,11:nrow(d)),]) # 8年9年交代
#' d$rn.ref[which(d$yr==9)]<-8 #9年は7年 rn=8 を参照する
#' d$rn.ref[which(d$yr==8)]<-10 #8年は9年 rn=10を参照する
#'
#'
#'
#' # edit(d)
#' #######　修復作業　>>>>
#'
#' L2<-L
#' Lplot2(L2,d$rn)
#' #'
#' L2<-L
#' i12<-1:nrow(d) #;i12<-1:9
#' for( i in i12){#nrow(d)
#' j<-d$rn[i]
#'   L2[[j]]<-IncompleteTreeRingsBind(L2,j,d$rn.ref[i])
#' }
#' # <<<<<<<<<
#' Lplot2(L2,d$rn[i12],nrow=3,ncol=3,type="b",col="red")#'
#' #WriteShapefile_AnnualRings (Lmove(L2,P00), "test20221112")
#'
IncompleteTreeRingsBind<-function(L,rn=2,rn0=1,plot.yn="FALSE"){#plot.yn="TRUE"
  l <-L[[rn ]] ;  x  <- l[,1]  ; y <- l[,2] ;rad  <- atan2( y, x);r  <- sqrt(x^2+y^2)
  l0<-L[[rn0]] ;  x0 <- l0[,1] ; y0<-l0[,2] ;rad0 <- atan2(y0,x0);r0 <- sqrt(x0^2+y0^2)
#通常外側だが内側参照もある　どちらでも対応できるコードが望ましいが・・・
  #　277は参照年輪にも大きな欠損がある
  #不完全年輪は区間最大が端末か途中
  ldst2<-dst(cbind(c(x,x[1]),c(y,y[1]))) # distance petween points plot(ldst2)
  dst.med <- median(ldst2)
  dst.max <- max(ldst2)
  i1. <- which(ldst2==dst.max)
#区間最大が端末にあるか、途中にあるか?
  if(i1.==length(x)){i1<-1;i2<-length(x)}else{i1<-i1. ; i2<-i1.+1}
  j<-1:nrow(l0)
  j1<-which.min((l0[,1]-l[i1,1])^2+(l0[,2]-l[i1,2])^2)
  j2<-which.min((l0[,1]-l[i2,1])^2+(l0[,2]-l[i2,2])^2)

  i12<-if(i1.==length(x)){against_numbers(rotate_numbers(j,j1,j2),j)}else{
    rotate_numbers(j,j1,j2)}

  lnew<-Lsort(rbind(l,l0[i12,]))

  if(plot.yn){
    plot(l0,col="blue",type="l",lty=2)
    points(l, col="red",type="l",lwd=2)
    lines(l0[i12,],col="blue")
    points(l0[j1,1],l0[j1,2],cex=3,col="blue")
    points(l0[j2,1],l0[j2,2],cex=3,col="blue")
    points(l0[i12,], col="blue", lwd=2)#points(l, col="red")

    plot(Lsort(lnew),type="l",main="check")
  }

  return(lnew)
}


#   l <-L[[rn ]] ;  x  <- l[,1]  ; y <- l[,2] ;rad  <- atan2( y, x);r  <- sqrt(x^2+y^2)
#   l0<-L[[rn0]] ;  x0 <- l0[,1] ; y0<-l0[,2] ;rad0 <- atan2(y0,x0);r0 <- sqrt(x0^2+y0^2)
#
#   ldst2<-dst(cbind(c(x,x[1]),c(y,y[1]))) # distance petween points plot(ldst2)
#   dst.med <- median(ldst2)
#   dst.max <- max(ldst2)
#   i1. <- which(ldst2==dst.max)
#   if(i1.==length(x)){i1<-1;i2<-length(x)}else{i1<-i1. ; i2<-i1.+1}
#   j1<-which.min((l0[,1]-l[i1,1])^2+(l0[,2]-l[i1,2])^2)
#   j2<-which.min((l0[,1]-l[i2,1])^2+(l0[,2]-l[i2,2])^2)
#   points(l0[j1,],sex=5,col="blue")
#   if(i1.==length(x)){j1_<-j1;j1<-j2;j1<-j1_}
#   if(j1<j2){i12<-j1:j2}else{i12<-c(j1:nrow(l0),1:j2)} #   rad0[c(j1,j2),] i12
#   if(plot.yn)plot(l0,col="blue",type="n")
#   if(plot.yn)lines(l0[i12,],col="blue")
#   if(plot.yn)points(l, col="red",type="l")
#   if(plot.yn)points(l0[i12,], col="blue")#points(l, col="red")
#   lnew<-Lsort(rbind(l,l0[i12,]))
#   if(plot.yn)plot(Lsort(lnew),type="l",main="check")
#       return(lnew)
# }



#' MissingRingCompensation compensate lack part using with spline curve estimation of
#' relationships between center angles and radius
#'
#' @param l. a matrix or data frame of x y coordinates with lack part of a tree ring
#'
#' @return a matrix  of x y coordinates that compensate lack part
#' @export
#'
#' @examples
#' par(mfrow=c(1,2))
#' plot(l.,type="b")
#' l.comp <- MissingRingCompensation(l.)
#' plot(l.comp,type="b")
#' L.comp<-L
#' L.comp[[1]]<-l.
#' Lplot(L.comp)
#' L.comp<-Lmove(L.comp,P00)
#' WriteShapefile_AnnualRings(L.comp,"L.comp")
#'
MissingRingCompensation<-function(l.,plot.yn="TRUE"){
  l.<-Lsort(l.)
  # relationships between tree ring radius and center angles #
  x<-l.[,1] ;   y<-l.[,2]
  r<-sqrt(x^2+y^2)
  rad<-atan2(y,x) # radian #
  (spline<-smooth.spline(rad,r, spar =0.0002))
  xxx<-seq(-pi,pi,0.01)
  if(plot.yn)plot(rad,r) ; lines(predict(spline,xxx),col="red")
  deg<-rad/degree  # degree #
  (spline<-smooth.spline(deg,r, spar =0.0002))
  xxx<-seq(-pi/degree,pi/degree,0.01)
  if(plot.yn)plot(deg,r) ;lines(predict(spline,xxx),col="red")
  # check points with large distance between adjacent points #
  dxy<-dstpp(l.[,1],l.[,2])
  med<-median(dxy)
  rn. <- which(dxy>5*med)-1
  med.deg<-median(diff(deg))
  if(plot.yn)plot(l.,type="b") ;  points(l.[rn.,],col="red")
  # points estimated #
  deg.<-c()
  for (i in 1:length(rn.)){
    j<-rn.[i]
    deg.<-c(deg.,seq.deg(deg[j],deg[j+1],med.deg))
  }
  # 欠測補完　#
  r.<-predict(spline,deg.)$y
  if(plot.yn)plot(deg,r) ; points(deg.,r.,col="red")
  x.<-r.*cos(deg.*degree)
  y.<-r.*sin(deg.*degree)
  if(plot.yn)plot(l.,type="p") ;  points(x.,y.,col="red")

  # bind estimated points and sort with center angle
  l..<-rbind(l.,cbind(x.,y.))
  l..<-Lsort(l..)
  return(l..)
}

# Tree rings interpolation ####

#' TreeRingsPoints read TreeRingsPoints shape file, check and save parameters
#'
#' @param TR is tree ring  class (class_TreeRingsInterpolate )
#'
#' #@return list of  (P,P00,YR_P,n_id,YR_P,n_YR)
#' @export
#'
#' @examples
#' P_filename<-"points277_h600"		#####　<<<　各人シェープファイル名入力　(拡張子はいらない)
#' TreeRingsPoints(TR)
#'
#' load(paste0(P_filename,".RData"))
TreeRingsPoints <-function(TR){
  d <- ReadShapefile_AnnualRingPoints(TR@P_filename,id.tag=TR@P_id.tag,ring.tag=TR@P_ring.tag)
  id <- d$id
  yr <- d$yr
  # 髄の中心，髄がない場合は放射計測線の中心点を【0,0】にする #
  i<-which(d$id==0); (P00<-c(d$x[i],d$y[i]))		###　原点 ■P00
  d$x<-d$x-P00[1];d$y<-d$y-P00[2]		###  原点 -> 0,0

  i<-order(d$id,d$yr)		# データの並べ替え #
  (d<-d[i,])			#　Qgisではデータ順をソートして保存できないため #
  r<-sqrt((d$x^2+d$y^2))	#### 髄からの距離
  l<-sqrt(diff(d$x)^2+diff(d$y)^2)	# 点間距離　年輪が歪んでいるときは点間距離が適当
  rad<-atan2(d$y,d$x)
  deg<-rad/degree
  P<-data.frame(d,rad,deg,r)	####　放射計測点データフレーム　■P
  #	id=0の中心点除去
  P<-subset(P,id!=0)
  P<-na.omit(P)
  head(P)
  YR_P<-unique(P$yr)   ##### ■　P$yr
  n_YR<-length(YR_P)		　　　　##### ■推定部分も含めた総年輪総数　(外側の形成層も含む)　
  n_id<-length(unique(P$id))		##### ■放射計測線本数　id0 髄データ(髄がない場合中心データは除く)

  tail(d)						#  確認 #
  str(d)
  table(yr)
  plot(d$yr)
  plot(d$id)
  plot(sort(d$yr))
  plot(r)
  plot(deg,r,xlab="放射線角度",ylab="髄からの距離")
  plot(d$id[-1],l,xlab="id",ylab="点間距離")

  TR@P <- P ; TR@P00 <- P00 ; TR@n_id<-n_id ; TR@YR_P <-YR_P ;TR@n_YR<-n_YR

  return(TR)

  # save(P,P00,YR_P,n_id,YR_P,n_YR,file=paste0(P_filename,".RData"))
  # return(list(P=P,P00=P00,YR_P=YR_P,n_id=n_id,YR_P=YR_P,n_YR=n_YR))

}


#' TreeRingsLines
#'
#' @param  TR is tree ring  class (class_TreeRingsInterpolate )
#'
#' @return
#' @export
#'
#' @examples
#'  TreeRingsLines(TR)
TreeRingsLines　<-function(TR){
  L<-TR@L ; P00 <-TR@P00 ;P <- TR@P ;L_ring_colname<-TR@L_ring.tag
   #　■L　年輪ポリゴンデータの追加と完成　■YR_L■ln #
  d<-read.shapefile(TR@L_filename)  	### ライン・データの読み込み
  ring<-as.numeric(as.vector(d$dbf$dbf[,L_ring_colname]))


  # 入力順に格納されているのを小さい順に直す　jjjはその順番 #
  jjj <- order(ring)
  (ln<-length(d$shp$shp))
  L<-c()
  for (i in 1:ln)L<-c(L,list(as.matrix(d$shp$shp[[jjj[i]]]$points))) # Lの追加

  (YR_L<-c(as.numeric(as.vector(d$dbf$dbf[,L_ring_colname])))[jjj])

  names(L)<-YR_L #　年輪数の名前を追加 #

  # 座標修正　髄の中心P00と【0,0】の差分  #
  for (i in 1:ln)L[[i]]<-t(t(L[[i]])-P00)

  # 年輪・計測線図化 #
  plot(L[[1]],type="l",col="red"); lapply(L,lines)
  points(P$x,P$y,pch=".",col="red");abline(h=0,v=0,col="blue")

  # Llist2dataframe #
  L_ <- Llist2dataframe(L)

  #　0～2πで髄からの距離はどのような関係にあるか? #
  #  年輪ポリゴン　【-180～180度×髄からの距離】#
  x<-L[[1]][,1];y<-L[[1]][,2]
  plot(atan2(y,x)/degree,sqrt(x^2+y^2),type="n", ylim=c(0,max(P$r,na.rm=TRUE)),
       xlab="-180～180度",ylab="髄からの距離 (m)")
  for (ii in 1:length(L)){
    x<-L[[ii]][,1];y<-L[[ii]][,2]
    points(atan2(y,x)/degree,sqrt(x^2+y^2),pch=".")
  }


  # !!! 放射計測線 new #
  points(P$deg,P$r,col="red",pch=".")

  #save(L,L_,file=paste0(L_filename,".RData"))
  TR@L<-L ; TR@L_<-L_ ; TR@YR_L <-YR_L ; TR@ln <- ln
  return(TR)

}


#' TreeRingsInterpolation_calc　interpolation tree ring between manual input tree rings with tree ring points
#'
#' @param TR
#'
#' @return TR
#' @export
#'
#' @examples
#' TreeRingsInterpolation_calc(TR)
#'
TreeRingsInterpolation_calc <-function(TR){

  TR@P -> P ; TR@P00 -> P00 ; TR@n_id -> n_id ; TR@YR_P -> YR_P ; TR@n_YR -> n_YR
  TR@L->L ; TR@L_->L_ ; TR@YR_L ->YR_L ; TR@ln -> ln

  ##YRn<-10:11 #### 補間用年輪ポリゴンの番号  <<<<< 数字を変えてスプライン曲線確認 2本以上
  YRn<-1:length(YR_L)##length(YR_L) #### 全データ実行

  (i12<-match(YR_L[YRn],YR_P))	# 放射計測点との合致と参照番号を確認(年+1，一番外側の年輪は年が　0　から始まるため)

  (YR2<-YR_L[YRn[1]]:YR_L[rev(YRn)[1]] ) #新たに構成する年輪ベクトル


  #　　>>> 年輪補間計算ループ開始 　

  L2<-c()	#　内挿用年輪ポリゴンデータL2初期化

  for(iii in 2:length(YRn)){	##個別実行　iii<-3　##全実行　2:length(YRn)
    yr12 <- YR_L[iii-1]:YR_L[iii]　###　検討区間年
    i12_ <-i12[iii-1]:i12[iii] ### 参照番号　年+1なので注意!!
    if(length(i12_ )==2) {   # !!!!!  間に推定すべき年輪がない場合スルー #
      L2<-c(L2,list(L[[iii-1]]))
      next}

    yr_pn <- c()
    for(i in 1:length(i12_)){
      yr_pn <- c(yr_pn,list(which(P$yr==yr12[i])))
    }
    names(yr_pn) <- yr12

    #### 年輪図と最近傍点
    L_out<-L[[YRn[iii-1]]];L_in<-L[[YRn[iii]]]
    np<-nstP(L_out,L_in)
    plot(L[[1]],type="l",col="red"); lapply(L,lines)
    segments(L_out[,1],L_out[,2],L_in[np,1],L_in[np,2],col="blue")


    #####　図化
    rdst.<-c()
    for( i in 2:(length(yr12)-1)){
      rdst.<-c(rdst.,list(rdst_MerginePlus(L,P,yr12[i])))
    }


    plot(0,type="n",ylim=c(0,1),xlim=c(-180,180),
         main=paste(yr12[1],rev(yr12)[1],sep = "-"),
         xlab="-180～180度",ylab="年輪間の距離の比率")
    for (i in 1:length(rdst.))points(rdst.[[i]],col=i)

    #### スプライン曲線推定　<<<線が乱れていたら入力ミスを確認!!!!　

    spl<-c()
    for (ii in 1:length(rdst.)){	##ii<-2
      #j <- yr_pn[[ii]]
      #px<-rdst.[[ii]];py<-P$y[j]
      #j<-!(is.na(px) | is.na(py))					#####　NA　除去
      (spline<-smooth.spline(rdst.[[ii]], spar =0.0002))
      xxx<-seq(-200,200,1)
      lines(predict(spline,xxx),col="red")
      spl<-c(spl,list(spline))  ##### 曲線関数格納
    }
    names(spl)<-yr12[2:(length(yr12)-1)]


    ######　年輪補間
    L2<-c(L2,list(L_out))
    for (jj in 1:length(spl)){	#jj<-36 <<<<1:length(spl)
      spline<-spl[[jj]]
      #plot(predict(spline,-180:180))
      dL<-L_out-L_in[np,]			###差分
      L.ave<-(L_out+L_in[np,])/2			###平均座標
      deg_spl<-atan2(L.ave[,2],L.ave[,1])/degree	####　髄からの角度
      ratio_<-predict(spline,deg_spl)$y
      L_cal<-L_in[np,]+dL*ratio_
      lines(L_cal,col="red")
      L2<-c(L2,list(L_cal))
    }

  }
  L2<-c(L2,list(L[[length(YRn)]]))	#	最内側の年輪を最後に追加 length(L2) length(YR2)
  #<<<　　年輪補間計算ループ終了

  names(L2)<-YR2		##	各年輪ポリゴンに名前をつける

  plot(L[[1]],type="l",col="red"); lapply(L2,lines,col="blue")
  points(P$x,P$y,pch=".",col="red")

  TR@L2 <- L2

  return(TR)





}




#' TreeRingInterpolation
#'
#' @param P_filename
#' @param L_filename
#' @param L2_filename
#' @param P_id.tag
#' @param P_ring.tag
#' @param L_ring.tag
#'
#' @return
#' @export
#'
#' @examples
#' TR<-TreeRingsInterpolation(
#' P_filename="points277_h600",
#' L_filename="277_h600不完全年輪追加",
#' L2_filename="Abies277_h600_interpolated",
#' P_id.tag="id",P_ring.tag="ring",
#' L_id.tag="id",L_ring.tag="ring")
#'
#' str(L)
#' Lplot2(TR@L,1:length(TR@L))
#'
#' #save(TR,file=paste0(TR@L2_filename,".RData"))
TreeRingsInterpolation <- function(P_filename,L_filename,L2_filename,
                                  P_id.tag="id",P_ring.tag="ring",L_id.tag="id",L_ring.tag="ring"){
  #  P_filename="points277_h600"; L_filename="line277_h600_2";L2_filename="Abies277_h600_interpolated"
  #  P_id.tag="id";P_ring.tag="ring";  L_id.tag="";L_ring.tag="id"

  # tree ring points
  TR<- new("class_TreeRingsInterpolate ") # treering_cls #str(TR)
  TR@P_filename<-P_filename #"points277_h600"
  TR@P_id.tag<-"id"
  TR@P_ring.tag<-"ring"
  # tree ring lines
  TR@L_filename<-L_filename # "line277_h600_2"
  TR@L_id.tag<-L_id.tag
  TR@L_ring.tag<-L_ring.tag
  # tree ring interpolate
  TR@L2_filename<-L2_filename #

  TR <- TreeRingsPoints(TR)
  #return(TR@L2_filename)
  TR <- TreeRingsLines(TR)

  # tree ling interpolation
  TR <- TreeRingsInterpolation_calc(TR)

  return(TR)

}


# if(0) ####
if(0){

z.<-  new("class_IncompleteTreeRingsFix")
z.(L2_filename="Abies277_h600_interpolated2",ln=5)
iTR
TR
str(TR)
slotNames(TR)
iTR
str(iTR)
a <- function(){
  z<-iTR(L2_filename="Abies277_h600_interpolated2",ln=5)
  return(z)
}
a()
z
iTR(L2_filename="test")
zz<-iTR
zz(L2_filename="test")
zz(L2_filename)
str(zz)
zz@className
zz@.Data()
}

