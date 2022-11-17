# IncompleteTreeRingFix.R  ####


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
#' Lsort_all( Lmove(iTR@L,-P00))

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
