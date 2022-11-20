
# TreeRingGrowth.R ####


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

  Year_TreeRingArea <- data.frame(year=yr_a[-1],TreeRingArea=diff(a))
  plot(Year_TreeRingArea,type="b")
  abline(v = seq(floor(min(yr_a)/10)*10, max(yr_a),10 ), col = "gray")

  return(Year_TreeRingArea)
}

#' return a numeric list of tree ring width from radial measurement points
#'
#' @param P data frame of tree ring points
#' @param idn length of id of radial measurement points
#'
#' @return list of tree ring width
#' @export
#'
#' @examples
#' trw<-TreeRingWidthMatrix_ID(P)
#' plot(trw[[1]],type="l")
#' for (i in 2:8)lines(trw[[i]],col=i)

TreeRingWidthMatrix_ID<-function(P,idn=8){
  trw<-c() # tree_ring_width
  for (i in 1:idn){
    i<-P$id==i
    trw<-c(trw,list(dst(P[i,c(1,2)])))
  }
  return(trw)
}







#' estimate trank area from radius as true circle
#'
#' @param r radius (length between a pith and cambium layer)
#'
#' @return numeric of  trunk area
#' @export
#'
#' @examples
#'
r2a<-function(r){
  pi*r^2
}

#' estimating growth of trunk area from tree ring width (trw)
#' @param trw list of tree ring width (cambium to pith)
#' @param id numeric, id of radial annual ring points
#'
#' @return numeric vector of trunk cross-sectional area
#' @export
#'
#' @examples
Ga_w<-function(trw, id){
  return(r2a(cumsum(rev(trw[[id]]))))
}

#' return tree ring index from sequence of year and tree ring area
#' using with spline curve
#'
#' @param ya
#'
#' @return list of spline curve function("spline") and tree ring index("TreeRingIndex")
#' @export
#'
#' @examples
#' par(mfrow=c(2,1))
#' plot(ya,type="b",ylab="Tree ring area",xlab="year")
#' data("Abies277_h600","")
#' sm <- TreeRingIndex(ya)$spline
#' idx<- TreeRingIndex(ya)$idx
#' lines(sm,col="red",lw=3)
#' plot(idx,type="b",ylab="Tree ring index",xlab="year")
#' abline(h=1,col="red")
#' abline(v=seq(1800,2020,10),col="gray")
#'
#'

TreeRingIndex <- function(ya){
  sm<-smooth.spline(ya,spar=0.8)
  idx<-ya[,2]/predict(sm,ya[,1])$y
  return(list(spline=sm,idx=data.frame(year=ya[,1],TreeRingIndex=idx)))
}

