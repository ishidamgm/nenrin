# plot.R ####

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
