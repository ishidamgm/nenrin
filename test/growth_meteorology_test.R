# growth_meteorology.R

# load("../data/Abies277_h600.RData")
# load("../data/Takayama_Temperature_1900-2018.RData")

plot(ya,type="b")

met

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




windows()



