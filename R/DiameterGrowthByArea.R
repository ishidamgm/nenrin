# DiameterGrowthByArea.R

#' return data frame and plot of tree trunk diameter estimated by annual ring area
#'
#' @param ya data frame of Year and TreeRingArea
#'
#' @return data frame of Year and diameter_cm
#' @export
#'
#' @examples
#' DiameterGrowthByArea(ya)
#'
DiameterGrowthByArea <- function(ya){
  dpi<-1200
  pxl<-2.54/dpi
  A <- cumsum(ya[,2]) # crosscection area of trunk
  diameter_cm <- sqrt(A/pi)*2*pxl
  yd <- data.frame(ya[,1],diameter_cm )
  plot(yd[,2],type="b",xlab="Year",ylab="Diameter (cm)")
  return(yd)

}




