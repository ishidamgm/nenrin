
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

  windows()
  Year_TreeRingArea <- data.frame(year=yr_a[-1],TreeRingArea=diff(a))
  plot(Year_TreeRingArea,type="b")
  abline(v = seq(floor(min(yr_a)/10)*10, max(yr_a),10 ), col = "gray")

  return(Year_TreeRingArea)
}
