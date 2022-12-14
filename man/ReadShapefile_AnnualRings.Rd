% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ShapeFile.R
\name{ReadShapefile_AnnualRings}
\alias{ReadShapefile_AnnualRings}
\title{ReadShapefile_AnnualRings}
\usage{
ReadShapefile_AnnualRings(filename = "line277_h600", ring.tag = "id")
}
\arguments{
\item{filename}{is a file name of shape file written to disk.
The extension (.shp) is unnecessary.}
}
\value{
a data frame of AnnualRingPoints (radial input and correction points)
}
\description{
ReadShapefile_AnnualRings
}
\examples{
#年輪計測点の読み込み ####
Ltest <- ReadShapefile_AnnualRings("line277_h600",ring.tag="id")
ReadShapefile_AnnualRings("L.comp",ring.tag="ring")
Lplot(Ltest)
}
