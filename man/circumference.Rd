% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculation.R
\name{circumference}
\alias{circumference}
\title{return a numeric of circumference length of polygon line}
\usage{
circumference(l.)
}
\arguments{
\item{l.}{}
}
\value{
a numeric of circumference length of polygon line
}
\description{
return a numeric of circumference length of polygon line
}
\examples{
l. <- data.frame(x=c(0,0,1,1),y=c(0,1,1,0))
plot(l.,type="b") ; polygon(l.)
circumference(l.)
}
