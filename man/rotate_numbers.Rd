% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculation.R
\name{rotate_numbers}
\alias{rotate_numbers}
\title{rotate_numbers returns rotate number sequence (for circle data)}
\usage{
rotate_numbers(j, j1, j2)
}
\arguments{
\item{j}{all number}

\item{j1}{start number}

\item{j2}{end number}
}
\value{
rotate number sequence
}
\description{
rotate_numbers returns rotate number sequence (for circle data)
}
\examples{
j<-1:100
rotate_numbers(j,5,20)
rotate_numbers(j,95,20)
against_numbers(rotate_numbers(j,95,20),j)

}
