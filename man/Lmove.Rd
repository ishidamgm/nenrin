% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculation.R
\name{Lmove}
\alias{Lmove}
\title{Lmove move the tree rings coordinates based on P00 (x,y movement coordinates).}
\usage{
Lmove(L, P00)
}
\arguments{
\item{L}{is a list of tree rings(x,y coordinates).}

\item{P00}{x, y coordinates of a center point (usually a pith).}
}
\description{
Lmove move the tree rings coordinates based on P00 (x,y movement coordinates).
}
\examples{
Lplot(L)
sapply(Lmove(L,P00),lines,col="red")
}
