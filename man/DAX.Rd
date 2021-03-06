% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AeMFA-data.R
\docType{data}
\name{DAX}
\alias{DAX}
\title{Daily prices and returns of the DAX 30}
\format{
A xts time series with 2273 observations of 2 variables:
\itemize{
\item \code{Kurs} daily price in Euro
\item \code{Rendite} daily return
}
}
\source{
The data were retrieved from Yahoo finance via \code{getSymbols()}
from the \code{quantmod} package.
}
\usage{
DAX
}
\description{
A dataset containing the daily (adjusted closing) prices and returns of the German DAX 30 index
from 2010-01-01 to 2018-12-31.
}
\keyword{datasets}
