% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AeMFA-data.R
\docType{data}
\name{KRED}
\alias{KRED}
\title{Various determinants in regard to credit default}
\format{
A data frame with 25570 observations of 8 variables:
\itemize{
\item \code{ausfall} credit default (no, yes)
\item \code{betrag} credit amount in USD
\item \code{zins} interest rate in percent
\item \code{bonitaet} credit worthiness (A -- highest, G -- lowest)
\item \code{alter} age of the borrower in years
\item \code{wohnen} type of living (Eigentum, Grundschuld, Miete, Sonstige)
\item \code{arbeit} employment term in years
\item \code{einkommen} yearly income in 1000 USD
}
}
\source{
The data are originally provided by Lending Club and modified by DataCamp.
}
\usage{
KRED
}
\description{
A dataset containing various details of private credit data, including credit default.
}
\keyword{datasets}
