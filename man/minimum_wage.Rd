% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/minimum_wage.R
\docType{data}
\name{minimum_wage}
\alias{minimum_wage}
\title{Minimum Wage in Portugal}
\format{
A data frame with 48 rows and 2 variables:
\describe{
\item{\code{year}}{Year of reference.}
\item{\code{nominal_mw}}{Minimum Wage in nominal euros.}
}
}
\source{
{
Pordata
Code used to alter it:
1st load data
the the code to alter it from pordata:
minimum_wage \%<>\%
dplyr::rename(nominal_mw = "sm",
year ="anos")
}
}
\usage{
minimum_wage
}
\description{
https://www.pordata.pt/Portugal/Sal\%c3\%a1rio+m\%c3\%adnimo+nacional-74-7895
}
\examples{
data(minimum_wage) # lazy loading. The RAM will not be immediately occupied.
}
\keyword{datasets}
