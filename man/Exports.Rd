\name{Exports}
\alias{Exports}
\docType{data}
\title{
  Example data set to demonstrate the use of Gvis 
}
\description{
  Example data set of information to demonstrate the use of Gvis
}
\usage{data(Exports)}
\format{
  A data frame with 10 observations on the following 3 variables.
  \describe{
    \item{\code{Country}}{a factor with levels \code{Brazil}, \code{Germany} ...}
    \item{\code{Profit}}{a numeric vector}
    \item{\code{Online}}{a logical vector}
  }
}
\examples{
data(Exports)
\dontrun{
 GeoMapPage(Exports,file="myExportAnalysis.html")
}
}
\keyword{datasets}
