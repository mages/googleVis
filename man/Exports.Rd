\name{Exports}
\alias{Exports}
\docType{data}
\title{
  Example data set
}
\description{
  Example data set of information to demonstrate the use of gvisGeoMap.
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
Exports
G <- gvisGeoMap(Exports[, 1:2], idvar="Country")
\dontrun{
plot(G)
}
}
\keyword{datasets}
