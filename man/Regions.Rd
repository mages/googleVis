\name{Regions}
\alias{Regions}
\docType{data}
\title{
  Example data set to demonstrate the use of Gvis 
}
\description{
  Example data set of information to demonstrate the use of Gvis
}
\usage{data(Regions)}
\format{
  A data frame with 11 observations on the following 4 variables.
  \describe{
    \item{\code{Region}}{a factor with levels \code{America}, \code{Asia} ...}
    \item{\code{parent}}{parent region identifier}
    \item{\code{Val}}{a numeric vector}
    \item{\code{Fac}}{a numeric vector}
  }
}
\examples{
data(Regions)
\dontrun{
 gvisTreeMap(Regions)
}
}
\keyword{datasets}
