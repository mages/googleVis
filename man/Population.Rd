\name{Population}
\alias{Population}
\docType{data}
\title{
  Example data set to demonstrate the use of Gvis 
}
\description{
  Example data set of information to demonstrate the use of Gvis
}
\usage{data(Population)}
\format{
  A data frame with 195 observations on the following 2 variables.
  \describe{
    \item{\code{country}}{country name as character}
    \item{\code{population}}{population}
  }
}
\examples{
data(Population)
\dontrun{
 gvisGeoMap(Population)
}
}
\keyword{datasets}
