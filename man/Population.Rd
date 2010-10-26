\name{Population}
\alias{Population}
\docType{data}
\title{
Population: googleVis example data set
}
\description{
Example data set to illustrate the use of the googleVis package.
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
G <-  gvisGeoMap(Population, idvar="country")
\dontrun{
plot(G)
}
}
\keyword{datasets}
