\name{Regions}
\alias{Regions}
\docType{data}
\title{
 Regions: googleVis example data set
}
\description{
 Example data set to illustrate the use of the googleVis package.
 }
\usage{data(Regions)}
\format{
  A data frame with 11 observations on the following 4 variables.
  \describe{
    \item{\code{Region}}{a factor with levels \code{America}, \code{Asia} ...}
    \item{\code{Parent}}{parent region identifier}
    \item{\code{Val}}{a numeric vector}
    \item{\code{Fac}}{a numeric vector}
  }
}
\examples{
data(Regions)
Tree <- gvisTreeMap(Regions,  "Region", "Parent", "Val", "Fac",
                    options=list(width=600, height=500,
                                 showScale=TRUE, fontSize=16))
\dontrun{
plot(Tree)
}
}
\keyword{datasets}
