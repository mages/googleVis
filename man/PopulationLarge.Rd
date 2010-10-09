\name{PopulationLarge}
\alias{PopulationLarge}
\docType{data}
\title{
  Example data set to demonstrate the use of Gvis 
}
\description{
  Example data set of information to demonstrate the use of Gvis
}
\usage{data(PopulationLarge)}
\format{
  A data frame with 195 observations on the following 7 variables.
  \describe{
    \item{\code{rank}}{a numeric vector with population ranking}
    \item{\code{country}}{country name as character}
    \item{\code{population}}{population}
    \item{\code{procential}}{procential par of world population}
    \item{\code{flag}}{html img with country flag}
    \item{\code{mode}}{logical test vector}
    \item{\code{date}}{date test vector}
  }
}
\examples{
data(PopulationLarge)
\dontrun{
 gvisTable(PopulationLarge)
}
}
\keyword{datasets}
