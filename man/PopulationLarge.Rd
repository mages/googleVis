\name{PopulationLarge}
\alias{PopulationLarge}
\docType{data}
\title{
  Example data set
}
\description{
  Example data set of information to demonstrate the use of Gvis
}
\usage{data(PopulationLarge)}
\format{
  A data frame with 195 observations on the following 7 variables.
  \describe{
    \item{\code{Rank}}{a numeric vector with population ranking}
    \item{\code{Country}}{country name as character}
    \item{\code{Population}}{population}
    \item{\code{\% of World Population}}{\% of world population}
    \item{\code{Flag}}{html image-tag to wikipedia to with country flag}
    \item{\code{Mode}}{logical test vector}
    \item{\code{Date}}{date test vector}
  }
}
\source{
Sourced from wikipedia:
\url{http://en.wikipedia.org/wiki/List_of_countries_by_population}
October 9, 2010. 
}
\examples{
data(PopulationLarge)
tbl <- gvisTable(PopulationLarge)

\dontrun{
plot(tbl)
}
}
\keyword{datasets}
