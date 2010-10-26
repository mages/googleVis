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
  A data frame with 195 observations on the following 7 variables.
  \describe{
    \item{\code{Rank}}{a numeric vector with population ranking}
    \item{\code{Country}}{country name as character}
    \item{\code{Population}}{population}
    \item{\code{\% of World Population}}{\% of world population}
    \item{\code{Flag}}{html image-tag to wikipedia with country flag}
    \item{\code{Mode}}{logical test vector}
    \item{\code{Date}}{date test vector}
  }
}
\source{
Sourced from Wikipedia (columns 1 to 5):
\url{http://en.wikipedia.org/wiki/List_of_countries_by_population},
9 October 2010. 
}
\examples{
data(Population)
tbl <- gvisTable(Population)

\dontrun{
plot(tbl)
}
}
\keyword{datasets}
