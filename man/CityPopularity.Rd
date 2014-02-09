\name{CityPopularity}
\alias{CityPopularity}
\docType{data}
\title{
CityPopularity: googleVis example data set
}
\description{
Example data set to illustrate the use of the googleVis package.
}
\usage{data(CityPopularity)}
\format{
  A data frame with 6 observations on the following 2 variables.
  \describe{
    \item{\code{City}}{a factor with levels \code{Boston} \code{Chicago} \code{Houston} \code{Los Angeles} \code{Miami} \code{New York}}
    \item{\code{Popularity}}{a numeric vector}
  }
}
%%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%%}
\source{
 Google Geo Map  API: \url{https://google-developers.appspot.com/chart/interactive/docs/gallery/geomap.html}
}
%%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(CityPopularity)
str(CityPopularity)
}
\keyword{datasets}
