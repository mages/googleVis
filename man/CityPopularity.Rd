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
 Google Geo Map  API: \url{http://code.google.com/apis/visualization/documentation/gallery/geomap.html}
}
%%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(CityPopularity)

G <-  gvisGeoMap(CityPopularity, locationvar='City' ,numvar='Popularity', 
      			options=list(region='US',
				     dataMode='markers', 
				     colors='[0xFF8747, 0xFFB581, 0xc06000]'))
\dontrun{
plot(G)
}
}
\keyword{datasets}
