\name{Andrew}
\alias{Andrew}
\docType{data}
\title{
  Hurricane Andrew: googleVis example data set
}
\description{
  Hurricane Andrew storm path from 16 August to 28 August 1992
}
\usage{data(Andrew)}
\format{
  A data frame with 47 observations on the following 8 variables.
  \describe{
    \item{\code{Date/Time UTC}}{a POSIXct}
    \item{\code{Lat}}{a numeric vector}
    \item{\code{Long}}{a numeric vector}
    \item{\code{Pressure_mb}}{a numeric vector}
    \item{\code{Speed_kt}}{a numeric vector}
    \item{\code{Category}}{a factor with levels \code{Hurricane} \code{Tropical Depression} \code{Tropical Storm}}
    \item{\code{LatLong}}{a character vector}
    \item{\code{Tip}}{a character vector}
  }
}
%%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%%}
\source{
  National Hurricane Center: \url{http://www.nhc.noaa.gov/1992andrew.html}
}
%%\references{
%%  ~~ possibly secondary sources and usages ~~
%%}
\examples{
data(Andrew)

AndrewGeoMap <- gvisGeoMap(Andrew, locationvar='LatLong', numvar='Speed_kt',
                          hovervar='Category',
                          options=list(width=800,height=400,
                          region='US', dataMode='Markers'))

AndrewMap <- gvisMap(Andrew, 'LatLong' , 'Tip',
                           options=list(showTip=TRUE, showLine=TRUE,
                           enableScrollWheel=TRUE,
                           mapType='hybrid', useMapTypeControl=TRUE,
			   width=800,height=400))

AndrewTable <- gvisTable(Andrew,options=list(width=800))

## Combine the outputs into one page:

AndrewVis <- gvisMerge(AndrewGeoMap, AndrewMap)

plot(AndrewVis)

}
\keyword{datasets}
