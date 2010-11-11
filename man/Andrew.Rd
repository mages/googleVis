\name{Andrew}
\alias{Andrew}
\docType{data}
\title{
Hurricane Andrew: googleVis example data set
}
\description{
Hurricane Andrew storm path
}
\usage{data(Andrew)}
\format{
  A data frame with 47 observations on the following 6 variables.
  \describe{
    \item{\code{Date_UTC}}{a factor with levels \code{1992-08-16 18:00} \code{1992-08-17 00:00} \code{1992-08-17 06:00} \code{1992-08-17 12:00} \code{1992-08-17 18:00} \code{1992-08-18 00:00} \code{1992-08-18 06:00} \code{1992-08-18 12:00} \code{1992-08-18 18:00} \code{1992-08-19 00:00} \code{1992-08-19 06:00} \code{1992-08-19 12:00} \code{1992-08-19 18:00} \code{1992-08-20 00:00} \code{1992-08-20 06:00} \code{1992-08-20 12:00} \code{1992-08-20 18:00} \code{1992-08-21 00:00} \code{1992-08-21 06:00} \code{1992-08-21 12:00} \code{1992-08-21 18:00} \code{1992-08-22 00:00} \code{1992-08-22 06:00} \code{1992-08-22 12:00} \code{1992-08-22 18:00} \code{1992-08-23 00:00} \code{1992-08-23 06:00} \code{1992-08-23 12:00} \code{1992-08-23 18:00} \code{1992-08-24 00:00} \code{1992-08-24 06:00} \code{1992-08-24 12:00} \code{1992-08-24 18:00} \code{1992-08-25 00:00} \code{1992-08-25 06:00} \code{1992-08-25 12:00} \code{1992-08-25 18:00} \code{1992-08-26 00:00} \code{1992-08-26 06:00} \code{1992-08-26 12:00} \code{1992-08-26 18:00} \code{1992-08-27 00:00} \code{1992-08-27 06:00} \code{1992-08-27 12:00} \code{1992-08-27 18:00} \code{1992-08-28 00:00} \code{1992-08-28 06:00}}
    \item{\code{Lat}}{a numeric vector}
    \item{\code{Long}}{a numeric vector}
    \item{\code{Presure_mb}}{a numeric vector}
    \item{\code{Speed_kt}}{a numeric vector}
    \item{\code{Category}}{a factor with levels \code{Hurricane} \code{Tropical Depression} \code{Tropical Storm}}
    \item{\code{Date_UCT}}{a POSIXct}
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

Andrew$LatLong=with(Andrew, paste(Lat, Long, sep=":"))
Andrew$Tip=with(Andrew,paste(Category, "<BR>Presure=",Presure_mb, "<BR>Speed=", Speed_kt, sep=""))
Map <- gvisMap(Andrew, "LatLong" , "Tip", 
      	      options=list(showTip=TRUE, showLine=TRUE, enableScrollWheel=TRUE,
		      mapType='terrain', useMapTypeControl=TRUE))


\dontrun{
plot(Map	) 
}

}
\keyword{datasets}
