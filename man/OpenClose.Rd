\name{OpenClose}
\alias{OpenClose}
\docType{data}
\title{
 OpenClose: googleVis example data set
}
\description{
  Example data set to illustrate the use of the googleVis package.
}
\usage{data(OpenClose)}
\format{
  A data frame with 5 observations on the following 5 variables.
  \describe{
    \item{\code{Weekday}}{a factor with levels \code{Fri} \code{Mon} \code{Thurs} \code{Tues} \code{Wed}}
    \item{\code{Low}}{a numeric vector}
    \item{\code{Open}}{a numeric vector}
    \item{\code{Close}}{a numeric vector}
    \item{\code{High}}{a numeric vector}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%%}
\source{
  Google Visualisation: Candlestick Chart
  \url{http://code.google.com/apis/chart/interactive/docs/gallery/candlestickchart.html}
}
%%\references{
%%  ~~ possibly secondary sources and usages ~~
%%}
\examples{
OpenClose
plot(gvisCandlestickChart(OpenClose, options=list(legend='none')))
}
\keyword{datasets}
