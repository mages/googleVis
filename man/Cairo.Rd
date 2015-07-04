  \name{Cairo}
\alias{Cairo}
\docType{data}
\title{
Daily temperature data for Cairo
}
\description{
The average air temperature (C) in Cairo from 2002 to 2004.
}
\usage{data(Cairo)}
\format{
  A data frame with 1091 observations on the following 2 variables.
  \describe{
    \item{\code{Date}}{calendar date}
    \item{\code{Temp}}{average temperatur in Celsius}
  }
}
%\details{
%}
\references{
Wood, S.N. (2006) Generalized Additive Models: An Introduction with R
}
\examples{
data(Cairo)
plot(gvisCalendar(Cairo))
}
\keyword{datasets}
