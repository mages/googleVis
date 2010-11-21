\name{Lehman}
\alias{Lehman}
\docType{data}
\title{
  Lehman: googleVis example data set
}
\description{
 Lehman Brothers Holding Stock Price Information from 2000-01-03 to 2008-09-19.
}
\usage{data(Lehman)}
\format{ 
 A data frame with 2187 observations on the following 9 variables.
  \describe{
    \item{\code{Date}}{a Date}
    \item{\code{Open}}{a numeric vector}
    \item{\code{High}}{a numeric vector}
    \item{\code{Low}}{a numeric vector}
    \item{\code{Close}}{a numeric vector}
    \item{\code{Volume}}{a numeric vector}
    \item{\code{Adj.Close}}{a numeric vector}
    \item{\code{Title}}{a character vector}
    \item{\code{Annotation}}{a character vector}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
Data from Yahoo Finance: 
\code{Lehman <-
  read.csv('http://ichart.finance.yahoo.com/table.csv?s=LEHMQ.PK&a=00&b=01&c=2000&d=08&e=20&f=2008&g=d&ignore=.csv',
  colClasses=c("Date", rep("numeric",6)))
}

Context from Wikipedia: \url{http://en.wikipedia.org/wiki/Lehman_Brothers}
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(Lehman)

summary(Lehman)

}
\keyword{datasets}
