\name{Fruits}
\alias{Fruits}
\docType{data}
\title{
Fruits: googleVis example data set
}
\description{
 Example data set to illustrate the use of the googleVis package.
}
\usage{data(Fruits)}
\format{
A data frame with 9 observations on the following 7 variables.
  \describe{
    \item{\code{Fruit}}{a factor with levels \code{Apples} \code{Bananas} \code{Oranges}}
    \item{\code{Year}}{a numeric vector}
    \item{\code{Location}}{a factor with levels \code{East} \code{West}}
    \item{\code{Sales}}{a numeric vector}
    \item{\code{Expenses}}{a numeric vector}
    \item{\code{Profit}}{a numeric vector}
    \item{\code{Date}}{a Date}
  }
}
\examples{
data(Fruits)
M <- gvisMotionChart(Fruits, idvar="Fruit", timevar="Year")

\dontrun{
 plot(M)
}
}
\keyword{datasets}
