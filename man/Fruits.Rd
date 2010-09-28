\name{Fruits}
\alias{Fruits}
\docType{data}
\title{
  Example data set to demonstrate the use of Motion Charts
}
\description{
  Example data set of fruit sells information to demonstrate the use of Motion Charts
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
\dontrun{
 MotionChartPage(Fruits, idvar="Fruit", timevar="Year",
                 file="myFruitAnalysis.html")
}
}
\keyword{datasets}
