\name{Stock}
\alias{Stock}
\docType{data}
\title{
  Stock: googleVis example data set
}
\description{
  Example data set to illustrate the use of the googleVis package.
}
\usage{data(Stock)}
\format{
  A data frame with 12 observations on the following 5 variables.
  \describe{
    \item{\code{Date}}{a Date}
    \item{\code{Device}}{a character vector}
    \item{\code{Value}}{a numeric vector}
    \item{\code{Title}}{a factor with levels \code{Bought pencils} \code{Out of stock}}
    \item{\code{Annotation}}{a factor with levels \code{Bought 200k pencils} \code{Ran of stock on pens at 4pm}}
  }
}
%%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%%}
\source{
  Google Annotated Time Line  API:
  \url{http://code.google.com/apis/visualization/documentation/gallery/annotatedtimeline.html}
}
%%\references{
%%  ~~ possibly secondary sources and usages ~~
%%}
\examples{
## Create data as used by Google in their annotated time line example

 Date <- as.Date(paste("2008-1-", 1:6, sep=""))
 Pencils <- c(3000, 14045, 5502, 75284, 41476, 333222)
 Pencils.titles <-c(rep(NA,4), 'Bought pencils', NA)
 Pencils.annotation <-c(rep(NA,4), 'Bought 200k pencils', NA)
 Pens <- c(40645, 20374, 50766, 14334, 66467, 39463)
 Pens.titles <- c(rep(NA, 3), 'Out of stock', NA, NA)
 Pens.annotation <- c(rep(NA, 3), 'Ran of stock on pens at 4pm', NA, NA)

 original.df=data.frame(Date, Pencils, Pencils.titles,
                        Pencils.annotation, Pens, Pens.titles,
                        Pens.annotation)  


 Stock <- reshape(original.df, idvar="Date", times=c("Pencils", "Pens"), 
                  timevar="Device",
                  varying=list(c("Pencils", "Pens"),
                               c("Pencils.titles", "Pens.titles"),
                               c("Pencils.annotation", "Pens.annotation")), 
                   v.names=c("Value", "Title", "Annotation"),
                   direction="long")

}
\keyword{datasets}
