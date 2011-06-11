\name{gvisCandlestickChart}
\alias{gvisCandlestickChart}
\title{
  Google Candlestick Chart with R
}
\description{
  
  An interactive candlestick chart.

  A candlestick chart is used to show an opening and closing value
  overlaid on top of a total variance. Candlestick charts are often used
  to show stock value behavior. In this chart, items where the opening
  value is less than the closing value (a gain) are drawn as filled boxes,
  and items where the opening value is more than the closing value (a
  loss) are drawn as hollow boxes.
 
}
\usage{
gvisCandlestickChart(data, xvar = "", yvar = "", options = list(), chartid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{xvar}{
%%     ~~Describe \code{xvar} here~~
}
  \item{yvar}{
%%     ~~Describe \code{yvar} here~~
}
  \item{options}{
%%     ~~Describe \code{options} here~~
}
  \item{chartid}{
%%     ~~Describe \code{chartid} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, xvar = "", yvar = "", options = list(), chartid) 
{
    gvisCoreChart(data, xvar, yvar, options, chartid, chart.type = "CandlestickChart")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
