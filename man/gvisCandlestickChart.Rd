\name{gvisCandlestickChart}
\alias{gvisCandlestickChart}
\title{Google Candlestick chart with R
\Sexpr{googleChartName <- "candlestickchart"}
\Sexpr{gvisChartName <- "gvisCandlestickChart"}}
\usage{
gvisCandlestickChart(data, xvar = "", low = "", open = "", close = "",
  high = "", options = list(), chartid)
}
\arguments{
  \item{data}{a \code{\link{data.frame}} to be displayed as
  a candlestick chart. The data has to have at least 5
  columns.}

  \item{xvar}{name of the character column which contains
  the category labels for the x-axes.}

  \item{low}{name of the numeric column specifying the
  low/minimum value of this marker. This is the base of the
  candle's center line.}

  \item{open}{name of the numeric column specifying the
  opening/initial value of this marker. This is one
  vertical border of the candle. If less than the
  \code{close} value, the candle will be filled; otherwise
  it will be hollow.}

  \item{close}{name of the numeric column specifying the
  closing/final value of this marker. This is the second
  vertical border of the candle. If less than the
  \code{open} value, the candle will be hollow; otherwise
  it will be filled.}

  \item{high}{name of the numeric column specifying the
  high/maximum value of this marker. This is the top of the
  candle's center line.}

  \item{options}{list of configuration options, see:

  % START DYNAMIC CONTENT

  \Sexpr[results=rd]{gsub("CHARTNAME", googleChartName,
  readLines(file.path(".", "inst", "mansections",
  "GoogleChartToolsURLConfigOptions.txt")))}

  \Sexpr[results=rd]{paste(readLines(file.path(".", "inst",
  "mansections", "gvisOptions.txt")))}}

  \item{chartid}{character. If missing (default) a random
  chart id will be generated based on chart type and
  \code{\link{tempfile}}}
}
\value{
\Sexpr[results=rd]{paste(gvisChartName)} returns list of
\code{\link{class}}
\Sexpr[results=rd]{paste(readLines(file.path(".", "inst",
"mansections", "gvisOutputStructure.txt")))}
}
\description{
An interactive candlestick chart.
}
\details{
The gvisCandlestickChart function reads a data.frame and
creates text output referring to the Google Visualisation
API, which can be included into a web page, or as a
stand-alone page. The actual chart is rendered by the web
browser using SVG or VML.

A candlestick chart is used to show an opening and closing
value overlaid on top of a total variance. Candlestick
charts are often used to show stock value behavior. In this
chart, items where the opening value is less than the
closing value (a gain) are drawn as filled boxes, and items
where the opening value is more than the closing value (a
loss) are drawn as hollow boxes.
}
\examples{
## Please note that by default the googleVis plot command
## will open a browser window and requires an internet
## connection to display the visualisation.

## Example data set
OpenClose

C1 <- gvisCandlestickChart(OpenClose, xvar="Weekday", low="Low",
                                      open="Open", close="Close",
                                      high="High",
                                      options=list(legend='none'))

plot(C1)
}
\author{
Markus Gesmann \email{markus.gesmann@gmail.com},

Diego de Castillo \email{decastillo@gmail.com}
}
\references{
Google Chart Tools API:
\Sexpr[results=rd]{gsub("CHARTNAME", googleChartName,
readLines(file.path(".", "inst", "mansections",
"GoogleChartToolsURL.txt")))}

% END DYNAMIC CONTENT
}
\seealso{
See also \code{\link{print.gvis}}, \code{\link{plot.gvis}}
for printing and plotting methods
}
\keyword{iplot}

