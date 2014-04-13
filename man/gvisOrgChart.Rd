\name{gvisOrgChart}
\alias{gvisOrgChart}
\title{Google Org Chart with R
\Sexpr{googleChartName <- "orgchart"}
\Sexpr{gvisChartName <- "gvisOrgChart"}}
\usage{
gvisOrgChart(data, idvar = "", parentvar = "", tipvar = "",
  options = list(), chartid)
}
\arguments{
  \item{data}{a \code{data.frame}. The data has to have at
  least three columns. Each row in the data table describes
  one node (a rectangle in the graph). Each node (except
  the root node) has one or more parent nodes. Each node is
  sized and colored according to its values relative to the
  other nodes currently shown.}

  \item{idvar}{column name of \code{data} describing the ID
  for each node. It should be unique among all nodes, and
  can include any characters, including spaces. This is
  shown on the node. You can specify a formatted value to
  show on the chart instead, but the unformatted value is
  still used as the ID.}

  \item{parentvar}{column name of \code{data} that match to
  entries in \code{idvar}. If this is a root node, leave
  this \code{NA}. Only one root is allowed.}

  \item{tipvar}{column name of \code{data} for the tip
  variable. Tool-tip text to show, when a user hovers over
  this node.}

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
An organizational chart that supports selection.
}
\details{
The gvisOrgChart function reads a data.frame and creates
text output referring to the Google Visualisation API,
which can be included into a web page, or as a stand-alone
page. The actual chart is rendered by the web browser.
}
\examples{
## Please note that by default the googleVis plot command
## will open a browser window and requires Internet
## connection to display the visualisation.

Regions
Org1 <- gvisOrgChart(Regions, idvar = "Region", parentvar = "Parent",
     			      tipvar="Val")
plot(Org1)

## Set a few options
Org2 <- gvisOrgChart(Regions, idvar = "Region", parentvar = "Parent",
     			      tipvar="Val",
     		     options=list(width=600, height=400,
                     	          size='large', allowCollapse=TRUE))
plot(Org2)
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
for printing and plotting methods.
}
\keyword{iplot}

