### Part of the R package googleVis
### Copyright 2010 - 2014 Markus Gesmann, Diego de Castillo

### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
### MA 02110-1301, USA



#' Google Pie Chart with R
#' \Sexpr{googleChartName <- "piechart"}
#' \Sexpr{gvisChartName <- "gvisPieChart"}
#' 
#' The gvisPieChart function reads a data.frame and creates text output
#' referring to the Google Visualisation API, which can be included into a web
#' page, or as a stand-alone page. The actual chart is rendered by the web
#' browser using SVG or VML.
#' 
#' 
#' @param data a \code{\link{data.frame}} to be displayed as a pie chart
#' @param labelvar Name of the character column which contains the category
#' labels for the slice labels.
#' @param numvar a vector of column names of the numerical variables of the
#' slice values.
#' @param options list of configuration options for Google Pie Charts, see:
#' 
#' % START DYNAMIC CONTENT
#' 
#' \Sexpr[results=rd]{gsub("CHARTNAME", 
#' googleChartName,
#' readLines(file.path(".", "inst",  "mansections", 
#' "GoogleChartToolsURLConfigOptions.txt")))}
#' 
#'  \Sexpr[results=rd]{paste(readLines(file.path(".", "inst", 
#'  "mansections", "gvisOptions.txt")))}
#'   
#' @param chartid character. If missing (default) a random chart id will be 
#' generated based on chart type and \code{\link{tempfile}}
#' 
#' @return \Sexpr[results=rd]{paste(gvisChartName)} returns list 
#' of \code{\link{class}}
#'  \Sexpr[results=rd]{paste(readLines(file.path(".", "inst", 
#'  "mansections", "gvisOutputStructure.txt")))}
#'   
#' @references Google Chart Tools API: 
#' \Sexpr[results=rd]{gsub("CHARTNAME", 
#' googleChartName, 
#' readLines(file.path(".", "inst",  "mansections", 
#' "GoogleChartToolsURL.txt")))}
#' 
#' % END DYNAMIC CONTENT
#' 
#' @author Markus Gesmann \email{markus.gesmann@@gmail.com},
#' 
#' Diego de Castillo \email{decastillo@@gmail.com}
#' @seealso See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for
#' printing and plotting methods
#' @keywords iplot
#' @examples
#' 
#' ## Please note that by default the googleVis plot command
#' ## will open a browser window and requires an internet
#' ## connection to display the visualisation.
#' 
#' Pie1 <- gvisPieChart(CityPopularity)
#' plot(Pie1)
#' 
#' ## Doughnut chart - a pie with a hole
#' Pie2 <- gvisPieChart(CityPopularity, options=list(
#'                     slices="{4: {offset: 0.2}, 0: {offset: 0.3}}",
#'                     title='City popularity',
#'                     legend='none',
#'                     pieSliceText='label',
#'                     pieHole=0.5))
#' plot(Pie2)
#' 
#' 
#' 
gvisPieChart <- function(data, labelvar="", numvar="", options=list(), chartid){

  my.type <- "PieChart"
  dataName <- deparse(substitute(data))

  my.options <- list(gvis=modifyList(list(allowHtml=TRUE),options), dataName=dataName,
                     data=list(locationvar=labelvar, numvar=numvar,
                       allowed=c("number", "string"))
                     )

 
  checked.data <- gvisCheckPieChartData(data, my.options)
  
  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options,
                      chartid=chartid, package="corechart") 
  
  return(output)
}

#' Google Gauge with R
#' \Sexpr{googleChartName <- "gauge"}
#' \Sexpr{gvisChartName <- "gvisGauge"}
#' 
#' The gvisGauge function reads a data.frame and creates text output referring
#' to the Google Visualisation API, which can be included into a web page, or
#' as a stand-alone page. The actual chart is rendered by the web browser using
#' SVG or VML.
#' 
#' @param data a \code{\link{data.frame}} to be displayed as a gauge
#' @param labelvar name of the character column which contains the category
#' labels for the slice labels.
#' @param numvar a vector of column names of the numerical variables of the
#' slice values.
#' @param options list of configuration options, see: 
#' 
#' % START DYNAMIC CONTENT
#' 
#' \Sexpr[results=rd]{gsub("CHARTNAME", 
#' googleChartName,
#' readLines(file.path(".", "inst",  "mansections", 
#' "GoogleChartToolsURLConfigOptions.txt")))}
#' 
#'  \Sexpr[results=rd]{paste(readLines(file.path(".", "inst", 
#'  "mansections", "gvisOptions.txt")))}
#'   
#' @param chartid character. If missing (default) a random chart id will be 
#' generated based on chart type and \code{\link{tempfile}}
#' 
#' @return \Sexpr[results=rd]{paste(gvisChartName)} returns list 
#' of \code{\link{class}}
#'  \Sexpr[results=rd]{paste(readLines(file.path(".", "inst", 
#'  "mansections", "gvisOutputStructure.txt")))}
#'   
#' @references Google Chart Tools API: 
#' \Sexpr[results=rd]{gsub("CHARTNAME", 
#' googleChartName, 
#' readLines(file.path(".", "inst",  "mansections", 
#' "GoogleChartToolsURL.txt")))}
#' 
#' % END DYNAMIC CONTENT
#' @author Markus Gesmann \email{markus.gesmann@@gmail.com},
#' 
#' Diego de Castillo \email{decastillo@@gmail.com}
#' @seealso See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for
#' printing and plotting methods
#' 
#' @keywords iplot
#' @examples
#' 
#' ## Please note that by default the googleVis plot command
#' ## will open a browser window and requires an internet
#' ## connection to display the visualisation.
#' 
#' Gauge1 <- gvisGauge(CityPopularity, options=list(min=0, max=800, greenFrom=500,
#'                     greenTo=800, yellowFrom=300, yellowTo=500,
#'                     redFrom=0, redTo=300))
#' 
#' plot(Gauge1)
#' 
gvisGauge <- function(data, labelvar="", numvar="", options=list(), chartid){

  my.type <- "Gauge"
  dataName <- deparse(substitute(data))

  my.options <- list(gvis=modifyList(list(allowHtml=TRUE),options), dataName=dataName,
                     data=list(locationvar=labelvar, numvar=numvar,
                       allowed=c("number", "string"))
                     )

 
  checked.data <- gvisCheckPieChartData(data, my.options)
  
  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options,
                      chartid=chartid, package="gauge") 
  
  return(output)
}


## plot(gvisGauge(CityPopularity, options=list(min=0, max=800, greenFrom=500,
##         greenTo=800, yellowFrom=300, yellowTo=500, redFrom=0, redTo=300)))

gvisCheckPieChartData <- function(data, options){

  data.structure <- list(
                         locationvar = list(mode="required",FUN=check.location),
                         numvar = list(mode="required",FUN=check.num)
                         )
  x <- gvisCheckData(data=data, options=options, data.structure=data.structure)

  return(data)
}

