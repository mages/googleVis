### Part of the R package googleVis
### Copyright 2011 - 2014 Markus Gesmann, Diego de Castillo

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



#' Google Bubble Chart with R
#' \Sexpr{googleChartName <- "bubblechart"}
#' \Sexpr{gvisChartName <- "gvisBubbleChart"}
#' 
#' 
#' The gvisBubbleChart function reads a data.frame and creates text output
#' referring to the Google Visualisation API, which can be included into a web
#' page, or as a stand-alone page.
#' 
#' A bubble chart is used to visualize a data set with 2 to 4 dimensions. The
#' first two dimensions are visualized as coordinates, the 3rd as color and the
#' 4th as size.
#' 
#' The bubble chart is rendered within the browser using SVG or VML and
#' displays tips when hovering over points.
#' 
#' 
#' @param data a \code{\link{data.frame}} to be displayed as a bubble chart.
#' The data has to have at least three columns for \code{idvar, xvar}, and
#' \code{yvar}.
#' @param idvar column name of \code{data} with the bubble
#' @param xvar column name of a numerical vector in \code{data} to be plotted
#' on the x-axis.
#' @param yvar column name of a numerical vector in \code{data} to be plotted
#' on the y-axis.
#' @param colorvar column name of data that identifies bubbles in the same
#' series. Use the same value to identify all bubbles that belong to the same
#' series; bubbles in the same series will be assigned the same color. Series
#' can be configured using the \code{series} option.
#' @param sizevar values in this column are mapped to actual pixel values using
#' the \code{sizeAxis} option.
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
#' generated based on chart type and \code{\link{tempfile}}.
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
#' @seealso See also \code{\link{gvisMotionChart}} for a moving bubble chart
#' over time, and \code{\link{print.gvis}}, \code{\link{plot.gvis}} for
#' printing and plotting methods.
#' @keywords iplot
#' @examples
#' 
#' 
#' bubble1 <- gvisBubbleChart(Fruits, idvar="Fruit", xvar="Sales", yvar="Expenses")
#' plot(bubble1)
#' 
#' ## Set color and size
#' bubble2 <- gvisBubbleChart(Fruits, idvar="Fruit", xvar="Sales", yvar="Expenses",
#'                           colorvar="Location", sizevar="Profit",
#'                           options=list(hAxis='{minValue:75, maxValue:125}'))
#' 
#' plot(bubble2)
#' 
#' ## Use year to color the bubbles
#' bubble3 <- gvisBubbleChart(Fruits, idvar="Fruit", xvar="Sales", yvar="Expenses",
#'                            colorvar="Year", sizevar="Profit",
#'                            options=list(hAxis='{minValue:75, maxValue:125}'))
#' plot(bubble3)
#' 
#' ## Gradient colour example
#' bubble4 <- gvisBubbleChart(Fruits, idvar="Fruit", xvar="Sales", yvar="Expenses",
#'                           sizevar="Profit",
#'                           options=list(hAxis='{minValue:75,  maxValue:125}', 
#'                                   colorAxis="{colors: ['lightblue', 'blue']}"))
#' plot(bubble4)
#' 
#' \dontrun{
#' ## Moving bubble chart over time, aka motion chart
#' 
#' M <- gvisMotionChart(Fruits, Fruit, Year)
#' plot(M)
#' }
#' 
#' 
#' 
#' 
gvisBubbleChart <- function(data, idvar="", xvar="", yvar="", colorvar="", sizevar="", options=list(), chartid){

  my.type <- "BubbleChart"
  dataName <- deparse(substitute(data))

  my.options <- list(gvis=options, 
                     dataName=dataName,
                     data=list(idvar=idvar,
                       xvar=xvar,
                       yvar=yvar,                       
                       colorvar=colorvar,
                       sizevar=sizevar,
                       allowed=c("number", "string")
                       )
                     )

 
  checked.data <- gvisCheckBubbleChartData(data, my.options)

  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options, chartid=chartid,
                      package="corechart")
  
  return(output)
}
   
  


gvisCheckBubbleChartData <- function(data, options){

  varNames <- names(data)
  
  data.structure <- list(
                         idvar = list(mode="required", FUN=check.char),
                         xvar = list(mode="required", FUN=check.num),
                         yvar  = list(mode="required", FUN=check.num),
                         colorvar  = list(mode="optional", FUN=check.char.num),
                         sizevar  = list(mode="optional", FUN=check.num))
	
  x <- gvisCheckData(data=data,options=options,data.structure=data.structure)
   x <- data.frame(x)
   
  return(x)
}
