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



#' Google Annotation Chart with R
#' \Sexpr{googleChartName <- "annotationchart"}
#' \Sexpr{gvisChartName <- "gvisAnnotationChart"}
#' 
#' gvisAnnotationChart charts are interactive time series line charts that support 
#' annotations. Unlike the gvisAnnotatedTimeLine, which uses Flash, 
#' annotation charts are SVG/VML and should be preferred whenever possible.
#' 
#' @param data a \code{data.frame}. The data has to have at least two columns,
#' one with date information (\code{datevar}) and one numerical variable.
#' @param datevar column name of \code{data} which shows the date dimension.
#' The information has to be of class \code{\link{Date}} or \code{POSIX*} time
#' series.
#' @param numvar column name of \code{data} which shows the values to be
#' displayed against \code{datevar}. The information has to be
#' \code{\link{numeric}}.
#' @param idvar column name of \code{data} which identifies different groups of
#' the data. The information has to be of class \code{\link{character}} or
#' \code{\link{factor}}.
#' @param titlevar column name of \code{data} which shows the title of the
#' annotations. The information has to be of class \code{\link{character}} or
#' \code{\link{factor}}.  Missing information can be set to \code{NA}. See
#' section 'Details' for more details.
#' @param annotationvar column name of \code{data} which shows the annotation
#' text. The information has to be of class \code{\link{character}} or
#' \code{\link{factor}}. Missing information can be set to \code{NA}. See
#' section 'Details' for more details.
#' @param date.format if \code{datevar} is of class \code{\link{Date}} then
#' this argument specifies how the dates are reformatted to be used by
#' JavaScript.
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
#' Diego de Castillo \email{decastillo@@gmail.com}
#' @seealso
#' 
#' See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for printing and
#' plotting methods. Further see \code{\link{reshape}} for reshaping data, e.g.
#' from a wide format into a long format.
#' @keywords iplot
#' @examples
#' 
#' ## Please note that by default the googleVis plot command
#' ## will open a browser window and requires Internet
#' ## connection to display the visualisation.
#' 
#' 
#' data(Stock)
#' Stock
#' A1 <- gvisAnnotationChart(Stock, datevar="Date",
#'                            numvar="Value", idvar="Device",
#'                            titlevar="Title", annotationvar="Annotation",
#'                            options=list(displayAnnotations=TRUE,
#'                             legendPosition='newRow',
#'                             width=600, height=350)
#'                            )
#' plot(A1)
#' 
#' ## Two Y-axis
#' A2 <- gvisAnnotationChart(Stock, datevar="Date",
#'                            numvar="Value", idvar="Device",
#'                            titlevar="Title", annotationvar="Annotation",
#'                            options=list(displayAnnotations=TRUE, 
#'                             width=600, height=350, scaleColumns='[0,1]',
#'                             scaleType='allmaximized')
#'                           )
#' plot(A2)
#' 
#' 
#' 
#' ## Colouring the area below the lines to create an area chart
#' A3 <- gvisAnnotationChart(Stock, datevar="Date",
#'                            numvar="Value", idvar="Device",
#'                            titlevar="Title", annotationvar="Annotation",
#'                            options=list(
#'                              width=600, height=350,
#'                              fill=10, displayExactValues=TRUE,
#'                              colors="['#0000ff','#00ff00']")
#'                            )
#'                           
#' plot(A3)
#' 
#' 
#' ## Data with POSIXct datetime variable
#' A4 <- gvisAnnotationChart(Andrew, datevar="Date/Time UTC",
#'                             numvar="Pressure_mb",
#'                             options=list(scaleType='maximized')
#'                             )
#' 
#' plot(A4)
#' 
#' # Change background to blue
#' A5 <- gvisAnnotationChart(Stock, datevar="Date",
#'               numvar="Value", idvar="Device",
#'               titlevar="Title", annotationvar="Annotation",
#'               options=list(
#'                 displayAnnotations=TRUE,
#'                   chart = "{chartArea:{backgroundColor:'#003b70'}}",
#'                             legendPosition='newRow',
#'                             width=600, height=350))
#'                             
#' plot(A5)
#' 
gvisAnnotationChart <- function(data, datevar="", 
                                  numvar="", idvar="", titlevar="", 
                                  annotationvar="", 
                                  date.format="%Y/%m/%d",
                                  options=list(), chartid){
  
  
  my.type <- "AnnotationChart"
  dataName <- deparse(substitute(data))
  
  my.options <- list(gvis=modifyList(list(width = 600, height=300), options), 
		     dataName=dataName,                     
                     data=list(datevar=datevar, numvar=numvar,
                       idvar=idvar, titlevar=titlevar, annotationvar=annotationvar,
		       date.format=date.format,
                       allowed=c("number", "string", "date", "datetime"))
                     )
  
  checked.data <- gvisCheckAnnotatedTimeLineData(data, my.options, datevar=datevar,
                                                 idvar=idvar, titlevar=titlevar, annotationvar=annotationvar)

  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options, chartid=chartid)
  
  return(output)
}

