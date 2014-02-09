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



#' Google Calendar Chart with R
#' \Sexpr{googleChartName <- "Calendar"}
#' \Sexpr{gvisChartName <- "gvisCalendar"}
#' 
#' A calendar chart is a visualization used to show activity over the course 
#' of a long span of time, such as months or years. They're best used when 
#' you want to illustrate how some quantity varies depending on the day of 
#' the week, or how it trends over time.
#' 
#' @param data a \code{data.frame}. The data has to have at least two columns,
#' one with date information (\code{datevar}) and one numerical variable.
#' @param datevar column name of \code{data} which shows the date dimension.
#' The information has to be of class \code{\link{Date}} or \code{POSIX*} time
#' series.
#' @param numvar column name of \code{data} which shows the values to be
#' displayed against \code{datevar}. The information has to be
#' \code{\link{numeric}}.
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
#' 
#' @section Warning:
#' The calendar chart may be undergoing substantial revisions in future 
#' Google Charts releases.
#' 
#' @seealso
#' 
#' See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for printing and
#' plotting methods. 
#' @keywords iplot
#' @examples
#' cl1 <- gvisCalendar(Cairo, datevar="Date", numvar="Temp")
#' plot(cl1)
#'
#' cl2 <- gvisCalendar(Cairo, datevar="Date", numvar="Temp",
#'                     options=list(
#'                     title="Daily temperature in Cairo",
#'                     height=500,
#'                     calendar="{yearLabel: { fontName: 'Times-Roman',
#'                                fontSize: 32, color: '#1A8763', bold: true},
#'                     cellColor: { stroke: 'red', strokeOpacity: 0.2 },
#'                     focusedCellColor: {stroke:'red'}}")
#'                   )
#' plot(cl2)
#'

gvisCalendar <- function(data, datevar="", 
                                numvar="", 
                                options=list(), chartid){
  
  
  my.type <- "Calendar"
  dataName <- deparse(substitute(data))
  
  my.options <- list(gvis=modifyList(list(width = 600, height=500), options), 
                     dataName=dataName,                     
                     data=list(datevar=datevar, numvar=numvar,
                               allowed=c("number", "date", "datetime"))
  )
  
  output <- gvisChart(type=my.type, checked.data=data, 
                      options=my.options, chartid=chartid)
  
  return(output)
}

