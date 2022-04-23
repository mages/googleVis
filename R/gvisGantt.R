### File R/gvisGantt.R
### Part of the R package googleVis
### Copyright 2010 - 2022 Markus Gesmann, Diego de Castillo

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

#' Google Gantt Chart with R
#' \Sexpr{googleChartName <- "ganttchart"}
#' \Sexpr{gvisChartName <- "gvisGantt"}
#' 
#' @description
#' A Gantt chart is a type of chart that illustrates the breakdown of a 
#' project into its component tasks. 
#' 
#' @param data data.frame that contains the data to be visualised
#' @param taskID a string that refers to the column name in 
#'  \code{data} for the task ID to be used
#' @param taskName a string that refers to the column name in 
#'  \code{data} for the task name to be used
#' @param resource a string that refers to the column name in 
#'  \code{data} for the resource to be used
#' @param start a string that refers to the date column name in 
#'  \code{data} for the start dates
#' @param end a string that refers to the date column name in 
#'  \code{data} for the end dates
#' @param duration a string that refers to the numeric column name in 
#'  \code{data} for the task duration in milliseconds
#' @param percentComplete a string that refers to the numeric column name in 
#'  \code{data} for the percent complete to be used
#' @param dependencies a string that refers to the column name in 
#'  \code{data} for the dependencies to be used
#' @param options list of configuration options.
#' The options are documented in detail by Google online:
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
#' @author Markus Gesmann \email{markus.gesmann@@gmail.com}
#' 
#' @keywords iplot
#' 
#' @examples
#' 
#' # Helper function to generate example data
#' daysToMilliseconds <- function(days){
#'  days * 24 * 60 * 60 * 1000
#' }
#' 
#' dat <- data.frame(
#'  taskID = c("Research", "Write", "Cite", "Complete", "Outline"),
#'  taskName = c("Find sources", "Write Paper",  "Create bibliography", 
#'               "Hand in paper", "Outline paper"),
#'  resource = c(NA, "write", "write", "complete", "write"),
#'  start = c(as.Date("2015-01-01"), NA, NA, NA, NA),
#'  end = as.Date(c("2015-01-05", "2015-01-09", "2015-01-07", 
#'                  "2015-01-10", "2015-01-06")),
#'  duration = c(NA, daysToMilliseconds(c(3, 1, 1, 1))),
#'  percentComplete = c(100, 25, 20, 0, 100),
#'  dependencies = c(NA, "Research, Outline", "Research", 
#'                   "Cite, Write", "Research")
#' )
#' 
#' gntt1 <- gvisGantt(dat, taskID = "taskID",
#'                   taskName = "taskName", 
#'                   resource = "resource",
#'                   start = "start",
#'                   end = "end", 
#'                   duration = "duration",
#'                   percentComplete = "percentComplete",
#'                   dependencies = "dependencies")
#' plot(gntt1)
#' 
#' ## gantt chart with options set
#' 
#' gntt2 <- gvisGantt(dat, taskID = "taskID",
#'                   taskName = "taskName", 
#'                   resource = "resource",
#'                   start = "start",
#'                   end = "end", 
#'                   duration = "duration",
#'                   percentComplete = "percentComplete",
#'                   dependencies = "dependencies",
#'                   options = list(
#'                        height = 275,
#'                        gantt  = "{
#'                          criticalPathEnabled: true,
#'                          innerGridHorizLine: {
#'                          stroke: '#ffe0b2',
#'                          strokeWidth: 2
#'                        },
#'                        innerGridTrack: {fill: '#fff3e0'},
#'                        innerGridDarkTrack: {fill: '#ffcc80'},
#'                        labelStyle: {
#'                          fontName: 'Arial',
#'                          fontSize: 14
#'                      }}"
#'  ))
#' plot(gntt2)
#' 
#' # Example with date time
#' dat <- data.frame(
#'      taskID = c("Research", "Write", "Complete"),
#'       taskName = c("Find sources", "Write Paper", "Hand in paper"),
#'       resource = c(NA, "write", "complete"),
#'       start = c(as.POSIXct("2015-01-01 6:00:00"), NA, NA),
#'       end = as.POSIXct(c("2015-01-01 8:00:00", "2015-01-01 13:30:00", 
#'       "2015-01-01 20:30:00")),
#'       duration = c(NA, daysToMilliseconds(c(.1, .05))),
#'       percentComplete = c(100, 25, 0),
#'       dependencies = c(NA, "Research", "Write"))
#' 
#' gntt3 <- gvisGantt(dat, taskID = "taskID",
#'                    taskName = "taskName",
#'                    resource = "resource",
#'                    start = "start",
#'                    end = "end",
#'                    duration = "duration",
#'                    percentComplete = "percentComplete",
#'                    dependencies = "dependencies")
#' plot(gntt3)
#' 

gvisGantt <- function(data, taskID="", taskName="", resource= "", start="", 
                      end="", duration = "", percentComplete = "", dependencies = "",
                      options=list(), chartid){
  
  my.type <- "Gantt"
  dataName <- deparse(substitute(data))
  
  my.options <- list(gvis=modifyList(list(width=600, height=200),options), 
                     dataName=dataName,
                     data=list(taskID = taskID, 
                               taskName = taskName, 
                               resource = resource, 
                               start = start, 
                               end = end, 
                               duration = duration, 
                               percentComplete = percentComplete, 
                               dependencies = dependencies,
                               allowed=c("number", "string", "date", "datetime"))
  )
  
  
  output <- gvisChart(type=my.type, checked.data=data, options=my.options,
                      chartid=chartid, package="gantt") 
  
  output$html$chart <- gsub("data.addColumn\\('datetime','start'\\)", 
                            "data.addColumn\\('date','start'\\)", 
                            output$html$chart)
  output$html$chart <- gsub("data.addColumn\\('datetime','end'\\)", 
                            "data.addColumn\\('date','end'\\)", 
                            output$html$chart)
  
  return(output)
}
