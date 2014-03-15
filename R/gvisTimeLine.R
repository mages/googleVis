### File R/gvisPieChart.R
### Part of the R package googleVis
### Copyright 2010, 2011, 2012, 2013 Markus Gesmann, Diego de Castillo

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

#' Google Timeline Chart with R
#' \Sexpr{googleChartName <- "timeline"}
#' \Sexpr{gvisChartName <- "gvisTimeline"}
#' 
#' @description
#' A timeline is a chart that depicts how a set of resources are used 
#' over time. One popular type of timeline is the Gantt chart.
#' 
#' @param data data.frame that contains the data to be visualised
#' @param rowlabel a string that referes to the column name in 
#'  \code{data} for the row labels to be used
#' @param barlabel a string that referes to the column name in 
#'  \code{data} for the bar labels to be used
#' @param start number, date or datetime for the start dates
#' @param end number, date or datetime for the end dates
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
#' @export
#' 
#' @keywords iplot
#' 
#' @examples
#' dat <- data.frame(Term=c("1","2","3"),
#'                   President=c("Whasington", "Adams", "Jefferson"),
#'                   start=as.Date(x=c("1789-03-29", "1797-02-03", "1801-02-03")),
#'                   end=as.Date(x=c("1797-02-03", "1801-02-03", "1809-02-03")))
#' 
#' tl <- gvisTimeline(data=dat[,-1], rowlabel="President", 
#'                    start="start", end="end")
#' plot(tl)
#' 
#' tl <- gvisTimeline(data=dat, barlabel="President", 
#'                    start="start", end="end")
#' plot(tl)
#' 
#' tl <- gvisTimeline(data=dat, rowlabel="President", 
#'                    start="start", end="end",
#'                    options=list(timeline="{showRowLabels:false}"))
#' plot(tl)
#' 
#' dat <- data.frame(Position=c(rep("President", 3), rep("Vice", 3)),
#'                   Name=c("Washington", "Adams", "Jefferson",
#'                          "Adams", "Jefferson", "Burr"),
#'                   start=as.Date(x=rep(c("1789-03-29", "1797-02-03", "1801-02-03"),2)),
#'                   end=as.Date(x=rep(c("1797-02-03", "1801-02-03", "1809-02-03"),2)))
#' 
#' tl <- gvisTimeline(data=dat, rowlabel="Name",barlabel="Position", 
#'                    start="start", end="end",
#'                    options=list(timeline="{showRowLabels:true}"))
#' plot(tl)
#' 
#' tl <- gvisTimeline(data=dat, rowlabel="Name",barlabel="Position", 
#'                    start="start", end="end",
#'                    options=list(timeline="{groupByRowLabel:false}",
#'                                 backgroundColor='#ffd', height=350,
#'                                 colors="['#cbb69d', '#603913', '#c69c6e']"))
#' 
#' plot(tl)
#'
#' # Datetime example 
#' dat <- data.frame(Room=c("Room 1","Room 2","Room 3"),
#'                   Language=c("English", "German", "French"),
#'                   start=as.POSIXct(c("2014-03-14 14:00", "2014-03-14 15:00", 
#'                                      "2014-03-14 14:30")),
#'                  end=as.POSIXct(c("2014-03-14 15:00", "2014-03-14 16:00", 
#'                                   "2014-03-14 15:30")))
#' tl <- gvisTimeline(data=dat, rowlabel="Language",
#'                    start="start", end="end")
#' plot(tl)
#'
#' \dontrun{
#' require(timeline)
#' data(ww2)
#' timeline(ww2, ww2.events, event.spots=2, event.label='', event.above=FALSE)
#' ww2$Person <- gsub("\\n" ," ", ww2$Person)
#' plot(gvisTimeline(ww2, barlabel="Person", rowlabel="Group",
#'                   start="StartDate", end="EndDate",
#'      options=list(width=600, height=350))
#' )
#' }

gvisTimeline <- function(data, rowlabel="", barlabel="", start="", 
                         end="", options=list(), chartid){

  my.type <- "Timeline"
  dataName <- deparse(substitute(data))

  my.options <- list(gvis=modifyList(list(width=600, height=200),options), dataName=dataName,
                     data=list(rowlabel=rowlabel, barlabel=barlabel, start=start, end=end,
                       allowed=c("number", "string", "date", "datetime"))
                     )

 
  checked.data <- gvisCheckTimelineData(data, rl=rowlabel, bl=barlabel,
                                        start=start, end=end)
  
  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options,
                      chartid=chartid, package="timeline") 
  
  return(output)
}

gvisCheckTimelineData <- function(data, rl, bl, start, end){

  if(any(c(rl, bl, start, end) %in% ""))
    return(data)
  else  
    return(data[, c(rl, bl, start, end)])
}

