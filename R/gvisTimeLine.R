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
#' @param start number or date for the start dates
#' @param end number or date for the end dates
#' @param list of configuration options.
#' The options are documented in detail by Google online:
#' \url{https://developers.google.com/chart/interactive/docs/gallery/timeline#Configuration_Options}
#' 
#' The parameters can be set via a named list. The parameters have to map those
#' of the Google documentation. 
#' \itemize{
#' \item{parameters with names that don't include a "." and are set with a single 
#' value, e.g. width and height. Those are set like one would do in R, that is
#' \code{options=list(width=200, height=300)}. Boolean arguments are set to 
#' either \code{TRUE} or \code{FALSE}, using the R syntax.
#' }
#' \item{parameters with names that don't include a "." and are set with 
#' multiple values, e.g. \code{color}. The values for those parameter have to 
#' be wrapped in "[ ]", e.g. 
#' \code{options=list(colors="['#cbb69d', '#603913', '#c69c6e']")}
#' }
#' \item{parameters with names that do include a "." These parameters have 
#' several sub-options and have to be set as a string wrapped in "{ }", 
#' and the value of those sub-options is set via parameter:value. Boolean values
#' have to stated as 'true' or 'false'. Suppose the Google documentaion explains
#' the formating options for the vertical axis and states the parameter 
#' \code{vAxis.format}. This paramter has to be set via R as:
#' \code{options=list(vAxis="{format:'#,###\%'}")}. 
#' }
#' \item{If several sub-options have to be set, e.g. 
#' \code{titleTextStyle.color, titleTextStyle.fontName} and 
#' \code{titleTextStyle.fontSize}, then those can be combined in one list item 
#' such as:
#' \code{options=list(titleTextStyle="{color:'red',fontName:'Courier',fontSize:16}")}
#' }
#' \item{paramters that can have more than one value per sub-options are 
#' wrapped in "[ ]". For example to set the labels for left and right axes use:
#' \code{options=list(vAxes="[{title:'val1'}, {title:'val2'}]")}
#' }
#' }
#' For more details see the Google API documentation and the R examples below.   
#' @param chartid character. If missing (default) a random chart id will be 
#' generated based on chart type and \code{\link{tempfile}}
#' 
#' @return \code{gvisTimeline} returns list of \code{\link{class}}
#' "\code{gvis}" and "\code{list}".   
#' An object of class "\code{gvis}" is a list containing at least the
#' following components:
#' \describe{
#' \item{\code{type}}{Google visualisation type, here 'Timeline'}
#' \item{\code{chartid}}{character id of the chart object. Unique chart
#' ids are required to place several charts on the same page.
#' }
#' \item{\code{html}}{a list with the building blocks for a page
#' \describe{
#' \item{\code{header}}{a character string of a html page header:
#' \code{<html>...<body>},}
#' \item{\code{chart}}{a named character vector of the chart's building blocks:
#'  \describe{
#'  \item{\code{jsHeader}}{Opening \code{<script>} tag and
#'  reference to Google's JavaScript library.
#'  }
#'  \item{\code{jsData}}{JavaScript function defining the input
#'  \code{data} as a JSON object.
#'  } 
#'  \item{\code{jsDrawChart}}{JavaScript function combing the data with 
#'  the visualisation API and user options.
#'  }
#'  \item{\code{jsDisplayChart}}{JavaScript function calling the
#'  handler to display the chart.
#'  }
#'  \item{\code{jsFooter}}{End tag \code{</script>}.
#'  }
#'  \item{\code{jsChart}}{Call of the \code{jsDisplayChart} function.
#'  }
#'  \item{\code{divChart}}{\code{<div>} container to embed the chart
#'  into the page.
#'  }
#'  }   
#'  }
#'  \item{\code{caption}}{character string of a standard caption,
#'  including data name and chart id.
#'  }  
#'  \item{\code{footer}}{character string of a html page footer:
#'  \code{</body>...</html>}, including the used R and googleVis version
#'  and link to Google's Terms of Use.}
#'  }}
#' }
#' 
#' @author Markus Gesmann \email{markus.gesmann@@gmail.com}
#' 
#' @references Google Timeline API: 
#' \url{https://developers.google.com/chart/interactive/docs/gallery/timeline} 
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

gvisTimeline <- function(data, rowlabel="", barlabel="", start="", 
                         end="", options=list(), chartid){

  my.type <- "Timeline"
  dataName <- deparse(substitute(data))

  my.options <- list(gvis=modifyList(list(width=600, height=200),options), dataName=dataName,
                     data=list(rowlabel=rowlabel, barlabel=barlabel, start=start, end=end,
                       allowed=c("number", "string", "date"))
                     )

 
  checked.data <- gvisCheckTimelineData(data, my.options)
  
  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options,
                      chartid=chartid, package="timeline") 
  
  return(output)
}

gvisCheckTimelineData <- function(data, options){

  data.structure <- list(
                         rowlabel = list(mode="required", FUN=check.char),
                         barlabel = list(mode="optional", FUN=check.char),
                         start = list(mode="required", FUN=check.num),
                         end = list(mode="required", FUN=check.num)
                         )
  x <- gvisCheckData(data=data, options=options, data.structure=data.structure)

  return(data)
}

