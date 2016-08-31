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



#' Google Annotated Time Line with R
#' \Sexpr{googleChartName <- "annotatedtimeline"}
#' \Sexpr{gvisChartName <- "gvisAnnotatedTimeLine"}
#' 
#' The gvisAnnotatedTimeLine function reads a data.frame and creates text
#' output referring to the Google Visualisation API, which can be included into
#' a web page, or as a stand-alone page.
#' 
#' An annotated time line is an interactive time series line chart with
#' optional annotations. The chart is rendered within the browser using Flash.
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
#' @section Warnings:
#' AnnotatedTimeline (gvisAnnotatedTimeLine) is Flash based, conisder using  AnnotationChart (gvisAnnotationChart) instead.
#' For more details visit: goo.gl/tkiEV8
#' 
#' Because of Flash security settings the chart might not work correctly when
#' accessed from a file location in the browser (e.g.,
#' file:///c:/webhost/myhost/myviz.html) rather than from a web server URL
#' (e.g. http://www.myhost.com/myviz.html). See the googleVis package vignette
#' and the Macromedia web site
#' (\url{http://www.macromedia.com/support/documentation/en/flashplayer/help/})
#' for more details.
#' 
#' Important: To use this visualization, you must specify the height and width 
#' of the container element explicitly on your page. So, for example:
#' options=list(width="600px", height="350px")
#' 
#' Use code{\link{gvisAnnotationChart}} for a non-Flash version of this plot.
#' 
#' @author Markus Gesmann \email{markus.gesmann@@gmail.com},
#' 
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
#' ## will open a browser window and requires Flash and Internet
#' ## connection to display the visualisation.
#' 
#' 
#' data(Stock)
#' Stock
#' A1 <- gvisAnnotatedTimeLine(Stock, datevar="Date",
#'                            numvar="Value", idvar="Device",
#'                            titlevar="Title", annotationvar="Annotation",
#'                            options=list(displayAnnotations=TRUE,
#'                             legendPosition='newRow',
#'                             width="600px", height="350px")
#'                            )
#' plot(A1)
#' 
#' ## Two Y-axis
#' A2 <- gvisAnnotatedTimeLine(Stock, datevar="Date",
#'                            numvar="Value", idvar="Device",
#'                            titlevar="Title", annotationvar="Annotation",
#'                            options=list(displayAnnotations=TRUE, 
#'                             width="600px", height="350px", scaleColumns='[0,1]',
#'                             scaleType='allmaximized')
#'                           )
#' plot(A2)
#' 
#' ## Zoom into the time window, no Y-axis ticks
#' A3 <- gvisAnnotatedTimeLine(Stock, datevar="Date",
#'                            numvar="Value", idvar="Device",
#'                            titlevar="Title", annotationvar="Annotation",
#'                            options=list(
#'                              width="600px", height="350px",
#'                             zoomStartTime=as.Date("2008-01-04"),
#'                             zoomEndTime=as.Date("2008-01-05")) 
#'                           )
#' plot(A3)
#' 
#' 
#' 
#' ## Colouring the area below the lines to create an area chart
#' A4 <- gvisAnnotatedTimeLine(Stock, datevar="Date",
#'                            numvar="Value", idvar="Device",
#'                            titlevar="Title", annotationvar="Annotation",
#'                            options=list(
#'                              width="600px", height="350px",
#'                              fill=10, displayExactValues=TRUE,
#'                              colors="['#0000ff','#00ff00']")
#'                            )
#'                           
#' plot(A4)
#' 
#' 
#' ## Data with POSIXct datetime variable
#' A5 <- gvisAnnotatedTimeLine(Andrew, datevar="Date/Time UTC",
#'                             numvar="Pressure_mb",
#'                             options=list(scaleType='maximized',
#'                                          width="600px", height="350px")
#'                             )
#' 
#' plot(A5)
#' 
#' 
#' \dontrun{
#' 
#' ## Plot Apple's monthly stock prices since 1984
#' 
#' ## Get current date
#' d <- Sys.time() 
#' current.year <- format(d, "%Y")
#' current.month <- format(d, "%m")
#' current.day <- format(d, "%d")
#' 
#' ## Yahoo finance sets January to 00 hence: 
#' month <- as.numeric(current.month)  - 1
#' month <- ifelse(month < 10, paste("0",month, sep=""), m)
#' 
#' ## Get weekly stock prices from Apple Inc.
#' tckr <- 'AAPL'
#' yahoo <- 'http://ichart.finance.yahoo.com/table.csv'
#' 
#' fn <- sprintf('%s?s=%s&a=08&b=7&c=1984&d=%s&e=%s&f=%s&g=w&ignore=.csv',
#'       yahoo, tckr, month, current.day, current.year)
#' 
#' ## Get data from Yahoo! Finance
#' data <- read.csv(fn, colClasses=c("Date", rep("numeric",6)))
#' 
#' AAPL <- reshape(data[,c("Date", "Close", "Volume")], idvar="Date", 
#'      		times=c("Close", "Volume"), 
#'                 timevar="Type",
#'                 varying=list(c("Close", "Volume")),
#'                 v.names="Value",
#'                 direction="long")
#' 
#' ## Calculate previous two years for zoom start time
#' lyd <- as.POSIXlt(as.Date(d))
#' lyd$year <- lyd$year-2
#' lyd <- as.Date(lyd)
#' 
#' aapl <- gvisAnnotatedTimeLine(AAPL, datevar="Date",
#'                            numvar="Value", idvar="Type",
#'                           options=list(
#'                             colors="['blue', 'lightblue']",
#'                             zoomStartTime=lyd,
#'                             zoomEndTime=as.Date(d),
#'                             legendPosition='newRow',
#'                             width="600px", height="400px", scaleColumns='[0,1]',
#'                             scaleType='allmaximized')
#'                            )
#' 
#' plot(aapl)
#' }
#' 
gvisAnnotatedTimeLine <- function(data, datevar="", 
                                  numvar="", idvar="", titlevar="", 
                                  annotationvar="", 
                                  date.format="%Y/%m/%d",
                                  options=list(), chartid){
  warning("AnnotatedTimeline (gvisAnnotatedTimeLine) is Flash based, conisder using  AnnotationChart (gvisAnnotationChart) instead.\nFor more details visit: goo.gl/tkiEV8")
  
  
  my.type <- "AnnotatedTimeLine"
  dataName <- deparse(substitute(data))
  
  my.options <- list(gvis=modifyList(list(width = "600px", height="300px"), options), 
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


gvisCheckAnnotatedTimeLineData <- function(data, options, datevar,idvar,
                                           titlevar, annotationvar){

  if( any(class(data[[datevar]]) %in% c("POSIXct", "POSIXlt")) ){   
    data.structure <- list(
                           datevar  = list(mode="required",FUN=check.datetime),
                           numvar   =  list(mode="required",FUN=check.num),
                           idvar  = list(mode="optional",FUN=check.char),                   
                           titlevar = list(mode="optional",FUN=check.char),
                           annotationvar  = list(mode="optional",FUN=check.char))
  }else{
    data.structure <- list(
                           datevar  = list(mode="required",FUN=check.date),
                           numvar   =  list(mode="required",FUN=check.num),
                           idvar  = list(mode="optional",FUN=check.char),                   
                           titlevar = list(mode="optional",FUN=check.char),
                           annotationvar  = list(mode="optional",FUN=check.char))
  }
  
  x <- gvisCheckData(data=data, options=options, data.structure=data.structure)
   

  x.df <- as.data.frame(x)


    ##check if idvar is missing that datevar define unique rows
  if(idvar==""){
    if(length(x.df[,1]) != length(unique(x.df[,1])))
      message("Warning: The data appears to more than one entry for the same date.\n",
              "Have you considered using the idvar variable?")
    ## if idvar is missing no reshape is required.
    return(x.df)
  }
  

  groups <- factor(x.df[[idvar]])
  ngroups <- nlevels(groups)


  checkTitleAnno <- c(titlevar != "", annotationvar != "")
  if(all(checkTitleAnno)){
    varying.vars <- c(2,4,5)
  }else{
    if(any(checkTitleAnno))
      varying.vars <-  c(2,4)
    else
      varying.vars <- 2
  }
  var.names <- names(x.df)[varying.vars]
  
  x.df <- reshape(x.df,
                  v.names=var.names, ##numvar , titlevar, annotationvar
                  idvar=names(x.df)[1], ## datevar 
                  timevar=names(x.df)[3], ## idvar
                  direction="wide") 

  names(x.df) <- gsub(paste(var.names[1], ".", sep=""), "", names(x.df))
  
  return(x.df)
}

