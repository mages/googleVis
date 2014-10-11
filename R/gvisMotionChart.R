### File R/gvisMotionChart.R
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

#' Google Motion Chart with R
#' \Sexpr{googleChartName <- "motionchart"}
#' \Sexpr{gvisChartName <- "gvisMotionChart"}
#'
#' @description
#' The gvisMotionChart function reads a data.frame and
#' creates text output referring to the Google Visualisation API, 
#' which can be included into a web page, or as a stand-alone page. 
#' The actual chart is rendered by the web browser in Flash.
#' A motion chart is a dynamic chart to explore several indicators over 
#' time.
#' 
#' @param data a \code{data.frame}. The data has to have at least four
#' columns with subject name (\code{idvar}), time (\code{timevar}) and
#' two columns of numeric values. Further columns, numeric and
#' character/factor are optional. The combination of \code{idvar} and
#' \code{timevar} has to describe a unique row. The column names of the
#' \code{idvar} and \code{timevar} have to be specified. Further
#' columns, if not specified by the other arguments (\code{xvar, yvar,
#' colorvar, sizevar}), will be assumed to be in the order of the 
#' arguments.
#' @param idvar column name of \code{data} with the subject to be 
#' analysed.
#' @param timevar column name of \code{data} which shows the time 
#' dimension. The information has to be either numeric, of class 
#' \code{\link{Date}} or a character which follows the pattern  
#' 'YYYYWww' (e.g. '2010W04' for weekly data) or 'YYYYQq' 
#' (e.g. '2010Q1' for quarterly data).
#' @param xvar column name of a numerical vector in \code{data} to be 
#' plotted on the x-axis.
#' @param yvar column name of a numerical vector in \code{data} to be 
#' plotted on the y-axis.
#' @param colorvar column name of data that identifies bubbles in the 
#' same series. Use the same value to identify all bubbles that belong 
#' to the same series; bubbles in the same series will be assigned the 
#' same color. Series can be configured using the \code{series} option.
#' @param sizevar values in this column are mapped to actual pixel 
#' values using the \code{sizeAxis} option.
#' @param date.format if \code{timevar} is of class \code{\link{Date}} 
#' then this argument specifies how the dates are reformatted to be 
#' used by JavaScript. 
#' @param options list of configuration options for Google Motion Chart.
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
#' @author Markus Gesmann \email{markus.gesmann@@gmail.com}, 
#' Diego de Castillo \email{decastillo@@gmail.com}
#' 
#' 
#' @section Warnings:
#' Because of Flash security settings the chart 
#' might not work correctly when accessed from a file location in the 
#' browser (e.g., file:///c:/webhost/myhost/myviz.html) rather than 
#' from a web server URL (e.g. http://www.myhost.com/myviz.html). 
#' See the googleVis package vignette and the Macromedia web 
#' site (\url{http://www.macromedia.com/support/documentation/en/flashplayer/help/}) 
#' for more details.
#' 
#' @section Note:
#' Please note that a \code{timevar} with values less than 100 will 
#' be shown as years 19xx.
#' 
#' @seealso 
#' See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} 
#' for printing and plotting methods.
#' 
#' @export
#' 
#' @keywords iplot
#' 
#' @examples
#' ## Please note that by default the googleVis plot command
#' ## will open a browser window and requires Flash and Internet
#' ## connection to display the visualisation.
#' M1 <- gvisMotionChart(Fruits, idvar="Fruit", timevar="Year")
#' plot(M1)
#' 
#' \dontrun{
#' ## Usage of date variable
#' M2 <- gvisMotionChart(Fruits, idvar="Fruit", timevar="Date",
#'                       date.format = "\%Y\%m\%d") 
#'                       plot(M2)
#'                       
#' ## Display weekly data:
#' M3 <- gvisMotionChart(Fruits, "Fruit", "Date", date.format="\%YW\%W")
#' plot(M3) 
#' }
#' ## Options: no side panel on the right
#' M4 <- gvisMotionChart(Fruits,"Fruit", "Year",
#'                       options=list(showSidePanel=FALSE))
#' plot(M4)
#' 
#' ## Options: trails un-ticked
#' M5 <- gvisMotionChart(Fruits, "Fruit", "Year",
#'                       options=list(state='{"showTrails":false};'))
#'                       
#' plot(M5)
#' 
#' ## You can change some of displaying settings via the browser,
#' ## e.g. the level of opacity of non-selected items, or the chart type.
#' ## The state string from the 'Advanced' tab can be used to set those
#' ## settings via R. Just copy and past the string from the browser into
#' ## the argument state of the options list.
#' ## Here is an example of a motion chart, with an initial line chart
#' ## displayed. Ensure that you have a newline at the start and end of
#' ## your settings string.
#' 
#' myStateSettings <-'
#' {"xZoomedDataMin":1199145600000,"colorOption":"2",
#' "duration":{"timeUnit":"Y","multiplier":1},"yLambda":1,
#' "yAxisOption":"4","sizeOption":"_UNISIZE",
#' "iconKeySettings":[],"xLambda":1,"nonSelectedAlpha":0,
#' "xZoomedDataMax":1262304000000,"iconType":"LINE",
#' "dimensions":{"iconDimensions":["dim0"]},
#' "showTrails":false,"uniColorForNonSelected":false,
#' "xAxisOption":"_TIME","orderedByX":false,"playDuration":15000,
#' "xZoomedIn":false,"time":"2010","yZoomedDataMin":0,
#' "yZoomedIn":false,"orderedByY":false,"yZoomedDataMax":100}
#' '
#' M6a <- gvisMotionChart(Fruits, "Fruit", "Year", 
#'                        options=list(state=myStateSettings))
#' plot(M6a)
#' 
#' ## Newline set explicitly
#' myStateSettings <-'\n{"iconType":"LINE"}\n'
#' M6b <- gvisMotionChart(Fruits, "Fruit", "Year", 
#'                        options=list(state=myStateSettings))
#' plot(M6b)
#' 
#' 
#' ## Define which columns are used for the initial setup of the various
#' ## dimensions
#' M7 <- gvisMotionChart(Fruits, idvar="Fruit", timevar="Year",
#'                       xvar="Profit", yvar="Expenses",
#'                       colorvar="Location", sizevar="Sales")
#' plot(M7)
#' ## For more information see:
#' ## https://developers.google.com/chart/interactive/docs/gallery/motionchart
#' 
#' ## See also the demo(WorldBank). It demonstrates how you can access
#' ## country level data from the World Bank to create Gapminder-like
#' ## plots.
#'  
gvisMotionChart <- function(data, idvar="id", timevar="time",
                            xvar="", yvar="", colorvar="", sizevar="",
                            date.format="%Y/%m/%d",
                            options=list(), chartid){
  
  
  my.type <- "MotionChart"
  dataName <- deparse(substitute(data))
  
  vars <- c(idvar, timevar)#, xvar, yvar)#, colorvar, sizevar) 
  vars.pos <- na.omit(match(vars, names(data)))
  nm <- c(names(data)[vars.pos], names(data)[-vars.pos])
  data <- cbind(data[, vars.pos], data[,-vars.pos])
  names(data) <- nm
  
  
  if(xvar != ""){
    x.col <- match(xvar, names(data) ) - 1
    xvar <- paste0("\"xAxisOption\":\"",x.col,"\",")   
  }
  if(yvar != ""){
    y.col <- match(yvar, names(data) ) - 1
    yvar <- paste0("\"yAxisOption\":\"",y.col,"\",")   
  }
  if(colorvar != ""){
    colour.col <- match(colorvar, names(data) ) - 1
    colorvar <- paste0("\"colorOption\":\"",colour.col,"\",")   
  }
  if(sizevar != ""){
    size.col <- match(sizevar, names(data) ) - 1
    sizevar <- paste0("\"sizeOption\":\"",size.col,"\",") 
  }
  
  myState <- ""
  if(!all(c(xvar, yvar, colorvar, sizevar) %in% "")){
    myState <- paste0("\n{", xvar, yvar, colorvar, sizevar,
                      "\"dimensions\":{\"iconDimensions\":[\"dim0\"]}}\n") 
  }
  
  ## Combine options for other generic functions
  my.options <- list(gvis=modifyList(list(width = 600, height=500,
                                          state=myState), options),
                     dataName=dataName,
                     data=list(idvar=idvar, timevar=timevar,
                               date.format=date.format,
                               allowed=c("number",
                                         "string", "date"))
  )
  
  checked.data <- gvisCheckMotionChartData(data, my.options)
  
  output <- gvisChart(type=my.type, checked.data=checked.data,
                      options=my.options, chartid)
  
  return(output)
}



gvisCheckMotionChartData <- function(data, options){
  
  ## Motion Charts require in the first column the idvar and time var in the second column
  ## The combination of idvar and timevar has to be unique
  
  ## Google Motion Chart needs a 'string' in the id variable (first column)
  ## A number or date in the time variable (second column)
  ## Everything else has to be a number or string
  
  ## Convert data.frame to list
  x <- as.list(data)
  varNames <- names(x)
  
  ## typeMotionChart will hold the Google DataTable formats of our data
  typeMotionChart <- as.list(rep(NA, length(varNames)))
  names(typeMotionChart) <- varNames
  
  ## Check if idvar and timevar match columns in the data
  idvar.timevar.pos <- match(c(options$data$idvar, 
                               options$data$timevar), varNames)
  if(sum(!is.na(idvar.timevar.pos)) < 2){
    stop("There is a missmatch between the idvar and timevar specified and the colnames of your data.")
  }
  
  
  typeMotionChart[[options$data$timevar]] <-
    testTimevar(x[[options$data$timevar]], options$data$date.format)
  
  if(typeMotionChart[[options$data$timevar]] == "string" &
       (options$data$date.format %in% c("%YW%W","%YW%U"))){
    ## only true for weekly data
    x[[options$data$timevar]] <- format.Date(x[[options$data$timevar]],
                                             options$data$date.format) 
  }
  
  
  ## idvar has to be a character, so lets try to convert it into a character
  if( ! is.character(x[[options$data$idvar]]) ){
    x[[options$data$idvar]] <- as.character(x[[options$data$idvar]])
  }
  typeMotionChart[[options$data$idvar]] <- "string"
  
  varOthers <- varNames[ -idvar.timevar.pos  ]  
  
  varOrder <- c(options$data$idvar, options$data$timevar, varOthers)
  x <- x[varOrder]
  
  typeMotionChart[varOthers] <- sapply(varOthers, function(.x)
    ifelse(is.numeric(x[[.x]]), "number","string"))
  
  typeMotionChart <- typeMotionChart[varOrder]
  x[varOthers] <- lapply(varOthers,function(.x){
    if(class(x[[.x]])=="Date") as.character(x[[.x]]) else x[[.x]]
  }) 
  
  
  ## check uniqueness of rows
  
  if( nrow(data) != nrow(unique(as.data.frame(x)[1:2]))  ){
    stop("The data must have rows with ",
         "unique combinations of idvar and timevar.\n",
         "Your data has ",
         nrow(data),
         " rows, but idvar and timevar only define ",
         nrow(unique(as.data.frame(x)[1:2])),
         " unique rows.")
  }
  
  X <- data.frame(x)
  names(X) <- varNames
  
  return(X)
}

testTimevar <- function(x, date.format){
  ## Check if timevar is either a numeric or date
  if( is.numeric(x) )
    return("number")  
  
  if(class(x)=="Date"& date.format %in% c("%YW%W","%YW%U"))
    return("string") 
  
  ##Quarters. Accept in ISO format as a character
  if(class(x)=="character" &  all(grepl("[0-9]{4}Q[1-4]" ,x)  == TRUE))
    return("string")
  
  ##Weeks. Accept in ISO format as a character
  if(class(x)=="character" &  all(grepl("[0-9]{4}W[0-4][0-9]|5[0-3]" , x)  == TRUE))
    return("string")
  
  if(class(x)=="Date")
    return("date")
  
  stop(paste("The timevar has to be of numeric or Date format. Currently it is ", class(x)))
  
}
