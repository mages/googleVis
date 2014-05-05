### File R/gvisCoreCharts.R
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


#' Google Line Chart with R
#' \Sexpr{googleChartName <- "linechart"}
#' \Sexpr{gvisChartName <- "gvisLineChart"}
#' 
#' The gvisLineChart function reads a data.frame and creates text output
#' referring to the Google Visualisation API, which can be included into a web
#' page, or as a stand-alone page. The actual chart is rendered by the web
#' browser using SVG or VML.
#' 
#' 
#' @param data a \code{\link{data.frame}} to be displayed as a line chart
#' @param xvar name of the character column which contains the category labels
#' for the x-axes.
#' @param yvar a vector of column names of the numerical variables to be
#' plotted. Each column is displayed as a separate line.
#' @param options list of configuration options, see
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
#' @keywords iplot
#' @examples
#' 
#' ## Please note that by default the googleVis plot command
#' ## will open a browser window and requires an internet
#' ## connection to display the visualisation.
#' 
#' df <- data.frame(country=c("US", "GB", "BR"), val1=c(1,3,4), val2=c(23,12,32))
#' 
#' ## Line chart
#' Line1 <- gvisLineChart(df, xvar="country", yvar=c("val1", "val2"))
#' plot(Line1)
#' 
#' 
#' ## Add a customised title and smoothed curve
#' Line2 <- gvisLineChart(df, xvar="country", yvar=c("val1", "val2"),
#'              options=list(title="Hello World",
#'                           titleTextStyle="{color:'red',fontName:'Courier',fontSize:16}",
#'                           curveType='function'))
#' plot(Line2)
#' 
#' \dontrun{
#' ## Change y-axis to percentages
#' Line3 <- gvisLineChart(df, xvar="country", yvar=c("val1", "val2"),
#'                        options=list(vAxis="{format:'#,###%'}"))
#' plot(Line3)
#' 
#' }
#' 
#' ## Create a chart with two y-axis:
#' Line4 <-  gvisLineChart(df, "country", c("val1","val2"),
#'                         options=list(series="[{targetAxisIndex: 0},
#'                                               {targetAxisIndex:1}]",
#'                           vAxes="[{title:'val1'}, {title:'val2'}]"
#'                           ))
#' plot(Line4)
#' 
#' ## Line chart with edit button
#' Line5 <- gvisLineChart(df, xvar="country", yvar=c("val1", "val2"),
#'                        options=list(gvis.editor="Edit me!"))
#' plot(Line5)
#' 
#' ## Customizing lines 
#' Dashed <-  gvisLineChart(df, xvar="country", yvar=c("val1","val2"),
#'              options=list(
#'              series="[{color:'green', targetAxisIndex: 0, 
#'                        lineWidth: 1, lineDashStyle: [2, 2, 20, 2, 20, 2]}, 
#'                       {color: 'blue',targetAxisIndex: 1, 
#'                        lineWidth: 2, lineDashStyle: [4, 1]}]",
#'                        vAxes="[{title:'val1'}, {title:'val2'}]"
#'                        ))
#' plot(Dashed)
#' 
gvisLineChart <- function(data, xvar="", yvar="", options=list(), 
                          chartid){##, editor

  ##  if(!missing(editor)){
  ##   options=list(options, gvis.editor=editor) 
  ## }
  gvisCoreChart(data, xvar, yvar, options, chartid, 
                chart.type="LineChart")
}



#' Google Area Chart with R
#' \Sexpr{googleChartName <- "areachart"}
#' \Sexpr{gvisChartName <- "gvisAreaChart"}
#' 
#' The gvisAreaChart function reads a data.frame and creates text output
#' referring to the Google Visualisation API, which can be included into a web
#' page, or as a stand-alone page.
#' 
#' The area chart is rendered within the browser using SVG or VML and displays
#' tips when hovering over points.
#' 
#' 
#' @param data a \code{\link{data.frame}} to be displayed as an area chart
#' @param xvar name of the character column which contains the category labels
#' for the x-axes.
#' @param yvar a vector of column names of the numerical variables to be
#' plotted. Each column is displayed as a separate line.
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
#' @keywords iplot
#' @examples
#' 
#' ## Please note that by default the googleVis plot command
#' ## will open a browser window and requires an internet
#' ## connection to display the visualisation.
#' 
#' df=data.frame(country=c("US", "GB", "BR"), val1=c(1,3,4), val2=c(23,12,32))
#' 
#' ## Area chart
#' Area1 <- gvisAreaChart(df, xvar="country", yvar=c("val1", "val2"))
#' plot(Area1)
#' 
#' ## Stacked chart
#' Area2 <- gvisAreaChart(df, xvar="country", yvar=c("val1", "val2"),
#'       options=list(isStacked=TRUE))
#' plot(Area2)
#' 
#' 
#' ## Add a customised title
#' Area3 <- gvisAreaChart(df, xvar="country", yvar=c("val1", "val2"),
#'              options=list(title="Hello World",
#'                           titleTextStyle="{color:'red',fontName:'Courier',fontSize:16}"))
#' plot(Area3)
#' 
#' \dontrun{
#' ## Change y-axis to percentages
#' Area3 <- gvisAreaChart(df, xvar="country", yvar=c("val1", "val2"),
#'                        options=list(vAxis="{format:'#,###%'}"))
#' plot(Area3)
#' }
#' 
gvisAreaChart <- function(data, xvar="", yvar="", options=list(), 
                          chartid){
  
  gvisCoreChart(data, xvar, yvar, options, chartid, 
                chart.type="AreaChart")
}


#' Google Stepped Area Chart with R
#' \Sexpr{googleChartName <- "steppedarechart"}
#' \Sexpr{gvisChartName <- "gvisSteppedAreChart"}
#' 
#' The gvisSteppedAreaChart function reads a data.frame and creates text output
#' referring to the Google Visualisation API, which can be included into a web
#' page, or as a stand-alone page.
#' 
#' The stepped area chart is rendered within the browser using SVG or VML and
#' displays tips when hovering over points.
#' 
#' 
#' @param data a \code{\link{data.frame}} to be displayed as a stepped area
#' chart.
#' @param xvar name of the character column which contains the category labels
#' for the x-axes.
#' @param yvar a vector of column names of the numerical variables to be
#' plotted.  Each column is displayed as a separate line.
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
#' @keywords iplot
#' @examples
#' 
#' ## Please note that by default the googleVis plot command
#' ## will open a browser window and requires an internet
#' ## connection to display the visualisation.
#' 
#' df=data.frame(country=c("US", "GB", "BR"), val1=c(1,3,4), val2=c(23,12,32))
#' 
#' ## Stepped Area chart
#' SteppedArea1 <- gvisSteppedAreaChart(df, xvar="country", yvar=c("val1", "val2"))
#' plot(SteppedArea1)
#' 
#' ## Stacked chart
#' SteppedArea2 <- gvisSteppedAreaChart(df, xvar="country", yvar=c("val1", "val2"),
#'       options=list(isStacked=TRUE))
#' plot(SteppedArea2)
#' 
#' 
#' ## Add a customised title
#' SteppedArea3 <- gvisSteppedAreaChart(df, xvar="country", yvar=c("val1", "val2"),
#'              options=list(title="Hello World",
#'                           titleTextStyle="{color:'red',fontName:'Courier',fontSize:16}"))
#' plot(SteppedArea3)
#' 
#' \dontrun{
#' ## Change y-axis to percentages
#' SteppedArea3 <- gvisSteppedAreaChart(df, xvar="country", yvar=c("val1", "val2"),
#'                        options=list(vAxis="{format:'#,###%'}"))
#' plot(SteppedArea3)
#' }
#' 
gvisSteppedAreaChart <- function(data, xvar="", yvar="", 
                                 options=list(), chartid){
  
  gvisCoreChart(data, xvar, yvar, options, chartid, 
                chart.type="SteppedAreaChart")
}



#' Google Bar Chart with R
#' \Sexpr{googleChartName <- "barchart"}
#' \Sexpr{gvisChartName <- "gvisBarChart"}
#' 
#' The gvisBarChart function reads a data.frame and creates text output
#' referring to the Google Visualisation API, which can be included into a web
#' page, or as a stand-alone page. The actual chart is rendered by the web
#' browser using SVG or VML.
#' 
#' 
#' @param data a \code{\link{data.frame}} to be displayed as a bar chart
#' @param xvar name of the character column which contains the category labels
#' for the x-axes.
#' @param yvar a vector of column names of the numerical variables to be
#' plotted.  Each column is displayed as a separate bar/column.
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
#' @keywords iplot
#' @examples
#' 
#' ## Please note that by default the googleVis plot command
#' ## will open a browser window and requires an internet
#' ## connection to display the visualisation.
#' 
#' df <- data.frame(country=c("US", "GB", "BR"), 
#'                            val1=c(1,3,4), 
#'                            val2=c(23,12,32))
#' 
#' ## Bar chart
#' Bar1 <- gvisBarChart(df, xvar="country", yvar=c("val1", "val2"))
#' plot(Bar1)
#' 
#' ## Stacked bar chart
#' Bar2 <- gvisBarChart(df, xvar="country", yvar=c("val1", "val2"),
#'      options=list(isStacked=TRUE))
#' plot(Bar2)
#' 
#' 
#' ## Add a customised title and change width of bars
#' Bar3 <- gvisBarChart(df, xvar="country", yvar=c("val1", "val2"),
#'              options=list(title="Hello World",
#'                           titleTextStyle="{color:'red',fontName:'Courier',fontSize:16}",
#'                           bar="{groupWidth:'100%'}"))
#' plot(Bar3)
#' 
#' \dontrun{
#' ## Change x-axis to percentages
#' Bar4 <- gvisBarChart(df, xvar="country", yvar=c("val1", "val2"),
#'                        options=list(hAxis="{format:'#,###%'}"))
#' plot(Bar4)
#' 
#' ## The following example reads data from a Wikipedia table and displays
#' ## the information in a bar chart.
#' ## We use the readHMLTable function of the XML package to get the data
#' library(XML)
#' ## Get the data of the biggest ISO container companies from Wikipedia
#' ##(table 3):
#' df=readHTMLTable(readLines("http://en.wikipedia.org/wiki/Intermodal_freight_transport"))[[3]][,1:2]
#' ## Rename the second column
#' names(df)[2]="TEU capacity"
#' ## The numbers are displayed with commas to separate thousands, so let's
#' ## get rid of them:
#' df[,2]=as.numeric(gsub(",", "", as.character(df[,2])))
#' 
#' ## Finally we can create a nice bar chart:
#' Bar5 <- gvisBarChart(df, options=list(
#'                     chartArea="{left:250,top:50,width:\"50%\",height:\"75%\"}",
#'                     legend="bottom", 
#'                     title="Top 20 container shipping companies in order of TEU capacity"))
#' 
#' plot(Bar5)
#' 
#' }
#' 
gvisBarChart <- function(data, xvar="", yvar="", options=list(), 
                         chartid){
  
  gvisCoreChart(data, xvar, yvar, options, chartid, 
                chart.type="BarChart")
}



#' Google Column Chart with R
#' \Sexpr{googleChartName <- "columnchart"}
#' \Sexpr{gvisChartName <- "gvisColumnChart"}
#' 
#' The gvisColumnChart function reads a data.frame and creates text output
#' referring to the Google Visualisation API, which can be included into a web
#' page, or as a stand-alone page. The actual chart is rendered by the web
#' browser using SVG or VML.
#' 
#' 
#' @param data a \code{\link{data.frame}} to be displayed as a column chart
#' @param xvar name of the character column which contains the category labels
#' for the x-axes.
#' @param yvar a vector of column names of the numerical variables to be
#' plotted.  Each column is displayed as a separate bar/column.
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
#' @keywords iplot
#' @examples
#' 
#' ## Please note that by default the googleVis plot command
#' ## will open a browser window and requires an internet
#' ## connection to display the visualisation.
#' 
#' df=data.frame(country=c("US", "GB", "BR"), val1=c(1,3,4), val2=c(23,12,32))
#' 
#' ## Column chart
#' Col1 <- gvisColumnChart(df, xvar="country", yvar=c("val1", "val2"))
#' plot(Col1)
#' 
#' ## Stacked column chart
#' Col2 <- gvisColumnChart(df, xvar="country", yvar=c("val1", "val2"),
#'      options=list(isStacked=TRUE))
#' plot(Col2)
#' 
#' 
#' ## Add a customised title and and change width of columns
#' Col3 <- gvisColumnChart(df, xvar="country", yvar=c("val1", "val2"),
#'              options=list(title="Hello World",
#'                           titleTextStyle="{color:'red',fontName:'Courier',fontSize:16}",
#'                           bar="{groupWidth:'100%'}"))
#' plot(Col3)
#' 
#' \dontrun{
#' ## Change y-axis to percentages
#' Col4 <- gvisColumnChart(df, xvar="country", yvar=c("val1", "val2"),
#'                        options=list(vAxis="{format:'#,###%'}"))
#' plot(Col4)
#' }
#' 
#' 
gvisColumnChart <- function(data, xvar="", yvar="", options=list(), 
                            chartid){
  
  gvisCoreChart(data, xvar, yvar, options, chartid, 
                chart.type="ColumnChart")
}


#' Google Combo Chart with R
#' \Sexpr{googleChartName <- "combochart"}
#' \Sexpr{gvisChartName <- "gvisComboChart"}
#' 
#' A chart that lets you render each series as a different marker type from the
#' following list: columns, lines, and area lines.
#' 
#' The gvisComboChart function reads a data.frame and creates text output
#' referring to the Google Visualisation API, which can be included into a web
#' page, or as a stand-alone page. The actual chart is rendered by the web
#' browser using SVG or VML.
#' 
#' 
#' @param data a \code{\link{data.frame}} to be displayed as a columns, line
#' and area chart.
#' @param xvar name of the character column which contains the category labels
#' for the x-axes.
#' @param yvar a vector of column names of the numerical variables to be
#' plotted.  Each column is displayed as a separate column, line or area
#' series.
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
#' @keywords iplot
#' @examples
#' 
#' ## Please note that by default the googleVis plot command
#' ## will open a browser window and requires an internet
#' ## connection to display the visualisation.
#' 
#' CityPopularity
#' ## Add the mean
#' CityPopularity$Mean=mean(CityPopularity$Popularity)
#' 
#' C1 <- gvisComboChart(CityPopularity, xvar="City",
#'                                      yvar=c("Mean", "Popularity"),
#'                                    options=list(seriesType="bars",
#'                                                 title="City Popularity",
#'                                                 series='{0: {type:"line"}}'))
#' plot(C1)
#' 
#' ## Changing the width of columsn
#' C2 <- gvisComboChart(CityPopularity, xvar="City",
#'                                      yvar=c("Mean", "Popularity"),
#'                                    options=list(seriesType="bars",
#'                                                 bar="{groupWidth:'100%'}",
#'                                                 title="City Popularity",
#'                                                 series='{0: {type:"line"}}'))
#' plot(C2)
#' 
#' 
gvisComboChart <- function(data, xvar="", yvar="", options=list(), 
                           chartid){

  gvisCoreChart(data, xvar, yvar, options, chartid, 
                chart.type="ComboChart")
}




#' Google Candlestick chart with R
#' \Sexpr{googleChartName <- "candlestickchart"}
#' \Sexpr{gvisChartName <- "gvisCandlestickChart"}
#' 
#' An interactive candlestick chart.
#' 
#' The gvisCandlestickChart function reads a data.frame and creates text output
#' referring to the Google Visualisation API, which can be included into a web
#' page, or as a stand-alone page. The actual chart is rendered by the web
#' browser using SVG or VML.
#' 
#' A candlestick chart is used to show an opening and closing value overlaid on
#' top of a total variance. Candlestick charts are often used to show stock
#' value behavior. In this chart, items where the opening value is less than
#' the closing value (a gain) are drawn as filled boxes, and items where the
#' opening value is more than the closing value (a loss) are drawn as hollow
#' boxes.
#' 
#' @param data a \code{\link{data.frame}} to be displayed as a candlestick
#' chart. The data has to have at least 5 columns.
#' @param xvar name of the character column which contains the category labels
#' for the x-axes.
#' @param low name of the numeric column specifying the low/minimum value of
#' this marker. This is the base of the candle's center line.
#' @param open name of the numeric column specifying the opening/initial value
#' of this marker. This is one vertical border of the candle. If less than the
#' \code{close} value, the candle will be filled; otherwise it will be hollow.
#' @param close name of the numeric column specifying the closing/final value
#' of this marker. This is the second vertical border of the candle. If less
#' than the \code{open} value, the candle will be hollow; otherwise it will be
#' filled.
#' @param high name of the numeric column specifying the high/maximum value of
#' this marker. This is the top of the candle's center line.
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
#' @keywords iplot
#' @examples
#' 
#' ## Please note that by default the googleVis plot command
#' ## will open a browser window and requires an internet
#' ## connection to display the visualisation.
#' 
#' ## Example data set
#' OpenClose
#' 
#' C1 <- gvisCandlestickChart(OpenClose, xvar="Weekday", low="Low",
#'                                       open="Open", close="Close",
#'                                       high="High",
#'                                       options=list(legend='none'))
#' 
#' plot(C1)
#' 
#' 
gvisCandlestickChart<- function(data, xvar="", low="", open="", close="", 
                                high="", options=list(), chartid){
  
  data <- gvisCheckCandestickChartData(data)
  
  gvisCoreChart(data, xvar, yvar=c(low, open, close, high), 
                options, chartid, chart.type="CandlestickChart")
}



#' Google Scatter Chart with R
#' \Sexpr{googleChartName <- "scatterchart"}
#' \Sexpr{gvisChartName <- "gvisScatterChart"}
#' 
#' The gvisScatterChart function reads a data.frame and creates text output
#' referring to the Google Visualisation API, which can be included into a web
#' page, or as a stand-alone page. The actual chart is rendered by the web
#' browser using SVG or VML.
#' 
#' 
#' @param data a \code{\link{data.frame}} to be displayed as a scatter chart.
#' Two or more columns are required, all must be numeric. The values in the
#' first column are used for the X-axis. The values in following columns are
#' used for the Y-axis. Each column is displayed with a separate color.
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
#' @keywords iplot
#' @examples
#' 
#' ## Please note that by default the googleVis plot command
#' ## will open a browser window and requires an internet
#' ## connection to display the visualisation.
#' 
#' 
#' ## Scatter chart
#' Scatter1 <- gvisScatterChart(women)
#' plot(Scatter1)
#' 
#' ## Using optional arguments
#' Scatter2 <- gvisScatterChart(women, options=list(legend="none",
#'                  lineWidth=2, pointSize=2,
#'                  title="Women", vAxis="{title:'weight (lbs)'}",
#'                  crosshair="{ trigger: 'both' }", 
#'                  hAxis="{title:'height (in)'}", width=500, height=400))
#'                  
#' plot(Scatter2)
#' 
#' 
#' df=data.frame(x=sin(1:100/3), 
#'               Circle=cos(1:100/3), 
#'  	      Ellipse=cos(1:100/3)*0.5)
#' 
#' ## Plot several variables as smooth curves
#' Scatter3 <- gvisScatterChart(df, 
#' 	    		options=list(curveType='function', 
#' 				     pointSize=0, 
#' 				     lineWidth=2))
#' plot(Scatter3)
#' 
#' ## Two series in the same plot with different
#' ## x-values
#' df <- data.frame(x=c(2,2,1,3,4),
#'                  y1=c(0,3,NA,NA,NA),
#'                  y2=c(NA,NA,0,3,2))
#' Scatter4 <- gvisScatterChart(df,
#'                              options=list(lineWidth=2,
#'                                           pointSize=2))
#' plot(Scatter4)
#' 
#' ## Customize points
#' M <- matrix(nrow=6,ncol=6)
#' M[col(M)==row(M)] <- 1:6
#' dat <- data.frame(X=1:6, M)
#' SC <- gvisScatterChart(dat, 
#'                         options=list(
#'                         title="Customizing points",
#'                         legend="right",
#'                         pointSize=30,
#'                         series="{
#'                              0: { pointShape: 'circle' },
#'                              1: { pointShape: 'triangle' },
#'                              2: { pointShape: 'square' },
#'                              3: { pointShape: 'diamond' },
#'                              4: { pointShape: 'star' },
#'                              5: { pointShape: 'polygon' }
#'                              }"))
#' plot(SC)
#'
gvisScatterChart <- function(data, options=list(), chartid){
  
  my.type <- "ScatterChart"
  dataName <- deparse(substitute(data))
  
  my.options <- list(gvis=modifyList(list(allowHtml=TRUE),options), 
                     dataName=dataName,
                     data=list(allowed=c("number")))
  
  checked.data <- gvisCheckScatterChartData(data)
  
  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options, chartid=chartid, package="corechart") 
  
  return(output)
}


gvisCheckCandestickChartData <- function(data){
  if(ncol(data) < 5)
    stop(paste("The input data requires 5 columns, for xvar, low, open, close, high.\n",
               "However, your data set has only:", ncol(data)))          
  return(data)
}

gvisCheckScatterChartData <- function(data){
  
  ## nothing to check at the moment here
  return(data)
}



gvisCoreChart <- function(data, xvar="", yvar="", options=list(), chartid, chart.type){
  
  if(!is.data.frame(data)){
    stop("Error: data has to be a data.frame.")
  }
  
  dataName <- deparse(substitute(data))
  
  my.options <- list(gvis=modifyList(list(allowHtml=TRUE),options), dataName=dataName,
                     data=list(xvar=xvar, yvar=yvar,
                       allowed=c("string", "number", "date", 
                                 "datetime"#, "boolean"
                                 ))
                     )
  
  
  checked.data <- gvisCheckCoreChartData(data, xvar=xvar, yvar=yvar)

  
  output <- gvisChart(type=chart.type, checked.data=checked.data, options=my.options, chartid=chartid, package="corechart") 
  
  return(output)
}


gvisCheckCoreChartData <- function(data, xvar, yvar){
  
  if(!is.data.frame(data)){
    stop("Error: data has to be a data.frame.")
  }

  
  if(xvar=="")
    xvar <- names(data)[1]

  if("integer" %in% class(data[,xvar]))
    data[,xvar] <- as.character(data[,xvar])
  
  if("" %in% yvar){
    yvar <- sapply(data, is.numeric)
    yvar <- names(yvar[yvar])
  }
  ord <- names(data) %in% c(xvar, yvar)
  #   if( ncol(data) > ncol(data[, ord]))
  #     data <-  cbind(data[, ord], data[,!ord])
  #   else
  #     
  nm <- names(data)[ord]
  data <- data[,ord]
  names(data) <- nm
  if(!any(sapply(data, is.numeric))){
    stop("Error: Your data has to have at least one numerical column.")
  }

  return(data)
}

