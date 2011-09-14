\name{gvisAnnotatedTimeLine}
\alias{gvisAnnotatedTimeLine}
\title{
  Google Annotated Time Line with R
}
\description{

  The gvisAnnotatedTimeLine function reads a data.frame and
  creates text output referring to the Google Visualisation API which can be
  included into a web page, or as a stand-alone page.
  
  An annotated time line is an interactive time series line chart with optional
  annotations. The chart is rendered within the browser using Flash. 
}
\usage{
gvisAnnotatedTimeLine(data,  datevar = "", numvar="", idvar = "",
                      titlevar="", annotationvar="",
                      date.format = "\%Y/\%m/\%d",
                      options = list(), chartid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
    a \code{data.frame}. The data has to have at least two
    columns, one with date information (\code{datevar})
    and one numerical variable. 
  }
  \item{datevar}{
    column name of \code{data} which shows the date dimension. The
    information has to be of class \code{\link{Date}} or \code{POSIX*}
    time series.
  }
  \item{numvar}{
    column name of \code{data} which shows the values to be displayed
    against \code{datevar}. The
    information has to be \code{\link{numeric}}.
  }
  \item{idvar}{
    column name of \code{data} which identifies different groups of the data. The
    information has to be of class \code{\link{character}} or
    \code{\link{factor}}.
  }
  \item{titlevar}{
    column name of \code{data} which shows the title of the annotations. The
    information has to be of class \code{\link{character}} or
    \code{\link{factor}}.  Missing information can be set to \code{NA}. See section
    'Details' for more details.
  }
  \item{annotationvar}{
    column name of \code{data} which shows the annotation text. The
    information has to be of class \code{\link{character}} or
    \code{\link{factor}}. Missing information can be set to \code{NA}. See section
    'Details' for more details.
  }
  \item{date.format}{
    if \code{datevar} is of class \code{\link{Date}} then this argument
    specifies how the dates are reformatted to be used by JavaScript. 
  }
  \item{options}{list of configuration options for Google Annotated Time Line.
    \describe{
      \item{\code{gvis.editor}}{a character label for an
	on-page button which opens an in-page dialog box that enables
	users to edit, change and customise the chart. By default no
	value is given and therefore no button is displayed. 
      }
      \item{\code{gvis.language}}{values may be 'ca', 'da', 'de', 'en', 'en_GB', 'en_IE', 'es', 
	'es_419', 'fi', 'fr', 'id', 'in', 'is', 'it', 'nl', 'no', 'pt',
	'pt_BR', 'pt_PT', 'sv'. If not set the API detects the language
	settings of the browser. 
      }       
    }
    Further possible components are, taken from
    \url{http://code.google.com/apis/visualization/documentation/gallery/annotatedtimeline.html#Configuration_Options}:
    \describe{
      \item{\code{height}}{height of the chart in pixels.
      }
      \item{\code{width}}{width of the chart in pixels.
      }
      \item{\code{allowHtml}}{boolean. Default \code{FALSE}. If set to
	\code{TRUE}, any annotation text that includes HTML tags will be
	rendered as HTML. 
      }
      \item{\code{allowRedraw}}{boolean. Default \code{FALSE}.	
	Enables a more efficient redrawing technique for the second and later
	calls to draw() on this visualization. It only redraws the chart
	area. To use this option, you must fulfill the following
	requirements: 
	\itemize{
	  \item{\code{allowRedraw} must be \code{TRUE}
	  }
	  \item{\code{displayAnnotations} must be \code{FALSE} (that is,
	    you cannot show annotations)
	  }
	  \item{you must pass in the same options and
	    values (except for the \code{allowRedraw} and
	    \code{displayAnnotations}) as in your first call to draw().	    
	  }
	}
      }
      \item{\code{allValuesSuffix}}{string. Default none. A suffix to be added
	to all values in the scales and the legend.
      }
      \item{\code{annotationsWidth}}{number. Default	25. The width (in
	percent) of the annotations area, out of the entire chart area. Must
	be a number in the range 5-80.
      }
      \item{\code{colors}}{Array of strings. Default colors. The colors to
	use for the chart lines and labels. An array of strings. Each element
	is a string in a valid HTML color format. For example 'red' or
	'#00cc00'.
      }
      \item{\code{dateFormat}}{string. Either 'MMMM dd, yyyy' or 'HH:mm
	MMMM dd, yyyy', depending on the type of the first column (date, or
	datetime, respectively).	The format used to display the date
	information in the top right corner. The format of this field is as
	specified by the java SimpleDateFormat class.
      }
      \item{\code{displayAnnotations}}{boolean. Default \code{FALSE}.	If set
	to \code{TRUE}, the chart will show annotations on top of selected
	values. When this option is set to \code{TRUE}, after every numeric column,
	two optional annotation string columns can be added, one for the
	annotation title and one for the annotation text.
      }
      \item{\code{displayAnnotationsFilter}}{boolean. Default \code{FALSE}. If
	set to \code{TRUE}, the chart will display a filter contol to filter
	annotations. Use this option when there are many annotations.
      }
      \item{\code{displayDateBarSeparator}}{boolean. Default
	\code{TRUE}. Whether to display a small bar separator ( | ) between
	the series values and the date in the legend, where \code{TRUE} means
	yes.
      }
      \item{\code{displayExactValues}}{boolean. Default \code{FALSE}. Whether
	to display a shortened, rounded version of the values on the top of
	the graph, to save space; false indicates that it may. For example, if
	set to false, 56123.45 might be displayed as 56.12k.
      }
      \item{\code{displayLegendDots}}{boolean. Default \code{TRUE}. Whether
	to display dots next to the values in the legend text, where
	\code{TRUE} means yes.
      }
      \item{\code{displayLegendValues}}{boolean. Default \code{TRUE}. Whether
	to display the highlighted values in the legend, where \code{TRUE}
	means yes.
      }
      \item{\code{displayRangeSelector}}{boolean. Default \code{TRUE}. Whether
	to show the zoom range selection area (the area at the bottom of the
	chart), where \code{FALSE} means no.
	
	The outline in the zoom selector is a log scale version of the
	last series in the chart, scaled to fit the height of the zoom
	selector.
      }
      \item{\code{displayZoomButtons}}{boolean. Default \code{TRUE}. Whether
	to show the zoom links ("1d 5d 1m" and so on), where \code{FALSE}
	means no.
      }
      \item{\code{fill}}{number. Default 0. A number from 0-100
	(inclusive) specifying the alpha of the fill below each line in
	the line graph. 100 means 100\% opaque fill, 0 means no fill at
	all. The fill color is the same color as the line above it.
      }
      \item{\code{highlightDot}}{string. Default 'nearest'.
	Which dot on the series to highlight, and corresponding values to
	show in the legend. Select from one of these values:
	\describe{
	  \item{'nearest'}{The values closest along the X axis to the
	    mouse.
	  }
	  \item{'last'}{The next set of values to the left of the mouse.
	  }
	}
      }
      \item{\code{legendPosition}}{string. Default 'sameRow'. Whether to
	put the colored legend on the same row with the zoom buttons and
	the date ('sameRow'), or on a new row ('newRow').
      }
      \item{\code{max}}{number. Default automatic. The maximum value to show
	on the Y axis. If the maximum data point exceeds this value, this
	setting will be ignored, and the chart will be adjusted to show
	the next major tick mark above the maximum data point. This will
	take precedence over the Y axis maximum determined by
	\code{scaleType}.
      }
      \item{\code{min}}{number. Default automatic. The minimum value to
	show on the Y axis. If the minimum data point is less than this
	value, this setting will be ignored, and the chart will be
	adjusted to show the next major tick mark below the minimum data
	point. This will take precedence over the Y axis minimum
	determined by \code{scaleType}.
      }
      \item{\code{numberFormats}}{String, or a map of number:String
	pairs. Default automatic. Specifies the number format patterns to
	be used to format the values at the top of the graph.
	
	The patterns should be in the format as specified by the java DecimalFormat class.
	\itemize{
	  \item{If not specified, the default format pattern is used.
	  }
	  \item{If a single string pattern is specified, it is used for
	    all of the values.
	  }
	  \item{If a map is specified, the keys are (zero-based) indexes
	    of series, and the values are the patterns to be used to format
	    the specified series.
	    
	    You are not required to include a format for every series on
	    the chart; any unspecified series will use the default format.
	  }
	  If this option is specified, the \code{displayExactValues}
	  option is ignored.
	}
      }
      \item{\code{scaleColumns}}{Array of numbers. Default automatic.	
	Specifies which values to show on the Y axis tick marks in the
	graph. The default is to have a single scale on the right
	side, which applies to both series; but you can have different
	sides of the graph scaled to different series values.
	
	This option takes an array of zero to three numbers specifying the
	(zero-based) index of the series to use as the scale value. Where
	these values are shown depends on how many values you include in
	your array:
	\itemize{      
	  \item{If you specify an empty array, the chart will not show Y
	    values next to the tick marks.
	  }
	  \item{If you specify one value, the scale of the indicated
	    series will be displayed on the right side of the chart only.
	  }
	  \item{If you specify two values, a the scale for the second
	    series will be added to the right of the chart.
	  }
	  \item{If you specify three values, a scale for the third
	    series will be added to the middle of the chart.
	  }
	  \item{Any values after the third in the array will be ignored.
	  }
	}
	When displaying more than one scale, it is advisable to set
	the \code{scaleType} option to either 'allfixed' or 'allmaximized'.
      }
      \item{\code{scaleType}}{string. Default 'fixed'. Sets the maximum
	and minimum values shown on the Y axis. The following options are
	available:
	\describe{	
	  \item{'maximized'}{The Y axis will span the minimum to the
	    maximum values of the series. If you have more than one series,
	    use 'allmaximized'.
	  }
	  \item{'fixed' [default]}{ The Y axis varies, depending on the
	    data values values:
	    \itemize{
	      \item{If all values are >=0, the Y axis will span from zero
		to the maximum data value.
	      }
	      \item{If all values are <=0, the Y axis will span from
		zero to the minimum data value.
	      }
	      \item{If values are both positive and negative, the Y axis will range
		from the series maximum to the series minimum.
		
		For multiple series, use 'allfixed'.
	      }
	    }
	  }	
	  \item{'allmaximized'}{Same as 'maximized,' but used when
	    multiple scales are displayed. Both charts will be maximized
	    within the same scale, which means that one will be
	    misrepresented against the Y axis, but hovering over each
	    series will display it's true value.
	  }
	  \item{'allfixed'}{Same as 'fixed,' but used when multiple
	    scales are displayed. This setting adjusts each scale to the
	    series to which it applies (use this in conjunction with
	    \code{scaleColumns}).
	  }
	  If you specify the min and/or max options, they will take
	  precedence over the minimum and maximum values determined by
	  your scale type.
	}
      }
      \item{\code{thickness}}{number. Default 0. A number from 0-10
	(inclusive) specifying the thickness of the lines, where 0
	is the thinnest.
      }
      \item{\code{wmode}}{string. Default 'window'. The Window Mode (wmode)
	parameter for the Flash chart. Available values are:
	'opaque', 'window' or 'transparent'.
      }
      \item{\code{zoomEndTime}}{Date. Default none. Sets the end date/time of the
	selected zoom range.
      }
      \item{\code{zoomStartTime}}{Date. Default none. Sets the start
	date/time of the selected zoom range.    
      }
    }
  }
  \item{chartid}{character. If missing (default) a random chart id will be generated based on
    chart type and \code{\link{tempfile}}
  }
}
\details{
From
\url{http://code.google.com/apis/visualization/documentation/gallery/annotatedtimeline.html#Data_Format}:

You can display one or more lines on your chart. Each row represents an
X position on the chart - that is, a specific time; each line is
described by a set of one to three columns.
\itemize{
  \item{The first column is of type date or datetime, and specifies the X value
    of the point on the chart. If this column is of type date (and not
    datetime) then the smallest time resolution on the X axis will be one
    day.
  }
  \item{Each data line is then described by a set of one to three additional
    columns as described here:
    \itemize{
      \item{ Y value - [Required, Number] The first column in each set describes the
	value of the line at the corresponding time from the first column. The
	column label is displayed on the chart as the title of that line.
      }
      \item{ Annotation title - [Optional, String] If a string column follows the
	value column, and the displayAnnotations option is true, this column
	holds a short title describing this point. For instance, if this line
	represents temperature in Brazil, and this point is a very high number,
	the title could be "Hottest day on record".
      }
      \item{Annotation text - [Optional string] If a second string column exists for
	this series, the cell value will be used as additional descriptive text
	for this point. You must set the option displayAnnotations to true to
	use this column. You can use HTML tags, if you set allowHtml to true;
	there is essentially no size limit, but note that excessively long
	entries might overflow the display section. You are not required to have
	this column even if you have an annotation title column for this
	point. The column label is not used by the chart. For example, if this
	were the hottest day on record point, you might say something like "Next
	closest day was 10 degrees cooler!".
	}
      }
    }
  }
}
\value{
  \code{gvisAnnotatedTimeLine} returns list of \code{\link{class}} "\code{gvis}"
  and "\code{list}". 
  
  An object of class "\code{gvis}" is a list containing at least the
  following components:
  \item{\code{type}}{Google visualisation type, here 'AnnotatedTimeLine'}
  \item{\code{chartid}}{character id of the chart object. Unique chart
    ids are required to place several charts on the same page.
  }
   \item{\code{html}}{a list with the building blocks for a page
    \describe{
      \item{\code{header}}{a character string of a html page header:
	\code{<html>...<body>},}
      \item{\code{chart}}{a named character vector of the chart's building blocks:
      	\describe{
	  \item{\code{jsHeader}}{Opening \code{<script>} tag and
	    reference to Google's JavaScript library.
	  }
	  \item{\code{jsData}}{JavaScript function defining the input
	    \code{data} as a JSON object.
	  } 
	  \item{\code{jsDrawChart}}{JavaScript function
	    combing the data with the visualisation API and user
	    options.
	  }
	  \item{\code{jsDisplayChart}}{JavaScript function calling the
	    handler to display the chart.
	  }
	  \item{\code{jsChart}}{Call of the \code{jsDisplayChart} 
	    function.
	  }
	  \item{\code{jsFooter}}{End tag \code{</script>}.
	  }
	  \item{\code{divChart}}{\code{<div>} container to embed the chart
	    into the page.
	  }
	}   
      }
      \item{\code{caption}}{character string of a standard caption,
	including data name and chart id.
      }	
      \item{\code{footer}}{character string of a html page footer:
	\code{</body>...</html>}, including the used R and googleVis version
	and link to Google's Terms of Use.}
    }
  }
}


\references{
  Google Annotated Time Line API:
  \url{http://code.google.com/apis/visualization/documentation/gallery/annotatedtimeline.html}

  Follow the link for Google's data policy.  
}
\author{
  Markus Gesmann \email{markus.gesmann@gmail.com},
  
  Diego de Castillo \email{decastillo@gmail.com}
}

\section{Warnings}{

  Because of Flash security settings the chart might not work correctly when accessed from a file
  location in the browser (e.g., file:///c:/webhost/myhost/myviz.html)
  rather than from a web server URL
  (e.g. http://www.myhost.com/myviz.html). See the Macromedia web 
  site (\url{http://www.macromedia.com/support/documentation/en/flashplayer/help/})
  for more details.

}


\seealso{
  
  See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for
  printing and plotting methods. Further see \code{\link{reshape}} for
  reshaping data, e.g. from a wide format into a long format.
  
}
\examples{
## Please note that by default the googleVis plot command
## will open a browser window and requires Flash and Internet
## connection to display the visualisation.


data(Stock)
Stock
A1 <- gvisAnnotatedTimeLine(Stock, datevar="Date",
                           numvar="Value", idvar="Device",
                           titlevar="Title", annotationvar="Annotation",
                           options=list(displayAnnotations=TRUE,
                            legendPosition='newRow',
                            width=600, height=350)
                           )
plot(A1)

## Two Y-axis
A2 <- gvisAnnotatedTimeLine(Stock, datevar="Date",
                           numvar="Value", idvar="Device",
                           titlevar="Title", annotationvar="Annotation",
                           options=list(displayAnnotations=TRUE, 
                            width=600, height=350, scaleColumns='[0,1]',
                            scaleType='allmaximized')
                          )
plot(A2)

## Zoom into the time window, no Y-axis ticks
A3 <- gvisAnnotatedTimeLine(Stock, datevar="Date",
                           numvar="Value", idvar="Device",
                           titlevar="Title", annotationvar="Annotation",
                           options=list(
                             width=600, height=350,
                            zoomStartTime=as.Date("2008-01-04"),
                            zoomEndTime=as.Date("2008-01-05")) 
                          )
plot(A3)



## Colouring the area below the lines to create an area chart
A4 <- gvisAnnotatedTimeLine(Stock, datevar="Date",
                           numvar="Value", idvar="Device",
                           titlevar="Title", annotationvar="Annotation",
                           options=list(
                             width=600, height=350,
                             fill=10, displayExactValues=TRUE,
                             colors="['#0000ff','#00ff00']")
                           )
                          
plot(A4)


## Data with POSIXct datetime variable
A5 <- gvisAnnotatedTimeLine(Andrew, datevar="Date/Time UTC",
                            numvar="Pressure_mb",
                            options=list(scaleType='maximized')
                            )

plot(A5)


\dontrun{

## Plot Apple's monthly stock prices since 1984

## Get current date
d <- Sys.time() 
current.year <- format(d, "\%Y")
current.month <- format(d, "\%m")
current.day <- format(d, "\%d")

## Yahoo finance sets January to 00 hence: 
month <- as.numeric(current.month)  - 1
month <- ifelse(month < 10, paste("0",month, sep=""), m)

## Get weekly stock prices from Apple Inc.
tckr <- 'AAPL'
fn <- sprintf('http://ichart.finance.yahoo.com/table.csv?s=\%s&a=08&b=7&c=1984&d=\%s&e=\%s&f=\%s&g=w&ignore=.csv',
      tckr, month, current.day, current.year)

## Get data from Yahoo! Finance
data <- read.csv(fn, colClasses=c("Date", rep("numeric",6)))

AAPL <- reshape(data[,c("Date", "Close", "Volume")], idvar="Date", 
     		times=c("Close", "Volume"), 
                timevar="Type",
                varying=list(c("Close", "Volume")),
                v.names="Value", "Type",
                direction="long")

## Calculate last year for zoom start time
lyd <- as.POSIXlt(as.Date(d))
lyd$year <- lyd$year-1
lyd <- as.Date(lyd)

aapl <- gvisAnnotatedTimeLine(AAPL, datevar="Date",
                           numvar="Value", idvar="Type",
                          options=list(
                            zoomStartTime=lyd,
                            zoomEndTime=as.Date(d),
                            legendPosition='newRow',
                            width=600, height=400, scaleColumns='[0,1]',
                            scaleType='allmaximized')
                           )

plot(aapl)

}
}
\keyword{ iplot }

%%## Example with ChainLadder triangle
%% library(ChainLadder)
%% lRAA=as.data.frame(RAA, na.rm=TRUE)
%% lRAA$dev.year.end <- as.Date(with(lRAA, paste(dev,"-12-31", sep="")))
%% lRAA$title <- NA
%% lRAA$annotation <- NA
%% 
%% lRAA$title[c(2,47)] <- c("Low first claim", "Negative development")
%% lRAA$annotation[c(2,47)] <- c("Change in underlying claims process resulted in delayed reporting", 
%%    "Joe Bloggs was on holiday and the person who covered for him
%% mixed up the signs.")	
%%  A <- gvisAnnotatedTimeLine(lRAA, datevar="dev.year.end", numvar="value", idvar="origin",
%%                                   titlevar="title", annotationvar="annotation", 
%%                                   options=list(legendPosition='newRow',
%%				   width=800, height=400,
%%				   displayAnnotations=TRUE))
%% plot(A) 							   


