\name{gvisComboChart}
\alias{gvisComboChart}
\title{
  Google Combo Chart with R
}
\description{
  A chart that lets you render each series as a different marker type
  from the following list: columns, lines, and area lines.

  The gvisComboChart function reads a data.frame and
  creates text output referring to the Google Visualisation API which can be
  included into a web page, or as a stand-alone page. The actual chart is
  rendered by the web browser using SVG or
  VML. 

}
\usage{
gvisComboChart(data, xvar = "", yvar = "", options = list(), chartid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a \code{\link{data.frame}} to be displayed as a columns,
    line and area chart.
  }
  \item{xvar}{
    name of the character column which contains the category labels for
    the x-axes.
  }
  \item{yvar}{a vector of column names of the numerical variables to be
    plotted. 
    Each column is displayed as a separate column, line or area series.
  }
  \item{options}{list of configuration options for Google Combo Chart.
        \describe{
\item{\code{gvis.editor}}{a character label for an
 on-page button which opens an in-page dialog box that enables
users to edit, change and customise the chart. By default no
value is given and therefore no button is displayed. 
      }
    }
    Further possible components are, taken from
    \url{http://code.google.com/apis/visualization/documentation/gallery/combochart.html#Configuration_Options}:
    \describe{
      \item{\code{areaOpacity}}{a number. Default 0.0 - 1.0. The default
	opacity of the colored area under an area chart series, where 0.0
	is fully transparent and 1.0 is fully opaque. To specify opacity
	for an individual series, set the areaOpacity value in the series
	property.
      }
 
      \item{\code{axisTitlesPosition}}{a string. Default 'out'. Where to place
	the axis titles, compared to the chart area. Supported values:
	\describe{      
	  \item{\code{'in'}}{Draw the axis titles inside the the chart area.}
	  \item{\code{'out'}}{Draw the axis titles outside the chart area.}
	  \item{\code{'none'}}{Omit the axis titles.}
	}
      }
      \item{\code{backgroundColor}}{a string or object. Default \code{'white'}. The
	background color for the main area of the chart. Can be either a
	simple HTML color string, for example: \code{'red'} or
	\code{'#00cc00'}, or an
	object with the following properties.
      }
      \item{\code{backgroundColor.stroke}}{a string. Default \code{'#666'}. The color of
	the chart border, as an HTML color string.
      }
      \item{\code{backgroundColor.strokeWidth}}{a number. Default 0. The border
	width, in pixels.
      }
      \item{\code{backgroundColor.fill}}{a string. Default \code{'white'}. The chart
	fill color, as an HTML color string.
      }
      \item{\code{chartArea}}{a string. Default \code{'null'}. An object
	with members to configure the 
	placement and size of the chart area (where the chart itself is drawn,
	excluding axis and legends). Two formats are supported: a number, or a
	number followed by \code{\%}. A simple number is a value in pixels; a number
	followed by \code{\%} is a percentage. Example:
	
	\code{\{left:20,top:0,width:\"50\%\",height:\"75\%\"\}}
      }
      \item{\code{chartArea.left}}{a number or string. Default auto. How far to draw
	the chart from the left border.
      }
      \item{\code{chartArea.top}}{a number or string. Default auto. How far to
	draw the chart from the top border.
      }
      \item{\code{chartArea.width}}{a number or string. Default auto.	Chart
	area width.
      }
      \item{\code{chartArea.height}}{a number or string. Default auto.	Chart
	area height.
      }
      \item{\code{colors}}{a JSON array of strings. Default 'colors'. The colors to
	use for the chart elements. An array of strings, where each element is
	an HTML color string, for example: \code{colors:['red','#004411']}.
      }
      \item{\code{curveType}}{[Line and area series only] a
	string. Default \code{'none'}. Controls the curve of the 
	lines. Can be one of the following:      
	\describe{
	  \item{\code{'none'}}{Straight lines without curve.}
	  \item{\code{'function'}}{The angles of the line will be smoothed.}
	}
      }
      \item{\code{enableInteractivity}}{boolean. Default \code{TRUE}. Whether
	the chart throws user-based events or reacts to user interaction. If
	false, the chart will not throw 'select' or other interaction-based
	events (but will throw ready or error events), and will not display
	hovertext or otherwise change depending on user input.
      }
     
      \item{\code{fontSize}}{a number. Default automatic. The default font
	size, in pixels, of all text in the chart. You can override this
	using properties for specific chart elements.
      }
      \item{\code{fontName}}{a string. Default \code{'Arial'}. The default font face for
	all text in the chart. You can override this using properties for
	specific chart elements.
      }
      \item{\code{gridlineColor}}{a string. Default \code{'#CCC'}. The color of the
	gridlines inside the chart area. Specify a valid HTML color
	string.
      }
      \item{\code{hAxis}}{a JSON object. Default \code{'null'}. An object with members to configure
	various horizontal axis elements. To specify properties of this
	object, you can use object literal notation, as shown here:
	%\describe{
	%\item{
	\code{\{title: 'Hello', titleTextStyle: \{color: '#FF0000'\}\}}
	%}
	%}
      }
      \item{\code{hAxis.direction}}{1 or -1. Default 1. The direction in which the
	values along the horizontal axis grow. Specify -1 to reverse the
	order of the values.
      }
      \item{\code{hAxis.direction}}{1 or -1. Default 1. The direction in which the
	values along the vertical axis grow. Specify -1 to reverse the
	order of the values.
      }
      \item{\code{hAxis.textPosition}}{a string. Default \code{'out'}. Position of the
	vertical axis text, relative to the chart area. Supported values:
	\code{'out', 'in', 'none'}.
      }
      \item{\code{hAxis.textStyle}}{a JSON object. Default
	\code{\{color: 'black',fontName: <global-font-name>, fontSize: <global-font-size>\}}. An
	object that specifies the vertical axis text style. The object has
	this format:
	\code{\{color: <string>, fontName: <string>, fontSize: <number>\}}
	The color can be any HTML color string, for example: \code{'red'} or
	\code{'#00cc00'}. Also see \code{fontName} and \code{fontSize}.
      }
      \item{\code{hAxis.title}}{a string. Default no title. \code{hAxis} property that
	specifies a title for the vertical axis.
      }
      \item{\code{hAxis.titleTextStyle}}{a JSON object. Default \code{\{color: 'black',
	fontName: <global-font-name>, fontSize: <global-font-size>\}}. An
	object that specifies the vertical axis title text style. The
	object has this format:
	\code{\{color: <string>, fontName: <string>, fontSize: <number>\}}
	The color can be any HTML color string, for example: \code{'red'} or
	\code{'#00cc00'}. Also see \code{fontName} and \code{fontSize}.
      }
      \item{\code{hAxis.slantedText}}{boolean. Default automatic. If \code{TRUE}, draw the
	horizontal axis text at an angle, to help fit more text along the
	axis; if false, draw horizontal axis text upright. Default behavior is
	to slant text if it cannot all fit when drawn upright.
      }
      \item{\code{hAxis.slantedTextAngle}}{a number, 1-90. Default 30. The angle
	of the horizontal axis text, if it's drawn slanted. Ignored if
	\code{hAxis.slantedText} is false, or is in auto mode, and the chart
	decided to draw the text horizontally.
      }
      \item{\code{hAxis.maxAlternation}}{a number. Default 2. Maximum number of
	levels of horizontal axis text. If axis text labels become too
	crowded, the server might shift neighboring labels up or down in
	order to fit labels closer together. This value specifies the
	most number of levels to use; the server can use fewer levels, if
	labels can fit without overlapping.
      }	
      \item{\code{hAxis.showTextEvery}}{a number. Default
      	automatic. How many horizontal axis labels to show, where 1
	means show every label, 2 means show every other label, and so
	on. Default is to try to show as many labels as possible
	without overlapping.
      }

      \item{\code{height}}{a number. Default height of the containing
	element. Height of the chart, in pixels.
      }
      \item{\code{interpolateNulls}}{boolean. Default \code{FALSE}. Whether to guess the
	value of missing points. If true, it will guess the value of any
	missing data based on neighboring points. If false, it will leave a
	break in the line at the unknown point.
      }
      \item{\code{legend}}{a string. Default \code{'right'}. Position of the legend. Can
	be one of the following:
	\describe{
	  \item{\code{'right'}}{To the right of the chart.
	  }
	  \item{\code{'top'}}{Above the chart.
	  }
	  \item{\code{'bottom'}}{Below the chart.
	  }
	  \item{\code{'none'}}{No legend is displayed.
	  }
	}
      }
      \item{\code{legendTextStyle}}{a JSON object. Default \code{\{color: 'black',
	fontName: <global-font-name>, fontSize: <global-font-size>\}}
	An object that specifies the legend text style. The object has this format:
	  \code{\{color: <string>, fontName: <string>, fontSize:
	    <number>\}}
	The color can be any HTML color string, for example: \code{'red'} or
	\code{'#00cc00'}. Also see \code{fontName} and \code{fontSize}.
      }
      \item{\code{reverseCategories}}{Boolean. Default \code{FALSE}. If set to true, will
	draw series from right to left. The default is to draw
	left-to-right.
      }
      \item{\code{series}}{a JSON array of objects, or object with
	nested objects. Default \code{'{}'}. An array of objects, each
	describing the format of the corresponding series in the
	chart. To use default values for a series, specify an empty
	object {}. If a series or a value is not specified, the default
	value will be used. Each object supports the following
	properties:
	\describe{
	  \item{\code{type}}{The type of marker for this series. Valid
	    values are \code{'line', 'bars'}, and \code{'area'}. Note that bars are
	    actually vertical bars (columns). The default value is specified
	    by the chart's seriesType option.
	  }
	  \item{\code{color}}{A valid HTML color string.
	  }
	  \item{\code{pointSize}}{[line and area series only] The size
	    of the circle on each data point, in pixels.
	  }
	  \item{\code{lineWidth}}{[line and area series only] The width
	    of the line, in pixels.
	  }
	}
	You can either specify an array of objects, each
	of which applies to the series in the order given, or you can
	specify an object where each child has a numeric key indicating
	which series it applies to. For example, the following two
	declarations are identical, and apply to the first and fourth
	series:
	
	\code{series:[\{color: 'black', type:
	  'bars'\},\{\}, \{\}, \{color: 'teal', lineWidth: 7\}]
	}
	\code{series:\{0:\{color: 'black', type: 'bars'\},
	  3:\{color: 'teal', lineWidth: 7\}\}
	}
	The following example sets the first series to a black line with
	3 pixel points, the fourth series to a teal, seven pixel wide
	area line, and all other series are default values:
	
	\code{series: [\{type: 'line', color: 'black',
	  pointSize:3\},\{\}, \{\}, \{type:'area', color: 'teal',
	  lineWidth: 7\}]
	}
      }
      \item{\code{seriesType}}{a string. Default \code{'line'}. The
	default line type for any series not specified in the series
	property. Available values are \code{'line', 'bars'}, and \code{'area'}.
      }
      \item{\code{title}}{a string. Default no title. Text to display above the chart.
      }
      \item{\code{titlePosition}}{a string. Default \code{'out'}.
	Where to place the chart title, compared to the chart
	area. Supported values:
	\describe{
	  \item{\code{'in'}}{Draw the title inside the chart area.
	  }
	  \item{\code{'out'}}{Draw the title outside the chart area.
	  }
	  \item{\code{'none'}}{Omit the title.
	  }
	}
      }
      \item{\code{titleTextStyle}}{a JSON object. Default
	\code{\{color:'black',fontName:<global-font-name>,fontSize:<global-font-size>\}}. An
	object that specifies the title text
	style. The object has this format:
	\code{\{color: <string>, fontName: <string>, fontSize: <number>\}}
	The color can be any HTML color string, for example: \code{'red'} or
	\code{'#00cc00'}. Also see \code{fontName} and \code{fontSize}.
      }
      \item{\code{tooltipTextStyle}}{a JSON object. Default \code{\{color: 'black',
	fontName: <global-font-name>, fontSize: <global-font-size>\}}
	An object that specifies the tooltip text style. The object has this format:
	\code{\{color: <string>, fontName: <string>, fontSize: <number>\}}
	The color can be any HTML color string, for example: \code{'red'} or
	\code{'#00cc00'}. Also see \code{fontName} and \code{fontSize}.
      }

      \item{\code{vAxes}}{a JSON array of objects, or object with child objects
	null. Specifies properties for individual vertical axes, if the
	chart has multiple vertical axes. Each child object is a \code{vAxis}
	object, and can contain all the properties supported by
	\code{vAxis}. These property values override any global settings for
	the same property.

	To specify a chart with multiple vertical axes, first define a
	new axis using \code{series.targetAxisIndex}, then configure the axis
	using \code{vAxes}. The following example assigns series 2 to the right
	axis and specifies a custom title and text style for it:
	
	\code{ series:\{2:\{targetAxisIndex:1\}\},
	  vAxes:\{1:\{title:'Losses',textStyle:\{color: 'red'\}\}\}
	}
	
	  This property can be either an object or an array: the object is a
	  collection of objects, each with a numeric label that
	  specifies the axis that it defines--this is the format shown
	  above; the array is an array of objects, one per axis. For
	  example, the following array-style notation is identical to
	  the \code{vAxis} object shown above:

	  \code{
	    vAxes:[
	    \{\}, // Nothing specified for axis 0
	    \{title:'Losses',textStyle:\{color: 'red'\}\} // Axis 1
	    ]
	  }
	}
      \item{\code{vAxis}}{a JSON object. Default \code{'null'}.
	An object with members to configure various vertical axis
	elements. To specify properties of this object, you can use object
	literal notation, as shown here:
	
	\code{\{title: 'Hello', titleTextStyle: \{color: '#FF0000'\}\}}
      }
      
      \item{\code{vAxis.baseline}}{a number. Default automatic. \code{vAxis} property
	that specifies the baseline for the vertical axis. If the
	baseline is smaller than the highest grid line or smaller than
	the lowest grid line, it will be rounded tothe closest gridline.
      }
      \item{\code{vAxis.baselineColor}}{a string. Default \code{'black'}. \code{vAxis} property
	that specifies the color of the baseline for the vertical axis. Can be
	any HTML color string, for example: \code{'red'} or \code{'#00cc00'}.
      }
      \item{\code{vAxis.direction}}{1 or -1. Default 1. The direction in which the
	values along the vertical axis grow. Specify -1 to reverse the
	order of the values.
      }
      \item{\code{vAxis.format}}{a string. Default auto. A format string for
	numeric axis labels. This is a subset of the ICU pattern set. For
	instance, \code{\{format:'#,###\%'\}} will display values 1,000\%, 750\%,
	and 50\% for values 10, 7.5, and 0.5.
      }
      \item{\code{vAxis.gridlineColor}}{a string. Default
	\code{'#CCC'}. The color of the vertical gridlines inside the
	chart area. Specify a valid HTML color string.
      }
  
      \item{\code{vAxis.logScale}}{boolean. Default \code{FALSE}. \code{vAxis} property that
	makes the vertical axis a logarithmic scale (requires all values to
	be positive). Set to \code{TRUE} for yes.
      }
      \item{\code{vAxis.textPosition}}{a string. Default \code{'out'}. Position of the
	vertical axis text, relative to the chart area. Supported values:
	\code{'out', 'in', 'none'}.
      }
      \item{\code{vAxis.textStyle}}{a JSON object. Default
	\code{\{color: 'black',fontName: <global-font-name>, fontSize: <global-font-size>\}}. An
	object that specifies the vertical axis text style. The object has
	this format:
	\code{\{color: <string>, fontName: <string>, fontSize: <number>\}}
	The color can be any HTML color string, for example: \code{'red'} or
	\code{'#00cc00'}. Also see \code{fontName} and \code{fontSize}.
      }
      \item{\code{vAxis.title}}{a string. Default no title. \code{vAxis} property that
	specifies a title for the vertical axis.
      }
      \item{\code{vAxis.titleTextStyle}}{a JSON object. Default \code{\{color: 'black',
	fontName: <global-font-name>, fontSize: <global-font-size>\}}. An
	object that specifies the vertical axis title text style. The
	object has this format:
	\code{\{color: <string>, fontName: <string>, fontSize: <number>\}}
	The color can be any HTML color string, for example: \code{'red'} or
	\code{'#00cc00'}. Also see \code{fontName} and \code{fontSize}.
      }
      \item{\code{vAxis.maxValue}}{a number. Default automatic. vAxis property
	that specifies the highest vertical axis grid line. The actual grid
	line will be the greater of two values: the maxValue option value,
	or the highest data value, rounded up to the next higher grid mark.
      }
      \item{\code{vAxis.minValue}}{a number. Default automatic. \code{vAxis} property that
	specifies the lowest vertical axis grid line. The actual grid line
	will be the lower of two values: the minValue option value, or the
	lowest data value, rounded down to the next lower grid mark.
      }
      \item{\code{vAxis.viewWindowMode}}{a string. Default
	\code{"pretty"} if \code{vAxis.viewWindow} is null,
	\code{"explicit"} otherwise. Specifies how to scale the vertical
	axis to render the values within the chart area. The following
	string values are supported:
	\describe{
	  \item{\code{'pretty'}}{Scale the vertical values so that the
	    maximum and minimum data values are rendered a bit inside the
	    top and bottom of the chart area.
	  }
	  \item{\code{'maximized'}}{Scale the vertical values so that
	    the maximum and minimum data values touch the top and bottom of
	    the chart area.
	  }
	  \item{code{'explicit'}}{Specify the top and bottom scale
	    values of the chart area. Data values outside these values will
	    be cropped. You must specify a \code{vAxis.viewWindow} object
	    describing the maximum and minimum values to show.
	  }
	}
      }
      \item{\code{vAxis.viewWindow}}{Object. Default
	\code{NULL}. Specifies the maximum and minimum data values to
	show on the vertical axis. Present only if
	\code{vAxis.viewWindowMode='explicit'}
      }
      \item{\code{vAxis.viewWindow.max}}{A number. Default 0.	The
	maximum vertical data value to render.
      }
      \item{\code{vAxis.viewWindow.min}}{A number. Default 0. The
	minimum vertical data value to render.
      }
     \item{\code{width}}{a number. Default width of the containing
	element. Width of the chart, in pixels.
      } 

    }
  } 
  \item{chartid}{character. If missing (default) a random chart id will be generated based on
    chart type and \code{\link{tempfile}}
  }
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
  \code{gvisComboChart} returns list of \code{\link{class}}
  "\code{gvis}" and "\code{list}".   
  
  An object of class "\code{gvis}" is a list containing at least the
  following components:
  \item{\code{type}}{Google visualisation type, here 'ComboChart'}
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
  Google Line Chart API:
  \url{http://code.google.com/apis/chart/interactive/docs/gallery/combochart.html} 

  Follow the link for Google's data policy.

}
\author{
  Markus Gesmann \email{markus.gesmann@gmail.com},
  
  Diego de Castillo \email{decastillo@gmail.com}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for
  printing and plotting methods
}
\examples{
## Please note that by default the googleVis plot command
## will open a browser window and requires an internet
## connection to display the visualisation.

CityPopularity
## Add the mean
CityPopularity$Mean=mean(CityPopularity$Popularity)

C1 <- gvisComboChart(CityPopularity, xvar="City",
                                     yvar=c("Mean", "Popularity"),
                                   options=list(seriesType="bars",
                                                title="City Popularity",
                                                series='{0: {type:"line"}}'))
plot(C1)

}
\keyword{ iplot }
