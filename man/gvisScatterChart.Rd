\name{gvisScatterChart}
\alias{gvisScatterChart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Google Scatter Chart with R
}
\description{
  The gvisScatterChart function reads a data.frame and
  creates text output referring to the Google Visualisation API which can be
  included into a web page, or as a stand-alone page. The actual chart is
  rendered by the web browser using SVG or
  VML. }
\usage{
gvisScatterChart(data, options = list(), chartid)
}
\arguments{
  \item{data}{a \code{\link{data.frame}} to be displayed as a scatter
    chart. Two or more columns are required, all must be numeric. The
    values in the first column are used for the X-axis. The values in
    following columns are used for the Y-axis. Each column is displayed
    with a separate color.
  }
  \item{options}{list of configuration options for Google Scatter Chart.
        \describe{
\item{\code{gvis.editor}}{a character label for an
 on-page button which opens an in-page dialog box that enables
users to edit, change and customise the chart. By default no
value is given and therefore no button is displayed. 
      }
    }
    Further possible components are, taken from
    \url{http://code.google.com/apis/visualization/documentation/gallery/scatterchart.html#Configuration_Options}:
    \describe{
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
      \item{\code{curveType}}{a string. Default \code{'none'}. Controls the curve of the
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
      \item{\code{hAxis}}{a JSON object. Default \code{'null'}. An
	object with members to configure	
	various horizontal axis elements. To specify properties of this
	object, you can use object literal notation, as shown here:
	%\describe{
	%\item{
	\code{\{title: 'Hello', titleTextStyle: \{color: '#FF0000'\}\}}
	%}
	%}
      }
      \item{\code{hAxis.baseline}}{a number. Default automatic.
	\code{hAxis} property that specifies the baseline for the
	horizontal axis. If the baseline is smaller than the highest grid
	line or smaller than the lowest grid line, it will be rounded to
	the closest gridline.
      }
      \item{\code{hAxis.baselineColor}}{a string. Default
	'black'. \code{hAxis} property that specifies the color of the
	baseline for the horizontal axis. Can be any HTML color string, for
	example: \code{'red'} or \code{'#00cc00'}.
      }
      \item{\code{hAxis.direction}}{1 or -1. Default 1. The direction in which the
	values along the horizontal axis grow. Specify -1 to reverse the
	order of the values.
      }
      \item{\code{hAxis.format}}{a string. Default auto. a format string for
	numeric axis labels. This is a subset of the ICU pattern set. For
	instance, \code{\{format:'#,###\%'\}} will display values 1,000\%, 750\%,
	and 50\% for values 10, 7.5, and 0.5.
      }
     \item{\code{hAxis.gridlineColor}}{a string. Default
	\code{'#CCC'}. The color of the horizontal gridlines inside the
	chart area. Specify a valid HTML color string.
      }
     
      \item{\code{hAxis.logScale}}{boolean. Default \code{FALSE}. \code{vAxis} property that
	makes the vertical axis a logarithmic scale (requires all values to
	be positive). Set to \code{TRUE} for yes.
      }
      \item{\code{hAxis.textPosition}}{a string. Default \code{'out'} Position of the
	horizontal axis text, relative to the chart area. Supported values:
	\code{'out', 'in', 'none'}.
      }
      \item{\code{hAxis.textStyle}}{a JSON object. Default \code{\{color: 'black',
	fontName: <global-font-name>, fontSize: <global-font-size>\}}
	An object that specifies the horizontal axis text style. The object has this format:
	\code{\{color: <string>, fontName: <string>, fontSize: <number>\}}
	The color can be any HTML color string, for example: \code{'red'} or
	\code{'#00cc00'}. Also see \code{fontName} and \code{fontSize}.
      }
      \item{\code{hAxis.title}}{a string. Default \code{'null'}. \code{hAxis} property that
	specifies the title of the horizontal axis.
      }
      \item{\code{hAxis.titleTextStyle}}{a JSON object. Default \code{\{color: 'black',
	fontName: <global-font-name>, fontSize: <global-font-size>\}}. An
	object that specifies the horizontal axis title text style. The object
	has this format:
	\code{\{color: <string>, fontName: <string>, fontSize: <number>\}}
	The color can be any HTML color string, for example: \code{'red'} or
	\code{'#00cc00'}. Also see \code{fontName} and \code{fontSize}.
      }
      \item{\code{hAxis.maxValue}}{a number. Default automatic. \code{hAxis} property
	that specifies the highest vertical axis grid line. The actual grid
	line will be the greater of two values: the maxValue option value,
	or the highest data value, rounded up to the next higher grid mark.
      }
      \item{\code{hAxis.minValue}}{a number. Default automatic. \code{hAxis} property that
	specifies the lowest vertical axis grid line. The actual grid line
	will be the lower of two values: the minValue option value, or the
	lowest data value, rounded down to the next lower grid mark.
      }
          \item{\code{hAxis.viewWindowMode}}{a string. Default
	\code{"pretty"} if \code{hAxis.viewWindow} is null,
	\code{"explicit"} otherwise. Specifies how to scale the horizontal
	axis to render the values within the chart area. The following
	string values are supported:
	\describe{
	  \item{\code{'pretty'}}{Scale the horizontal values so that the
	    maximum and minimum data values are rendered a bit inside the
	    left and right of the chart area.
	  }
	  \item{\code{'maximized'}}{Scale the horizontal values so that
	    the maximum and minimum data values touch the left and right of
	    the chart area.
	  }
	  \item{code{'explicit'}}{Specify the left and right scale
	    values of the chart area. Data values outside these values will
	    be cropped. You must specify a \code{hAxis.viewWindow} object
	    describing the maximum and minimum values to show.
	  }
	}
      }
      \item{\code{hAxis.viewWindow}}{Object. Default
	\code{NULL}. Specifies the maximum and minimum data values to
	show on the horizontal axis. Present only if
	\code{vAxis.viewWindowMode='explicit'}
      }
      \item{\code{hAxis.viewWindow.max}}{A number. Default 0.	The
	maximum vertical data value to render.
      }
      \item{\code{hAxis.viewWindow.min}}{A number. Default 0. The
	minimum vertical data value to render.
      }    
  
      \item{\code{height}}{a number. Default height of the containing
	element. Height of the chart, in pixels.
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
      \item{\code{lineWidth}}{a number. Default 2. Line width in pixels. Use zero to
	hide all lines and show only the points.
      }
      \item{\code{pointSize}}{a number. Default 0. Diameter of data points, in pixels. Use
	zero to hide all points.
      }
      \item{\code{series}}{a JSON array of objects, or object with nested
	objects. Default \{\}.	
	An array of objects, each describing the format of the
	corresponding series in the chart. To use default values for a
	series, specify an empty object {}. If a series or a value is not
	specified, the global value will be used. Each object supports the
	following properties:
	\describe{
	  \item{\code{color}}{The color to use for this series. Specify a valid
	    HTML color string.
	  }
	  \item{\code{targetAxisIndex}}{Which axis to assign this series
	    to, where 0 is the default axis, and 1 is the opposite
	    axis. Default value is 0; set to 1 to define a chart where
	    different series are rendered against different axes. You can
	    define a different scale for different axes.
	  }
	  \item{\code{pointSize}}{Overrides the global pointSize value
	    for this series.
	  }
	  \item{\code{lineWidth}}{Overrides the global lineWidth value
	    for this series.
	  }
	  \item{\code{curveType}}{Overrides the global curveType value
	    for this series.
	    }
	    \item{\code{visibleInLegend}}{A boolean value, where true
	      means that the series should have a legend entry, and false means
	      that it should not. Default is \code{TRUE}.
	    }
	  }
	  You can specify either an array of objects, each of which
	  applies to the series in the order given, or you can specify an
	  object where each child has a numeric key indicating which series
	  it applies to. For example, the following two declarations are
	  identical, and declare the first series as black and absent from
	  the legend, and the fourth as red and absent from the legend:
	  
	  \code{
	    series: [\{color: 'black', visibleInLegend: false\},\{\}, \{\}, \{color:
	    'red', visibleInLegend: false\}]
	  }

	  \code{
	    series: \{0:\{color: 'black', visibleInLegend: false\}, 3:\{color: 'red',
	    visibleInLegend: false\}\}
	  }
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
      \item{\code{vAxis.titleTextStyle}}{a JSON object. Default

	\code{\{color: 'black',
	  fontName: <global-font-name>, fontSize: <global-font-size>\}}.

	An
	object that specifies the vertical axis title text style. The
	object has this format:
	
	\code{\{color: <string>, fontName: <string>, fontSize:
	  <number>\}}
	
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
  \code{gvisScatterChart} returns list of \code{\link{class}}
  "\code{gvis}" and "\code{list}".   
  
  An object of class "\code{gvis}" is a list containing at least the
  following components:
  \item{\code{type}}{Google visualisation type, here 'ScatterChart'}
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
  Google Scatter Chart API:
  \url{http://code.google.com/apis/chart/interactive/docs/gallery/scatterchart.html} 

  Follow the link for Google's data policy.

}
\author{
  Markus Gesmann \email{markus.gesmann@gmail.com},
  
  Diego de Castillo \email{decastillo@gmail.com}
}
\section{Warning}{
  Google Visualisation API: You cannot load both scatterchart and corechart
  packages at the same time on the same page.
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


## Scatter chart
Scatter1 <- gvisScatterChart(women)
plot(Scatter1)

## Using optional arguments
Scatter2 <- gvisScatterChart(women, options=list(legend="none",
                 lineWidth=2, pointSize=0,
                 title="Women", vAxis="{title:'weight (lbs)'}",
                 hAxis="{title:'height (in)'}", width=300, height=300))
                 
plot(Scatter2)


df=data.frame(x=sin(1:100/3), 
              Circle=cos(1:100/3), 
 	      Ellipse=cos(1:100/3)*0.5)

## Plot several variables as smooth curves
Scatter3 <- gvisScatterChart(df, 
	    		options=list(curveType='function', 
				     pointSize=0, 
				     lineWidth=2))
plot(Scatter3)

}

\keyword{ iplot }
