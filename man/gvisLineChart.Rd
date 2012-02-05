\name{gvisLineChart}
\alias{gvisLineChart}
\title{
Google Line Chart with R
}
\description{
  The gvisLineChart function reads a data.frame and
  creates text output referring to the Google Visualisation API, which can be
  included into a web page, or as a stand-alone page. The actual chart is
  rendered by the web browser using SVG or
  VML. 
}
\usage{
gvisLineChart(data, xvar = "", yvar = "", options = list(), chartid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a \code{\link{data.frame}} to be displayed as a line chart
  }
  \item{xvar}{
    name of the character column which contains the category labels for
    the x-axes.
  }
  \item{yvar}{a vector of column names of the numerical variables to be
    plotted. Each column is displayed as a separate line.
  }
  \item{options}{list of configuration options for Google Line Chart.
    \describe{
      \item{\code{gvis.editor}}{a character label for an
	on-page button which opens an in-page dialog box that enables
	users to edit, change and customise the chart. By default no
	value is given and therefore no button is displayed. 
      }
    }
    Further possible components are, taken from
    \url{http://code.google.com/apis/visualization/documentation/gallery/linechart.html#Configuration_Options}:
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
	
	\preformatted{\{left:20,top:0,width:\"50\%\",height:\"75\%\"\}}
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
      \item{\code{colors}}{An array of strings. Default 'colors'. The colors to
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
     \item{\code{focusTarget}}{a string. Default \code{'datum'	}.
	The type of the entity that receives focus on mouse hover. Also affects
	which entity is selected by mouse click, and which data table
	element is associated with events. Can be one of the following:
	\describe{
	  \item{\code{'datum'}}{Focus on a single data point. Correlates
	    to a cell in the data.
	  }
	  \item{\code{'category'}}{Focus on a grouping of all data
	    points along the major axis. Correlates to a row in the data
	    table.
	  }
	}
	In focusTarget \code{'category'} the tooltip displays all the
	category values. This may be useful for comparing values of
	different series.
      }                
      \item{\code{fontSize}}{a number. Default automatic. The default font
	size, in pixels, of all text in the chart. You can override this
	using properties for specific chart elements.
      }
      \item{\code{fontName}}{a string. Default \code{'Arial'}. The default font face for
	all text in the chart. You can override this using properties for
	specific chart elements.
      }
      \item{\code{hAxis}}{a JSON object. Default \code{'null'}. An
	object with members to configure 
	various horizontal axis elements. To specify properties of this
	object, you can use object literal notation, as shown here:
	%\describe{
	%\item{
	\preformatted{\{title: 'Hello', titleTextStyle: \{color: '#FF0000'\}\}}
	%}
	%}
      }
      \item{\code{hAxis.baseline}}{a number. Default automatic. The baseline for the horizontal axis.
	This option is only supported for a continuous axis.	
      }
      \item{\code{hAxis.baselineColor}}{a string. Default
	\code{'black'}. The color of the baseline for the horizontal
	axis. Can be any HTML color string, for example: \code{'red'} or
	\code{'#00cc00'}.
	This option is only supported for a continuous axis.
      }
      \item{\code{hAxis.direction}}{1 or -1. Default 1. The direction in which the
	values along the vertical axis grow. Specify -1 to reverse the
	order of the values.
      }
      \item{\code{hAxis.format}}{a string. Default auto.
	A format string for numeric or date axis labels.
	
	For number axis labels, this is a subset of the decimal
	formatting ICU pattern set. For instance, \preformatted{\{format:'#,###\%'\}.}will
	display values \\code{"1,000\%", "750\%"}, and \code{"50\%"} for values 10,
	7.5, and 0.5.
	
	For date axis labels, this is a subset of the date formatting
	ICU pattern set. For instance, \preformatted{\{format:'MMM d, y'\}.}will
	display the value "Jul 1, 2011" for the date of July first in
	2011.
	
	The actual formatting applied to the label is derived from the
	locale the API has been loaded with. For more details, see
	loading charts with a specific locale.

	This option is only supported for a continuous axis.
      }
      \item{\code{hAxis.gridlines}}{a JSON object. Default null. An
	object with members to configure the gridlines on the horizontal
	axis. To specify properties of this object, you can use object
	literal notation, as shown here:      
	\preformatted{\{color: '#333', count: 4\}}
	This option is only supported for a continuous axis.
      }
      \item{\code{hAxis.gridlines.color}}{a string. Default
	\code{'#CCC'}. The color of the vertical gridlines inside the
	chart area. Specify a valid HTML color string.
      }
      \item{\code{hAxis.gridlines.count}}{a number. Default 5.The number
       of vertical gridlines inside the chart area. Minimum value is 2.       
      }
      \item{\code{hAxis.logScale}}{boolean. Default \code{FALSE}. \code{vAxis} property that
	makes the vertical axis a logarithmic scale (requires all values to
	be positive). Set to \code{TRUE} for yes.
	This option is only supported for a continuous axis.
      }
      \item{\code{hAxis.textPosition}}{a string. Default \code{'out'} Position of the
	horizontal axis text, relative to the chart area. Supported values:
	\code{'out', 'in', 'none'}.
      }
      \item{\code{hAxis.textStyle}}{a JSON object. Default \preformatted{\{color: 'black',
	fontName: <global-font-name>, fontSize: <global-font-size>\}}
	An object that specifies the horizontal axis text style. The object has this format:
	\preformatted{\{color: <string>, fontName: <string>, fontSize: <number>\}}
	The color can be any HTML color string, for example: \code{'red'} or
	\code{'#00cc00'}. Also see \code{fontName} and \code{fontSize}.
      }
      \item{\code{hAxis.title}}{a string. Default \code{'null'}. \code{hAxis} property that
	specifies the title of the horizontal axis.
      }
      \item{\code{hAxis.titleTextStyle}}{a JSON object. Default \preformatted{\{color: 'black',
	fontName: <global-font-name>, fontSize: <global-font-size>\}.} An
	object that specifies the horizontal axis title text style. The object
	has this format:
	\preformatted{\{color: <string>, fontName: <string>, fontSize: <number>\}}
	The color can be any HTML color string, for example: \code{'red'} or
	\code{'#00cc00'}. Also see \code{fontName} and \code{fontSize}.
      }
      \item{\code{hAxis.slantedText}}{boolean. Default automatic. If
	true, draw the horizontal axis text at an angle, to help fit more
	text along the axis; if false, draw horizontal axis text
	upright. Default behavior is to slant text if it cannot all fit
	when drawn upright. Notice that this option is available only when
	the \code{hAxis.textPosition} is set to \code{'out'} (which is the default).      

	This option is only supported for a discrete axis.
      }
      \item{\code{hAxis.slantedTextAngle}}{a number, 1-90. Default 30. The angle
	of the horizontal axis text, if it's drawn slanted. Ignored if
	\code{hAxis.slantedText} is false, or is in auto mode, and the chart
	decided to draw the text horizontally.

	This option is only supported for a discrete axis.
      }
      \item{\code{hAxis.maxAlternation}}{a number. Default 2. Maximum number of
	levels of horizontal axis text. If axis text labels become too
	crowded, the server might shift neighboring labels up or down in
	order to fit labels closer together. This value specifies the
	most number of levels to use; the server can use fewer levels, if
	labels can fit without overlapping.

	This option is only supported for a discrete axis.
      }
      \item{\code{hAxis.showTextEvery}}{a number. Default automatic. How many
	horizontal axis labels to show, where 1 means show every label, 2
	means show every other label, and so on. Default is to try to
	show as many labels as possible without overlapping.

	This option is only supported for a discrete axis.
      }
      \item{\code{hAxis.maxValue}}{a number. Default automatic. \code{hAxis} property
	that specifies the highest vertical axis grid line. The actual grid
	line will be the greater of two values: the maxValue option value,
	or the highest data value, rounded up to the next higher grid
	mark.

	This option is only supported for a continuous axis.
      }
      \item{\code{hAxis.minValue}}{a number. Default automatic. \code{hAxis} property that
	specifies the lowest vertical axis grid line. The actual grid line
	will be the lower of two values: the minValue option value, or the
	lowest data value, rounded down to the next lower grid mark.

	This option is only supported for a continuous axis.
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
	  \item{\code{'explicit'}}{Specify the left and right scale
	    values of the chart area. Data values outside these values will
	    be cropped. You must specify a \code{hAxis.viewWindow} object
	    describing the maximum and minimum values to show.
	  }
	}
	This option is only supported for a continuous axis.
      }
      \item{\code{hAxis.viewWindow}}{Object. Default
	\code{NULL}. Specifies the cropping range of the horizontal axis.
      }
      
      \item{\code{hAxis.viewWindow.max}}{A number. Default auto.
	
	\describe{
	  
	  \item{For a continuous axis}{
	    
	    The maximum horizontal data value to render. Has an effect
	    only if \code{hAxis.viewWindowMode='explicit'}.	    
	  }
	  \item{For a discrete axis}{
	    
	    The zero-based row index where the cropping window ends. Data points at
	    this index and higher will be cropped out. In conjunction with
	    \code{vAxis.viewWindowMode.min}, it defines a half-opened range \code{[min, max)} that
	    denotes the element indices to display. In other words, every index such
	    that \code{min <= index < max} will be displayed.
	  }
	}	
      }
      \item{\code{hAxis.viewWindow.min}}{A number. Default auto.
	\describe{
	  
	  \item{For a continuous axis}{
	    The minimum horizontal data value to render. Has an effect
	    only if \code{hAxis.viewWindowMode='explicit'}.	    
	  }
	  \item{For a discrete axis}{
	    The zero-based row index where the cropping window
	    begins. Data points at indices lower than this will be
	    cropped out. In conjunction with \code{vAxis.viewWindowMode.max},
	    it defines a half-opened range \code{[min, max)} that denotes the
	    element indices to display. In other words, every index such
	    that \code{min <= index < max} will be displayed.	    
	  }
	}	
      }    
            
      \item{\code{height}}{a number. Default height of the containing
	element. Height of the chart, in pixels.
      }
      \item{\code{interpolateNulls}}{boolean. Default \code{FALSE}. Whether to guess the
	value of missing points. If true, it will guess the value of any
	missing data based on neighbouring points. If false, it will leave a
	break in the line at the unknown point.
      }
      \item{\code{legend}}{a JSON object. Default \code{NULL}.
	An object with members to configure various aspects of the
	legend. To specify properties of this object, you can use object
	literal notation, as shown here:
	
	\preformatted{\{position: 'top', textStyle: \{color: 'blue', fontSize: 16\}\}}
      }
      \item{\code{legend.position}}{a string. Default \code{'right'}. Position of the legend. Can
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
      \item{\code{legend.textStyle}}{a JSON object. Default \preformatted{\{color: 'black',
	  fontName: <global-font-name>, fontSize: <global-font-size>\}}
	An object that specifies the legend text style. The object has this format:
	\preformatted{\{color: <string>, fontName: <string>, fontSize:
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
      \item{\code{reverseCategories}}{boolean. Default \code{FALSE}. If set to true, will
	draw series from right to left. The default is to draw
	left-to-right.
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
	  
	  \preformatted{
	    series: [\{color: 'black', visibleInLegend: false\},\{\}, \{\}, \{color:
	    'red', visibleInLegend: false\}]
	  }

	  \preformatted{
	    series: \{0:\{color: 'black', visibleInLegend: false\}, 3:\{color: 'red',
	    visibleInLegend: false\}\}
	  }
	}
      \item{\code{theme}}{a string. Default \code{NULL}.
	A theme is a set of predefined option values that work together
	to achieve a specific chart behavior or visual effect. Currently
	only one theme is available:
	\describe{
	  \item{\code{maximized}}{Maximizes the area of the chart, and
	    draws the legend and all of the labels inside the chart area. Sets
	    the following options:	    
	    \preformatted{
	      chartArea: \{width: '100\%', height: '100\%'\},
	      legend: \{position: 'in'\},
	      titlePosition: 'in', axisTitlesPosition: 'in',
	      hAxis: \{textPosition: 'in'\}, vAxis: \{textPosition: 'in'\}
	    }
	  }
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
	\preformatted{\{color:'black', fontName:<global-font-name>,fontSize:<global-font-size>\}.} An
	object that specifies the title text
	style. The object has this format:
	\preformatted{\{color: <string>, fontName: <string>, fontSize: <number>\}}
	The color can be any HTML color string, for example: \code{'red'} or
	\code{'#00cc00'}. Also see \code{fontName} and \code{fontSize}.
      }
      \item{\code{tooltip}}{a JSON object. Default \code{NULL}.
	An object with members to configure various tooltip elements. To
	specify properties of this object, you can use object
	literal notation, as shown here:
	
	\preformatted{\{textStyle: {color: '#FF0000'}, showColorCode: true\}}
      }
      \item{\code{tooltip.showColorCode}}{boolean. Default automatic.
	If true, show colored squares next to the series information in
	the tooltip. The default is true when \code{focusTarget} is set to
	\code{'category'}, otherwise the default is \code{FALSE}.
      }
      \item{\code{tooltip.TextStyle}}{a JSON object. Default \preformatted{\{color: 'black',
	  fontName: <global-font-name>, fontSize: <global-font-size>\}}
	An object that specifies the tooltip text style. The object has this format:
	\preformatted{\{color: <string>, fontName: <string>, fontSize: <number>\}}
	The color can be any HTML color string, for example: \code{'red'} or
	\code{'#00cc00'}. Also see \code{fontName} and \code{fontSize}.
      }
      \item{\code{tooltip.trigger}}{
	The user interaction that causes the tooltip to be displayed:
	\describe{
	  \item{\code{'hover'}}{ The tooltip will be displayed when the user hovers over an element.
	  }
	  \item{\code{'none'}}{The tooltip will not be displayed.
	  }	  
	}
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
	
	\preformatted{series:\{2:\{targetAxisIndex:1\}\},
	  vAxes:\{1:\{title:'Losses',textStyle:\{color: 'red'\}\}\}
	  }
	  This property can be either an object or an array: the object is a
	  collection of objects, each with a numeric label that
	  specifies the axis that it defines--this is the format shown
	  above; the array is an array of objects, one per axis. For
	  example, the following array-style notation is identical to
	  the \code{vAxis} object shown above:

	  \preformatted{
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
	\preformatted{\{title: 'Hello', titleTextStyle: \{color: '#FF0000'\}\}}
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
	instance, \preformatted{\{format:'#,###\%'\}.}will display values 1,000\%, 750\%,
	and 50\% for values 10, 7.5, and 0.5.
      }
      \item{\code{vAxis.gridlines}}{a JSON object. Default
	\code{NULL}.
	An object with members to configure the gridlines on the
	vertical axis. To specify properties of this object, you can use
	object literal notation, as shown here:
	\preformatted{
	  \{color: '#333', count: 4\}
	}
      }      
      \item{\code{vAxis.gridlines.color}}{a string. Default
	\code{'#CCC'}. The color of the vertical gridlines inside the
	chart area. Specify a valid HTML color string.
      }
      \item{\code{vAxis.gridlines.count}}{a number. Default 5.The number
       of vertical gridlines inside the chart area. Minimum value is 2.       
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
	\preformatted{\{color: 'black', fontName: <global-font-name>, fontSize: <global-font-size>\}.} An
	object that specifies the vertical axis text style. The object has
	this format:
	\preformatted{\{color: <string>, fontName: <string>, fontSize: <number>\}}
	The color can be any HTML color string, for example: \code{'red'} or
	\code{'#00cc00'}. Also see \code{fontName} and \code{fontSize}.
      }
      \item{\code{vAxis.title}}{a string. Default no title. \code{vAxis} property that
	specifies a title for the vertical axis.
      }
      \item{\code{vAxis.titleTextStyle}}{a JSON object. Default \preformatted{\{color: 'black',
	fontName: <global-font-name>, fontSize: <global-font-size>\}.} An
	object that specifies the vertical axis title text style. The
	object has this format:
	\preformatted{\{color: <string>, fontName: <string>, fontSize: <number>\}}
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
	  \item{\code{'explicit'}}{Specify the top and bottom scale
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
  \code{gvisLineChart} returns list of \code{\link{class}}
  "\code{gvis}" and "\code{list}".   
  
  An object of class "\code{gvis}" is a list containing at least the
  following components:
  \item{\code{type}}{Google visualisation type, here 'LineChart'}
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
  \url{http://code.google.com/apis/chart/interactive/docs/gallery/linechart.html} 

  Follow the link for Google's data policy.

}
\author{
  Markus Gesmann \email{markus.gesmann@gmail.com},
  
  Diego de Castillo \email{decastillo@gmail.com}
}
\section{Warning}{
  Google Visualisation API: You cannot load both linechart and corechart
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

df <- data.frame(country=c("US", "GB", "BR"), val1=c(1,3,4), val2=c(23,12,32))

## Line chart
Line1 <- gvisLineChart(df, xvar="country", yvar=c("val1", "val2"))
plot(Line1)


## Add a customised title and smoothed curve
Line2 <- gvisLineChart(df, xvar="country", yvar=c("val1", "val2"),
             options=list(title="Hello World",
                          titleTextStyle="{color:'red',fontName:'Courier',fontSize:16}",
                          curveType='function'))
plot(Line2)

\dontrun{
## Change y-axis to percentages
Line3 <- gvisLineChart(df, xvar="country", yvar=c("val1", "val2"),
                       options=list(vAxis="{format:'#,###\%'}"))
plot(Line3)

}

## Create a chart with two y-axis:
Line4 <-  gvisLineChart(df, "country", c("val1","val2"),
                        options=list(series="[{targetAxisIndex: 0},
                                              {targetAxisIndex:1}]",
                          vAxes="[{title:'val1'}, {title:'val2'}]"
                          ))
plot(Line4)

## Line chart with edit button
Line5 <- gvisLineChart(df, xvar="country", yvar=c("val1", "val2"),
                       options=list(gvis.editor="Edit me!"))
plot(Line5)


}
\keyword{ iplot }
