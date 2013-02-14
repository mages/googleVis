\name{gvisBubbleChart}
\alias{gvisBubbleChart}
\title{
Google Bubble Chart with R
}
\description{

  The gvisBubbleChart function reads a data.frame and
  creates text output referring to the Google Visualisation API, which can be
  included into a web page, or as a stand-alone page.

  A bubble chart is used to visualize a data set with 2 to 4
  dimensions. The first two dimensions are visualized as coordinates,
  the 3rd as color and the 4th as size.
 

  The bubble chart is rendered within the browser using SVG or VML and
  displays tips when hovering over points.
}
\usage{
gvisBubbleChart(data, idvar = "", xvar = "", yvar = "",
                      colorvar = "", sizevar = "",
                      options = list(), chartid)
}
\arguments{
  \item{data}{a \code{\link{data.frame}} to be displayed as a bubble
    chart. The data has to have at least three columns for \code{idvar,
      xvar}, and \code{yvar}. 
  }
  \item{idvar}{column name of \code{data} with the bubble
  }
  \item{xvar}{
    column name of a numerical vector in \code{data} to be plotted on the x-axis.
  }
  \item{yvar}{
    column name of a numerical vector in \code{data} to be plotted on the y-axis.
  }
  \item{colorvar}{column name of data that identifies bubbles in the same
    series. Use the same value to identify all bubbles that belong to the
    same series; bubbles in the same series will be assigned the same
    color. Series can be configured using the \code{series} option.
  }
  \item{sizevar}{values in this column are mapped to actual pixel values
    using the \code{sizeAxis} option.
  }
  \item{options}{list of configuration options for Google Bubble Chart.
    \describe{
      \item{\code{gvis.editor}}{a character label for an
	on-page button which opens an in-page dialog box that enables
	users to edit, change and customise the chart. By default no
	value is given and therefore no button is displayed. 
      }
    }
    Further possible components are, taken from
    \url{https://google-developers.appspot.com/chart/interactive/docs/gallery/bubblechart.html#Configuration_Options}:
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
      \item{\code{bubble}}{a JSON object. Default \code{NULL}.
	An object with members to configure the visual properties of the bubbles.
     }
     \item{\code{bubble.opacity}}{a number between 0.0 - 1.0. Default
       0.8. The opacity of the bubbles, where 0 is fully transparent and 1 is fully opaque.
     }
     \item{\code{bubble.stroke}}{a string. Default \code{'#ccc'}.
       The color of the bubbles' stroke.
     }
     \item{\code{bubble.textStyle}}{a JSON object. Default
	\preformatted{\{color: 'black', fontName: <global-font-name>, fontSize:
	  <global-font-size>\}.}
	
	An object that specifies the bubble text style. The object has this format:
	\preformatted{
	  \{color: <string>, fontName: <string>, fontSize: <number>\}
	  }
	  The color can be any HTML color string, for example:
	\code{'red'} or \code{'#00cc00'}. Also see \code{fontName} and
	\code{fontSize}.	
      }     
      \item{\code{chartArea}}{a string. Default \code{'null'}. An object
	with members to configure the
	placement and size of the chart area (where the chart itself is drawn,
	excluding axis and legends). Two formats are supported: a number, or a
	number followed by \code{\%}. A simple number is a value in pixels; a number
	followed by \code{\%} is a percentage. Example:

	\preformatted{\{left:20,top:0,width:\"50\%\",height:\"75\%\"\}}
      }
      \item{\code{chartArea.height}}{a number or string. Default auto.	Chart
	area height.
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
      \item{\code{colors}}{a JSON array of strings. Default 'colors'. The colors to
	use for the chart elements. An array of strings, where each element is
	an HTML color string, for example: \code{colors:[red','#004411']}.
      }
      \item{\code{colorAxis}}{a string. Default \code{'null'}	An object that
	specifies a mapping between colors and color column values. To
	specify properties of this object, you can use object literal
	notation, as shown here:
	
	\preformatted{\{minValue: 0,  colors: ['#FF0000', '#00FF00']\}}
      }
      \item{\code{colorAxis.minValue}}{a number. Default minimum value
	of color column in chart data.	If present, specifies a minimum
	value for chart color data. Color data values of this value and
	lower will be rendered as the first color in the
	\code{colorAxis.colors} range.
      }
      \item{\code{colorAxis.maxValue}}{a number. Default maximum value
	of color column in chart data	If present, specifies a maximum
	value for chart color data. Color data values of this value and
	higher will be rendered as the last color in the
	\code{colorAxis.colors} range.
      }
      \item{\code{colorAxis.values}}{a JSON array of numbers. Default
	\code{'null'}. Controls how values are associated with
	colors. Each value is associated with the corresponding color in
	the \code{colorAxis.colors} array. These values apply to the color
	value for a region or marker. Regions are colored according to a
	gradient of the values specified here. Not specifying a value
	for this option is equivalent to specifying \code{[minValue,
	  maxValue]}.
      }
      \item{\code{colorAxis.colors}}{a JSON array of color
	strings. Default \code{'null'}.	Colors to assign to values in
	the visualization. An array of strings, where each element is an
	HTML color string, for example: colorAxis:
	\preformatted{\{colors:['red','#004411']\}.} You must have at least two values;
	the gradient will include all your values, plus calculated
	intermediary values, with the first color as the smallest value,
	and the last color as the highest.
      }
      \item{\code{colorAxis.legend}}{an object. Default \code{null}. An
	object that specifies the style of the gradient color legend.
      }
      \item{\code{colorAxis.legend.position}}{a string.	Default
	\code{'top'}. Position of the legend. Can be one of the following:
	 \describe{
	   \item{\code{'top'}}{Above the chart.
	   }
	   \item{\code{'bottom'}}{Below the chart.
	   }
	   \item{\code{'in'}}{Inside the chart, at the top.
	   }
	   \item{\code{'none'}}{No legend is displayed.
	   }
	 }
       }
       \item{\code{colorAxis.legend.textStyle}}{an object. Default
      	\preformatted{\{color: 'black', fontName: <global-font-name>, fontSize:
	<global-font-size>\}}
      
      An object that specifies the legend text style. The object has this format:
      	\preformatted{
	  \{color: <string>, fontName: <string>, fontSize: <number>\}
	  }
	  The color can be any HTML color string, for example:
      	\preformatted{'red'} or \preformatted{'#00cc00'}. Also
      	see \code{fontName} and \code{fontSize}.
      }
      \item{\code{colorAxis.legend.numberFormat}}{a string. Default
      	\code{'auto'}. A format string for
	  numeric labels. This is a subset of the ICU pattern set. For instance,
	  \preformatted{\{numberFormat:'.##'\}} will display values
      	  \preformatted{"10.66", "10.6"}, and 	\preformatted{"10.0"} for  
	  values 10.666, 10.6, and 10.
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
      %%\item{\code{gridlineColor}}{a string. Default \code{'#CCC'}. The color of the
      %% gridlines inside the chart area. Specify a valid HTML color
      %%string.
      %%}
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
      \item{\code{sizeAxis}}{a JSON object. Default \code{NULL}.
	An object with members to configure how values are associated
	with bubble size. To specify properties of this object, you can
	use object literal notation, as shown here:
	
	\preformatted{\{minValue: 0,  maxSize: 20\}}
      }
      \item{\code{sizeAxis.maxSize}}{a number. Default 30.
	Maximum radius of the largest possible bubble, in pixels.
      }      
      \item{\code{sizeAxis.maxValue}}{a number. Default set to maximum
	value of size column in chart data.
	The size value (as appears in the chart data) to be mapped to
	\code{sizeAxis.maxSize}. Larger values will be cropped to this value.		
      }
      \item{\code{sizeAxis.minSize}}{a number. Default 5.
	Minimum radius of the largest possible bubble, in pixels.
      }
      \item{\code{sizeAxis.minValue}}{a number. Default set to minimum
	value of size column in chart data.
	The size value (as appears in the chart data) to be mapped to
	\code{sizeAxis.minSize}. Smaller values will be cropped to this value.		
      }      
      \item{\code{sortBubblesBySize}}{boolean. Default \code{TRUE}. If
	true, sorts the bubbles by size so the smaller bubbles appear
	above the larger bubbles. If false, bubbles are sorted according
	to their order in \code{data}.      
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
      \item{\code{tooltip.textStyle}}{a JSON object. Default \preformatted{\{color: 'black',
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
      \item{\code{vAxis.viewWindow}}{a JSON object. Specifies the
	cropping range of the vertical axis.	
      }
      \item{\code{vAxis.viewWindow.max}}{A number. Default 0.	The
	maximum vertical data value to render.

	Has an effect only if vAxis.viewWindowMode='explicit'.
      }
      \item{\code{vAxis.viewWindow.min}}{A number. Default 0. The
	minimum vertical data value to render.

	Has an effect only if vAxis.viewWindowMode='explicit'.
      }
      \item{\code{width}}{a number. Default width of the containing
	element. Width of the chart, in	pixels.
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
  \code{gvisBubbleChart} returns list of \code{\link{class}}
  "\code{gvis}" and "\code{list}".   
  
  An object of class "\code{gvis}" is a list containing at least the
  following components:
  \item{\code{type}}{Google visualisation type, here 'BubbleChart'}
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
	  \item{\code{jsFooter}}{End tag \code{</script>}.
	  }
	  \item{\code{jsChart}}{Call of the \code{jsDisplayChart} 
	    function.
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
  Google Bubble Chart API:
  \url{http://code.google.com/apis/chart/interactive/docs/gallery/bubblechart.html} 

  Follow the link for Google's data policy.

}
\author{
  Markus Gesmann \email{markus.gesmann@gmail.com},
  
  Diego de Castillo \email{decastillo@gmail.com}
}
\section{Warning}{
  Google Visualisation API: You cannot load both bubblechart and corechart
  packages at the same time on the same page.
}


\seealso{
  See also \code{\link{gvisMotionChart}} for a moving bubble chart over
  time, and \code{\link{print.gvis}}, \code{\link{plot.gvis}} for
  printing and plotting methods.
}
\examples{

bubble1 <- gvisBubbleChart(Fruits, idvar="Fruit", xvar="Sales", yvar="Expenses")
plot(bubble1)

## Set color and size
bubble2 <- gvisBubbleChart(Fruits, idvar="Fruit", xvar="Sales", yvar="Expenses",
                          colorvar="Location", sizevar="Profit",
                          options=list(hAxis='{minValue:75, maxValue:125}'))

plot(bubble2)

## Use year to color the bubbles
bubble3 <- gvisBubbleChart(Fruits, idvar="Fruit", xvar="Sales", yvar="Expenses",
                           colorvar="Year", sizevar="Profit",
                           options=list(hAxis='{minValue:75, maxValue:125}'))
plot(bubble3)

## Gradient colour example
bubble4 <- gvisBubbleChart(Fruits, idvar="Fruit", xvar="Sales", yvar="Expenses",
                          sizevar="Profit",
                          options=list(hAxis='{minValue:75,  maxValue:125}', 
                                  colorAxis="{colors: ['lightblue', 'blue']}"))
plot(bubble4)

\dontrun{
## Moving bubble chart over time, aka motion chart

M <- gvisMotionChart(Fruits, Fruit, Year)
plot(M)
}



}
\keyword{ iplot }
