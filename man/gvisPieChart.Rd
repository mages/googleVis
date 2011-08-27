\name{gvisPieChart}
\alias{gvisPieChart}
\title{
Google Pie Chart with R
}
\description{
  The gvisPieChart function reads a data.frame and
  creates text output referring to the Google Visualisation API which can be
  included into a web page, or as a stand-alone page. The actual chart is
  rendered by the web browser using SVG or
  VML. 
}
\usage{
gvisPieChart(data, labelvar = "", numvar = "", options = list(), chartid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a \code{\link{data.frame}} to be displayed as a pie chart
  }
  \item{labelvar}{
    Name of the character column which contains the category labels for
    the slice labels.
  }
  \item{numvar}{a vector of column names of the numerical variables of the slice values. 
  }
  \item{options}{list of configuration options for Google Pie Chart.
        \describe{
\item{\code{gvis.editor}}{a character label for an
 on-page button which opens an in-page dialog box that enables
users to edit, change and customise the chart. By default no
value is given and therefore no button is displayed. 
      }
    }
    Further possible components are, taken from
    \url{http://code.google.com/apis/visualization/documentation/gallery/piechart.html#Configuration_Options}:
    \describe{
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
      \item{\code{chartArea}}{A string. Default \code{'null'}. An object
	with members to configure the 
	placement and size of the chart area (where the chart itself is drawn,
	excluding axis and legends). Two formats are supported: a number, or a
	number followed by \code{\%}. A simple number is a value in pixels; a number
	followed by \code{\%} is a percentage. Example:
	
	\code{\{left:20,top:0,width:\"50\%\",height:\"75\%\"\}}
      }
      \item{\code{chartArea.left}}{A number or string. Default auto. How far to draw
	the chart from the left border.
      }
      \item{\code{chartArea.top}}{A number or string. Default auto. How far to
	draw the chart from the top border.
      }
      \item{\code{chartArea.width}}{A number or string. Default auto.	Chart
	area width.
      }
      \item{\code{chartArea.height}}{A number or string. Default auto.	Chart
	area height.
      }
      \item{\code{colors}}{An array of strings. Default 'colors'. The colors to
	use for the chart elements. An array of strings, where each element is
	an HTML color string, for example: \code{colors:['red','#004411']}.
      }
      \item{\code{fontSize}}{A number. Default automatic. The default font
	size, in pixels, of all text in the chart. You can override this
	using properties for specific chart elements.
      }
      \item{\code{fontName}}{A string. Default \code{'Arial'}. The default font face for
	all text in the chart. You can override this using properties for
	specific chart elements.
      }
      \item{\code{height}}{A number. Default height of the containing
	element. Height of the chart, in pixels.
      }
      \item{\code{is3D}}{Boolean. Default \code{FALSE}.	If set to
	\code{TRUE}, displays a three-dimensional chart.
	}
      \item{\code{legend}}{A string. Default \code{'right'}. Position of the legend. Can
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
      \item{\code{legendTextStyle}}{A json object. Default \code{\{color: 'black',
	fontName: <global-font-name>, fontSize: <global-font-size>\}}
	An object that specifies the legend text style. The object has this format:
	  \code{\{color: <string>, fontName: <string>, fontSize:
	    <number>\}}
	The color can be any HTML color string, for example: \code{'red'} or
	\code{'#00cc00'}. Also see \code{fontName} and \code{fontSize}.
      }
      \item{\code{pieSliceText}}{A string. Default 'percentage'.
	The content of the text displayed on the slice. Can be one of
	the following:
	\describe{
	  \item{\code{'percentage'}}{The percentage of the slice size out
	    of the total.
	  }
	  \item{\code{'value'}}{The quantitative value of the slice.
	  }
	  \item{\code{'label'}}{ The name of the slice.
	  }
	  \item{\code{'none'}}{No text is displayed.}
	}
      }
      \item{\code{pieSliceTextStyle}}{A json object. Default
	\code{\{color: 'black', fontName: <global-font-name>, fontSize:
	  <global-font-size>\}}. An object that specifies the slice text
      style. The object has this format: 
	\code{\{color: <string>, fontName: <string>, fontSize:
	  <number>\}}. The color can be any HTML color string, for
	example: \code{'red'} or \code{'#00cc00'}. Also see \code{fontName} and
	\code{fontSize}.
      }	
      \item{\code{reverseCategories}}{Boolean. Default \code{FALSE}. If
	set to \code{TRUE}, will draw slices counterclockwise. The
	default is to draw clockwise.	
      }
      \item{\code{sliceVisibilityThreshold}}{A number. Default
	1/720. The slice relative part, below which a slice will not
	show individually. All slices that have not passed this
	threshold will be combined to a single slice, whose size is the
	sum of all their sizes. Default is not to show individually any
	slice which is smaller than half a degree.
      }
      \item{\code{pieResidueSliceColor}}{A string. Default
	\code{'#ccc'}. Color for the combination slice that holds all
	slices below \code{sliceVisibilityThreshold}.
      }
      \item{\code{pieResidueSliceLabel}}{A string. Default \code{'Other'}. A
	label for the combination slice that holds all slices below
	\code{sliceVisibilityThreshold}.
      }
      \item{\code{title}}{A string. Default no title. Text to display above the chart.
      }
      \item{\code{titleTextStyle}}{A json object. Default
	\code{\{color:'black',fontName:<global-font-name>,fontSize:<global-font-size>\}}. An
	object that specifies the title text
	style. The object has this format:
	\code{\{color: <string>, fontName: <string>, fontSize: <number>\}}
	The color can be any HTML color string, for example: \code{'red'} or
	\code{'#00cc00'}. Also see \code{fontName} and \code{fontSize}.
      }
      \item{tooltipText}{A string. Default \code{'both'}. What information to
	display when the user hovers over a pie slice. The following
	values are supported:
	\describe{
	  \item{\code{'both'}}{[Default] Display both the absolute value of the
	    slice and the percentage of thewhole.
	  }
	  \item{\code{'value'}}{Display only the absolute value of the
	    slice.
	  }
	  \item{\code{'percentage'}}{
	    Display only the percentage of the whole represented by the slice.
	  }
	}
      }
      \item{\code{tooltipTextStyle}}{A json object. Default \code{\{color: 'black',
	fontName: <global-font-name>, fontSize: <global-font-size>\}}
	An object that specifies the tooltip text style. The object has this format:
	\code{\{color: <string>, fontName: <string>, fontSize: <number>\}}
	The color can be any HTML color string, for example: \code{'red'} or
	\code{'#00cc00'}. Also see \code{fontName} and \code{fontSize}.
      }
      \item{\code{width}}{A number. Default width of the containing
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
  \code{gvisPieChart} returns list of \code{\link{class}}
  "\code{gvis}" and "\code{list}".   
  
  An object of class "\code{gvis}" is a list containing at least the
  following components:
  \item{\code{type}}{Google visualisation type, here 'PieChart'}
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
  Google Pie Chart API:
  \url{http://code.google.com/apis/chart/interactive/docs/gallery/piechart.html} 

  Follow the link for Google's data policy.

}
\author{
  Markus Gesmann \email{markus.gesmann@gmail.com},
  
  Diego de Castillo \email{decastillo@gmail.com}
}
\section{Warning}{
  Google Visualisation API: You cannot load both piechart and corechart
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

Pie1 <- gvisPieChart(CityPopularity)
plot(Pie1)

}
\keyword{ iplot }
