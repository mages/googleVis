\name{gvisGeoChart}
\alias{gvisGeoChart}
\title{
  Google Geo Chart with R
}
\description{
  The gvisGeoChart function reads a data.frame and
  creates text output referring to the Google Visualisation API, which can be
  included into a web page, or as a stand-alone page.
  
  A geo chart is a map of a country, a continent, or a region with two modes:
  The region mode colorizes whole regions, such as countries, provinces,
  or states. The marker mode marks designated regions using bubbles that
  are scaled according to a value that you specify.

  A geo chart is rendered within the browser using
  SVG or VML. Note that the map is not scrollable or draggable.
}
\usage{
gvisGeoChart(data, locationvar = "", colorvar="", sizevar="", options = list(), chartid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
    a \code{data.frame}. The data has to have at least one
    column with location name (\code{locationvar}), value to be mapped
    to location. The format of the data varies depending on which display mode that
    you use: Regions or Markers.
  }
  \item{locationvar}{column name of \code{data} with the geo locations to be
    analysed. The locations can be provide in two formats:
    \describe{	    
      \item{Format 1}{'latitude:longitude'. See the example below.}
      \item{Format 2}{Address, country name, region name locations, or
	US metropolitan area codes, see
	\url{http://code.google.com/apis/adwords/docs/developer/adwords_api_us_metros.html}. This
	format works with the \code{dataMode} option set to either
	'markers' or 'regions'. The following formats are accepted: A
	specific address (for example, "1600 Pennsylvania Ave"). A
	country name as a string (for example, "England"), or an uppercase ISO-3166 code
	or its English text equivalent (for example, "GB" or "United Kingdom").
	An uppercase ISO-3166-2 region code name or its English text
	equivalent (for example, "US-NJ" or "New Jersey"). 
      }
    }
  }
  \item{colorvar}{column name of \code{data} with the optional numeric
    column used to assign a color to this marker, based on the scale
    specified in the \code{colorAxis.colors} property. If this column is
    not present, all markers will be the same color. If the column is
    present, null values are not allowed. Values are scaled relative to
    each other, or absolutely to values specified in the
    \code{colorAxis.values} property. 
  }
  \item{sizevar}{only used for \code{displayMode='markers'}. Column name of \code{data} with the optional numeric
    column used to assign the marker size, relative to the other marker
    sizes. If this column is not present, the value from the previous
    column will be used (or default `size, if no color column is
    provided as well). If the column is present, null valuesare not
    allowed. 
  }
  \item{options}{list of configuration options for Google Geo Chart.
    \describe{
      \item{\code{gvis.editor}}{a character label for an
	on-page button which opens an in-page dialog box that enables
	users to edit, change and customise the chart. By default no
	value is given and therefore no button is displayed. 
      }
    }
    Further possible components are, taken from
    \url{https://google-developers.appspot.com/chart/interactive/docs/gallery/geochart.html#Configuration_Options}:
    
    \describe{
      
      \item{\code{backgroundColor}}{a string or object. Default
	\code{white}. The background color for the main area of the
	chart. Can be either a simple HTML color string, for example:
	\code{'red'} or \code{'#00cc00'}, or an object with the following properties.
      }
      \item{\code{backgroundColor.fill}}{a string. Default
	\code{white}. The chart fill color, as an HTML color string.
      }
      \item{\code{backgroundColor.stroke}}{a string. Default
	\code{'#666'}. The color of the chart border, as an HTML color
	string.
      }
      \item{\code{backgroundColor.strokeWidth}}{a number. Default
	0. The border width, in pixels.
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
      \item{\code{datalessRegionColor}}{a string. Default
	\code{'F5F5F5'}. Colors to assign to regions with no associated
	data.
      }
      \item{\code{displayMode}}{a string. Default \code{'auto'}. Which
	type of map this is. The DataTable format must match the value
	specified. The following values are supported:
	
	\code{'auto'}: Choose based on the format of the DataTable.
	\code{'regions'}: This is a region map
	\code{'markers'}: This is a marker map.
      }
      \item{\code{enableRegionInteractivity}}{boolean. Default
	automatic. If true, enable region interactivity, including focus
	and tool-tip elaboration on mouse hover, and region selection
	and firing of regionClick and select events on mouse click.
	
	The default is \code{TRUE} in region mode, and \code{FALSE} in marker mode.
      }
      \item{\code{height}}{number. The default height is 347 pixels,
	unless the width option is specified and \code{keepAspectRatio} is
	set to true - in which case the height is calculated accordingly.
      }
      
      \item{\code{keepAspectRatio}}{boolean. Default \code{TRUE}. If
	true, the map will be drawn at the largest size that can fit
	inside the chart area at its natural aspect ratio. If only one of
	the width and height options is specified, the other one will be
	calculated according to the aspect ratio.
	
	If false, the map will be stretched to the exact size of the chart
	as specified by the width and height options.
      }
      
      \item{\code{legend}}{a JSON object / \code{'none'}. Default \code{'null'}.	
	An object with members to configure various aspects of the legend, or
	\code{'none'}, if no legend should appear. To specify properties of this
	object, you can use object literal notation, as shown here:
	
	\preformatted{\{textStyle: \{color: 'blue', fontSize: 16\}\}}
      }
      \item{\code{legend.numberFormat}}{a string. Default
	\code{'auto'}. A format string for numeric axis labels. This is
	a subset of the ICU pattern set. For instance,
	\preformatted{\{numberFormat:'.##'\}.}will display values "10.66", "10.6", and
	"10.0" for values 10.666, 10.6, and 10.
      }
      \item{\code{legend.textStyle}}{a JSON object. Default \preformatted{\{color:
	  'black', fontName: <global-font-name>, fontSize:
	  <global-font-size>\}.}  An object that specifies the legend text
	style. The object has this format:
	\preformatted{\{color: <string>, fontName: <string>, fontSize: <number>\}}
	The color can be any HTML color string, for example: \code{'red'} or
	\code{'#00cc00'}. Also see \code{fontName} and \code{fontSize}.
      }
      \item{\code{region}}{string, default \code{'world'}.
	The area to display on the map. (Surrounding areas will be
	displayed as well.) Can be one of the following:
	\itemize{	  
	  \item \code{'world'} A map of the entire world.
	  
	  \item A continent or a sub-continent, specified by its 3-digit
	    code, e.g., '011' for Western Africa.
	  
	  \item A country, specified by its ISO 3166-1 alpha-2 code,
	    e.g., \code{'AU'} for Australia.
	  
	  \item A state in the United States, specified by its ISO
	    3166-2:US code, e.g., \code{'US-AL'} for Alabama. Note that
	    the resolution option must beset to
	    either \code{'provinces'} or \code{'metros'}.
	}
	For more information see:
	\url{http://code.google.com/apis/chart/interactive/docs/gallery/geochart.html#Configuration_Options}	
      }

      \item{\code{magnifyingGlass}}{an object. Default
	\preformatted{\{enable: true, zoomFactor: 5.0\}}
	
	An object with members to configure various aspects of the
	magnifying glass. To specify properties of this object, you can
	use object literal notation, as shown here:
	\preformatted{\{enable: true, zoomFactor: 7.5\}}
      }
      \item{\code{magnifyingGlass.enable}}{boolean. Default \code{TRUE}.
	If true, when the user lingers over a cluttered marker, a
	magnifiying glass will be opened.
	
	Note: this feature is not supported in browsers that do not
	support SVG, i.e. Internet Explorer version 8 or earlier.
      }
      \item{\code{magnifyingGlass.zoomFactor}}{a number. Default 5.0. The zoom
	factor of the magnifying glass. Can be any number greater than 0.
      }
      \item{\code{markerOpacity}}{number, between 0.0 - 1.0. Default 1.0.
      }
      
      \item{\code{resolution}}{a string. Default \code{'countries'}
	The resolution of the map borders. Choose one of the following values:
	\describe{
	  \item{\code{'countries'}}{
	  }
	  \item{\code{'provinces'}}{ Not supported for all countries;
	    please test a country to see whether this option is supported.
	  }
	  \item{\code{'metros'}}{Supported for the US country region and
	    US state regions only.
	  }
	}
      }
      \item{\code{sizeAxis}}{a JSON object. Default \code{'null'}. An object with
	members to configure how values are associated with bubble
	size. To specify properties of this object, you can use object
	literal notation, as shown here:
	
	\preformatted{\{minValue: 0,  maxSize: 20\}}
      }
      \item{\code{sizeAxis.maxSize}}{a number. Default 30. Maximum size
	of the largest marker, in pixels.
      }
      \item{\code{sizeAxis.maxValue}}{a	number. Default	maximum value of
	size column in chart data	Maximum size column value. Larger values
	will be cropped to this value.
      }
      \item{\code{sizeAxis.minSize}}{a number. Default 2.	
	Mininum size of the smallest marker, in pixels.
      }
      \item{\code{sizeAxis.minValue}}{a	number. Default	minimum value of
	size column in chart data	Minimum size column value. Smaller
	values will be clamped to this value.
      }
      \item{\code{tooltip}}{a JSON object. Default	\code{'null'}.	
	An object with members to configure various tooltip elements. To specify
	properties of this object, you can use object literal notation, as
	shown here:

	\preformatted{\{textStyle: \{color: '#FF0000'\}, showColorCode: true\}}
      }
      \item{\code{tooltip.textStyle}}{a JSON object. Default \preformatted{\{color:
	  'black', fontName: <global-font-name>, fontSize:
	  <global-font-size>\}}
	An object that specifies the tooltip text style. The object has this format:
	
	\preformatted{\{color: <string>, fontName: <string>, fontSize: <number>\}}
    
	The color can be any HTML color string, for example: \code{'red'} or \code{'#00cc00'}.
      }
      \item{\code{tooltip.trigger}}{a string. Default \code{'focus'}.
	The user interaction that causes the tooltip to be displayed:
	\describe{
	  \item{\code{'focus'}}{The tooltip will be displayed when the
	    user hovers over an element.	  
	  }     
	  \item{\code{'none'}}{The tooltip will not be displayed.
	  }
	}
      }
      \item{\code{width}}{number, default 556. Width of the
	visualization. If no units are given, the default unit is pixels.
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
  \code{gvisGeoChart} returns list of \code{\link{class}} "\code{gvis}" and "\code{list}". 
  
  An object of class "\code{gvis}" is a list containing at least the following components:
  \item{\code{type}}{Google visualisation type, here 'GeoChart'}
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
  Google Geo Chart  API: \url{https://google-developers.appspot.com/chart/interactive/docs/gallery/geochart.html}

  Follow the link for Google's data policy.

}
\author{
Markus Gesmann \email{markus.gesmann@gmail.com},

Diego de Castillo \email{decastillo@gmail.com}
}
\note{
  This is the new version of the GeoChart chart. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{

  See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for
  printing and plotting methods, \code{\link{gvisGeoMap}} and
  \code{\link{gvisIntensityMap}} for an alternative to \code{gvisGeoChart}.  
}
\examples{
## Please note that by default the googleVis plot command
## will open a browser window and requires Internet
## connection to display the visualisation.

## Regions examples
## The regions style fills entire regions (typically countries) with
## colors corresponding to the values that you assign

G1 <- gvisGeoChart(Exports, locationvar='Country', colorvar='Profit') 

plot(G1)

## Plot only Europe
G2 <- gvisGeoChart(Exports, "Country", "Profit",
                   options=list(region="150"))

plot(G2)


## Example showing US data by state 
require(datasets)

states <- data.frame(state.name, state.x77)
G3 <- gvisGeoChart(states, "state.name", "Illiteracy",
                 options=list(region="US", displayMode="regions", resolution="provinces",
 		 width=600, height=400))
plot(G3)

## Markers Example
## A marker style map renders bubble-shaped markers at specified
## locations with the color and size that you specify.

G4 <- gvisGeoChart(CityPopularity, locationvar='City', colorvar='Popularity',
                      options=list(region='US', height=350, 
                                   displayMode='markers',
				   colorAxis="{values:[200,400,600,800],
                                   colors:[\'red', \'pink\', \'orange',\'green']}")
                      ) 
plot(G4)

G5 <- gvisGeoChart(Andrew, "LatLong", colorvar='Speed_kt',
                   options=list(region="US"))
plot(G5)


G6 <- gvisGeoChart(Andrew, "LatLong", sizevar='Speed_kt',
                   colorvar="Pressure_mb", options=list(region="US"))
plot(G6)

## Create lat:long values and plot a map of Oceania
## Set background colour to light-blue

require(stats)
data(quakes)
head(quakes)
quakes$latlong<-paste(quakes$lat, quakes$long, sep=":")

G7 <- gvisGeoChart(quakes, "latlong", "depth", "mag",
                   options=list(displayMode="Markers", region="009",
                   colorAxis="{colors:['red', 'grey']}",
                   backgroundColor="lightblue"))

plot(G7)

\dontrun{
## Plot world wide earth quakes of the last 30 days with magnitude >= 4.0 
library(XML)
## Get earthquake data of the last 30 days
eq <- readHTMLTable(readLines("http://www.iris.edu/seismon/last30.html"),
	colClasses=c("factor", rep("numeric", 4), "factor"))
eq <- eq[[2]] ## extract the eq table
eq$loc=paste(eq$LAT, eq$LON, sep=":") ## create a lat:long location variable

G8 <- gvisGeoChart(eq, "loc", "DEPTH km", "MAG",
                   options=list(displayMode="Markers", 
                   colorAxis="{colors:['purple', 'red', 'orange', 'grey']}",
                   backgroundColor="lightblue"))
plot(G8)
}
}
\keyword{ iplot }
