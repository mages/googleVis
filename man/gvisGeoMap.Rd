\name{gvisGeoMap}
\alias{gvisGeoMap}
\title{
  Google Geo Map with R
}
\description{
  The gvisGeoMap function reads a data.frame and
  creates text output referring to the Google Visualisation API which can be
  included into a web page, or as a standalone page.
  
  A geomap is a map of a country, continent, or region map, with colors
  and values assigned to specific regions. Values are displayed as a color
  scale, and you can specify optional hovertext for regions. The map is
  rendered in the browser using an embedded Flash player. Note that the
  map is not scrollable or draggable, but can be configured to allow
  zooming.
}
\usage{
gvisGeoMap(data, locationvar='', numvar='', hovervar='',options = list())
}
\arguments{
  \item{data}{
    Your DataTable must include every optional column preceding any
    column that you want to use. So, for example, if you want to specify
    a lat/long table, and only wanted to use columns 1, 2, and 4, your
    DataTable must still define column 3 (though you don't need to add
    any values to it): 
  }
    \item{locationvar}{column name of \code{data} with the geo locations to be
      analysed.
  }
    \item{numvar}{column name of \code{data} with the values to be
      shown.
  }
    \item{hovervar}{column name of \code{data} with the hover text.
  }
  \item{options}{list of configuration options for the motion chart.
    The possible components are, taken from \url{http://code.google.com/apis/visualization/documentation/gallery/geomap.html} :
    \describe{
      \item{region}{string, default \code{'world'}.	
	The area to display on the map. (Surrounding areas will be
	displayed as well.) Can be either a country code (in uppercase
	ISO-3166 format), or a one of the following strings
	\tabular{ll}{
	  world \tab (Whole world)\cr
	  us_metro \tab (United States, metro areas)\cr
	  005 \tab (South America)\cr
	  013 \tab (Central America)\cr
	  021 \tab (North America)\cr
	  002 \tab (All of Africa)\cr
	  017 \tab (Central Africa)\cr
	  015 \tab (Northern Africa)\cr
	  018 \tab (Southern Africa)\cr
	  030 \tab (Eastern Asia)\cr
	  034 \tab (Southern Asia)\cr
	  035 \tab (Asia/Pacific region)\cr
	  143 \tab (Central Asia)\cr
	  145 \tab (Middle East)\cr
	  151 \tab (Northern Asia)\cr
	  154 \tab (Northern Europe)\cr
	  155 \tab (Western Europe)\cr
	  039 \tab (Southern Europe)
	}	  
	Geomap does not enable scrolling or dragging behavior, and only
	  limited zooming behavior. A basic zoom out can be enabled by setting
	  the showZoomOut property.
	}
	\item{\code{dataMode}}{string, default	'regions'	
	  How to display values on the map. Two values are supported:
	  \code{regions} - Colors a whole region with the appropriate
	  color. This option cannot be used with latitude/longitude
	  addresses. See Regions Example.
	} 
	\item{\code{markers}}{Displays a dot over a region, with the
	  color and size indicating the value. See Markers Example.
	}
	\item{\code{width}}{string, default '556px'.Width of the
	  visualization. If no units are given, the default unit is pixels.
	}
	\item{\code{height}}{default, string '347px'. Height of the
	  visualization. If no units are given, the default unit is
	  pixels. 
	}
	\item{\code{colors}}{Array of RGB numbers in the format 0xRRGGBB
	  [0xE0FFD4, 0xA5EF63, 0x50AA00, 0x267114]. Color gradient
	  to assign to values in the visualization. You must have at least	  
	  two values; the gradient will include all your values, plus
	  calculated intermediary values, with the lightest color as the
	  smallest value, and the darkest color as the highest.
	}
	\item{\code{showLegend}}{boolean, default \code{TRUE}. If true,
	  display a legend for the map.
	}
	\item{\code{showZoomOut}}{boolean, default \code{FALSE}. If true,
	  display a button with the label specified by the zoomOutLabel
	  property. Note that this button does nothing when clicked,
	  except throw the zoomOut event. To handle zooming, catch this
	  event and change the region option. You can only specify
	  showZoomOut if the region option is smaller than the world
	  view. One way of enabling zoom in behavior would be to listen
	  for the regionClick event, change the region property to the
	  appropriate region, and reload the map.
	}
	\item{\code{zoomOutLabel}}{string, default 'Zoom Out'. Label for
	  the zoom button. 
	}	            
      }
    }
  }
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \code{gvisGeoMap} returns list of \code{\link{class}} "\code{gvis}" and "\code{list}". 
  
  An object of class "\code{gvis}" is a list containing at least the following components:
  \item{\code{type}}{Google visualisation type, here 'GeoMap'}
  \item{\code{chartid}}{character id of the chart object. The string is build
    at run time from chart type and date/time when the function is
    executed. Unique chart ids are required to place several charts on
    the same page.
  }
  \item{\code{html}}{a list with the building blocks for a page
    \describe{
      \item{\code{header}}{a character string of a html page header:
	\code{<html>...<body>},}
      \item{\code{chart}}{a character string of the chart object with
	reference to the Google API. Only this string is needed to embed the
	chart into pages, see also the examples in the \code{googleVis} demo,}
      \item{\code{caption}}{a character string of a standard caption,
	including data set, chart id and R version used; further a link to Google's
	Terms of Use,}
      \item{\code{footer}}{a character string of a html page footer:
	\code{</html>...</body>}.}
    }
  }
}
\references{
  Google Geo Map  API: \url{http://code.google.com/apis/visualization/documentation/gallery/geomap.html}
}
\author{
Markus Gesmann \email{markus.gesmann@gmail.com},

Diego de Castillo \email{decastillo@gmail.com}
}
\note{
  Note: A map can display a maximum of 400 entries; if your DataTable or
  DataView holds more than 400 rows, only the first 400 will be
  shown. The fastest modes are dataMode='regions' with locations
  specified as ISO codes, and dataMode='markers' with lat/long
  entries. The slowest mode is dataMode='markers' with a string
  address. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{

  See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for printing and plotting methods. 

}
\examples{

## Regions Example
## The regions style fills entire regions (typically countries) with colors
## corresponding to the values that you assign. Specify the regions style
## by assigning options['dataMode'] = 'regions' in your code.

G1 <- gvisGeoMap(Exports[,1:2], locationvar='Country', numvar='Profit', options=list(dataMode="regions"))

\dontrun{
plot(G1)
}

## Markers Example
## The "markers" style displays a circle, sized and colored to indicate
## a value, over the regions that you specify. 

G2 <- gvisGeoMap(CityPopularity, locationvar='City' , numvar='Popularity',options=list(region='US',
       dataMode='markers', colors='[0xFF8747, 0xFFB581, 0xc06000]'))	

\dontrun{
plot(G2) 
}
}

\keyword{ hplot }
