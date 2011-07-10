\name{gvisGeoMap}
\alias{gvisGeoMap}
\title{
  Google Geo Map with R
}
\description{
  The gvisGeoMap function reads a data.frame and
  creates text output referring to the Google Visualisation API which can be
  included into a web page, or as a stand-alone page.
 
  A Geo map is a map of a country, continent, or region map, with colours
  and values assigned to specific regions. Values are displayed as a colour
  scale, and you can specify optional hover-text for regions. The map is
  rendered in the browser. Note that the
  map is not scroll-able or drag-gable, but can be configured to allow
  zooming.

}
\usage{
gvisGeoMap(data, locationvar='', numvar='', hovervar='',
           options = list(), chartid)
}
\arguments{
  \item{data}{
    a \code{data.frame}. The data has to have at least two
    columns with location name (\code{locationvar}), value to be mapped
    to location (\code{numvar}) and an optional variable to display any
    text while the mouse hovers over the location (\code{hovervar}).
    
    %%Your DataTable must include every optional column preceding any
    %%column that you want to use. So, for example, if you want to specify
    %%a lat/long table, and only wanted to use columns 1, 2, and 4, your
    %%DataTable must still define column 3 (though you don't need to add
    %%any values to it): 
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
  \item{numvar}{column name of \code{data} with the numeric value
    displayed when the user hovers over this region.
  }
  \item{hovervar}{column name of \code{data} with the additional string
    text displayed when the user hovers over this region. 
  }
  \item{options}{list of configuration options for Google Geo Map.
    The possible components are, taken from
    \url{http://code.google.com/apis/visualization/documentation/gallery/geomap.html#Configuration_Options}:
  
    \describe{
      \item{\code{region}}{string, default \code{'world'}.	
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
	\item{\code{dataMode}}{string, default	'regions'.	
	  How to display values on the map. Two values are supported:

	  \code{regions} - Colors a whole region with the appropriate
	  color. This option cannot be used with latitude/longitude
	  addresses. See Regions Example.
	  
	  \code{markers} - Displays a dot over a region, with the
	  color and size indicating the value. See Markers Example.
	}
	\item{\code{width}}{string, default '556px'. Width of the
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
	  \code{showZoomOut} if the region option is smaller than the world
	  view. One way of enabling zoom in behavior would be to listen
	  for the regionClick event, change the region property to the
	  appropriate region, and reload the map.
	}
	\item{\code{zoomOutLabel}}{string, default 'Zoom Out'. Label for
	  the zoom button. 
	}	                  
	\item{\code{gvis.listener.jscode}}{ character string which will be
	  placed inside select event. A valid value is
	  \code{alert('a region was selected');}.
	  You may also use the method \code{getSelection}.
	}      
	%%	\item{\code{gvis.listener.jscode}}{ character string which will be
	%%	  placed inside select event. A valid value is
	%%	  \code{alert('a table row was selected');}.
	%%	  You may also use the method \code{getSelection}.
	%%	}
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
  \code{gvisGeoMap} returns list of \code{\link{class}} "\code{gvis}" and "\code{list}". 
  
  An object of class "\code{gvis}" is a list containing at least the following components:
  \item{\code{type}}{Google visualisation type, here 'GeoMap'}
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
  Google Geo Map  API: \url{http://code.google.com/apis/visualization/documentation/gallery/geomap.html}

  Follow the link for Google's data policy.

}
\author{
Markus Gesmann \email{markus.gesmann@gmail.com},

Diego de Castillo \email{decastillo@gmail.com}
}
\note{
  A map can display a maximum of 400 entries; if your DataTable or
  DataView holds more than 400 rows, only the first 400 will be
  shown. The fastest modes are \code{dataMode='regions'} with locations
  specified as ISO codes, and \code{dataMode='markers'} with lat/long
  entries. The slowest mode is \code{dataMode='markers'} with a string
  address. 
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
  printing and plotting methods, \code{\link{gvisGeoChart}} and
  \code{\link{gvisIntensityMap}} for an alternative to \code{gvisGeoMap}.  
  
}
\examples{
## Please note that by default the googleVis plot command
## will open a browser window and requires Internet
## connection to display the visualisation.

## Regions Example
## The regions style fills entire regions (typically countries) with colors
## corresponding to the values that you assign. Specify the regions style
## by assigning options['dataMode'] = 'regions' in your code.

G1 <- gvisGeoMap(Exports, locationvar='Country', numvar='Profit',
                 options=list(dataMode="regions")) 

plot(G1)

## Markers Example
## The "markers" style displays a circle, sized and colored to indicate
## a value, over the regions that you specify. 

G2 <- gvisGeoMap(CityPopularity, locationvar='City', numvar='Popularity',
                 options=list(region='US', height=350, 
                              dataMode='markers',
                              colors='[0xFF8747, 0xFFB581, 0xc06000]'))	

plot(G2) 

## Example showing US data by state 

require(datasets)
states <- data.frame(state.name, state.x77)

G3 <- gvisGeoMap(states, "state.name", "Illiteracy",
                 options=list(region="US", dataMode="regions",
		 width=600, height=400))
plot(G3) 

## Example with latitude and longitude information
## Show Hurricane Andrew (1992) storm track
G4 <- gvisGeoMap(Andrew, locationvar="LatLong", numvar="Speed_kt", 
      			 hovervar="Category", 
      		         options=list(height=350, region="US", dataMode="markers"))

plot(G4) 

## World population
WorldPopulation=data.frame(Country=Population$Country, 
			   Population.in.millions=round(Population$Population/1e6,0),
			   Rank=paste(Population$Country, "Rank:", Population$Rank))

G5 <- gvisGeoMap(WorldPopulation, "Country", "Population.in.millions", "Rank", 
      		      options=list(dataMode="regions", width=600, height=300))
plot(G5)

\dontrun{
library(XML)
## Get earthquake data of the last 30 days
eq=readHTMLTable("http://www.iris.edu/seismon/last30.html")
eq <- eq[[2]] ## extract the eq table
eq$loc=paste(eq$LAT, eq$LON, sep=":") ## create a lat:long location variable
plot(gvisGeoMap(eq, "loc", "MAG","DATE"))


## The demo 'AnimatedGeoMap' shows how a Geo Map can be animated
## with additional JavaScript. 
## Thanks to Manoj Ananthapadmanabhan and Anand Ramalingam, who
## provided the idea and initial code.
## Please note: This demo requires the package 'pscl' for its data set
## 'presidentalElections'.

demo(AnimatedGeoMap)
}

}

\keyword{ iplot }
