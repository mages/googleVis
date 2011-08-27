\name{gvisGeoChart}
\alias{gvisGeoChart}
\title{
  Google Geo Chart with R
}
\description{
  The gvisGeoChart function reads a data.frame and
  creates text output referring to the Google Visualisation API which can be
  included into a web page, or as a stand-alone page.

  A geo chart is an SVG-based map of a country, continent, or region map,
  with colours and values assigned to specific regions. Values are
  displayed as a color scale, and you can specify optional hovertext for
  regions. Note that the
  map is not scrollable or draggable.
  
}
\usage{
gvisGeoChart(data, locationvar = "", numvar = "", options = list(), chartid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
    a \code{data.frame}. The data has to have at least two
    columns with location name (\code{locationvar}), value to be mapped
    to location (\code{numvar}).
  }
  \item{locationvar}{column name of \code{data} with the country name,
    or an uppercase ISO-3166 code.    
  }
  \item{numvar}{column name of \code{data} with the numeric value
    displayed when the user hovers over this region.
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
    \url{http://code.google.com/apis/visualization/documentation/gallery/geochart.html#Configuration_Options}:
    
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
      }
      \item{\code{width}}{number, default 556. Width of the
	visualization. If no units are given, the default unit is pixels.
      }
      \item{\code{height}}{number, default 347. Height of the
	visualization. If no units are given, the default unit is
	pixels. 
      }
      \item{\code{backgroundColor}}{a string or object. Default
	\code{white}. The background color for the main area of the
	chart. Can be either a simple HTML color string, for example:
	\code{'red'} or \code{'#00cc00'}, or an object with the following properties.
      }
      \item{\code{backgroundColor.stroke}}{a string. Default
	\code{'#666'}. The color of the chart border, as an HTML color
	string.
      }
      \item{\code{backgroundColor.strokeWidth}}{a number. Default
	0. The border width, in pixels.
      }
      \item{\code{backgroundColor.fill}}{a string. Default
	\code{white}. The chart fill color, as an HTML color string.
      }
      \item{\code{colors}}{a json array of strings. Default
	colors. Color gradient to assign to values in the
	visualization. An array of strings, where each element is an HTML
	color string, for example: \code{colors:['red','#004411']}. You must have
	at least two values; the gradient will include all your values,
	plus calculated intermediary values, with the first color as the
	smallest value, and the last color as the highest.
      }
      \item{\code{resolution}}{a string. Default \code{'countries'}
	The resolution of the map borders. Choose one of the following values:
	\describe{
	  \item{\code{'countries'}}{
	  }
	  \item{\code{'provinces'}}{ Not supported for all countries;
	    please test a country to see whether this option is supported.
	  }
	  \item{\code{'metros'}}{USA only.
	  }
	}
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
  Google Geo Map  API: \url{http://code.google.com/apis/visualization/documentation/gallery/geochart.html}

  Follow the link for Google's data policy.

}
\author{
Markus Gesmann \email{markus.gesmann@gmail.com},

Diego de Castillo \email{decastillo@gmail.com}
}
%\note{
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{

  See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for
  printing and plotting methods, \code{\link{gvisGeoMap}} and \code{\link{gvisIntensityMap}} for an
  alternative to \code{gvisGeoChart}.  
  
}
\examples{
## Please note that by default the googleVis plot command
## will open a browser window and requires Internet
## connection to display the visualisation.


G1 <- gvisGeoChart(Exports, locationvar='Country', numvar='Profit') 

plot(G1)

## Plot only Europe
G2 <- gvisGeoChart(Exports, "Country", "Profit",
                   options=list(region="150"))

plot(G2)

}
\keyword{ iplot }
