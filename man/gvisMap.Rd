\name{gvisMap}
\alias{gvisMap}
\title{
  Google Maps with R
}
\description{
  The gvisMap function reads a data.frame and
  creates text output referring to the Google Visualisation API which can be
  included into a web page, or as a stand-alone page.
  
  The maps are the well known Google Maps. 
}
\usage{
gvisMap(data, locationvar='', tipvar='',
           options = list())
}
\arguments{
  \item{data}{
    a \code{data.frame}. The data has to have at least two
    columns with location name (\code{locationvar})
    and the variable to display the text in the tip icon (\code{tipvar}).
    
  }
  
  \item{locationvar}{column name of \code{data} with the geo locations to be
    analysed. The locations can be provide in two formats:
    \describe{	    
      \item{Format 1}{'latitude:longitude'. See the example below.}
      \item{Format 2}{The first column should be a string that contains
	an address. This address should be as complete as you can make
	it.  
      }
    }
  }
  \item{tipvar}{column name of \code{data} with the string
    text displayed over the tip icon. 
  }
  \item{options}{list of configuration options for Google Map.
    The possible components are, taken from
    \url{http://code.google.com/apis/visualization/documentation/gallery/map.html#Configuration_Options}:
    
    \describe{
      \item{\code{enableScrollWheel}}{boolean. Default \code{FALSE}.	If set
	to \code{TRUE}, enables zooming in and out using the mouse scroll wheel.
      }
      \item{\code{showTip}}{boolean. Default \code{FALSE}.	If set to \code{TRUE}, shows
	the location description as a tool-tip when the mouse is positioned
	above a point marker.
      }
      \item{\code{showLine}}{boolean. Default \code{FALSE}.	If set to \code{TRUE}, shows a
	Google Maps polyline through all the points.
      }
      \item{\code{lineColor}}{string	default color. If showLine is
	\code{TRUE}, defines the line color. For example: '#800000'.
      }
      \item{\code{lineWidth}}{number. Default 10. If showLine is true,
	defines the line width (in pixels).
      }
      \item{\code{mapType}}{string. Default: 'hybrid'. The type of map to
	show. Possible values are 'normal', 'terrain', 'satellite' or
	'hybrid'.
      }
      \item{\code{useMapTypeControl}}{boolean. Default \code{FALSE}.	Show a
	map type selector that enables the viewer to switch between [map,
	satellite, hybrid, terrain]. When \code{useMapTypeControl} is \code{FALSE}
	(default) no selector is presented and the type is determined by the
	\code{mapType} option.
      }
      \item{\code{zoomLevel}}{number/ Default automatic. An integer
	indicating the initial zoom level of the map, where 0 is completely
	zoomed out (whole world) and 19 is the maximum zoom level. (See "Zoom
	Levels" in the Google Maps API.)
	}
      }
    }
  }
  
  %%\details{
  %%  ~~ If necessary, more details than the description above ~~
  %%}
  \value{
  \code{gvisMap} returns list of \code{\link{class}} "\code{gvis}" and "\code{list}". 
  
  An object of class "\code{gvis}" is a list containing at least the following components:
  \item{\code{type}}{Google visualisation type, here 'Map'}
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
	\code{</body>...</html>}.}
    }
  }
}
\note{
  The Lat-Long pairs option loads maps much faster, especially with
  large data. We recommend that you use this option for large data
  sets. Please visit Google Maps API to find out how to transform your
  addresses to lat-long points.
}
\references{
  Google Map  API: \url{http://code.google.com/apis/visualization/documentation/gallery/map.html}

  Follow the link also for Google's Data Policy.
}
\author{
Markus Gesmann \email{markus.gesmann@gmail.com},

Diego de Castillo \email{decastillo@gmail.com}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{

  See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for
  printing and plotting methods, \code{\link{gvisGeoMap}} for an 
  alternative to \code{gvisMap}. 
  
  Further the packages:
  \itemize{   
    \item R2GoogleMaps: Provides a mechanism to generate JavaScript code
    from R that displays data using Google Maps,  
    \url{http://www.omegahat.org/R2GoogleMaps/}. 
    \item RgoogleMaps:  Overlays on Google map tiles in R, 
    \url{http://cran.r-project.org/web/packages/RgoogleMaps/index.html}.
  } 
}
\examples{
## Please note that by default the plot (better plot.gvis) command will
## try to write the output into the rsp/myAnalysis folder of the
## googleVis library. Further the plot command will open a
## browser window and requires Flash and Internet connection to
## display the visualisation.
## Because of those requirements the plot commands are not executed
## automatically as part of the example. 
## Please copy and paste the plot commands manually
## to see the visualisation output, see also ?plot.gvis for further
## details.  
  
## Example with latitude and longitude information
## Plot Hurricane Andrew (1992) storm path:

data(Andrew)

M1 <- gvisMap(Andrew, "LatLong" , "Tip", 
      	      options=list(showTip=TRUE, showLine=TRUE, enableScrollWheel=TRUE,
		      mapType='terrain', useMapTypeControl=TRUE))

\dontrun{
plot(M1) 
}


## Example with address, here UK post-code

df <- data.frame(Postcode=c("EC3M 7HA", "EC2P 2EJ"),
                 Tip=c("Lloyd's", "Guildhall"))
 
M2 <- gvisMap(df, "Postcode", "Tip",
              options=list(showTip=TRUE, mapType='normal',
              enableScrollWheel=TRUE))

\dontrun{
plot(M2) 
}

}

\keyword{ hplot }
