\name{gvisGauge}
\alias{gvisGauge}
\title{
Google Gauge with R
}
\description{
  The gvisGauge function reads a data.frame and
  creates text output referring to the Google Visualisation API which can be
  included into a web page, or as a stand-alone page. The actual chart is
  rendered by the web browser using SVG or
  VML. 
}
\usage{
gvisGauge(data, labelvar = "", numvar = "", options = list(), chartid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a \code{\link{data.frame}} to be displayed as a gauge
  }
  \item{labelvar}{name of the character column which contains the category labels for
    the slice labels.
  }
  \item{numvar}{a vector of column names of the numerical variables of the slice values. 
  }
  \item{options}{list of configuration options for Google Gauge.
    The possible components are, taken from
    \url{http://code.google.com/apis/visualization/documentation/gallery/gauge.html#Configuration_Options}:
    \describe{
      \item{\code{greenColor}}{a string. Default \code{'#109618'}. The color
	to use for the green section, in HTML color notation.
      }
      \item{\code{greenFrom}}{a	number. Default	none. The lowest value
	for a range marked by a green color.
      }
      \item{\code{greenTo}}{a number. Default	none. The highest value
	for a range marked by a green color.
      }
      \item{\code{height}}{a number. Default container's width.	Height
	of the chart in pixels.
      }
      \item{\code{majorTicks}}{an json array of strings. Default none.
	Labels for major tick marks. The number of labels define the
	number of major ticks in all gauges. The default is five major
	ticks, with the labels of the minimal and maximal gauge value.
	}
	\item{\code{max}}{a number. Default 100. The maximal
	  value of a gauge.
	}
	\item{\code{min}}{a number. Default 0. The minimal value of a
	  gauge.
	}
	\item{\code{minorTicks}}{a number. Default 2. The number of
	  minor tick section in each major tick section.
	}
	\item{\code{redColor}}{a string. Default \code{'#DC3912'}. The
	  color to use for the red section, in HTML color notation.
	}
	\item{\code{redFrom}}{a number. Default	none. The lowest value
	  for a range marked by a red color.
	}
	\item{\code{redTo}}{a number. Default none. The highest value
	  for a range marked by a red color.
	}
	\item{\code{width}}{a number. Default container's width.
	  Width of the chart in pixels.
	}
	\item{\code{yellowColor}}{a string. Default '#FF9900'.	The
	  color to use for the yellow section, in HTML color notation.
	}
	\item{\code{yellowFrom}}{a number. Default none.	The
	  lowest value for a range marked by a yellow color.
	}
	\item{\code{yellowTo}}{a number/ Default none. The highest value
	  for a range marked by a yellow color.
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
  \code{gvisGauge} returns list of \code{\link{class}}
  "\code{gvis}" and "\code{list}".   
  
  An object of class "\code{gvis}" is a list containing at least the
  following components:
  \item{\code{type}}{Google visualisation type, here 'Gauge'}
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
  Google Gauge API:
  \url{http://code.google.com/apis/chart/interactive/docs/gallery/gauge.html} 

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

Gauge1 <- gvisGauge(CityPopularity, options=list(min=0, max=800, greenFrom=500,
                    greenTo=800, yellowFrom=300, yellowTo=500,
                    redFrom=0, redTo=300))

plot(Gauge1)
}
\keyword{ iplot }
