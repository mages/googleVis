\name{gvisIntensityMap}
\alias{gvisIntensityMap}
\title{
Google Intensity Map with R
}
\description{
  An intensity map highlights regions or countries based on relative values.

  The gvisIntensityMap function reads a data.frame and
  creates text output referring to the Google Visualisation API, which can be
  included into a web page, or as a stand-alone page.
}
\usage{
gvisIntensityMap(data, locationvar = "", numvar = "",
                       options = list(), chartid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
    a \code{data.frame}. The data has to have at least two
    columns with location name (\code{locationvar}) and any number of
    numeric columns (\code{numvar}) to be mapped. 
  }
  \item{locationvar}{column name of \code{data} with the geo locations to be
    analysed. The location has to contain country ISO codes or USA state codes.
  }
  \item{numvar}{column names of \code{data} with the numeric values to be
    displayed.
  }
  \item{options}{list of configuration options for Google Intensity Map.
        \describe{
\item{\code{gvis.editor}}{a character label for an
 on-page button which opens an in-page dialog box that enables
users to edit, change and customise the chart. By default no
value is given and therefore no button is displayed. 
      }
    }
    Further possible components are, taken from
    \url{https://google-developers.appspot.com/chart/interactive/docs/gallery/intensitymap.html#Configuration_Options}:
    
    \describe{
      
      \item{\code{colors}}{a JSON array of strings. The colors to use for each
	tab. An array of strings. Each element is a string in the format
	\code{#rrggbb}. For example \code{'#00cc00'}.
      }      
      \item{\code{height}}{a number. Default 220. Height of the map in
	pixels. The maximum height of the visualization is 220. Note
	that this height assumes a one-row tab. If your tab text is
	long, it will wrap the tab to multiple lines, and the extra
	lines will exceed the specified height.
      }
      \item{\code{region}}{a string. Default \code{'world'}. The required
	region. Possible values are: \code{'world', 'africa', 'asia',
	  'europe', 'middle_east', 'south_america', and 'usa'}.
      }
      \item{\code{showOneTab}}{boolean. Default \code{FALSE}. The
	intensity map can display one or more numeric columns. Each
	column is displayed as a separate map, and tabs on top enable
	selection of which map to show. When the data table contains
	only one numeric column, the tabs are not displayed. To display
	tabs even for a single numeric column, set this option to \code{TRUE}.
      }
      \item{\code{width}}{a number. Default 440. Width of the map in
	pixels. Note: The maximum width of the visualization is 440.
      }
    }
  }
  \item{chartid}{character. If missing (default) a random chart id will be generated based on
    chart type and \code{\link{tempfile}}
  }
}
\value{
  \code{gvisIntensityMap} returns list of \code{\link{class}} "\code{gvis}" and "\code{list}". 
  
  An object of class "\code{gvis}" is a list containing at least the following components:
  \item{\code{type}}{Google visualisation type, here 'IntensityMap'}
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
  Google Intensity Map  API: \url{https://google-developers.appspot.com/chart/interactive/docs/gallery/intensitymap.html}

  Follow the link for Google's data policy.

}
\author{
Markus Gesmann \email{markus.gesmann@gmail.com},

Diego de Castillo \email{decastillo@gmail.com}
}
\note{
  Map images are generated using the Google Charts API
  (\url{http://code.google.com/apis/chart/image/docs/gallery/map_charts.html}). Please
  refer to the Chart API logging policy (\url{http://code.google.com/apis/chart/interactive/faq.html#logging}).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{

  See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for
  printing and plotting methods, \code{\link{gvisMap}} and \code{\link{gvisGeoMap}} for an
  alternative to \code{gvisIntensityMap}.  

}
\examples{
## Please note that by default the googleVis plot command
## will open a browser window and requires Internet
## connection to display the visualisation.

df=data.frame(country=c("US", "GB", "BR"), val1=c(1,3,4), val2=c(23,12,32))
Intensity1 <- gvisIntensityMap(df, locationvar="country", numvar=c("val1", "val2"))
plot(Intensity1)

## Set colours for each tab
Intensity2 <- gvisIntensityMap(df,
              options=list(colors="['#4682b4', '#0073CF']"))
plot(Intensity2)


}
\keyword{ iplot }
