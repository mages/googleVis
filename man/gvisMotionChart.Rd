\name{gvisMotionChart}
\alias{gvisMotionChart}

\title{ Google Motion Chart with R}
\description{
   The gvisMotionChart function reads a data.frame and
  creates text output referring to the Google Visualisation API which can be
  included into a web page, or as a standalone page. The actual chart is
  rendered by the web browser in flash.

  A Motion Chart is a dynamic chart to explore several indicators over time.

}
\usage{
gvisMotionChart(data, idvar = "id", timevar = "time", 
		      date.format = "\%Y/\%m/\%d", 
		      options = list())
}

\arguments{
  \item{data}{
    a \code{data.frame}. The data has to have at least four
    columns with subject name (\code{idvar}), time (\code{timevar}) and
    two columns of numeric values. Further columns, numeric and
    character/factor are optional. The combination of \code{idvar} and
    \code{timevar} has to describe a unique row. 
  }
  \item{idvar}{column name of \code{data} with the subject to be
    analysed.
  }
  \item{timevar}{
    column name of \code{data} which shows the time dimension. The
    information has to be either numeric or of class \code{\link{Date}}.
  }
  \item{date.format}{
    if \code{timevar} is of class \code{\link{Date}} then this argument
    specifies how the dates are reformatted to be used by javascript. 
  }
  \item{options}{list of configuration options for Google Motion Chart.
    The possible components are, taken from
    \url{http://code.google.com/apis/visualization/documentation/gallery/motionchart.html#Configuration_Options}: 
    \describe{
      \item{\code{gvis.language}}{values may be 'ca', 'da', 'de', 'en', 'en_GB', 'en_IE', 'es', 
	'es_419', 'fi', 'fr', 'id', 'in', 'is', 'it', 'nl', 'no', 'pt', 'pt_BR', 'pt_PT', 'sv'. If not set the API
	detects the language settings of the browser.
      } 
      \item{\code{height}}{height of the chart in pixels.
      }
      \item{\code{width}}{width of the chart in pixels.
      }
      \item{\code{state}}{An initial display state for the chart. This
	is a serialised JSON object that describes zoom level, selected
	dimensions, selected bubbles/entities, and other state
	descriptions. For more details see
	\url{http://code.google.com/apis/visualization/documentation/gallery/motionchart.html#Motion_Chart_initial_state}
      }
      \item{\code{showChartButtons}}{logical, default=\code{TRUE}.
	\code{FALSE} hides the buttons that control the chart type (bubbles /
	lines / columns) at top right corner. 
      }
      \item{\code{showHeader}}{logical, default=\code{TRUE}.
	\code{FALSE} hides the title label of the entities (derived from the
	label of the first column in the data table).
      }
      \item{\code{showSelectListComponent}}{logical,
	default=\code{TRUE}.
	\code{FALSE} hides the list of visible entities.
      }
      \item{\code{showSidePanel}}{logical, default=\code{TRUE}.
	\code{FALSE} hides the right hand panel.
      }
      \item{\code{showXMetricPicker}}{logical, default=\code{TRUE}.
	\code{FALSE} hides the metric picker for x.
      }
      \item{\code{showYMetricPicker}}{logical, default=\code{TRUE}.
	\code{FALSE} hides the metric picker for y. 
      }
      \item{\code{showXScalePicker}}{logical, default=\code{TRUE}.
	\code{FALSE} hides the scale picker for x. 
      }	
      \item{\code{showYScalePicker}}{logical, default=\code{TRUE}.
	\code{FALSE} hides the scale picker for y.  
      }	
      \item{\code{showAdvancedPanel}}{logical, default=\code{TRUE}.
	\code{FALSE} hides disables the options compartment in the settings panel.
      }
      
    } 
  }
}
%%\details{
%%
%%}

\value{
  \code{gvisMotionChart} returns list of \code{\link{class}} "\code{gvis}" and "\code{list}". 
  
  An object of class "\code{gvis}" is a list containing at least the following components:
  \item{\code{type}}{Google visualisation type, here 'MotionChart'}
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
\references{
  Google Motion Chart API:
  \url{http://code.google.com/apis/visualization/documentation/gallery/motionchart.html} 

  Follow the link also for Google's Data Policy.
}
\author{
  Markus Gesmann \email{markus.gesmann@gmail.com},
  
  Diego de Castillo \email{decastillo@gmail.com}
}
\note{
  
  In 2006 Hans Rosling gave an inspiring talk at TED
  \url{http://www.ted.com/talks/hans_rosling_shows_the_best_stats_you_ve_ever_seen.html}
  about social and economic developments in  the world over the last 50
  years, which challenged the views and perceptions of many
  listeners. Rosling had used extensive data analysis to come to his
  conclusions.  To visualise his talk, he and his team at
  Gapminder \url{http://www.gapminder.org} had developed animated bubble charts, aka
  motion charts.

  In March 2007 Google acquired Trendalyzer from the Gapminder
  Foundation and the Gapminder team of developers joined Google in
  California in April 2007.
  
  Yihui Xie, the maintainer of the \code{animation-package}
  showed in an early blog entry the usage of the Google Motion Chart API
  with R:

  \url{http://yihui.name/en/2008/11/brownian-motion-using-google-visualization-api-and-r/} 
  
  Further examples of displaying data with motion charts are available
  via the Google Public Data Explorer:
  \url{http://www.google.com/publicdata/home}.

  Stephen Thompson at Lloyd's developed an Excel version that mimics
  much of the functionality of the Google motion charts: 	  
  \url{http://www.lloyds.com/The-Market/Tools-and-Resources/Resources/Statistics-Relating-to-Lloyds/Visualisation}
  
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
  See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for
  printing and plotting methods, further \code{\link{browseRsp}}, and also the
  \code{animation-package} and its function \code{Rosling.bubbles}.
}

\examples{
## Please note that by default the plot (better plot.gvis) command will
## try to write the output into the rsp/myAnalysis folder of the
## googleVis library. Further the plot command will open a
## browser window and requires Flash and internet connection to
## display the visualisation.
## Because of those requirements the plot commands are not executed
## automatically as part of the example. 
## Please copy and paste the plot commands manually
## to see the visualisation output, see also ?plot.gvis for further
## details.  

Fruits	
## timevar Year
M1 <- gvisMotionChart(Fruits, idvar="Fruit", timevar="Year")
str(M1)
## print.gvis, will concatenate the list into a one long string
M1

## plot.gvis will create a file and open it in your browser.
\dontrun{
plot(M1)
}

\dontrun{
## Usage of date variable
M2 <- gvisMotionChart(Fruits, idvar="Fruit", timevar="Date",
                               date.format = "/%Y/%m/%d") 
plot(M2)
}
## Options: no side panel on the right
M3 <- gvisMotionChart(Fruits,"Fruit","Year",
                 options=list(showSidePanel=FALSE))
\dontrun{
plot(M3)
}

## Options: trails un-ticked
M4 <- gvisMotionChart(Fruits, "Fruit", "Year",
                 options=list(state='{\"showTrails\":false};'))

\dontrun{
plot(M4)
}
}
\keyword{ hplot }
