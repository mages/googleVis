\name{Google Motion Chart}
\alias{gvisMotionChart}

\title{ Google Motion Chart with R}
\description{
  A Motion Chart is dynamic chart to explore several indicators over time.
  
  The gvisMotionChart function reads a data.frame and
  creates text output referring to the Google Visualisation API which can be
  included into a web page, or as a standalone page. The actual chart is
  rendered by the web browser in flash.
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
    if \code{timevar} is of class \code{\link{Date}} than this argument
    specifies how the dates are reformatted to be used by javascript. 
  }
  \item{options}{list of configuration options for the motion chart.
    The possible components are, taken from
  \url{http://code.google.com/apis/visualization/documentation/gallery/motionchart.html}: 
  \describe{
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
	\code{</html>...</body>}.}
    }
  }
}
\references{
  Google Motion Chart API:
  \url{http://code.google.com/apis/visualization/documentation/gallery/motionchart.html} 
  
  See \code{vignette("googleVis")} for a brief introduction of
  the graphical user interface of Google motion charts.
}
\author{
  Markus Gesmann \email{markus.gesmann@gmail.com},
  
  Diego de Castillo \email{decastillo@gmail.com}
}
\note{
  Motion charts have been popularised by Hans Rosling, who has used them
  to illustrate social and economic transitions over time in the
  development world. You can see his fascinating TED talk where he used
  motion charts extensively here:
  \url{http://www.ted.com/talks/hans_rosling_shows_the_best_stats_you_ve_ever_seen.html}
  
  Further examples and data can be found on \url{http://www.gapminder.org}.
  
  Yihui Xie, the maintainer of the \code{animation-package}
  showed in an early blog entry the usage of the Google Motion Chart API
  with R:
  \url{http://yihui.name/en/2008/11/brownian-motion-using-google-visualization-api-and-r/} 
  
  Further examples of displaying data with motion charts are available
  with the Google Public Data Explorer:
  \url{http://www.google.com/publicdata/home}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
  See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for printing and plotting methods. 
  Further see also \code{\link{browseRsp}}, and also the
  \code{animation-package} and there the function \code{Rosling.bubbles}.
}
\examples{

 Fruits	
 ## timevar Year
 M1 <- gvisMotionChart(Fruits, idvar="Fruit", timevar="Year")
 str(M1)
 ## print.gvis, will concatenate the list into a one long string
 M1

\dontrun{

 M2 <- gvisMotionChartPage(Fruits, idvar="Fruit", timevar="Year")

## plot.gvis will create a tempfile and open it in your browser.
 plot(M2)


## Usage of date variable
 M3 <- gvisMotionChart(Fruits, idvar="Fruit", timevar="Date")
 plot(M3)

## Options: no side panel on the right
 M4 <- gvisMotionChart(Fruits,"Fruit","Year",
                 options=list(showSidePanel=FALSE))
 plot(M4)

## Options: trails unticked
 M5 <- gvisMotionChart(Fruits,"Fruit","Year",
                 options=list(state='{"showTrails":false};'))
 plot(M5)
}
## For more details see also the demos.
## Type: demo(package='googleVis') to see a list of available demos.
}
\keyword{ hplot }
