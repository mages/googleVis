\name{Google Visulisation Charts}
\alias{gvisMotionChart}
\alias{gvisGeoMap}
\alias{gvisTable}
\alias{gvisTreeMap}

\title{ R interfaces to the Google Visualisation API}
\description{

  A Motion Chart is a dynamic flash based bubble chart to explore
  several indicators over time.
  The gvisMotionChart function reads a data.frame and
  creates text output referring to the Google Visualisation API which can be
  included into a web page, or as a standalone page. The actual chart is
  rendered by the web browser in flash.
}
\usage{
gvisMotionChart(data, idvar = "id", timevar = "time", 
		      date.format = "\%Y/\%m/\%d", 
		      options = list())
gvisGeoMap(data, options = list())
gvisTable(data, options = list())
gvisTreeMap(data, idvar = "", parentvar = "", options = list())
}

\arguments{
  \item{data}{
    a \code{data.frame}. The data has to have at least four
    columns with subject name (\code{idvar}), time (\code{timevar}) and two columns of
    numeric values. Further columns, numeric and character/factor are optional. 
    The combination of \code{idvar} and \code{timevar} has to describe a unique row.
  }
  \item{idvar}{column name of \code{data} with the subject to be
    analysed.
  }
  \item{parentvar}{column name of \code{data} that match to entries in idvar..
  }
  \item{timevar}{
    column name of \code{data} which shows the time dimension. The information has to
    be either numeric or of class \code{\link{Date}}.
  }
  \item{date.format}{
    if \code{timevar} is of class \code{\link{Date}} than this argument specifies how the dates are reformatted to be used by javascript.
  }
  \item{options}{list of configuration options for the motion chart.
      The possible components are :

      \describe{
	\item{\code{height}}{height of the chart in pixels.
	}
	\item{\code{width}}{width of the chart in pixels.
	}
	\item{\code{state}}{An initial display state for the chart. This
	is a serialised JSON object that describes zoom level, selected
	dimensions, selected bubbles/entities, and other state
	descriptions.}
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

      } More information on:    
    \url{http://code.google.com/apis/visualization/documentation/gallery/motionchart.html#Motion_Chart_initial_state}
  }
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}

\value{
\code{gvisMotionChart} return an object of \code{\link{class}} "\code{gvis}" and "\code{list}". 
An object of class "\code{gvis}" is a list containing at least the following components:
  \item{htmlHeader}{html header text}
  \item{htmlChart}{html chart object, referring to Google}
  \item{htmlCaption}{chart caption}
  \item{htmlFooter}{html footer text}

}
\references{
  Motion charts have been popularised by Hans Rosling, who has used them
  to illustrate social and economic transitions over time in the
  development world.

  You can see his fascinating TED talk where he used motion charts
  extensively here:
  \url{http://www.ted.com/talks/hans_rosling_shows_the_best_stats_you_ve_ever_seen.html}
  
  Further examples and data can be found on \url{http://www.gapminder.org}.

  Google has made a very similar API available which we are using
  here. Google motion charts are documented in detail as part of their
  Visualisation API here:
  \url{http://code.google.com/apis/visualization/documentation/gallery/motionchart.html}. Please
  note that no data is send to Google.

  Yihui Xie, the maintainer of the \code{animation-package}
  showed in an early blog entry the usage of the Google Motion Chart API
  with R: \url{http://yihui.name/en/2008/11/brownian-motion-using-google-visualization-api-and-r/}
}
\author{
Markus Gesmann, \email{markus.gesmann@googlemail.com},

Diego de Castillo \email{decastillo@gmail.com}
}
\note{
  See \code{vignette("googleVis")} for a brief introduction of
  the graphical user interface of Google motion charts.

  Further examples of displaying data with motion charts are available with the Google Public Data Explorer:
  \url{http://www.google.com/publicdata/home}.

  Many thanks to Henrik Bengtsson for providing the R.rsp package
  \url{http://www.braju.com/R/}, which allows us to display motion
  charts locally.

  Type \code{browseRsp(path="/library/googleVis/rsp/")} to find out a
  bit more how you can integrate motion charts into rsp-pages.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for printing and plotting methods. 
Further see also \code{\link{browseRsp}}, and also the
\code{animation-package} and there the function \code{Rosling.bubbles}
}
\examples{

 Fruits	
 ## timevar Year
 M <- gvisMotionChart(Fruits, idvar="Fruit", timevar="Year")
 str(M)
 ## print.gvis, will concatenate the list into a one long string
 M

\dontrun{

 M <- gvisMotionChartPage(Fruits, idvar="Fruit", timevar="Year")

## plot.gvis will create a tempfile and open it in your browser.
 plot(M)


## Usage of date variable
 gvisMotionChart(Fruits, idvar="Fruit", timevar="Date")


## Options: no side panel on the right
 gvisMotionChart(Fruits,"Fruit","Year",
                 options=list(showSidePanel=FALSE))

## Options: trails unticked
 gvisMotionChart(Fruits,"Fruit","Year",
                 options=list(state='{"showTrails":false};')
                              )
}
## For more details see also the demos.
## Type: demo(package='googleVis') to see a list of available demos.
}
\keyword{ hplot }
