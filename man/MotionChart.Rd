%% File man/MotionChart.Rd
%% Part of the R package GoogleMotionChart
%% Copyright 2010 Markus Gesmann
%% Distributed under GPL 2 or later

\name{Google Motion charts}
\alias{GoogleMotionChart}
\alias{MotionChart}
\alias{MotionChartPage}
\title{
  Visualising data with the Google motion chart API.
}
\description{
  A Motion Chart is a dynamic flash based bubble chart to explore
  several indicators over time.
  The MotionChart and MotionChartPage functions read a data.frame and
  create text output using the Google Visualisation API to be either
  included into a web page, or as a standalone page. The actual chart is
  rendered by the web browser in flash.
}
\usage{
MotionChart(data,
            idvar = "id",
            timevar = "time",
            date.format="\%Y/\%m/\%d",
            options=list(width = 600, height = 500),
            file = "",
            append = TRUE)
MotionChartPage(data,
                idvar = "id",
                timevar = "time",
                date.format = "\%Y/\%m/\%d",
                options=list(width = 600, height = 500),
                title = paste("Motion Chart:",
                          deparse(substitute(data))), 
                caption = paste("", Sys.time(),
                           R.Version()$version.string, sep = "<BR>"), 
                file = "",
                dirname = system.file(file.path("rsp", "myAnalysis"),
                           package = "GoogleMotionChart"), 
                repos = paste("http://127.0.0.1:8074/",
                           basename(dirname(system.file(package =
                           "GoogleMotionChart"))), 
                           "/GoogleMotionChart/rsp/myAnalysis/", sep = ""), 
                view = TRUE)

}
\arguments{
  \item{data}{
    a \code{data.frame}. The data has to have at least four
    columns with subject name (\code{idvar}), time (\code{timevar}) and two columns of
    numeric values. Further columns are optional.
  }
  \item{idvar}{column name of \code{data} with the subject to be
    analysed.
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
	is a serialized JSON object that describes zoom level, selected
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

      } More informtion on:    
    \url{http://code.google.com/apis/visualization/documentation/gallery/motionchart.html#Motion_Chart_initial_state}
  }
  \item{title}{
    title for the html page. Default mentions the data set used.
  }
  \item{caption}{
    caption to be written under the motion chart. Default is date and R version.
  }
  \item{file}{
    file name to be used internally by \code{\link{cat}}. If "" (the
    default), output will be printed to the standard output connection, the console unless
    redirected by \code{\link{sink}}
  }
  \item{dirname}{directory name in which the output file will be
    stored. By default a sub-directory of the \code{GoogleMotionChart}
    library folder will be used, which is also accessible by the web server.
  }
  \item{repos}{repository for the output file. By default the rsp-folder
    of the \code{GoogleMotionChart} will be used.
  }  
  \item{append}{
    logical. If TRUE output will be appended to file;
    otherwise, it will overwrite the contents of file. See also \code{\link{cat}}
  }
   \item{view}{logical. If TRUE a web browser will be started to view the
    motion chart, see also \code{\link{browseRsp}}.
  }
  
}
\details{
  \code{MotionChart} provides you with output which can be included
  directly into a html page, either via copy and paste, or if you use
  \code{\link{R.rsp}} you can include it with the following statement:
  \code{<\%@include file="myfilename.rsp"\%>}. Type \code{browseRsp()}
  to find some examples.
    
  \code{MotionChartPage} uses the function \code{MotionChart} and warps it
  into a complete html page, and if the argument \code{view} is set to
  \code{TRUE}, the page can be immediately displayed in a web browser
  using the technology of the \code{R.rsp} package. Please note also
  that Flash is required.
}
\value{
  By default \code{MotionChart} and \code{MotionChartPage} return the file
  name into which the output was written.
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

  Yihui Xie, the maintainer of the \code{\link[animation]{animation-package}}
  showed in an early blog entry the usage of the Google Motion Chart API
  with R: \url{http://yihui.name/en/2008/11/brownian-motion-using-google-visualization-api-and-r/}
}
\author{
Markus Gesmann, \email{markus.gesmann@googlemail.com}
}
\note{
  See \code{vignette("GoogleMotionChart")} for a brief introduction of
  the graphical user interface of Google motion charts.

  Further examples of displaying data with motion charts are available with the Google Public Data Explorer:
  \url{http://www.google.com/publicdata/home}.

  Many thanks to Henrik Bengtsson for providing the R.rsp package
  \url{http://www.braju.com/R/}, which allows us to display motion
  charts locally.

  Type \code{browseRsp(path="/library/GoogleMotionChart/rsp/")} to find out a
  bit more how you can integrate motion charts into rsp-pages.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{browseRsp}}, and also the
\code{\link[animation]{animation-package}} and there the function \code{\link[animation]{Rosling.bubbles}}
}
\examples{

 Fruits	
 ## timevar Year
 MotionChart(Fruits, idvar="Fruit", timevar="Year")

 ## timevar Date
 MotionChart(Fruits, idvar="Fruit", timevar="Date")

\dontrun{
 MotionChartPage(Fruits, idvar="Fruit", timevar="Year",
                 file="myFruitAnalysisYear.rsp")

## Usage of date variable
 MotionChartPage(Fruits, idvar="Fruit",
                 timevar="Date", file="myFruitAnalysisDate.rsp")


## Options: no side panel on the right
 MotionChartPage(Fruits,"Fruit","Year", file="test.rsp",
                 options=list(width=800, height=500,
                              showSidePanel=FALSE)

## Options: trails unticked
 MotionChartPage(Fruits,"Fruit","Year", file="test.rsp",
                 options=list(width=600,
                              height=500,
                              state='{"showTrails":false};')
                              )

}
## For more details see also the demos.
## Type: demo(package='GoogleMotionChart') to see a list of available demos.
}
\keyword{ aplot }
