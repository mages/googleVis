\name{gvisMotionChart}
\alias{gvisMotionChart}

\title{Google Motion Chart with R}
\description{
  The gvisMotionChart function reads a data.frame and
  creates text output referring to the Google Visualisation API, which can be
  included into a web page, or as a stand-alone page. The actual chart is
  rendered by the web browser in flash.

  A motion chart is a dynamic chart to explore several indicators over time.

}
\usage{
gvisMotionChart(data, idvar = "id", timevar = "time", 
		      date.format = "\%Y/\%m/\%d", 
		      options = list(), chartid)
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
    information has to be either numeric, of class \code{\link{Date}} or
    a character which follows the pattern  'YYYYWww' (e.g. '2010W04' for
    weekly data) or 'YYYYQq' (e.g. '2010Q1' for quarterly data).
  }
  \item{date.format}{
    if \code{timevar} is of class \code{\link{Date}} then this argument
    specifies how the dates are reformatted to be used by JavaScript. 
  }
  \item{options}{list of configuration options for Google Motion Chart.
    \describe{
      \item{\code{gvis.editor}}{a character label for an
	on-page button which opens an in-page dialog box that enables
	users to edit, change and customise the chart. By default no
	value is given and therefore no button is displayed. 
      }
      \item{\code{gvis.language}}{values may be 'ca', 'da', 'de', 'en', 'en_GB', 'en_IE', 'es', 
	'es_419', 'fi', 'fr', 'id', 'in', 'is', 'it', 'nl', 'no', 'pt', 'pt_BR', 'pt_PT', 'sv'. If not set the API
	detects the language settings of the browser.
      }   
    }
    Further possible components are, taken from
    \url{https://google-developers.appspot.com/chart/interactive/docs/gallery/motionchart.html#Configuration_Options}: 
    \describe{
      \item{\code{height}}{height of the chart in pixels.
      }
      \item{\code{width}}{width of the chart in pixels.
      }
      \item{\code{state}}{An initial display state for the chart. This
	is a serialised JSON object that describes zoom level, selected
	dimensions, selected bubbles/entities, and other state
	descriptions. For more details see
	\url{https://google-developers.appspot.com/chart/interactive/docs/gallery/motionchart.html#Motion_Chart_initial_state}
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
  \item{chartid}{character. If missing (default) a random chart id will be generated based on
    chart type and \code{\link{tempfile}}
  }
}
%%\details{
%%
%%}

\value{
  \code{gvisMotionChart} returns list of \code{\link{class}}
  "\code{gvis}" and "\code{list}".   
  
  An object of class "\code{gvis}" is a list containing at least the
  following components:
  \item{\code{type}}{Google visualisation type, here 'MotionChart'}
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
  Google Motion Chart API:
  \url{https://google-developers.appspot.com/chart/interactive/docs/gallery/motionchart.html} 

  Follow the link for Google's data policy.

  In 2006 Hans Rosling gave an inspiring talk at TED
  \url{http://www.ted.com/talks/hans_rosling_shows_the_best_stats_you_ve_ever_seen.html}
  about social and economic developments in  the world over the last 50
  years, which challenged the views and perceptions of many
  listeners. Rosling had used extensive data analysis to reach his
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
\section{Warnings}{

  Because of Flash security settings the chart might not work correctly when accessed from a file
  location in the browser (e.g., file:///c:/webhost/myhost/myviz.html)
  rather than from a web server URL
  (e.g. http://www.myhost.com/myviz.html). See the googleVis package
  vignette and the Macromedia web 
  site (\url{http://www.macromedia.com/support/documentation/en/flashplayer/help/})
  for more details.

}

\author{
  Markus Gesmann \email{markus.gesmann@gmail.com},
  
  Diego de Castillo \email{decastillo@gmail.com}
}
\note{
  Please note that a \code{timevar} with values less than 100 will be
  shown as years 19xx.

  
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
  See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for
  printing and plotting methods, and also the
  \code{animation-package} and its function \code{Rosling.bubbles}.
}

\examples{
## Please note that by default the googleVis plot command
## will open a browser window and requires Flash and Internet
## connection to display the visualisation.

Fruits	
## timevar Year
M1 <- gvisMotionChart(Fruits, idvar="Fruit", timevar="Year")
str(M1)
## print.gvis, will concatenate the list into a one long string
M1

plot(M1)

## Combine with another chart, e.g. table
tbl <- gvisTable(Fruits, options=list(height=220))
Mtbl <- gvisMerge(M1, tbl)
plot(Mtbl)

\dontrun{
## Usage of date variable
M2 <- gvisMotionChart(Fruits, idvar="Fruit", timevar="Date",
                               date.format = "\%Y\%m\%d") 
plot(M2)

## Display weekly data:
M3 <- gvisMotionChart(Fruits, "Fruit", "Date", date.format="\%YW\%W")

plot(M3) 
}
## Options: no side panel on the right
M4 <- gvisMotionChart(Fruits,"Fruit","Year",
                 options=list(showSidePanel=FALSE))
plot(M4)

## Options: trails un-ticked
M5 <- gvisMotionChart(Fruits, "Fruit", "Year",
                 options=list(state='{"showTrails":false};'))

plot(M5)

## You can change some of displaying settings via the browser,
## e.g. the level of opacity of non-selected items, or the chart type.
## The state string from the 'Advanced' tab can be used to set those
## settings via R. Just copy and past the string from the browser into
## the argument state of the options list.
## Here is an example of a motion chart, with an initial line chart
## displayed. 

myStateSettings <-'
{"xZoomedDataMin":1199145600000,"colorOption":"2",
"duration":{"timeUnit":"Y","multiplier":1},"yLambda":1,
"yAxisOption":"4","sizeOption":"_UNISIZE",
"iconKeySettings":[],"xLambda":1,"nonSelectedAlpha":0,
"xZoomedDataMax":1262304000000,"iconType":"LINE",
"dimensions":{"iconDimensions":["dim0"]},
"showTrails":false,"uniColorForNonSelected":false,
"xAxisOption":"_TIME","orderedByX":false,"playDuration":15000,
"xZoomedIn":false,"time":"2010","yZoomedDataMin":0,
"yZoomedIn":false,"orderedByY":false,"yZoomedDataMax":100}
'
M6 <- gvisMotionChart(Fruits, "Fruit", "Year", options=list(state=myStateSettings))
plot(M6)
## For more information see:
## http://code.google.com/apis/chart/interactive/docs/gallery/motionchart.html#Motion_Chart_initial_state

## See also the demo(WorldBank). It demonstrates how you can access
## country level data from the World Bank to create Gapminder-like
## plots.


}
\keyword{ iplot }
