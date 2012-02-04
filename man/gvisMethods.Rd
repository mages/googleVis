\name{gvis Methods}
\alias{print.gvis}
\alias{plot.gvis}

\title{
  Print and plot gvis objects
}
\description{
  Methods to print and plot \code{gvis} objects
}
\usage{	
\method{print}{gvis}(x, tag="html", file = "", ...)
		
\method{plot}{gvis}(x,...)
}
\arguments{
  \item{x}{
    An object of class \code{gvis}.
  }
  \item{tag}{name tag of the objects to be extracted from a gvis object. The
    default tag "html" will show a complete web page with the
    visualisation. The tag "chart" will present all code for the
    visualisation chart only. For more information see the details section.
  }	
  \item{file}{
    file name. If "" (the
    default), output will be printed to the standard output connection,
    the console unless redirected by \code{\link{sink}}.
  }
  \item{\dots}{arguments passed on to \code{\link{cat}}
      (\code{print.gvis}) or \code{\link{browseURL}} (\code{plot.gvis}).
  }
}

\details{

  An object of class "\code{gvis}" is a list containing at least the
  following components (tags):
  \describe{
    \item{\code{type}}{Google visualisation type, e.g. 'MotionChart'}
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
}

\value{
  \item{\code{print.gvis}}{ None (invisible \code{NULL}).}
  
  \item{\code{plot.gvis}}{Returns the file name invisibly.}
}
\references{
  Please see also the package vignette for the usage of the googleVis
  package with RApache and R.rsp.
}
\author{
  Markus Gesmann \email{markus.gesmann@gmail.com},
  
  Diego de Castillo \email{decastillo@gmail.com}
}
\note{
  The \code{plot} command does not open a graphics device in the
  traditional way. Instead it creates HTML files in a temporary
  directory and uses the R HTTP server to display the output  
  of a \code{googleVis} function locally. 
  A browser with Flash and Internet connection is required.
  The displayed page includes a link (click on the chart id) to a
  further page, which shows the code of the chart for the user to copy
  and paste into her own page.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  See also \code{\link{cat}}, \code{\link{browseURL}},
  \code{\link{createGoogleGadget}} and \code{\link{gvisMerge}} for combining charts.
}
\examples{

M <- gvisMotionChart(Fruits, "Fruit", "Year")
str(M)
## The output for a complete web page
M

## Access only the plot,
M$html$chart

## wrap it in cat and it becomes more readable,
cat(unlist(M$html$chart))

## or use the print function.
print(M, "chart")

## Extract the data as a JavaScript function.
print(M, "jsData")

## Display the visualisation.
## A web browser with Internet connection and Flash is required.
plot(M)

## Combine with another chart, e.g. table
tbl <- gvisTable(Fruits, options=list(height=220))
Mtbl <- gvisMerge(M, tbl)
plot(Mtbl)

\dontrun{

  ## Suppose you have an existing web page in which you embedded a
  ## motion chart with the id "mtnc".
  ## Now you have a new set of data, but you would like to avoid
  ## touching the html file again.
  ## The idea is to write the data and JavaScript functions into separate
  ## files and to refer to these in the html page.

  ## In this example we call the chart id "mtnc"
  ## To display the example we use the R HTTP server again, and
  ## write the files into a temp directory

  myChartID <- "mtnc"
  ## baseURL should reflect your web address, e.g. http://myHomePage.com
  baseURL <- sprintf("http://127.0.0.1:\%s/custom/googleVis", tools:::httpdPort)
  wwwdir <- tempdir() ## the www repository on your computer

  
  ## Create a motion chart
  M=gvisMotionChart(Fruits, "Fruit", "Year", chartid=myChartID)

  ## Here is our plot again
  plot(M)

  ## Write the data and functions into separate files:
  cat(M$html$chart['jsData'], file=file.path(wwwdir, "gvisData.js"))
  cat(M$html$chart[c('jsDrawChart', 'jsDisplayChart', 'jsChart')], 
  				    file=file.path(wwwdir, "gvisFunctions.js"))

  
  ## Create a html page with reference to the above
  ## JavaScript files
  
  html <- sprintf('
  <html>
  <head>
  <script type="text/javascript" src="http://www.google.com/jsapi">
  </script>
  <script type="text/javascript" src="\%s/gvisFunctions.js"></script>
  <script type="text/javascript" src="\%s/gvisData.js"></script>
  <script type="text/javascript">
  displayChart\%s()
  </script>
  </head>
  <body>
  <div id="\%s" style="width: 600px; height: 500px;"></div>
  </body>
  </html>
  ', baseURL, baseURL, myChartID, myChartID)
  
  ## Write html scaffold into a file
  cat(html, file=file.path(wwwdir, paste("Chart", myChartID, ".html", sep="")))

  ## Display the result via
  URL <- paste(baseURL,"/Chart", myChartID, ".html", sep="")
  browseURL(URL)

  ## Update the data, say the data should have shown North and South
  ## instead of East and West as a location  
  FruitsUpdate <- Fruits
  levels(FruitsUpdate$Location)=c("North", "South")

  Mupdate=gvisMotionChart(FruitsUpdate, "Fruit", "Year", chartid=myChartID)

  ## Only update the file gvisData.js:
  cat(Mupdate$html$chart['jsData'], file=file.path(wwwdir, "gvisData.js"))

  ## Redisplay the chart with the updated data
  browseURL(URL)

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ iplot }
\keyword{ methods }
\keyword{ print }
