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
\method{print}{gvis}(x, tag=NULL, file = "", ...)
		
\method{plot}{gvis}(x, tag=NULL,...)
}
\arguments{
  \item{x}{
    An object of class \code{gvis}, or a HTML file in case of
    plot.gvis. 
  }
  \item{tag}{Default \code{NULL}. Name tag of the objects to be
    extracted from a gvis object. If tag is missing 
    then the values of \code{getOption("gvis.print.tag")},
    \code{getOption("gvis.plot.tag")} will be used for \code{print.gvis}
    and \code{plot.gvis} respectively.  

    A complete list of available tags is given by the command
    \code{getOption("gvis.tags")}. 

    The default value of \code{gvis.print.tag} is \code{"html"}, which
    means \code{print.gvis} will show a complete web page with the
    visualisation, while the tag \code{"chart"} will present the code
    for the visualisation chart only. For more information see the
    details section.

    The default tag for \code{plot.gvis.tag} is \code{NULL}, which will
    result in R opening a browser window, while any tag which is not
    \code{NULL} will give the same behaviour as \code{print.gvis},
    e.g. \code{tag='chart'} or setting \code{options(gvis.plot.tag='chart')}
    will produce the same output as \code{print(x, tag='chart')}.
    This behaviour is particular helpful when googleVis is used in
    scripts, like \code{knitr} or \code{R.rsp}. The plot commands can
    be used initially in an interactive mode and with one change in
    \code{options()} produce the HTML output required for a programmatic
    run of the script. See the example section below for a
    \code{knitr} case study.

    \code{plot.gvis} ignores the argument \code{tag} it \code{x} is a
    HTML file name.
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
  package with RApache, brew, knitr and R.rsp.
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
## Show gvis options
sapply(c("gvis.print.tag", "gvis.plot.tag", "gvis.tags"), getOption)

M <- gvisMotionChart(Fruits, "Fruit", "Year")
str(M)
## The output for a complete web page
M

## Access only the plot,
M$html$chart

## wrap it in cat and it becomes more readable,
cat(unlist(M$html$chart))

## or use the print function.
print(M, tag="chart")

## Extract the data as a JavaScript function.
print(M, tag="jsData")

## Display the visualisation.
## A web browser with Internet connection and Flash is required.
plot(M)

## Combine with another chart, e.g. table
tbl <- gvisTable(Fruits, options=list(height=220))
Mtbl <- gvisMerge(M, tbl)
plot(Mtbl)


## Example of using googleVis with knitr and markdown

\dontrun{
## Simple knitr/markdown file with googleVis
knitrRmd <-"
# Markdown example with knitr and googleVis
===========================================
This is a little Markdown example file.
Set the googleVis options first.
In this case change the behaviour of plot.gvis
```{r setOptions, message=FALSE}
library(googleVis)
op <- options(gvis.plot.tag='chart')
```
The following plot statements will automatically return  the HTML
required for the 'knitted' output. 

## Combo chart
```{r ComboExample, results='asis', tidy=FALSE}
## Add the mean
CityPopularity$Mean=mean(CityPopularity$Popularity)
CC <- gvisComboChart(CityPopularity, xvar='City',
          yvar=c('Mean', 'Popularity'),
          options=list(seriesType='bars',
                       width=450, height=300,
                       title='City Popularity',
                       series='{0: {type:\"line\"}}'))
plot(CC)
```
Example of gvisComboChart with R code shown above.

## Place two charts next to each other
```{r gvisMergeExample, results='asis', echo=FALSE}
Geo <- gvisGeoChart(Exports, locationvar='Country', colorvar='Profit', 
                    options=list(height=300, width=350)) 
Tbl <- gvisTable(Exports, options=list(height=300, width=200))
plot(gvisMerge(Geo, Tbl, horizontal=TRUE))
``````
Example of a gvisGeoChart with gvisTable and R code hidden.

## Motion Chart
```{r MotionChartExample, results='asis', tidy=FALSE}
M <- gvisMotionChart(Fruits, 'Fruit', 'Year',
         options=list(width=400, height=350))
plot(M)
```
Please note that the Motion Chart is only displayed when hosted on a
web server, or is placed in a directory which has been added to the 
trusted sources in the [security settings of Macromedia]
(http://www.macromedia.com/support/documentation/en/flashplayer/help/settings_manager04.html). 
See the googleVis package vignette for more details. 

```{r resetOptions}
## Set options back to original options
options(op)
```
"

## Write the content of knitrRmd into a Rmd-file, knit it and convert it
## into a html output. Finally show the file with the R-help http
## server, this will ensure that also the motion chart is visible.

library(knitr)
library(markdown)

wd <- getwd()
setwd(tempdir())
fn=tempfile()
fn.Rmd <- paste(fn, ".Rmd", sep="")
fn.md <- paste(fn, ".md", sep="")
fn.html <- paste(fn, "-out.html", sep="") 
## Write R Markdown into a file
cat(knitrRmd, file=fn.Rmd)
render_markdown()
knit(fn.Rmd, fn.md)
knit2html(fn.md)

## Open output in browser
## Use plot.gvis which will use the R-help http server
## The URL will start with http://127.0.0.1...
## The HTML file will be copied into a temporary directory
plot.gvis(fn.html)
## Compare to browseURL, its URL will start with file://... the motion
## chart is unlikely to be displayed because of Flash security
## settings. See the googleVis vignette for more details. 
browseURL(fn.html)
setwd(wd)
}

\dontrun{
## Updating the data of an existing googleVis web page

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
