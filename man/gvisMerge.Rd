\name{gvisMerge}
\alias{gvisMerge}
\title{
  Merge two googleVis charts into one gvis-object 
}
\description{
  gvisMerge merges two gvis-objects, either next or below
  each other into one gvis-object. The objects are arranged in a HTML table. 
}
\usage{
gvisMerge(x, y, horizontal = FALSE,
                tableOptions = "border=\"0\"", chartid)
}
\arguments{
  \item{x}{a \code{gvis}-object.  
  }
  \item{y}{a \code{gvis}-object.
  }
  \item{horizontal}{boolean. Default \code{FALSE}. If \code{FALSE} the
    two \code{gvis}-objects are arranged below each other, otherwise
    next to each other.
  }
  \item{tableOptions}{a valid HTML table option string. Default \code{"border=\"0\""}. 
  }
  \item{chartid}{character. If missing (default) a random chart id will be generated based on
    chart type and \code{\link{tempfile}}
  }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  \code{gvisMerge} returns list of \code{\link{class}} "\code{gvis}" and "\code{list}". 
  
  An object of class "\code{gvis}" is a list containing at least the following components:
  \item{\code{type}}{Google visualisation type, here 'gvisMerge'}
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
	  \item{\code{jsFooter}}{End tag \code{</script>}.
	  }
	  \item{\code{jsChart}}{Call of the \code{jsDisplayChart} 
	    function.
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
  Google Chart Tools API: \url{http://code.google.com/apis/chart/index.html}

  Follow the link for Google's data policy.
}
\author{
  Markus Gesmann \email{markus.gesmann@gmail.com},
}
%%\note{
%%  ~~further notes~~
%%}

\seealso{
  See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for
  printing and plotting methods
}
\examples{
## Please note that by default the googleVis plot command
## will open a browser window and requires Internet
## connection to display the visualisation.

Pie1 <- gvisPieChart(CityPopularity)

## Doughnut chart - a pie with a hole
Pie2 <- gvisPieChart(CityPopularity, options=list(
  slices="{4: {offset: 0.2}, 0: {offset: 0.3}}",
  title='City popularity',
  legend='none',
  pieSliceText='label',
  pieHole=0.5))

plot(gvisMerge(Pie2, Pie1, 
        tableOptions = "cellspacing=\"20\" bgcolor=\"#AABBCC\"",
        horizontal=TRUE))
               
## Nested charts               
               
G <- gvisGeoChart(Exports, "Country", "Profit", 
                  options=list(width=250, height=100))
T <- gvisTable(Exports, 
                  options=list(width=250, height=300))

GT <- gvisMerge(G,T, horizontal=FALSE) 
plot(GT)

M <- gvisMotionChart(Fruits, "Fruit", "Year",
                     options=list(width=400, height=410))

GTM <- gvisMerge(GT, M, horizontal=TRUE,
                     tableOptions="cellspacing=10")
plot(GTM)


line <- gvisLineChart(OpenClose, "Weekday", c("Open", "Close"), 
        options=list(legend='none', width=300, height=150))
column <- gvisColumnChart(OpenClose, "Weekday", c("Open", "Close"),
              options=list(legend='none', width=300, height=150))
area <- gvisAreaChart(OpenClose, "Weekday", c("Open", "Close"),
              options=list(legend='none', width=300, height=150))
bar <- gvisBarChart(OpenClose, "Weekday", c("Open", "Close"),
              options=list(legend='none', width=300, height=150))
LBCA <- gvisMerge(gvisMerge(line, bar), gvisMerge(column, area),
                  horizontal=TRUE, tableOptions="bgcolor=\"#AABBCC\"") 

plot(LBCA)

## Applying gvisMerge successively

p <- Reduce(gvisMerge, list(line, column, area, bar))
plot(p)               

}
\keyword{ aplot }
