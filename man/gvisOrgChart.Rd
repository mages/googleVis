\name{gvisOrgChart}
\alias{gvisOrgChart}
\title{
  Google Org Chart with R
}
\description{
  An organizational chart that supports selection.
  
  The gvisOrgChart function reads a data.frame and
  creates text output referring to the Google Visualisation API, which can be
  included into a web page, or as a stand-alone page. The actual chart is
  rendered by the web browser.
  
}
\usage{
gvisOrgChart(data, idvar = "", parentvar = "", tipvar = "",
             options = list(), chartid)
}
\arguments{
  \item{data}{a \code{data.frame}. The data has to have at least three columns. Each
    row in the data table describes one node (a rectangle 
    in the graph). Each node (except the root node) has one or more parent
    nodes. Each node is sized and colored according to its values relative
    to the other nodes currently shown.  
  }
  \item{idvar}{column name of \code{data} describing the ID for each
    node. It should be unique among all nodes, and can include any
    characters, including spaces. This is shown on the node. You can
    specify a formatted value to show on the chart instead, but the
    unformatted value is still used as the ID.
  }
  \item{parentvar}{column name of \code{data} that match to entries in
    \code{idvar}. If this is a root node, leave this \code{NA}. Only one
    root is allowed. 
  }
  \item{tipvar}{column name of \code{data} for the tip
    variable. Tool-tip text to show, when a user hovers over this node.
  }
  \item{options}{list of configuration options for Google Org Chart.
        \describe{
\item{\code{gvis.editor}}{a character label for an
 on-page button which opens an in-page dialog box that enables
users to edit, change and customise the chart. By default no
value is given and therefore no button is displayed. 
      }
    }
    Further possible components are, taken from
    \url{https://google-developers.appspot.com/chart/interactive/docs/gallery/orgchart.html#Configuration_Options}: 
    \describe{
      \item{\code{allowCollapse}}{Boolean. Default
	\code{FALSE}. Determines if double click will collapse a node.
      }
      \item{\code{allowHtml}}{Boolean. Default \code{FALSE}.  If set to
	\code{TRUE}, names that includes HTML tags will be rendered as
	HTML.
      }
      \item{\code{nodeClass}}{A string. Default default class name. A class
	name to assign to node elements. Apply CSS to this class name to
	specify colors or styles for the chart elements.
      }
      \item{\code{selectedNodeClass}}{A string. Default class name. A class
	name to assign to selected node elements. Apply CSS to this class name
	to specify colors or styles for selected chart elements.
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
  \code{gvisOrgChart} returns list of \code{\link{class}} "\code{gvis}"
  and "\code{list}". 
  
  An object of class "\code{gvis}" is a list containing at least the
  following components:
  \item{\code{type}}{Google visualisation type, here 'OrgChart'}
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
  Google Org Chart  API:
  \url{https://google-developers.appspot.com/chart/interactive/docs/gallery/orgchart.html}

  Follow the link for Google's data policy.  
}
\author{
Markus Gesmann \email{markus.gesmann@gmail.com},

Diego de Castillo \email{decastillo@gmail.com}
}

\seealso{

  See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for
  printing and plotting methods. 
}
\examples{
## Please note that by default the googleVis plot command
## will open a browser window and requires Internet
## connection to display the visualisation.

Regions
Org1 <- gvisOrgChart(Regions, idvar = "Region", parentvar = "Parent", 
     			      tipvar="Val")
plot(Org1)

## Set a few options
Org2 <- gvisOrgChart(Regions, idvar = "Region", parentvar = "Parent", 
     			      tipvar="Val", 
     		     options=list(width=600, height=400,
                     	          size='large', allowCollapse=TRUE))
plot(Org2)

}
\keyword{ iplot }
