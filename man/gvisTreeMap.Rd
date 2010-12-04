\name{gvisTreeMap}
\alias{gvisTreeMap}
\title{
  Google Tree Map with R
}
\description{

  The gvisTreeMap function reads a data.frame and
  creates text output referring to the Google Visualisation API which can be
  included into a web page, or as a stand-alone page. The actual chart is
  rendered by the web browser in flash.

  A tree map is a visual representation of a data tree, where each node
  can have zero or more children, and one parent (except for the root,
  which has no parents). Each node is displayed as a rectangle, sized
  and colored according to values that you assign. Sizes and colors are
  valued relative to all other nodes in the graph. You can specify how
  many levels to display simultaneously, and optionally to display
  deeper levels in a hinted fashion. If a node is a leaf node, you can
  specify a size and color; if it is not a leaf, it will be displayed as
  a bounding box for leaf nodes. The default behavior is to move down the
  tree when a user left-clicks a node, and to move back up the tree when
  a user right-clicks the graph.
  
  The total size of the graph is determined by the size of the containing
  element that you insert in your page. If you have leaf nodes with names
  too long to show, the name will be truncated with an ellipsis (...).
}
\usage{
gvisTreeMap(data,
            idvar = "", parentvar = "",
            sizevar = "", colorvar = "",
            options = list() )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a \code{data.frame}. The data has to have at least four columns. Each
    row in the data table describes one node (a rectangle 
    in the graph). Each node (except the root node) has one or more parent
    nodes. Each node is sized and colored according to its values relative
    to the other nodes currently shown.  
  }
  \item{idvar}{column name of \code{data} describing the ID for each node. It can
    be any valid JavaScript string, including spaces, and any length that
    a string can hold. This value is displayed as the node header. 
  }
  \item{parentvar}{column name of \code{data} that match to entries in
    \code{idvar}. If this is a root node, leave this \code{NA}. Only one
    root is allowed per treemap. 
  }
  \item{sizevar}{column name of \code{data} with positive values to
    define the size of maps.
    Any positive value is allowed. This value determines the size of the
    node, computed relative to all other nodes currently shown. This value
    is ignored for non-leaf nodes (it is actually calculated from the size
    of all its children).
  }
  \item{colorvar}{column name of \code{data} with values to define range
    of color. The value is used to calculate a color for this node. Any
    value, positive or negative, is allowed. The color value is first
    recomputed on a scale from \code{minColorValue} to \code{maxColorValue}, and then
    the node is assigned a color from the gradient between \code{minColor} and
    \code{maxColor}.
  }
  \item{options}{list of configuration options for Google Tree Map.
    The possible components are, taken from
    \url{http://code.google.com/apis/visualization/documentation/gallery/treemap.html#Configuration_Options}: 
    \describe{
      \item{\code{headerColor}}{string, default	'#988f86'. The color of
	the header section for each node. Specify an HTML color value.
      }
      \item{\code{headerHeight}}{number, default 0. The height of the
	header section for each node, in pixels (can be zero).
      }
      \item{\code{headerHighlightColor}}{string, default \code{'null'}. The color
	of the header of a node being hovered over. Specify an HTML color
	value, null, or 'auto'; if null or 'auto', this value will be
	\code{headerColor} lightened by 35\%
      }
      \item{\code{maxColor}}{string, default #00dd00. The color for a rectangle
	with a \code{sizevar} value of \code{maxColorValue}. Specify an HTML color value.
      }
      \item{\code{maxDepth}}{number, default 1. The maximum number of node
	levels to show in the current view. Levels will be flattened into the
	current plane. If your tree has more levels than this, you will have
	to go up or down to see them. You can additionally see \code{maxPostDepth}
	levels below this as shaded rectangles within these nodes.
      }
      \item{\code{maxHighlightColor}}{string, default	null. The highlight
	color to use for the node with the largest value in column 3. Specify
	an HTML color value, null, or 'auto. If null or 'auto', this value
	will be the value of \code{maxColor} lightened by 35\%
      }
      \item{\code{maxPostDepth}}{number, default 1. How many levels of nodes
	beyond \code{maxDepth} to show in "hinted" fashion. Hinted nodes are shown as
	shaded rectangles within a node that is within the \code{maxDepth} limit.
      }
      \item{\code{maxColorValue}}{number, default null. The maximum value
	allowed in column \code{sizevar}. All values greater than this will be trimmed to
	this value. If set to null or 'auto', it will be set to the max value
	in the column.
      }
      \item{\code{midColor}}{string, default	#000000. The color for a
	rectangle with a column \code{sizevar} value midway between \code{maxColorValue} and
	\code{minColorValue}. Specify an HTML color value.
      }
      \item{\code{midHighlightColor}}{string, default	null. The highlight
	color to use for the node with a column \code{sizevar} value near the median of
	\code{minColorValue} and \code{maxColorValue}. Specify an HTML color value or
	'auto'. If null or 'auto', this value will be the value of \code{midColor}
	lightened by 35\%.
      }
      \item{\code{minColor}}{string, default	#dd0000. The color for a
	rectangle with the column \code{sizevar} value of \code{minColorValue}. Specify an HTML
	color value. 
      }
      \item{\code{minHighlightColor}}{string, default	null. The highlight
	color to use for the node with a column \code{sizevar} value nearest to
	\code{minColorValue}. Specify an HTML color value or 'auto'. If null or
	'auto', this value will be the value of \code{minColor} lightened by 35'%.
      }
      \item{\code{minColorValue}}{number, default null. The minimum value
	allowed in column \code{sizevar}. All values less than this will be trimmed to this
	value. If set to null or 'auto', it will be calculated as the minimum
	value in the column.
      }
      \item{\code{noColor}}{string, default #000000. The color to use for a
	rectangle when a node has no value for column \code{sizevar}, and that node is a
	leaf (or contains only leaves). Specify an HTML color value.
      }
      \item{\code{noHighlightColor}}{string, default null. The color to use
	for a rectangle of "no" color when highlighted. This will be the value
	of \code{noColor} lightened by 35\%. Specify an HTML value.
      }
      \item{\code{showScale}}{boolean, default \code{FALSE}. Whether or not to
	show a color gradient scale from \code{minColor} to \code{maxColor} along the top of
	the chart. Specify true to show the scale.
      }
      \item{\code{showTooltips}}{boolean, default \code{TRUE}. Whether or
	not to show tooltips.
      }
      \item{\code{fontColor}}{string, default	#ffffff. The text color. Specify
	an HTML color value.
      }
      \item{\code{fontFamily}}{string, default auto. The font family to use
	for all text.
      }
      \item{\code{fontSize}}{number, default 12. The font size for all text,
	in points.
      }
    }
  }
}
%%\details{
  %%  ~~ If necessary, more details than the description above ~~
%%}
\value{
  \code{gvisTreeMap} returns list of \code{\link{class}} "\code{gvis}"
  and "\code{list}". 
  
  An object of class "\code{gvis}" is a list containing at least the
  following components:
  \item{\code{type}}{Google visualisation type, here 'TreeMap'}
  \item{\code{chartid}}{character id of the chart object. The string is
    build at run time from chart type and date/time when the function is
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
  Google Tree Map  API:
  \url{http://code.google.com/apis/visualization/documentation/gallery/treemap.html}

  Follow the link also for Google's Data Policy.  
}
\author{
Markus Gesmann \email{markus.gesmann@gmail.com},

Diego de Castillo \email{decastillo@gmail.com}
}
%%\note{
%%  \code{treemap} and function{tmPlot}
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{

  See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for
  printing and plotting methods. 

  Please note that the \code{treemap} package offeres a
  static version of tree maps via its \code{tmPlot} function. 

}
\examples{
## Please note that by default the plot (better plot.gvis) command will
## try to write the output into the rsp/myAnalysis folder of the
## googleVis library. Further the plot command will open a
## browser window and requires Flash and Internet connection to
## display the visualisation.
## Because of those requirements the plot commands are not executed
## automatically as part of the example. 
## Please copy and paste the plot commands manually
## to see the visualisation output, see also ?plot.gvis for further
## details.  

Tree <- gvisTreeMap(Regions,  idvar="Region", parentvar="Parent",
                    sizevar="Val", colorvar="Fac")
\dontrun{
plot(Tree)
}

Tree2 <- gvisTreeMap(Regions,  "Region", "Parent", "Val", "Fac",
                    options=list(width=600, height=500,
                                 fontSize=16,
                                 minColor='#EDF8FB',
                                 midColor='#66C2A4',
                                 maxColor='#006D2C',
                                 headerHeight=20,
                                 fontColor='black',
                                 showScale=TRUE))

\dontrun{
plot(Tree2)
}

## Simple static treemap with no drill down options based on US states
## and their area. However we still have to create a parent id to use
## gvisTreeMap
 
require(datasets)
states <- data.frame(state.name, state.area)

## Create parent variable

total=data.frame(state.area=sum(states$state.area), state.name="USA")

my.states <- rbind(total, states)
my.states$parent="USA"
## Set parent variable to NA at root level
my.states$parent[my.states$state.name=="USA"] <- NA

my.states$state.area.log=log(my.states$state.area)
statesTree <- gvisTreeMap(my.states, "state.name", "parent",
                          "state.area", "state.area.log")
\dontrun{
plot(statesTree)
}


## We add US regions to the above data set to enable drill down capabilities

states2 <- data.frame(state.region, state.name, state.area)

regions <- aggregate(list(region.area=states2$state.area),
                     list(region=state.region), sum)

my.states2 <- data.frame(regionid=c("USA",
                                    as.character(regions$region),
                                    as.character(states2$state.name)),
                         parentid=c(NA, rep("USA", 4),
                                   as.character(states2$state.region)),
                         state.area=c(sum(states2$state.area),
                                      regions$region.area, states2$state.area))

my.states2$state.area.log=log(my.states2$state.area)

statesTree2 <- gvisTreeMap(my.states2, "regionid", "parentid",
                           "state.area", "state.area.log")

\dontrun{
plot(statesTree2)
}

## Now we add another layer with US divisions

states3 <- data.frame(state.region, state.division, state.name, state.area)

regions <- aggregate(list(region.area=states3$state.area),
                     list(region=state.region), sum)

divisions <- aggregate(list(division.area=states3$state.area),
                     list(division=state.division, region=state.region),
                     sum)

my.states3 <- data.frame(regionid=c("USA",
                                    as.character(regions$region),
                                    as.character(divisions$division),
                                    as.character(states3$state.name)),
                         parentid=c(NA, rep("USA", 4), 
                                   as.character(divisions$region),
                                   as.character(states3$state.division)),
                         state.area=c(sum(states3$state.area),
                                      regions$region.area,
                                      divisions$division.area,
                                      states3$state.area))

my.states3$state.area.log=log(my.states3$state.area)

statesTree3 <- gvisTreeMap(my.states3, "regionid", "parentid",
                           "state.area", "state.area.log")

\dontrun{
plot(statesTree3)
}


}
\keyword{ hplot }
