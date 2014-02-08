### File R/gvisTreeMap.R
### Part of the R package googleVis
### Copyright 2010 - 2014 Markus Gesmann, Diego de Castillo

### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
### MA 02110-1301, USA

#' Google Tree Map with R
#' \Sexpr{googleChartName <- "treemap"}
#' \Sexpr{gvisChartName <- "gvisTreeMap"}
#' 
#' The gvisTreeMap function reads a data.frame and creates text output
#' referring to the Google Visualisation API, which can be included into a web
#' page, or as a stand-alone page. The actual chart is rendered by the web
#' browser.
#' 
#' A tree map is a visual representation of a data tree, where each node can
#' have zero or more children, and one parent (except for the root, which has
#' no parents). Each node is displayed as a rectangle, sized and colored
#' according to values that you assign. Sizes and colors are valued relative to
#' all other nodes in the graph. You can specify how many levels to display
#' simultaneously, and optionally to display deeper levels in a hinted fashion.
#' If a node is a leaf node, you can specify a size and color; if it is not a
#' leaf, it will be displayed as a bounding box for leaf nodes. The default
#' behavior is to move down the tree when a user left-clicks a node, and to
#' move back up the tree when a user right-clicks the graph.
#' 
#' The total size of the graph is determined by the size of the containing
#' element that you insert in your page. If you have leaf nodes with names too
#' long to show, the name will be truncated with an ellipsis (...).
#' 
#' @param data a \code{data.frame}. The data has to have at least four columns.
#' Each row in the data table describes one node (a rectangle in the graph).
#' Each node (except the root node) has one or more parent nodes. Each node is
#' sized and colored according to its values relative to the other nodes
#' currently shown.
#' @param idvar column name of \code{data} describing the ID for each node. It
#' can be any valid JavaScript string, including spaces, and any length that a
#' string can hold. This value is displayed as the node header.
#' @param parentvar column name of \code{data} that match to entries in
#' \code{idvar}. If this is a root node, leave this \code{NA}. Only one root is
#' allowed per treemap.
#' @param sizevar column name of \code{data} with positive values to define the
#' size of maps.  Any positive value is allowed. This value determines the size
#' of the node, computed relative to all other nodes currently shown. This
#' value is ignored for non-leaf nodes (it is actually calculated from the size
#' of all its children).
#' @param colorvar column name of \code{data} with values to define range of
#' color. The value is used to calculate a color for this node. Any value,
#' positive or negative, is allowed. The color value is first recomputed on a
#' scale from \code{minColorValue} to \code{maxColorValue}, and then the node
#' is assigned a color from the gradient between \code{minColor} and
#' \code{maxColor}.
#' @param options list of configuration options, see:
#' 
#' % START DYNAMIC CONTENT
#' 
#' \Sexpr[results=rd]{gsub("CHARTNAME", 
#' googleChartName,
#' readLines(file.path(".", "inst",  "mansections", 
#' "GoogleChartToolsURLConfigOptions.txt")))}
#' 
#'  \Sexpr[results=rd]{paste(readLines(file.path(".", "inst", 
#'  "mansections", "gvisOptions.txt")))}
#'   
#' @param chartid character. If missing (default) a random chart id will be 
#' generated based on chart type and \code{\link{tempfile}}
#' 
#' @return \Sexpr[results=rd]{paste(gvisChartName)} returns list 
#' of \code{\link{class}}
#'  \Sexpr[results=rd]{paste(readLines(file.path(".", "inst", 
#'  "mansections", "gvisOutputStructure.txt")))}
#'   
#' @references Google Chart Tools API: 
#' \Sexpr[results=rd]{gsub("CHARTNAME", 
#' googleChartName, 
#' readLines(file.path(".", "inst",  "mansections", 
#' "GoogleChartToolsURL.txt")))}
#' 
#' % END DYNAMIC CONTENT
#' 
#' @section Warning :
#' 
#' Tree maps display a tree like structure where every child has to have a
#' unique parent.
#' 
#' Values in column \code{sizevar} should be greater than zero and finite.
#' @author Markus Gesmann \email{markus.gesmann@@gmail.com},
#' 
#' Diego de Castillo \email{decastillo@@gmail.com}
#' @seealso
#' 
#' See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for printing and
#' plotting methods.
#' 
#' Please note that the \code{treemap} package offeres a static version of tree
#' maps via its \code{tmPlot} function.
#' @keywords iplot
#' @examples
#' 
#' ## Please note that by default the googleVis plot command
#' ## will open a browser window and requires Internet
#' ## connection to display the visualisation.
#' 
#' Tree <- gvisTreeMap(Regions,  idvar="Region", parentvar="Parent",
#'                     sizevar="Val", colorvar="Fac")
#' plot(Tree)
#' 
#' 
#' Tree2 <- gvisTreeMap(Regions,  "Region", "Parent", "Val", "Fac",
#'                     options=list(width=600, height=500,
#'                                  fontSize=16,
#'                                  minColor='#EDF8FB',
#'                                  midColor='#66C2A4',
#'                                  maxColor='#006D2C',
#'                                  headerHeight=20,
#'                                  fontColor='black',
#'                                  showScale=TRUE))
#' 
#' plot(Tree2)
#' 
#' ## Simple static treemap with no drill down options based on US states
#' ## and their area. However we still have to create a parent id to use
#' ## gvisTreeMap
#'  
#' require(datasets)
#' states <- data.frame(state.name, state.area)
#' 
#' ## Create parent variable
#' 
#' total=data.frame(state.area=sum(states$state.area), state.name="USA")
#' 
#' my.states <- rbind(total, states)
#' my.states$parent="USA"
#' ## Set parent variable to NA at root level
#' my.states$parent[my.states$state.name=="USA"] <- NA
#' 
#' my.states$state.area.log=log(my.states$state.area)
#' statesTree <- gvisTreeMap(my.states, "state.name", "parent",
#'                           "state.area", "state.area.log")
#' plot(statesTree)
#' 
#' 
#' ## We add US regions to the above data set to enable drill down capabilities
#' 
#' states2 <- data.frame(state.region, state.name, state.area)
#' 
#' regions <- aggregate(list(region.area=states2$state.area),
#'                      list(region=state.region), sum)
#' 
#' my.states2 <- data.frame(regionid=c("USA",
#'                                     as.character(regions$region),
#'                                     as.character(states2$state.name)),
#'                          parentid=c(NA, rep("USA", 4),
#'                                    as.character(states2$state.region)),
#'                          state.area=c(sum(states2$state.area),
#'                                       regions$region.area, states2$state.area))
#' 
#' my.states2$state.area.log=log(my.states2$state.area)
#' 
#' statesTree2 <- gvisTreeMap(my.states2, "regionid", "parentid",
#'                            "state.area", "state.area.log")
#' 
#' plot(statesTree2)
#' 
#' ## Now we add another layer with US divisions
#' 
#' states3 <- data.frame(state.region, state.division, state.name, state.area)
#' 
#' regions <- aggregate(list(region.area=states3$state.area),
#'                      list(region=state.region), sum)
#' 
#' divisions <- aggregate(list(division.area=states3$state.area),
#'                      list(division=state.division, region=state.region),
#'                      sum)
#' 
#' my.states3 <- data.frame(regionid=c("USA",
#'                                     as.character(regions$region),
#'                                     as.character(divisions$division),
#'                                     as.character(states3$state.name)),
#'                          parentid=c(NA, rep("USA", 4), 
#'                                    as.character(divisions$region),
#'                                    as.character(states3$state.division)),
#'                          state.area=c(sum(states3$state.area),
#'                                       regions$region.area,
#'                                       divisions$division.area,
#'                                       states3$state.area))
#' 
#' my.states3$state.area.log=log(my.states3$state.area)
#' 
#' statesTree3 <- gvisTreeMap(my.states3, "regionid", "parentid",
#'                            "state.area", "state.area.log")
#' 
#' plot(statesTree3)
#' 
#' 
#' 
gvisTreeMap <- function(data, idvar="", parentvar="", sizevar="", colorvar="",options=list(), chartid){

  my.type <- "TreeMap"
  dataName <- deparse(substitute(data))
  my.options <- list(gvis=modifyList(list(width = 600, height=500),options), 
		     dataName=dataName, 
                     data=list(idvar=idvar, parentvar=parentvar, sizevar=sizevar,colorvar=colorvar,
		      allowed=c("number", "string"))
                     )
  
  checked.data <- gvisCheckTreeMapData(data, my.options)

  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options, chartid)
  
  return(output)
}

gvisCheckTreeMapData <- function(data, options){

  data.structure <- list(
        	     idvar     = list(mode="required",FUN=check.char),
        	     parentvar = list(mode="required",FUN=check.char),
        	     sizevar   = list(mode="required",FUN=check.num.pos),
        	     colorvar  = list(mode="required",FUN=check.num))
	
  x <- gvisCheckData(data=data,options=options,data.structure=data.structure)

  # is there parent for every id?
  parent.match.id <- x[[2]][!(x[[2]] %in% x[[1]])]
  if (sum(is.na(parent.match.id))!=1 || length(parent.match.id)!=1){
     stop("parentvar and idvar do not fit together.")
  }
  x  <- data.frame(x)

##  less.equal.zero <- sum(x[['sizevar']] <= 0) 
##  infinite <- sum(is.infinite(x[['sizevar']]))


##  if(less.equal.zero > 0){
##    warning(paste("Values in column sizevar should be greater than 0,\n",
##                  "however, your data has ", less.equal.zero, "values less or equal to 0.\n",
##                  sep=""))
##  }
##  if(infinite>0){
##    warning(paste("Values in column sizevar have to be greater than 0 and finite,\n",
##                  "however, your data has", infinite, "infinite values.",
##                  sep=""))
##  }

     
  return(x)
     
}
