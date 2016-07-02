### File R/gvisOrgChart.R
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



#' Google Org Chart with R
#' \Sexpr{googleChartName <- "orgchart"}
#' \Sexpr{gvisChartName <- "gvisOrgChart"}
#' 
#' An organizational chart that supports selection.
#' 
#' The gvisOrgChart function reads a data.frame and creates text output
#' referring to the Google Visualisation API, which can be included into a web
#' page, or as a stand-alone page. The actual chart is rendered by the web
#' browser.
#' 
#' 
#' @param data a \code{data.frame}. The data has to have at least three
#' columns. Each row in the data table describes one node (a rectangle in the
#' graph). Each node (except the root node) has one or more parent nodes. Each
#' node is sized and colored according to its values relative to the other
#' nodes currently shown.
#' @param idvar column name of \code{data} describing the ID for each node. It
#' should be unique among all nodes, and can include any characters, including
#' spaces. This is shown on the node. You can specify a formatted value to show
#' on the chart instead, but the unformatted value is still used as the ID.
#' @param parentvar column name of \code{data} that match to entries in
#' \code{idvar}. If this is a root node, leave this \code{NA}. Only one root is
#' allowed.
#' @param tipvar column name of \code{data} for the tip variable. Tool-tip text
#' to show, when a user hovers over this node.
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
#' @author Markus Gesmann \email{markus.gesmann@@gmail.com},
#' 
#' Diego de Castillo \email{decastillo@@gmail.com}
#' @seealso
#' 
#' See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for printing and
#' plotting methods.
#' @keywords iplot
#' 
#' @examples
#' 
#' ## Please note that by default the googleVis plot command
#' ## will open a browser window and requires Internet
#' ## connection to display the visualisation.
#' 
#' Regions
#' Org1 <- gvisOrgChart(Regions, idvar = "Region", parentvar = "Parent", 
#'      			      tipvar="Val")
#' plot(Org1)
#' 
#' ## Set a few options
#' Org2 <- gvisOrgChart(Regions, idvar = "Region", parentvar = "Parent", 
#'      			      tipvar="Val", 
#'      		     options=list(width=600, height=400,
#'                      	          size='large', allowCollapse=TRUE))
#' plot(Org2)
#' 



gvisOrgChart <- function(data, idvar="", parentvar="", tipvar="", options=list(), chartid){

  my.type <- "OrgChart"
  dataName <- deparse(substitute(data))
  my.options <- list(gvis=modifyList(list(width = 600, height=400), options), 
		     dataName=dataName, 
                     data=list(idvar=idvar, parentvar=parentvar, tipvar=tipvar,
		      allowed=c("string"))
                     )
  
  checked.data <- gvisCheckOrgChartData(data, my.options)

  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options, chartid)
  
  return(output)
}


## plot(gvisOrgChart(Regions))
## plot(gvisOrgChart(Regions, options=list(width=600, height=400, size='large', allowCollapse=TRUE)))

gvisCheckOrgChartData <- function(data, options){

  data.structure <- list(
        	     idvar     = list(mode="required",FUN=check.char),
        	     parentvar = list(mode="required",FUN=check.char),
        	     tipvar  = list(mode="required",FUN=check.char))
	
  x <- gvisCheckData(data=data,options=options,data.structure=data.structure)

  # is there parent for every id?
  parent.match.id <- x[[2]][!(x[[2]] %in% x[[1]])]
  if (sum(is.na(parent.match.id))!=1 || length(parent.match.id)!=1){
     stop("parentvar and idvar do not fit together.")
  }

  return(data.frame(x))
}
