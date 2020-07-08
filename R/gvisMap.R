### File R/gvisMap.R
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



#' Google Maps with R
#' \Sexpr{googleChartName <- "map"}
#' \Sexpr{gvisChartName <- "gvisMap"}
#' 
#' The gvisMap function reads a data.frame and creates text output referring to
#' the Google Visualisation API, which can be included into a web page, or as a
#' stand-alone page.
#' 
#' The maps are the well known Google Maps.
#' 
#' @param data a \code{data.frame}. The data has to have at least two columns
#' with location name (\code{locationvar}) and the variable to display the text
#' in the tip icon (\code{tipvar}).
#' 
#' @param locationvar column name of \code{data} with the geo locations to be
#' analysed. The locations can be provide in two formats: \describe{
#' \item{Format 1}{'latitude:longitude'. See the example below.} \item{Format
#' 2}{The first column should be a string that contains an address. This
#' address should be as complete as you can make it.  } }
#' @param tipvar column name of \code{data} with the string text displayed over
#' the tip icon.
#' @param options list of configuration options for Google Map.  
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
#' @author Markus Gesmann \email{markus.gesmann@@gmail.com},
#' 
#' Diego de Castillo \email{decastillo@@gmail.com}
#' @seealso
#' 
#' See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for printing and
#' plotting methods, \code{\link{gvisGeoChart}} for an alternative to \code{gvisMap}.
#' 
#' @keywords iplot
#' @examples
#' 
#' ## Please note that by default the googleVis plot command
#' ## will open a browser window and requires Internet
#' ## connection to display the visualisation.
#'   
#' ## Example with latitude and longitude information
#' ## Plot Hurricane Andrew (1992) storm path:
#' 
#' data(Andrew)
#' 
#' M1 <- gvisMap(Andrew, "LatLong" , "Tip",
#'               options=list(showTip=TRUE, showLine=TRUE, enableScrollWheel=TRUE,
#'                            mapType='hybrid', useMapTypeControl=TRUE,
#'                            width=800,height=400))
#' 
#' plot(M1) 
#' 
#' 
#' ## Example with address, here UK post-code and some html code in tooltip
#' 
#' df <- data.frame(Postcode=c("EC3M 7HA", "EC2P 2EJ"),
#'                  Tip=c("<a href='https://www.lloyds.com'>Lloyd's</a>", 
#'                  "<a href='https://www.guildhall.cityoflondon.gov.uk/'>Guildhall</a>"))
#'  
#' M2 <- gvisMap(df, "Postcode", "Tip",
#'               options=list(showTip=TRUE, mapType='normal',
#'               enableScrollWheel=TRUE))
#'  
#' plot(M2)
#' 
#' ## Change mapping icons
#' M3 <- gvisMap(df, "Postcode", "Tip",
#'               options=list(showTip=TRUE, mapType='normal',
#'               enableScrollWheel=TRUE,
#'               icons=paste0("{",
#'               "'default': {'normal': 'https://icons.iconarchive.com/",
#'               "icons/icons-land/vista-map-markers/48/",
#'               "Map-Marker-Ball-Azure-icon.png',\n",
#'               "'selected': 'https://icons.iconarchive.com/",
#'               "icons/icons-land/vista-map-markers/48/",
#'               "Map-Marker-Ball-Right-Azure-icon.png'",
#'               "}}")))
#'                         
#' plot(M3)

gvisMap <- function(data, locationvar="", tipvar="",options=list(), chartid){

  my.type <- "Map"
  dataName <- deparse(substitute(data))
  my.options <- list(gvis=modifyList(list(showTip = TRUE),options), 
		     dataName=dataName, 
                     data=list(locationvar=locationvar, tipvar=tipvar,
		      allowed=c("number","string"))
                     )
  
  checked.data <- gvisCheckMapData(data, my.options)

  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options, chartid=chartid)
  
  return(output)
}

gvisCheckMapData <- function(data, options){

  data.structure <- list(
        	     locationvar   = list(mode="required",FUN=check.location),
        	     tipvar  = list(mode="required",FUN=check.char))
	
  x <- gvisCheckData(data=data,options=options,data.structure=data.structure)

  if (sum(nchar(gsub("[[:digit:].-]+:[[:digit:].-]+", "", x[[1]]))) == 0){
  	# split first index and delete this one
  	latlong <- as.data.frame(do.call("rbind",strsplit(as.character(x[[1]]),':')))
  	x[[1]] <- NULL
	varNames <- names(x)
  	x$Latitude <- as.numeric(as.character(latlong$V1))
  	x$Longitude <- as.numeric(as.character(latlong$V2))
    	x <- x[c("Latitude","Longitude",varNames)]
  }
  
  return(data.frame(x))
}
