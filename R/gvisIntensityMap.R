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

#' Google Intensity Map with R
#' \Sexpr{googleChartName <- "intensitymap"}
#' \Sexpr{gvisChartName <- "gvisIntensityMap"}
#' 
#' An intensity map highlights regions or countries based on relative values.
#' 
#' The gvisIntensityMap function reads a data.frame and creates text output
#' referring to the Google Visualisation API, which can be included into a web
#' page, or as a stand-alone page.
#' 
#' 
#' @param data a \code{data.frame}. The data has to have at least two columns
#' with location name (\code{locationvar}) and any number of numeric columns
#' (\code{numvar}) to be mapped.
#' @param locationvar column name of \code{data} with the geo locations to be
#' analysed. The location has to contain country ISO codes or USA state codes.
#' @param numvar column names of \code{data} with the numeric values to be
#' displayed.
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
#' plotting methods, \code{\link{gvisMap}} and \code{\link{gvisGeoMap}} for an
#' alternative to \code{gvisIntensityMap}.
#' 
#' @keywords iplot
#' @examples
#' 
#' ## Please note that by default the googleVis plot command
#' ## will open a browser window and requires Internet
#' ## connection to display the visualisation.
#' 
#' df=data.frame(country=c("US", "GB", "BR"), val1=c(1,3,4), val2=c(23,12,32))
#' Intensity1 <- gvisIntensityMap(df, locationvar="country", numvar=c("val1", "val2"))
#' plot(Intensity1)
#' 
#' ## Set colours for each tab
#' Intensity2 <- gvisIntensityMap(df,
#'               options=list(colors="['#4682b4', '#0073CF']"))
#' plot(Intensity2)
#' 
#' 
#' 
gvisIntensityMap <- function(data, locationvar="", numvar="", options=list(), chartid){

  my.type <- "IntensityMap"
  dataName <- deparse(substitute(data))

  my.options <- list(gvis=modifyList(list(width = 600),options), 
                     dataName=dataName,
                     data=list(locationvar=locationvar, numvar=numvar,
                     allowed=c("number", "string")))
  
 checked.data <- gvisCheckIntensityMapData(data, locationvar, numvar)

  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options, chartid=chartid)
  
  return(output)
}

gvisCheckIntensityMapData <- function(data, locationvar, numvar){

  if(! is.data.frame(data) ){
    stop("Error: data has to be a data.frame.")
  }
  if(locationvar=="")
    locationvar <- 1
  data[[locationvar]] <- as.character(data[[locationvar]])

   if("" %in% numvar)
    numvar <- 2:ncol(data)

  data[numvar] <-  sapply(data[numvar], as.numeric)

  data <-  data[c(locationvar, numvar)]
  
  return(data)
}
