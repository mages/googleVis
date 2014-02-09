### Part of the R package googleVis
### Copyright 2011 - 2014 Markus Gesmann, Diego de Castillo

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


#' Google Histogram Chart with R
#' \Sexpr{googleChartName <- "histogram"}
#' \Sexpr{gvisChartName <- "gvisHistogram"}
#' 
#' The gvisHistogram function reads a data.frame and creates text output
#' referring to the Google Visualisation API, which can be included into a web
#' page, or as a stand-alone page. The actual chart is rendered by the web
#' browser using SVG or VML.
#' 
#' 
#' @param data a \code{\link{data.frame}} to be displayed as a histogram. 
#' Each column will be displayed as a histogram.
#' 
#' @param options list of configuration options, see
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
#' generated based on chart type and \code{\link{tempfile}}.
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
#' @seealso See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for
#' printing and plotting methods
#' @keywords iplot
#' @examples
#' 
#' ## Please note that by default the googleVis plot command
#' ## will open a browser window and requires an internet
#' ## connection to display the visualisation.
#' 
#' 
#' hist1 <- gvisHistogram(dino)
#' plot(hist1)
#' 
#' ## Histogram of the top 20 countries
#' pop <- Population[1:20,c("Country", "Population")]
#' pop=transform(pop, Population=round(Population/1e6))
#' 
#' hist2 <- gvisHistogram(pop, option=list(title="Country Populations",
#'                                     legend="{ position: 'none' }",
#'                                     colors="['green']"))
#' plot(hist2)
#'                                     
#' set.seed(123)
#' dat=data.frame(A=rpois(100, 20), 
#'                B=rpois(100, 5), 
#'                C=rpois(100, 50))
#' hist3 <- gvisHistogram(dat, options=list(
#'                        legend="{ position: 'top', maxLines: 2 }",
#'                        colors="['#5C3292', '#1A8763', '#871B47']"))
#' 
#' plot(hist3)

gvisHistogram <- function(data, options=list(), chartid){##, editor
  
  my.type <- "Histogram"
  dataName <- deparse(substitute(data))
  
  my.options <- list(gvis=modifyList(list(allowHtml=TRUE),options), 
                     dataName=dataName,
                     data=list(allowed=c("number", "string")))
  
  output <- gvisChart(type=my.type, checked.data=data, 
                      options=my.options, chartid=chartid, 
                      package="corechart") 
}
