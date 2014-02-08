### File R/gvisTable.R
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



#' Google Table Chart with R
#' \Sexpr{googleChartName <- "table"}
#' \Sexpr{gvisChartName <- "gvisTable"}
#' 
#' 
#' The gvisTable function reads a data.frame and creates text output referring
#' to the Google Visualisation API, which can be included into a web page, or
#' as a stand-alone page. The actual chart is rendered by the web browser.
#' 
#' A table that can be sorted and paged. Table cells can be formatted using
#' format strings, or by directly inserting HTML as cell values. Numeric values
#' are right-aligned; boolean values are displayed as check marks. Users can
#' select single rows either with the keyboard or the mouse. Users can sort
#' rows by clicking on column headers. The header row remains fixed as the user
#' scrolls. The table fires a number of events corresponding to user
#' interaction.
#' 
#' 
#' @param data a \code{\link{data.frame}} to be displayed as a table
#' @param formats named list. If \code{NULL} (default) no specific format will 
#' be used. The named list needs to contain the column names of the data and 
#' the specified format. The format string is a subset of the ICU pattern set. 
#' For instance, {pattern:'#,###\%'} will result in output values "1,000\%", 
#' "750\%", and "50\%" for values 10, 7.5, and 0.5. 
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
#' @author Markus Gesmann \email{markus.gesmann@@gmail.com},
#' 
#' Diego de Castillo \email{decastillo@@gmail.com}
#' @seealso See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for
#' printing and plotting methods.
#' @keywords iplot
#' @examples
#' 
#' ## Please note that by default the googleVis plot command
#' ## will open a browser window and requires Flash and Internet
#' ## connection to display the visualisation.
#' 
#' ## Table with links to wikipedia (flags) 
#' tbl1 <- gvisTable(Population)
#' plot(tbl1)
#' 
#' ## Table with enabled paging
#' tbl2 <- gvisTable(Population, options=list(page='enable', height=300))
#' 
#' plot(tbl2)
#' 
#' ## Table with formating options
#' tbl3 <- gvisTable(Population, formats=list(Population="#,###"))
#' 
#' Population[['% of World Population']] <- Population[['% of World Population']]/100 
#' tbl4 <- gvisTable(Population, formats=list(Population="#,###", 
#'                                            '% of World Population'='#.#%'))
#' plot(tbl4)                                           
#' 
gvisTable <- function(data, options=list(), chartid, formats = NULL){

  my.type <- "Table"
  dataName <- deparse(substitute(data))

  my.options <- list(gvis=modifyList(list(allowHtml=TRUE),options), dataName=dataName,
                     data=list(allowed=c("number","string","date","datetime","boolean")))
  
  checked.data <- gvisCheckTableData(data)
  
  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options, 
                      chartid=chartid, formats = formats) 
  
  return(output)
}


gvisCheckTableData <- function(data){

  # nothing to check at the moment here
  return(data)
}
