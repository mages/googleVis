### Copyright 2010 - 2014 Markus Gesmann
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

#' Google Sankey Chart with R
#' \Sexpr{googleChartName <- "sankey"}
#' \Sexpr{gvisChartName <- "gvisSankey"}
#' @description
#' A sankey diagram is a visualization used to depict a flow from one set of 
#' values to another. The things being connected are called nodes and the 
#' connections are called links. They're named after Captain Sankey, who created 
#' a diagram of steam engine efficiency that used arrows having widths 
#' proportional to heat loss.
#'  
#' @param data data.frame that contains the data to be visualised
#' @param from a string that refers to the column name in 
#'  \code{data} for the source nodes to be used
#' @param to a string that refers to the column name in 
#'  \code{data} for the destination nodes to be used
#' @param weight name of the column with the numerical weight of the connections 
#' @param options list of configuration options.
#' The options are documented in detail by Google online:
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
#' @author Markus Gesmann \email{markus.gesmann@@gmail.com}
#' 
#' @section Warning:
#' The sankey chart may be undergoing substantial revisions in 
#' future Google Charts releases.
#'  
#' @keywords iplot
#' 
#' @examples
#' dat <- data.frame(From=c(rep("A",3), rep("B", 3)), 
#'                   To=c(rep(c("X", "Y", "Z"),2)), 
#'                   Weight=c(5,7,6,2,9,4))
#'
#' sk1 <- gvisSankey(dat, from="From", to="To", weight="Weight")
#' plot(sk1)
#' 
#' sk2 <- gvisSankey(dat, from="From", to="To", weight="Weight",
#'                 options=list(sankey="{link: {color: { fill: '#d799ae' } },
#'                                      node: { color: { fill: '#a61d4c' },
#'                                      label: { color: '#871b47' } }}"))
#' plot(sk2)
#' 

gvisSankey <- function(data, from="", to="", weight="", 
                       options=list(), chartid){

  my.type <- "Sankey"
  dataName <- deparse(substitute(data))

  my.options <- list(gvis=modifyList(list(width=400, height=400),options), dataName=dataName,
                     data=list(from=from, to=to, weight=weight,
                       allowed=c("number", "string"))
                     )

 
  #checked.data <- gvisCheckSankeyData(data, my.options)
  
  output <- gvisChart(type=my.type, checked.data=data, options=my.options,
                      chartid=chartid, package="sankey") 
  
  return(output)
}
# 
# gvisCheckSankeyData <- function(data, options){
# 
#   data.structure <- list(
#                          from = list(mode="required", FUN=check.char),
#                          to = list(mode="required", FUN=check.char),
#                          weight = list(mode="required", FUN=check.num)
#                          )
#   data <- gvisCheckData(data=data, options=options, data.structure=data.structure)
# 
#   return(data)
# }

