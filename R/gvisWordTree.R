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

#' Google Word Tree with R
#' \Sexpr{googleChartName <- "wordtree"}
#' \Sexpr{gvisChartName <- "gvisWordTree"}
#'
#' @description
#' A word tree depicts multiple parallel sequences of words. It could be used to 
#' show which words most often follow or precede a target word 
#' (e.g., "Cats are...") or to show a hierarchy of terms (e.g., a decision tree).
#' 
#' @param data \code{data.frame} that contains the data to be visualised
#' @param textvar a string that refers to the column name in \code{data} that
#' contains the text to be used.
#' @param sizevar a string that refers to the column name in \code{data} that
#' contains the size of the words in the word tree.
#' @param stylevar a string that refers to the column name in \code{data} that
#' contains the stlye of the text. 
#' For no warning message in plot, should be called \code{<textvar>.style}.
#' @param idvar (only when format is explicit) a string that refers to the 
#' column name in \code{data} that contains the unique ID of the text.
#' @param parentvar (only when format is explicit) a string that refers to the 
#' column name in \code{data} that contains the ID of the parent of the text.
#' @param method a string to say whether the word tree is either:
#' \itemize{
#' \item{\code{implicit}}{ - will weight and connect automatically, or} 
#' \item{\code{explicit}}{ - can specify weights and connections}
#' }
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
#' @author Ashley Baldry
#' 
#' @section Warning:
#' The word tree chart may be undergoing substantial revisions in 
#' future Google Charts releases.
#'  
#' @keywords iplot
#' 
#' @examples
#' ## Please note that by default the googleVis plot command
#' ## will open a browser window and requires Internet
#' ## connection to display the visualisation.
#' 
#' wt1 <- gvisWordTree(Cats, textvar = "Phrase")
#' plot(wt1)
#' 
#' Cats2 <- Cats
#' Cats2$Phrase.style <- ifelse(Cats$Sentiment >= 7, "green", 
#'                              ifelse(Cats$Sentiment <= 3, "red", "black"))
#'                              
#' wt2 <- gvisWordTree(Cats2, textvar = "Phrase", stylevar = "Phrase.style",
#'                     options = list(fontName = "Times-Roman",
#'                                    wordtree = "{word: 'cats'}",
#'                                    backgroundColor = "#cba"))
#' plot(wt2)
#' 
#' # Explicit word tree
#' exp.data <- data.frame(id = as.numeric(0:9),
#'                        label = letters[1:10],
#'                        parent = c(-1, 0, 0, 0, 2, 2, 4, 6, 1, 7),
#'                        size = c(10, 5, 3, 2, 2, 2, 1, 1, 5, 1),
#'                        stringsAsFactors = FALSE)
#' 
#' wt3 <- gvisWordTree(exp.data, idvar = "id", textvar = "label", 
#'                     parentvar = "parent", sizevar = "size",
#'                     options = list(wordtree = "{format: 'explicit'}"),
#'                     method = "explicit")
#' plot(wt3)
#' 
gvisWordTree <- function(data, textvar = "", sizevar = "", stylevar = "", idvar = "", 
                         parentvar = "", options = list(), method = "implicit", chartid) {
  
  my.type <- "WordTree"
  dataName <- deparse(substitute(data))
  
  if (method == "implicit") {
    data.list <- list(textvar = textvar, sizevar = sizevar, stylevar = stylevar, allowed = c("number", "string"))
  } else {
    data.list <- list(idvar = idvar, textvar = textvar, parentvar = parentvar, sizevar = sizevar,
                      stylevar = stylevar, allowed = c("number", "string"))
  }
  my.options <- list(gvis = modifyList(list(width = 600, height = 500), options),
                     dataName = dataName, data = data.list)
  
  checked.data <- gvisCheckWordTreeData(data, my.options, method)
  
  output <- gvisChart(type = my.type, checked.data = checked.data, options = my.options,
                                  chartid = chartid, package = "wordtree")
  
  return(output)
}

gvisCheckWordTreeData <- function(data, options, method) {
  if (method == "explicit") {
    data.structure <- list(
      idvar     = list(mode = "required", FUN = check.num),
      textvar   = list(mode = "required", FUN = check.char),
      parentvar = list(mode = "required", FUN = check.num),
      sizevar   = list(mode = "required", FUN = check.num.pos),
      stylevar  = list(mode = "optional", FUN = check.char)
    )
  } else {
    data.structure <- list(
      textvar  = list(mode = "required", FUN = check.char),
      sizevar  = list(mode = "optional", FUN = check.num.pos),
      stylevar = list(mode = "optional", FUN = check.char)
    )  
  }
  
  x <- gvisCheckData(data = data, options = options, data.structure = data.structure)
  
  x <- data.frame(x)
  
  return(x)
}
