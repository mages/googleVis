#' Google Word Tree with R
#'
#' @description
#' The gvisWordTree function reads a data.frame and creates text outpute referring to the Google Visualisation API,
#' which can be included into a web page, or as a stand-alone page. The actual chart is rendered by the web browser.
gvisWordTree <- function(data, textvar = "", sizevar = "", stylevar = "", options = list(), chartid) {
  
  my.type <- "WordTree"
  dataName <- deparse(substitute(data))
  
  my.options <- list(
    gvis = modifyList(list(width = 600, height = 500), options),
    dataName = dataName,
    data = list(textvar = textvar, sizevar = sizevar, stylevar = stylevar, allowed = c("number", "string"))
  )
  
  checked.data <- gvisCheckWordTreeData(data, my.options)
  
  output <- gvisChart(type = my.type, checked.data = checked.data, options = my.options,
                                  chartid = chartid, package = "wordtree")
  
  return(output)
}

gvisCheckWordTreeData <- function(data, options) {
  data.structure <- list(
    textvar  = list(mode = "required", FUN = check.char),
    sizevar  = list(mode = "optional", FUN = check.num.pos),
    stylevar = list(mode = "optional", FUN = check.char)
  )
  
  x <- gvisCheckData(data = data, options = options, data.structure = data.structure)
  
  x <- data.frame(x)
  
  return(x)
}
