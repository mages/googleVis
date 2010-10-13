### File R/gvisTreeMap.R
### Part of the R package googleVis
### Copyright 2010 Markus Gesmann, Diego de Castillo

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

gvisTreeMap <- function(data, idvar="", parentvar="", options=list()){

  my.type <- "TreeMap"
  dataName <- deparse(substitute(data))
  my.options <- list(gvis=modifyList(list(width = 600, height=500),options), 
		     dataName=dataName, 
                     data=list(idvar=idvar, parentvar=parentvar,
		      allowed=c("number", "string"))
                     )
  
  checked.data <- gvisCheckTreeMapData(data, my.options)

  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options)
  
  ## fixme: should be in gvisFormat
  htmlChart <- output$html$Chart
  htmlChart <- gsub(sprintf('<div id="%s">', output$chartid),
                    paste('<div id="',output$chartid,'" style="width: ', my.options$gvis$width, 'px; height: ', my.options$gvis$height, 'px;">', sep=""), htmlChart)
  
  output$html$Chart <- htmlChart
  
  return(output)
}


gvisCheckTreeMapData <- function(data, options){

  ## Convert data.frame to list
  x <- as.list(data)
  varNames <- names(x)
  
  ## Check if idvar and parentvar match columns in the data
  idvar.parentvar.pos <- match(c(options$data$idvar, options$data$parentvar), varNames)
  if(sum(!is.na(idvar.parentvar.pos)) < 2){
	if (length(varNames)<2){    
           stop("There is a missmatch between the idvar and parentvar specified and the colnames of your data.")
        } else {
           options$data$idvar <- varNames[1]
           options$data$parentvar <- varNames[2]           
        }
  }
  idvar.parentvar.pos <- match(c(options$data$idvar, options$data$parentvar), varNames)
  
  ## idvar,parentvar has to be a character, so lets try to convert it into a character
  x[[options$data$idvar]] <- as.character(x[[options$data$idvar]])
  x[[options$data$parentvar]] <- as.character(x[[options$data$parentvar]])

  ## Check if idvar, parentvar is character
  if(! is.character(x[[options$data$idvar]])){
    stop(paste("The idvar has to be of character format. Currently it is", class(x[[options$data$idvar]])))
  }
  if(! is.character(x[[options$data$parentvar]])){
    stop(paste("The parentvar has to be of character format. Currently it is", class(x[[options$data$parentvar]])))
  }
  
  varOthers <- varNames[ -idvar.parentvar.pos  ]
  # only numeric is allowed for the rest, all other columns are dropped
  varOthers <- names(grep(TRUE,sapply(x[varOthers],is.numeric),value=TRUE))
  # only one or two more numeric are allowed, fixme: first must be > 0  
  varOthers <- if (length(varOthers)<=2) varOthers else varOthers[c(1,2)]
  varOrder <- c(options$data$idvar, options$data$parentvar, varOthers)
  x <- x[varOrder]
  
  ## check parent match id
  parent.match.id <- x[[options$data$parentvar]][!(x[[options$data$parentvar]] %in% x[[options$data$idvar]])]
  if (sum(is.na(parent.match.id))!=1 || length(parent.match.id)!=1){
     stop("parentvar and idvar does not fit together.")
  }
  return(data.frame(x))
}
