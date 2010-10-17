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

gvisTreeMap <- function(data, idvar="", parentvar="", sizevar="", colorvar="",options=list()){

  my.type <- "TreeMap"
  dataName <- deparse(substitute(data))
  my.options <- list(gvis=modifyList(list(width = 600, height=500),options), 
		     dataName=dataName, 
                     data=list(idvar=idvar, parentvar=parentvar, sizevar=sizevar,colorvar=colorvar,
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
  # 
  data.structure <- list(
        	     idvar     = list(mode="required",FUN=check.char),
        	     parentvar = list(mode="required",FUN=check.char),
        	     sizevar   = list(mode="required",FUN=check.num.pos),
        	     colorvar  = list(mode="optional",FUN=check.num))
	
  x <- gvisCheckData(data=data,options=options,data.structure=data.structure)

# does not fit in new check syntax  
#  ## check parent match id
#  parent.match.id <- x[[options$data$parentvar]][!(x[[options$data$parentvar]] %in% x[[options$data$idvar]])]
#  if (sum(is.na(parent.match.id))!=1 || length(parent.match.id)!=1){
#     stop("parentvar and idvar does not fit together.")
#  }

  return(data.frame(x))
}
