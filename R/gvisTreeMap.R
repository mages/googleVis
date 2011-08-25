### File R/gvisTreeMap.R
### Part of the R package googleVis
### Copyright 2010, 2011 Markus Gesmann, Diego de Castillo

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

gvisTreeMap <- function(data, idvar="", parentvar="", sizevar="", colorvar="",options=list(), chartid){

  my.type <- "TreeMap"
  dataName <- deparse(substitute(data))
  my.options <- list(gvis=modifyList(list(width = 600, height=500),options), 
		     dataName=dataName, 
                     data=list(idvar=idvar, parentvar=parentvar, sizevar=sizevar,colorvar=colorvar,
		      allowed=c("number", "string"))
                     )
  
  checked.data <- gvisCheckTreeMapData(data, my.options)

  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options, chartid)
  
  return(output)
}

gvisCheckTreeMapData <- function(data, options){

  data.structure <- list(
        	     idvar     = list(mode="required",FUN=check.char),
        	     parentvar = list(mode="required",FUN=check.char),
        	     sizevar   = list(mode="required",FUN=check.num.pos),
        	     colorvar  = list(mode="required",FUN=check.num))
	
  x <- gvisCheckData(data=data,options=options,data.structure=data.structure)

  # is there parent for every id?
  parent.match.id <- x[[2]][!(x[[2]] %in% x[[1]])]
  if (sum(is.na(parent.match.id))!=1 || length(parent.match.id)!=1){
     stop("parentvar and idvar do not fit together.")
  }
  x  <- data.frame(x)

##  less.equal.zero <- sum(x[['sizevar']] <= 0) 
##  infinite <- sum(is.infinite(x[['sizevar']]))


##  if(less.equal.zero > 0){
##    warning(paste("Values in column sizevar should be greater than 0,\n",
##                  "however, your data has ", less.equal.zero, "values less or equal to 0.\n",
##                  sep=""))
##  }
##  if(infinite>0){
##    warning(paste("Values in column sizevar have to be greater than 0 and finite,\n",
##                  "however, your data has", infinite, "infinite values.",
##                  sep=""))
##  }

     
  return(x)
     
}
