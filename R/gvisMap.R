### File R/gvisMap.R
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

gvisMap <- function(data, locationvar="", tipvar="",options=list()){

  my.type <- "Map"
  dataName <- deparse(substitute(data))
  my.options <- list(gvis=modifyList(list(showTip = TRUE),options), 
		     dataName=dataName, 
                     data=list(locationvar=locationvar, tipvar=tipvar,
		      allowed=c("number","string"))
                     )
  
  checked.data <- gvisCheckMapData(data, my.options)

  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options)
  
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
