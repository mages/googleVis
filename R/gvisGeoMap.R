### File R/gvisGeoMap.R
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

gvisGeoMap <- function(data, locationvar="", numvar="", hovervar="", options=list(), chartid){

  my.type <- "GeoMap"
  dataName <- deparse(substitute(data))

  my.options <- list(gvis=modifyList(list(width = 600),options), 
                     dataName=dataName,
                     data=list(locationvar=locationvar, numvar=numvar,
                       hovervar=hovervar,  
                     allowed=c("number", "string")))
  
  checked.data <- gvisCheckGeoMapData(data, my.options)

  if(any("numeric" %in% lapply(checked.data[,c(1,2)],class))){
    my.options <- modifyList(list(gvis=list(dataMode = "markers")), my.options)
  }
  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options, chartid=chartid)
  
  return(output)
}

gvisCheckGeoMapData <- function(data, options){

  data.structure <- list(
        	     locationvar = list(mode="required",FUN=check.location),
        	     numvar      = list(mode="optional",FUN=check.num),
        	     hovervar    = list(mode="optional",FUN=check.char))
	
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


gvisGeoChart <- function(data, locationvar="", numvar="", hovervar="", options=list(), chartid){

  my.type <- "GeoChart"
  dataName <- deparse(substitute(data))

  my.options <- list(gvis=modifyList(list(width = 600),options), 
                     dataName=dataName,
                     data=list(locationvar=locationvar, numvar=numvar,
                       hovervar=hovervar,  
                     allowed=c("number", "string")))
  
  checked.data <- gvisCheckGeoMapData(data, my.options)

  if(any("numeric" %in% lapply(checked.data[,c(1,2)],class))){
    my.options <- modifyList(list(gvis=list(dataMode = "markers")), my.options)
  }
  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options, chartid=chartid)
  
  return(output)
}
