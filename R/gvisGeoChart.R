### File R/gvisGeoMap.R
### Part of the R package googleVis
### Copyright 2010, 2011, 2012, 2013 Markus Gesmann, Diego de Castillo

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

gvisGeoChart <- function(data, locationvar="", ## numvar="",
                         colorvar="", sizevar="",
                         hovervar="",
                         options=list(), chartid){

  my.type <- "GeoChart"
  dataName <- deparse(substitute(data))

  my.options <- list(gvis=modifyList(list(width = 556, height=347),options), 
                     dataName=dataName,
                     data=list(locationvar=locationvar,
                               hovervar=hovervar,  
                               ## numvar=numvar,
                       colorvar=colorvar,
                       sizevar=sizevar,
                       
                       allowed=c("number", "string")
                       )
                     )
  
  checked.data <- gvisCheckGeoChartData(data, my.options)

  if(any("numeric" %in% lapply(checked.data[,1], class))){
    my.options <- modifyList(list(gvis=list()), my.options)
  }
  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options, chartid=chartid)
  
  return(output)
}


gvisCheckGeoChartData <- function(data, options){

  data.structure <- list(
        	     locationvar = list(mode="required",FUN=check.location),
        	     hovervar    = list(mode="optional",FUN=check.char),
        	     ## numvar      = list(mode="optional",FUN=check.num),
                     colorvar      = list(mode="optional",FUN=check.num),
        	     sizevar    = list(mode="optional",FUN=check.num))
	
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

## gvisCheckGeoMapData <- function(data, options){
## 
##   data.structure <- list(
##                          locationvar = list(mode="required",FUN=check.location),
##                          numvar      = list(mode="optional",FUN=check.num),
##                          sizevar     = list(mode="optional",FUN=check.num)
##                          )
## 	
##   x <- gvisCheckData(data=data,options=options,data.structure=data.structure)
##   
##   return(data.frame(x))
## }
