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

gvisGeoMap <- function(data,  idvar="id", options=list()){

  my.type <- "GeoMap"
  dataName <- deparse(substitute(data))

  my.options <- list(gvis=modifyList(list(width = 600),options), dataName=dataName,
                     data=list(allowed=c("number", "string")))
  
  checked.data <- gvisCheckGeoMapData(data, idvar)
  
  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options)
  
  return(output)
}

## Fixme
## gvisGeoMap should also accept Latitude and Longitude
##  df=data.frame(Latitude=47, Longitude=-122, Value=1, HoverText="Hello World")
## plot(gvisGeoMap(df, idvar="Latitude", options=list(dataMode="markers")))


gvisCheckGeoMapData <- function(data, idvar){

  ## currently doesn't do anything.

  id <- match(idvar, names(data))
  if( length(id) < 1 )
      stop("There is a missmatch between the idvar specified and the colnames of your data.")

  data <- data[, c(id, c(1:ncol(data)))[-id]]
  return(data)
}
