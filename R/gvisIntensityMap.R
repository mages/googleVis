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

gvisIntensityMap <- function(data, locationvar="", numvar="", options=list(), chartid){

  my.type <- "IntensityMap"
  dataName <- deparse(substitute(data))

  my.options <- list(gvis=modifyList(list(width = 600),options), 
                     dataName=dataName,
                     data=list(locationvar=locationvar, numvar=numvar,
                     allowed=c("number", "string")))
  
 checked.data <- gvisCheckIntensityMapData(data, locationvar, numvar)

  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options, chartid=chartid)
  
  return(output)
}

gvisCheckIntensityMapData <- function(data, locationvar, numvar){

  if(! is.data.frame(data) ){
    stop("Error: data has to be a data.frame.")
  }
  if(locationvar=="")
    locationvar <- 1
  data[[locationvar]] <- as.character(data[[locationvar]])

   if("" %in% numvar)
    numvar <- 2:ncol(data)

  data[numvar] <-  sapply(data[numvar], as.numeric)

  data <-  data[c(locationvar, numvar)]
  
  return(data)
}
