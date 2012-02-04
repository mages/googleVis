### File R/gvisPieChart.R
### Part of the R package googleVis
### Copyright 2010, 2011, 2012 Markus Gesmann, Diego de Castillo

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

gvisPieChart <- function(data, labelvar="", numvar="", options=list(), chartid){

  my.type <- "PieChart"
  dataName <- deparse(substitute(data))

  my.options <- list(gvis=modifyList(list(allowHtml=TRUE),options), dataName=dataName,
                     data=list(locationvar=labelvar, numvar=numvar,
                       allowed=c("number", "string"))
                     )

 
  checked.data <- gvisCheckPieChartData(data, my.options)
  
  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options,
                      chartid=chartid, package="corechart") 
  
  return(output)
}

## plot(gvisPieChart(CityPopularity))

gvisGauge <- function(data, labelvar="", numvar="", options=list(), chartid){

  my.type <- "Gauge"
  dataName <- deparse(substitute(data))

  my.options <- list(gvis=modifyList(list(allowHtml=TRUE),options), dataName=dataName,
                     data=list(locationvar=labelvar, numvar=numvar,
                       allowed=c("number", "string"))
                     )

 
  checked.data <- gvisCheckPieChartData(data, my.options)
  
  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options,
                      chartid=chartid, package="gauge") 
  
  return(output)
}


## plot(gvisGauge(CityPopularity, options=list(min=0, max=800, greenFrom=500,
##         greenTo=800, yellowFrom=300, yellowTo=500, redFrom=0, redTo=300)))

gvisCheckPieChartData <- function(data, options){

  data.structure <- list(
                         locationvar = list(mode="required",FUN=check.location),
                         numvar = list(mode="required",FUN=check.num)
                         )
  x <- gvisCheckData(data=data, options=options, data.structure=data.structure)

  return(data)
}

