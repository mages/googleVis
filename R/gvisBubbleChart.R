### File R/gvisCoreCharts.R
### Part of the R package googleVis
### Copyright 2011 Markus Gesmann, Diego de Castillo

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

gvisBubbleChart <- function(data, idvar="", xvar="", yvar="", colorvar="", sizevar="", options=list(), chartid){

  my.type <- "BubbleChart"
  dataName <- deparse(substitute(data))

  my.options <- list(gvis=options, 
                     dataName=dataName,
                     data=list(idvar=idvar,
                       xvar=xvar,
                       yvar=yvar,                       
                       colorvar=colorvar,
                       sizevar=sizevar,
                       allowed=c("number", "string")
                       )
                     )

 
  checked.data <- gvisCheckBubbleChartData(data, my.options)

  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options, chartid=chartid,
                      package="corechart")
  
  return(output)
}
   
  


gvisCheckBubbleChartData <- function(data, options){

   varNames <- names(data)

##   typeMotionChart[[options$data$timevar]] <-
##     testTimevar(x[[options$data$timevar]], options$data$date.format)

   
  data.structure <- list(
                         idvar = list(mode="required", FUN=check.char),
                         xvar = list(mode="required", FUN=check.num),
                         yvar  = list(mode="required", FUN=check.num),
                         colorvar  = list(mode="optional", FUN=check.char),
                         sizevar  = list(mode="optional", FUN=check.num))
	
  x <- gvisCheckData(data=data,options=options,data.structure=data.structure)
   x <- data.frame(x)
   
  return(x)
}
