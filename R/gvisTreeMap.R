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

gvisTreeMap <- function(data, options=list(width = 600, height=500)){

  my.type <- "TreeMap"
  dataName <- deparse(substitute(data))
  my.options <- list(gvis=options, dataName=dataName, data=list(allowed=c("number", "string")))
  
  checked.data <- gvisCheckTreeMapData(data)

  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options)
  
  ## fixme: should be in gvisFormat
  htmlChart <- output$html$Chart
  htmlChart <- gsub('"NA"', 'null', htmlChart)
  htmlChart <- gsub(sprintf('<div id="%s">', output$chartid),
                    paste('<div id="',output$chartid,'" style="width: ', options$width, 'px; height: ', options$height, 'px;">', sep=""), htmlChart)
  
  output$html$Chart <- htmlChart
  
  return(output)
}


gvisCheckTreeMapData <- function(data){

  # nothing to check at the moment here
  return(data)
}
