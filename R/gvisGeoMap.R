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

gvisGeoMap <- function(data, options=list(width = 600, height=500)){

  
  my.options <- list(gvis=options, data=list(allowed=c("number", "string")))
  
  checked.data <- gvisCheckGeoMapData(data)

  
  htmlChart = gvis(type="GeoMap", checked.data, options=my.options)
  
  htmlScaffold <- gvisHtmlWrapper(title=paste("Geo Map:", deparse(substitute(data))))
  
  output <- list(htmlHeader=htmlScaffold[["htmlHeader"]],
                 htmlChart=htmlChart,
                 htmlCaption=htmlScaffold[["htmlCaption"]],
                 htmlFooter=htmlScaffold[["htmlFooter"]]
                 )
  
  class(output) <- c("gvis", class(output))

  return(output)
}


gvisCheckGeoMapData <- function(data){

  ## currently doesn't do anything.
  
  return(data)
}
