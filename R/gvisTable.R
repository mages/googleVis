### File R/gvisTable.R
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

gvisTable <- function(data, options=list(), chartid){

  my.type <- "Table"
  dataName <- deparse(substitute(data))

  my.options <- list(gvis=modifyList(list(allowHtml=TRUE),options), dataName=dataName,
                     data=list(allowed=c("number","string","date","datetime","boolean")))
  
  checked.data <- gvisCheckTableData(data)
  
  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options, chartid=chartid) 
  
  return(output)
}


gvisCheckTableData <- function(data){

  # nothing to check at the moment here
  return(data)
}
