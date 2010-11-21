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

gvisAnnotatedTimeLine <- function(data, datevar="",
                                  date.format="%Y/%m/%d",
                                  options=list()){
  

  my.type <- "AnnotatedTimeLine"
  dataName <- deparse(substitute(data))
  
  my.options <- list(gvis=modifyList(list(width = 600, height=300),options), 
		     dataName=dataName,                     
                     data=list(date.format=date.format,
                       allowed=c("number","string","date"))
                     )
  
  
  checked.data <- gvisCheckAnnotatedTimeLineData(data)

  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options)
  
  return(output)
}

### Fixme:
## zoomStartTime and zoomEndTime have to json objects, e.g.
## new Date(2000,05,25)
## However, setting this in the options list as character does not work. 

gvisCheckAnnotatedTimeLineData <- function(data){

  # nothing to check at the moment here
  return(data)
}

