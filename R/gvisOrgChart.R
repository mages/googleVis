### File R/gvisOrgChart.R
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

gvisOrgChart <- function(data, idvar="", parentvar="", tipvar="", options=list(), chartid){

  my.type <- "OrgChart"
  dataName <- deparse(substitute(data))
  my.options <- list(gvis=modifyList(list(width = 600, height=400), options), 
		     dataName=dataName, 
                     data=list(idvar=idvar, parentvar=parentvar, tipvar=tipvar,
		      allowed=c("string"))
                     )
  
  checked.data <- gvisCheckOrgChartData(data, my.options)

  output <- gvisChart(type=my.type, checked.data=checked.data, options=my.options, chartid)
  
  return(output)
}


## plot(gvisOrgChart(Regions))
## plot(gvisOrgChart(Regions, options=list(width=600, height=400, size='large', allowCollapse=TRUE)))

gvisCheckOrgChartData <- function(data, options){

  data.structure <- list(
        	     idvar     = list(mode="required",FUN=check.char),
        	     parentvar = list(mode="required",FUN=check.char),
        	     tipvar  = list(mode="required",FUN=check.char))
	
  x <- gvisCheckData(data=data,options=options,data.structure=data.structure)

  # is there parent for every id?
  parent.match.id <- x[[2]][!(x[[2]] %in% x[[1]])]
  if (sum(is.na(parent.match.id))!=1 || length(parent.match.id)!=1){
     stop("parentvar and idvar do not fit together.")
  }

  return(data.frame(x))
}
