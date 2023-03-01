### File R/zzz.R
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


.onLoad<- function(lib, pkg,...)
{

  options(gvis.plot.tag=NULL)
  options(gvis.print.tag="html")
  options(googleVis.viewer=getOption("viewer"))

  ## Set possible gvis.out.options
  ## Output from
  ## unique(unlist(strsplit(names(unlist( gvisTable(data.frame(x=1, y=1)))),".", fixed=TRUE)))

  gvis.tags <- c("type",  "chartid", "html", "header", "chart", "jsHeader", "jsData", "jsDrawChart", 
                 "jsDisplayChart", "jsFooter", "jsChart", "divChart", "caption", "footer")
  options(gvis.tags=gvis.tags)
  
  ## This function are required to set option in googleVis functions,
  ## e.g. to set startZoom and stopZoom windows in gvisAnnotatedTimeLine
  suppressMessages(
  setMethod("toJSON", "Date",
            function(x, container = length(x) > 1 || length(names(x)) > 0, ...) {
              dt <- as.Date(x)
              y <- format(dt,"%Y")
              m <- as.numeric(format(dt,"%m")) -1
              d <- as.numeric(format(dt,"%d"))
              
              tmp <- paste("new Date(",y,",",m,",",d,")",sep="")
              paste(tmp, collapse=", ")
            })
  )
  suppressMessages(
  setMethod("toJSON", "POSIXct",
            function(x, container = length(x) > 1 || length(names(x)) > 0, ...) {
              dt <- as.POSIXct(x)
              y <- format(dt,"%Y")
              m <- as.numeric(format(dt,"%m")) -1
              d <- as.numeric(format(dt,"%d"))
              H <- as.numeric(format(dt,"%H"))
              M <- as.numeric(format(dt,"%M"))
              S <- as.numeric(format(dt,"%S"))
              
              tmp <- paste("new Date(",y,",",m,",",d,",",H,",",M,",",S,")",sep="")
              paste(tmp, collapse=", ")
            })
  )
  suppressMessages(
  setMethod("toJSON", "POSIXlt",
            function(x, container = length(x) > 1 || length(names(x)) > 0, ...) {
              dt <- as.POSIXlt(x)
              y <- format(dt,"%Y")
              m <- as.numeric(format(dt,"%m")) -1
              d <- as.numeric(format(dt,"%d"))
              H <- as.numeric(format(dt,"%H"))
              M <- as.numeric(format(dt,"%M"))
              S <- as.numeric(format(dt,"%S"))
              
              tmp <- paste("new Date(",y,",",m,",",d,",",H,",",M,",",S,")",sep="")
              paste(tmp, collapse=", ")
            })
  ) 
  invisible()
}

.onAttach <- function(lib, pkg,...){
  packageStartupMessage(gvisWelcomeMessage())
}
gvisWelcomeMessage <- function(){
  
 # if(is.null(getOption("viewer")))
    display <- "the standard browser to display its output.\n"
#  else
#    display <- paste("the RStudio Viewer pane to display its output.\n",
#                     "Set options('googleVis.viewer'=NULL) to use your browser.\n",
#                     sep="")
  paste("\n",     
        "Welcome to googleVis version ", packageDescription("googleVis")$Version, "\n",
        "\n",
        "Please read Google's Terms of Use\n",
        "before you start using the package:\n",
        "https://developers.google.com/terms/\n",
        "\n",
        "Note, the plot method of googleVis will by default use\n",
        display,
        "\n",
        "See the googleVis package vignettes for more details,\n",
        "or visit https://mages.github.io/googleVis/.\n",
        "\n",               
        "To suppress this message use:\n",
        "suppressPackageStartupMessages(library(googleVis))\n",  
          sep="")
}
