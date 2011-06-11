### File R/zzz.R
### Part of the R package googleVis
### Copyright 2010, 2011 Markus Gesmann, Diego de Castillo

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
  setMethod("toJSON", "Date",
            function(x, container = length(x) > 1 || length(names(x)) > 0, ...) {
              dt <- as.Date(x)
              y <- format(dt,"%Y")
              m <- as.numeric(format(dt,"%m")) -1
              d <- as.numeric(format(dt,"%d"))
              
              tmp <- paste("new Date(",y,",",m,",",d,")",sep="")
              paste(tmp, collapse=", ")
            })
  
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

  library(utils)
  packageStartupMessage(gvisWelcomeMessage())
  invisible()
}

gvisWelcomeMessage <- function(){
  
  paste("To suppress the following message use the statement:\n",
        "suppressPackageStartupMessages(library(googleVis))\n\n",       
        "Welcome to googleVis version ", packageDescription("googleVis")$Version, "\n\n",
        "Type ?googleVis to access the overall documentation and\n",
        "vignette('googleVis') for the package vignette.\n",
        "You can execute the demo of the package via: demo(googleVis)\n\n",  
        "More information is available on the googleVis project web-site:\n",
        "http://code.google.com/p/google-motion-charts-with-r/\n\n",
        
        "Please read also the Google Visualisation API Terms of Use:\n",
        "http://code.google.com/apis/visualization/terms.html\n\n",
        
        "Feel free to send us an email <rvisualisation@gmail.com>\n",
        "if you would like to be keept informed of new versions,\n",
        "or if you have any feedback, ideas, suggestions or would\n",
        "like to collaborate.\n",
          sep="")
}
