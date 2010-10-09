### File R/zzz.R
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


.onLoad<- function(lib, pkg, ...)
{
    setMethod("toJSON", "Date",
           function(x, container = length(x) > 1 || length(names(x)) > 0, ...) {
              tmp <- format(as.Date(x),"new Date(%Y,%m,%d)")
              paste(tmp, collapse=", ")
           })

    require(utils)
    cat("\n",paste("Wellcome to googleVis version", packageDescription("googleVis")$Version,
                   "by:\nMarkus Gesmann <markus.gesmann@gmail.com> and Diego de Castillo <decastillo@gmail.com>\n\n"),
        "Type ?googleVis to see the overall documentation and\n",
        "vignette('googleVis') to see the package vignette.\n",
        ##   "which gives you an overview of the graphical user interface of a motion chart.\n\n",
        "See demo(package='googleVis') for a list of demos and type:\n",
        "browseRsp('http://127.0.0.1:8074/library/googleVis/rsp/index.rsp')\n",
        "to view examples in your browser.\n\n",
        "Feel free to send us an email if you would like to keep informed of\n",
        "new versions or if you have any feedback, ideas, suggestions or would\n",
        "like to collaborate.\n\n",
        "More information is available on the googleVis project web-site:\n",
        "http://code.google.com/p/google-motion-charts-with-r/\n\n",
        "Please read also the Google Visualisation API Terms of Use:\n",
        "http://code.google.com/apis/visualization/terms.html\n\n",
        sep='')

    rjsonio <- require(RJSONIO)
    if(!rjsonio){
      cat(paste("\n\n!! WARNING !!\n\n",
                "You will need the RJSONIO package in order to use googleVis.\n",
                "Please install the RJSONIO from Omegahat, see:\n",
                "http://www.omegahat.org/RJSONIO/\n",
                "You can install RJSONIO via:\n",
                "install.packages('RJSONIO', repos = 'http://www.omegahat.org/R', type='source')\n",
                "Please check the R documentation if you are unfamilar with installing packages from source:\n",
                "http://cran.r-project.org/doc/manuals/R-admin.html#Installing-packages\n",
                "\n", sep=""))
    }
    invisible()
}
