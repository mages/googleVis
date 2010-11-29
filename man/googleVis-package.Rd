\name{googleVis-package}
\alias{googleVis-package}
\alias{googleVis}
\docType{package}
\title{
Using the Google Visualisation API with R
}
\description{

 googleVis provides an interface between R and the Google Visualisation API. 
 The functions of the package allow the user to visualise data stored
 in R data frames with the Google Visualisation API.

 To view the output a browser with Flash and internet connection is required.
 
} 
\details{
\tabular{ll}{
	Package: \tab googleVis\cr
	Type: \tab Package\cr
	Version: \tab 0.2.1\cr
	Date: \tab 20010-11-30\cr
	License: \tab GPL version 2 or later\cr
	 }
	}
\author{
	Markus Gesmann, Diego de Castillo
	}
\note{
  See \code{vignette("googleVis")} for an introduction to the
  \code{googleVis} package. 

  Many thanks to 
  \itemize{  
    \item{Henrik Bengtsson for providing the \code{R.rsp} package
      (\url{http://www.braju.com/R/}) that allows us to display motion
      charts locally.

      To find out more about how you can integrate googleVis charts into
      rsp-pages type: \code{browseRsp()}.  
    } 
    \item{Duncan Temple Lang for providing the \code{RJSONIO} package
      (\url{http://www.omegahat.org/RJSONIO/}) that allows us to
      transform R objects into JSON objects.
    } 
  }
  For examples using the Google Visualisation API see the Google
  Public Data Explorer: \url{http://www.google.com/publicdata/home}

  Of course there are many alternative visualisation toolkits out there, to
  name but a few:
  \itemize{
    \item Many Eyes:
    \url{http://services.alphaworks.ibm.com/manyeyes/page/Create_a_Visualization.html}    
    \item Open Flash Chart (Flash):
    \url{http://teethgrinder.co.uk/open-flash-chart/} 
    \item OpenLayers (JavaScript): \url{http://www.openlayers.org/} 
    \item Processing (Java): \url{http://processing.org/}
    \item simile (AJAX): \url{http://simile.mit.edu/}
    \item FLARE (ActionScript): \url{http://flare.prefuse.org/}
  }
}

\references{
  \itemize{
	\item googleVis project site: \url{http://code.google.com/p/google-motion-charts-with-r/}
	\item Google Visualisation API: \url{http://code.google.com/apis/visualization/interactive_charts.html}
	\item Google Terms of Use: \url{http://code.google.com/apis/visualization/documentation/gallery/treemap.html}
	}
}
\section{Warning }{
  By default the visualisation files will be stored in the
  \code{rsp/myAnlysis} folder of the googleVis library. The following
  R-command will show the path to this folder:
  
  \code{system.file(file.path("rsp", "myAnalysis"), package="googleVis")}
  
  It is preferable that you have write-access to this directory; so you
  may want to install the package locally (e.g. in your home
  folder). Please contact your system administrator if you need further
  help.
}
\examples{
\dontrun{
  demo(googleVis)
  }
}
\keyword{ package }


