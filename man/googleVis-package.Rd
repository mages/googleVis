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
 
 googleVis makes use of the internal R HTTP server to display the
 output locally. A browser with Flash and Internet connection is
 required.
} 
\details{
\tabular{ll}{
	Package: \tab googleVis\cr
	Type: \tab Package\cr
	Version: \tab 0.2.4\cr
	Date: \tab 20011-02-07\cr
	License: \tab GPL version 2 or later\cr
	 }
	}
\author{
	Markus Gesmann, Diego de Castillo
	}
\note{
  See \code{vignette("googleVis")} for an introduction to the
  \code{googleVis} package. 
  
  Of course there are many alternative visualisation toolkits out
  there, e.g. 
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

  For examples using the Google Visualisation API see the Google
  Public Data Explorer: \url{http://www.google.com/publicdata/home}

}

\references{
  \itemize{
	\item googleVis project site: \url{http://code.google.com/p/google-motion-charts-with-r/}
	\item Google Visualisation API: \url{http://code.google.com/apis/visualization/interactive_charts.html}
	\item Google Terms of Use: \url{http://code.google.com/apis/visualization/documentation/gallery/treemap.html}
	}
}
\examples{
\dontrun{
  demo(googleVis)
  }
}
\keyword{ package }


