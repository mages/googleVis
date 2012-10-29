\name{googleVis-package}
\alias{googleVis-package}
\alias{googleVis}
\docType{package}
\title{
Using the Google Chart Tools with R
}
\description{
  The googleVis package provides an interface between R and the Google
  Chart Tools. It allows users to create web pages with interactive
  charts based on R data frames, using the Google Chart Tools and to
  display them either via the local R HTTP help server or within their
  own sites, without uploading the data to Google.
  
  A modern browser with Internet connection is required and for some
  charts Flash. Please visit the project web site for more information
  and examples:

  \url{http://code.google.com/p/google-motion-charts-with-r/}.
} 
\details{
\tabular{ll}{
	Package: \tab googleVis\cr
	Type: \tab Package\cr
	Version: \tab 0.3.3\cr
	Date: \tab 2012-10-31\cr
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
    \item d3js: \url{http://d3js.org}
    \item Many Eyes:
    \url{http://www-958.ibm.com/software/data/cognos/manyeyes/}    
    \item Open Flash Chart (Flash):
    \url{http://teethgrinder.co.uk/open-flash-chart/} 
    \item OpenLayers (JavaScript): \url{http://www.openlayers.org/} 
    \item Processing (Java): \url{http://processing.org/}
    \item simile (AJAX): \url{http://simile.mit.edu/}
    \item FLARE (ActionScript): \url{http://flare.prefuse.org/}
  }

  See the Google Public Data Explorer for examples using the Google
  Chart Tools: \url{http://www.google.com/publicdata/home}
  
  Other R packages of interest to you might be:
  \itemize{
    \item plotGoogleMaps: Plot HTML output with Google Maps API and your own data. \url{http://cran.r-project.org/web/packages/plotGoogleMaps/}
    \item RgoogleMaps: Overlays on Google map tiles in R. \url{http://cran.r-project.org/web/packages/RgoogleMaps/index.html}
    \item animation: A Gallery of Animations in Statistics and Utilities to Create Animations. \url{http://cran.r-project.org/web/packages/animation/}
    \item gridSVG: Export grid graphics as SVG \url{http://cran.r-project.org/web/packages/gridSVG/}
    \item SVGAnnotation: Tools for Post-Processing SVG Plots Created in R \url{http://www.omegahat.org/SVGAnnotation/}
    \item RSVGTipsDevice: An R SVG graphics device with dynamic tips and hyperlinks. \url{http://cran.r-project.org/web/packages/RSVGTipsDevice/}
    \item iWebPlots: Interactive web-based plots. \url{http://cran.r-project.org/web/packages/iWebPlots/}
  }   
}
\references{
  \itemize{
	\item googleVis project site: \url{http://code.google.com/p/google-motion-charts-with-r/}
	\item Google Chart Tools API: \url{https://developers.google.com/chart/}
	\item Google Terms of Use: \url{https://developers.google.com/terms/}
	\item Google Maps API Terms of Service: \url{https://developers.google.com/maps/terms}
      }
}
\examples{
\dontrun{
  demo(googleVis)
  ## For other demos see
  demo(package='googleVis')	
  }
}
\keyword{ package }


