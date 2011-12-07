\name{googleVis-package}
\alias{googleVis-package}
\alias{googleVis}
\docType{package}
\title{
Using the Google Visualisation API with R
}
\description{
  googleVis provides an interface between R and the Google Visualisation
  API to create interactive charts. The functions of the package allow
  the user to visualise data stored in R data frames with the Google
  Visualisation API in a web browser, without uploading the data to
  Google. 

  The output of a googleVis function is html code that contains the data
  and references to JavaScript functions hosted by Google. 
  googleVis makes use of the internal R HTTP server to
  display the output locally, e.g. http://localhost/. A browser with 
  Flash and Internet connection is required. Please visit the
  project web site for more information and examples.
     
  Please visit the project web site for more
  information and examples:
  \url{http://code.google.com/p/google-motion-charts-with-r/}.
 
} 
\details{
\tabular{ll}{
	Package: \tab googleVis\cr
	Type: \tab Package\cr
	Version: \tab 0.2.12\cr
	Date: \tab 2011-12-07\cr
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

  Other R packages of interest to you might be:
  \itemize{
    \item plotGoogleMaps: Plot HTML output with Google Maps API and your own data. \url{http://cran.r-project.org/web/packages/plotGoogleMaps/}
    \item R2GoogleMaps: Provides a mechanism to generate JavaScript code from R that displays data using Google Maps. \url{http://www.omegahat.org/R2GoogleMaps/}
    \item RgoogleMaps: Overlays on Google map tiles in R. \url{http://cran.r-project.org/web/packages/RgoogleMaps/index.html}
    \item animation: A Gallery of Animations in Statistics and Utilities to Create Animations. \url{http://cran.r-project.org/web/packages/animation/}
    \item gridSVG: Export grid graphics as SVG/ \url{http://cran.r-project.org/web/packages/gridSVG/}
    \item SVGAnnotation: Tools for Post-Processing SVG Plots Created in R \url{http://www.omegahat.org/SVGAnnotation/}
    \item RSVGTipsDevice: An R SVG graphics device with dynamic tips and hyperlinks. \url{http://cran.r-project.org/web/packages/RSVGTipsDevice/}
    \item iWebPlots: Interactive web-based plots. \url{http://cran.r-project.org/web/packages/iWebPlots/}
  }   
}
\references{
  \itemize{
	\item googleVis project site: \url{http://code.google.com/p/google-motion-charts-with-r/}
	\item Google Visualisation API: \url{http://code.google.com/apis/visualization/interactive_charts.html}
	\item Google Terms of Use: \url{http://code.google.com/apis/chart/interactive/terms.html}
	\item Google Maps API Terms of Service: \url{http://code.google.com/apis/maps/terms.html}
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


