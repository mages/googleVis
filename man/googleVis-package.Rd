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
	Version: \tab 0.2.0\cr
	Date: \tab 20010-11-01\cr
	License: \tab GPL version 2 or later\cr
	 }
	}
\author{
	Markus Gesmann, Diego de Castillo
	}
\note{
  See \code{vignette("googleVis")} for a introduction to the
  \code{googleVis} package. 

  Many thanks to Henrik Bengtsson for providing the R.rsp package
  \url{http://www.braju.com/R/}, which allows us to display motion
  charts locally.

  Type \code{browseRsp(path="/library/googleVis/rsp/")} to find out a
  bit more how you can integrate googleVis charts into rsp-pages.

  The Google Public Data Explorer
  (\url{http://www.google.com/publicdata/home}) provides lots of
  examples using the Google Visualisation API. 
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


