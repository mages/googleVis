\name{googleVis-package}
\alias{googleVis-package}
\alias{googleVis}
\docType{package}
\title{
Using Google Charts with R
}
\description{
  The googleVis package provides an interface between R and Google
  Charts. It allows users to create web pages with interactive
  charts based on R data frames, using the Google Charts API and to
  display them either via the local R HTTP help server or within their
  own sites, without uploading the data to Google.
  
  A modern browser with Internet connection is required and for some
  charts Flash. Please visit the project web site for more information:

  \url{http://github.com/mages/googleVis}.

  You find further notes on Markus' blog:
  \url{http://lamages.blogspot.co.uk/search/label/googleVis}
} 
%\details{
%	}
\author{
	Markus Gesmann, Diego de Castillo
	}
\note{
  See \code{vignette("googleVis")} for an introduction to the
  \code{googleVis} package. 
  }
\references{
  \itemize{
		\item Google Charts API: \url{https://developers.google.com/chart/}
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
