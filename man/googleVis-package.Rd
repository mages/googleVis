\name{googleVis-package}
\alias{googleVis-package}
\alias{googleVis}
\docType{package}
\title{
  R Interface to Google Charts
}
\description{
  R interface to Google Charts API, allowing users 
  to create interactive charts based on data frames. Charts 
  are displayed locally via the R HTTP help server. A modern 
  browser with Internet connection is required and for some 
  charts Flash. The data remains local and is not uploaded to 
  Google.
    
  Please visit the project web site for more information:
  \url{https://github.com/mages/googleVis}.

  You find further notes on Markus' blog:
  \url{https://magesblog.com/tags/googlevis/}
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
