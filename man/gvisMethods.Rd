\name{gvis Methods}
\alias{print.gvis}
\alias{plot.gvis}

\title{
  Print and plot gvis objects
}
\description{
  Methods to print and plot \code{gvis} objects
}
\usage{	
\method{print}{gvis}(x, file = "", ...)
		
\method{plot}{gvis}(x,...)
}
\arguments{
  \item{x}{
    An object of class \code{gvis}.
  }
  \item{file}{
    file name to be used internally by \code{\link{cat}}. If "" (the
    default), output will be printed to the standard output connection, the console unless
    redirected by \code{\link{sink}}.
  }
  \item{\dots}{arguments passed on to \code{\link{cat}}
      (\code{print.gvis}) or \code{\link{browseURL}} (\code{plot.gvis}).
  }
}

%%\details{
%%  The plot function creates two temporary fi
%%}
\value{
  \item{\code{print.gvis}}{ None (invisible \code{NULL}).}
  
  \item{\code{plot.gvis}}{Returns the file name invisibly.}
}
\references{
  Please see also the package vignette for the usage of the googleVis
  package with RApache and R.rsp.
}
\author{
  Markus Gesmann \email{markus.gesmann@gmail.com},
  
  Diego de Castillo \email{decastillo@gmail.com}
}
\note{
  The \code{plot} command does not open a graphics device in the
  traditional way. Instead it creates HTML files in a temporary
  directory and uses the R HTTP server to display the output  
  of a \code{googleVis} function locally. 
  A browser with Flash and Internet connection is required.
  The displayed page includes a link (click on the chart id) to a
  further page which shows the code of the chart for the user to copy
  and paste in her/his own page.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  See also \code{\link{cat}}, \code{\link{browseURL}},
  \code{\link{createGoogleGadget}}
}
\examples{

M <- gvisMotionChart(Fruits, "Fruit", "Year")
str(M)
M
## access only the plot
cat(unlist(M$html$chart))

## Web browser with Internet connection and Flash required.
plot(M)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
\keyword{ methods }
\keyword{ print }
