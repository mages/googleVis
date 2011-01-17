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
		
\method{plot}{gvis}(x, file, 
		    repos = "http://127.0.0.1:8074/", 
	     	     ...)
}
\arguments{
  \item{x}{
   An object of class \code{gvis}.
}
  \item{file}{
    file name to be used internally by \code{\link{cat}}. If "" (the
    default), output will be printed to the standard output connection, the console unless
    redirected by \code{\link{sink}}.

    If no file name is provided for the plot function a temporary file
    will be created. 
}
  \item{repos}{repository for the output file. By default the directory
    of the \code{file} will be used. 
}
  \item{\dots}{arguments passed on to \code{\link{cat}}.
}
}
%\details{
%}
\value{
\item{\code{print.gvis}}{ None (invisible \code{NULL}).}

\item{\code{plot.gvis}}{Returns the file name invisibly.}
}
\references{
  Please note that the temporary directory (see \code{\link{tempdir}})
  will be added to the Http daemon root path (see \code{\link{HttpDaemon}}).

  For more details on rps-pages type: \code{\link{browseRsp}()}.
  From this page you access not only help pages and demos on
  how to use RSP, but also other packages with RSP pages, such as the
  \code{googleVis} package. 

  Please see also the package vignette for the usage of the googleVis
  package with RApache.
}
\author{
Markus Gesmann \email{markus.gesmann@gmail.com},

Diego de Castillo \email{decastillo@gmail.com}
}
\note{
  The \code{plot} command does not open a graphics device in the
  traditional way. Instead it uses the \code{R.rsp} package to display the output
  of a \code{googleVis} function in a browser using the local web server
  provided by \code{R.rsp}. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{cat}}, \code{\link{browseRsp}},
\code{\link{createGoogleGadget}},  \code{\link{tempdir}}, \code{\link{HttpDaemon}}
}
\examples{


M <- gvisMotionChart(Fruits, "Fruit", "Year")
str(M)
M
\dontrun{
## Web browser with Internet connection and Flash required.
plot(M)
}			

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
\keyword{ methods }
\keyword{ print }
