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
		
\method{plot}{gvis}(x, filename = NULL, 
		    repos = paste("http://127.0.0.1:8074/", 
		       	     basename(dirname(system.file(package = "googleVis"))), 
			       		"/googleVis/rsp/myAnalysis/", sep = ""), 
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
}
  \item{filename}{
      If \code{filename=NULL} (Default) plot.gvis will create a
      rsp-file, based on the chard id, in the \code{rsp/myAnalysis} folder of the
      \code{googleVis} library folder, see Details for more information.      
}
  \item{repos}{repository for the output file. By default the
    \code{rsp/myAnalysis} folder of the \code{googleVis} package will be
    used. 
}
  \item{\dots}{arguments passed on to \code{\link{cat}}.
}
}
\details{
  You can find the path to your \code{googleVis} folder with the
  following R command:
  \code{filePath( system.file( file.path( "rsp", "myAnalysis" ), package
		    = "googleVis" ))}.
  The default \code{filename} of the plot command is build as:  
  \code{filePath(system.file( file.path( "rsp", "myAnalysis" ), 
    package = "googleVis"), paste( x$chartid, ".rsp", 
    sep = "" ))}, 		  
  where \code{x} is a \code{gvis} object.
}
\value{
\item{\code{print.gvis}}{ None (invisible \code{NULL}).}

\item{\code{plot.gvis}}{Returns an invisible list with filename and repos.}
}
\references{
  For more details on rps-pages type: \code{\link{browseRsp}()}, which
  starts the internal web browser and opens the URL in the default web
  browser. From this page you access not only help pages and demos on
  how to use RSP, but also other package RSP pages, such as the
  \code{googleVis} package. 
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
\section{Warning }{
  By default the visualisation files will be stored in the
  \code{rsp/myAnlysis} folder of the googleVis library. The following
  R-command will show the path to this folder:
  
  \code{system.file(file.path("rsp", "myAnalysis"), package="googleVis")}
  
  It is preferable that you have write-access to this directory, so you
  may want to install the package locally (e.g. in your home
  folder). Please contact your system administrator if you need further
  help.

  Therefore the \code{plot} commands are not executed automatically as
  part of the examples.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{cat}}, \code{\link{browseRsp}}, \code{\link{createGoogleGadget}}
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
