\name{gvis Methods}
\alias{print.gvis}
\alias{plot.gvis}

\title{
	Print and plot gvis objects
}
\description{
	Print and plot \code{gvis} objects
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
      If \code{filename=NULL} (Default) plot.gvis will create a file in the rsp folder of the \code{googleVis} library folder:
      \code{filePath(system.file(file.path("rsp", "myAnalysis"), package = "googleVis"), paste(x$chartid ,".rsp", sep=""))}, 
      where \code{x} is a gvis object.
}
  \item{repos}{repository for the output file. By default the rsp-folder
    of the \code{googleVis} package will be used.
}
  \item{\dots}{ arguments passed on to \code{\link{cat}}.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
\item{\code{print.gvis}}{ None (invisible \code{NULL}).}

\item{\code{plot.gvis}}{Returns an invisible list with filename and repos.}
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Markus Gesmann, \email{markus.gesmann@googlemail.com},

Diego de Castillo \email{decastillo@gmail.com}
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{cat}}, \code{\link{browseRsp}}
}
\examples{

\dontrun{
Motion=gvisMotionChart(Fruits, "Fruit", "Year")
Geo=gvisGeoMap(Population)
Table=gvisTable(PopulationLarge)
Tree=gvisTreeMap(Regions)

Page <- list(type="MotionGeoTableTree", 
			 chartid=format(Sys.time(), "MotionGeoTableTree-%Y-%m-%d-%H-%M-%S"), 
			 html=list(Header=Motion$html$Header,
			 		Chart1=Motion$html$Chart,
					Caption1=Motion$html$Caption,
					Chart2=Geo$html$Chart,
					Caption2=Geo$html$Caption,
					Chart3=Table$html$Chart,
					Caption3=Table$html$Caption,
					Chart4=Tree$html$Chart,
					Caption4=Tree$html$Caption,
					Footer=Tree$html$Footer)
			)
		

class(Page) <- list("gvis", class(Page))
plot(Page)
}			

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
\keyword{ methods }
\keyword{ print }
