\name{gvis Methods}
\alias{print.gvis}
\alias{plot.gvis}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print and plot gvis objects
}
\description{
Print and plot \code{gvis} objects
}
\usage{
\method{print}{gvis}(x, file = "", ...)

\method{plot}{gvis}(x, file = paste(tempfile(tmpdir = system.file(file.path("rsp", "myAnalysis"), package = "googleVis")), ".rsp", sep = ""), 
	     repos = paste("http://127.0.0.1:8074/", basename(dirname(system.file(package = "googleVis"))), "/googleVis/rsp/myAnalysis/", sep = ""), 
	     ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
   An object of class \code{gvis}.
}
  \item{file}{
    file name to be used internally by \code{\link{cat}}. If "" (the
    default), output will be printed to the standard output connection, the console unless
    redirected by \code{\link{sink}}. 
    In the case of \code{plot.gvis} by default a temporary file in the myAnalysis folder of the googleVis package will be gernarted
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

\item{\code{plot.gvis}}{Returns nothing.}
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

## The print.gvis function is currently defined as
function(x,file="",...){

  ##cat(paste(unlist(x), "\n", sep=""), file=file, ...)
  cat(unlist(x), file=file, ...)

  }

## The plot.gvis function is currently defined as
function(x, file=paste(tempfile(tmpdir=system.file(file.path("rsp", "myAnalysis"),
                                            package = "googleVis")),".rsp",sep="") ,
                      repos=paste("http://127.0.0.1:8074/",
                        basename(dirname(system.file(package="googleVis"))),
                        "/googleVis/rsp/myAnalysis/", sep=""),
                      ...){
  print.gvis(x, file, ...)

  require(R.rsp)
  browseRsp(paste(repos, basename(file), sep=""))

  }

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
\keyword{ methods }
\keyword{ print }
