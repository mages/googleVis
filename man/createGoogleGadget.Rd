\name{createGoogleGadget}
\alias{createGoogleGadget}
\title{
Create a Google Gadget
}
\description{
Create a Google Gadget based on a Google Visualisation Object
}
\usage{
createGoogleGadget(gvis)
}
\arguments{
  \item{gvis}{an object of class 'gvis', e.g. output of a Google
    Visualisation functions. 
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  createGoogleGadget returns a character XML string of a Google Gadget.
}
\references{
  For more information about Google Gadgets see:
  \url{http://code.google.com/apis/gadgets/docs/overview.html}
}
\author{
Markus Gesmann
}
\note{
Google Gadget can be embedded in various Google products, for example as part of a Google Code wiki page, e.g.

<wiki:gadget url="http://example.com/gadget.xml" height="200" border="0" /> 
}


\seealso{
See also as \code{\link{print.gvis}}, \code{\link{cat}}
}
\examples{
M <- gvisMotionChart(Fruits, idvar="Fruit", timevar="Year")
gdgt <- createGoogleGadget(M)
cat(gdgt)
}
\keyword{ intreface }
