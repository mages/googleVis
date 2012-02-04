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
  \item{gvis}{an object of class 'gvis', e.g. output of a googleVis visualisation functions. 
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  createGoogleGadget returns a Google Gadget XML string.
}
\references{
  For more information about Google Gadgets see:
  \url{http://www.google.com/webmasters/gadgets/}
}
\author{
Markus Gesmann
}
\note{
Google Gadgets can be embedded in various Google products, for example
as part of a Google Code wiki page, Blogger or Google Sites.
In all cases the XML gadget file has to be hosted online, e.g. using Google Docs.

In Blogger the gadgets can be embedded via the design tab, and in a Google Sites via
the menu "Insert" -> "More gadgets ..." -> "Add gadget ULR".  

In a Google Code wiki the gadget can be embedded via

\code{<wiki:gadget url="http://example.com/gadget.xml" height="200" border="0" />}

You find examples on the googleVis project site: 
\url{http://code.google.com/p/google-motion-charts-with-r/wiki/GadgetExamples}
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
