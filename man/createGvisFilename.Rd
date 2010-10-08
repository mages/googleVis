\name{createGvisFilename}
\alias{createGvisFilename}
\title{
Create a reasonable file name for a givs object
}
\description{
Create a reasonable file name for a givs object.
}
\usage{
createGvisFilename(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ object of class \code{gvis}.
}
}
\details{
\code{createGvisFilename} uses the information of the \code{gvis} html snippets to construct a string based on gvis-type, data name and date.  
}
\value{
\code{createGvisFilename} gives a character back.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Markus Gesmann
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
 M <- gvisMotionChart(Fruits, "Fruit", "Year")
 createGvisFilename(M)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ file }
