\name{Cats}
\alias{Cats}
\docType{data}
\title{
Cat Search Terms
}
\description{
A set of responses for search terms for cats
}
\usage{data(Cats)}
\format{
  A data frame with 18 observations on the following 3 variables.
  \describe{
    \item{\code{Phrase}}{Search term for cats}
    \item{\code{Size}}{Number of times term has been searched}
    \item{\code{Sentiment}}{Numeric value from 0-10 of sentiment of term}
  }
}
%\details{
%}
\examples{
data(Cats)
plot(gvisWordTree(Cats, textvar = "Phrase"))
}
\keyword{datasets}
