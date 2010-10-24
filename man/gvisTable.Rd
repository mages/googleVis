\name{gvisTable}
\alias{gvisTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Google Table Chart with R
}
\description{
  A table that can be sorted and paged. Table cells can be formatted
  using format strings, or by directly inserting HTML as cell
  values. Numeric values are right-aligned; boolean values are displayed
  as check marks. Users can select single rows either with the keyboard
  or the mouse. Users can sort rows by clicking on column headers. The
  header row remains fixed as the user scrolls. The table fires a number
  of events corresponding to user interaction.

  The gvisTable function reads a data.frame and
  creates text output referring to the Google Visualisation API which can be
  included into a web page, or as a standalone page. The actual chart is
  rendered by the web browser in flash.

}
\usage{
gvisTable(data, options = list())
}
\arguments{
  \item{data}{a \code{\link{data.frame}} to be displayed as a table
  }
  \item{options}{list of configuration options for the motion chart.
    The possible components are :
    \describe{
      \item{\code{height}}{height of the chart in pixels.
      }
      \item{\code{width}}{width of the chart in pixels.
      }
      \item{\code{state}}{
      }
    }
  }
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

 \code{gvisTable} returns list of \code{\link{class}} "\code{gvis}" and "\code{list}". 
  
  An object of class "\code{gvis}" is a list containing at least the following components:
  \item{\code{type}}{Google visualisation type, here 'Table'}
  \item{\code{chartid}}{character id of the chart object. The string is build
    at run time from chart type and date/time when the function is
    executed. Unique chart ids are required to place several charts on
    the same page.
  }
  \item{\code{html}}{a list with the building blocks for the page
    \describe{
      \item{\code{header}}{a character string of a html page header:
	\code{<html> ... <body>},}
      \item{\code{chart}}{a character string of the chart object with
	reference to Google API. Only this string is needed to embed the
	chart into pages, see also the examples in the \code{googleVis} demo,}
      \item{\code{caption}}{a character string of a standard caption,
	including data set use, chart id, R version and link to Google's
	Terms of Use,}
      \item{\code{footer}}{a character string of a html page footer:
	\code{</html> ... </body>}.}
    }
  }
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Markus Gesmann, \email{markus.gesmann@gmail.com},

Diego de Castillo \email{decastillo@gmail.com}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for printing and plotting methods. 
}
\examples{

## 
tbl1 <- gvisTable(PopulationLarge)
\dontrun{
plot(tbl1)
}


tbl2 <- gvisTable(PopulationLarge, options=list(page='enable', height=300))

\dontrun{
plot(tbl2)
}
}

\keyword{ hplot }
