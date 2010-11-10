\name{gvisMap}
\alias{gvisMap}
\title{
  Google Maps with R
}
\description{
  The gvisMap function reads a data.frame and
  creates text output referring to the Google Visualisation API which can be
  included into a web page, or as a standalone page.
  
  The maps are the well known Google Maps. 
}
\usage{
gvisMap(data, locationvar='', tipvar='',
           options = list())
}
\arguments{
  \item{data}{
    a \code{data.frame}. The data has to have at least two
    columns with location name (\code{locationvar})
    and the variable to display the text in the tip icon (\code{tipvar}).
    
  }
  \item{locationvar}{column name of \code{data} with the geo locations to be
    analysed. The locations can be provide in two formats:
    \describe{	    
      \item{Format 1}{'latitude:longitude'. See the example below.}
      \item{Format 2}{Address, may be very slow.}
    }
  }
  \item{tipvar}{column name of \code{data} with the string
    text displayed over the tip icon. 
  }
  \item{options}{list of configuration options for the map.
    The possible components are, taken from
    \url{http://code.google.com/apis/visualization/documentation/gallery/map.html}:
  
    \describe{
	\item{\code{showTip}}{boolean, default \code{FALSE}. If true,
	  display the tip icon.
	}
	\item{\code{mapType}}{string, default \code{hybrid}. 
          Possible values are 'normal', 'terrain', 'satellite' or 'hybrid'.
	}
      }
    }
  }
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
  \code{gvisMap} returns list of \code{\link{class}} "\code{gvis}" and "\code{list}". 
  
  An object of class "\code{gvis}" is a list containing at least the following components:
  \item{\code{type}}{Google visualisation type, here 'GeoMap'}
  \item{\code{chartid}}{character id of the chart object. The string is build
    at run time from chart type and date/time when the function is
    executed. Unique chart ids are required to place several charts on
    the same page.
  }
  \item{\code{html}}{a list with the building blocks for a page
    \describe{
      \item{\code{header}}{a character string of a html page header:
	\code{<html>...<body>},}
      \item{\code{chart}}{a character string of the chart object with
	reference to the Google API. Only this string is needed to embed the
	chart into pages, see also the examples in the \code{googleVis} demo,}
      \item{\code{caption}}{a character string of a standard caption,
	including data set, chart id and R version used; further a link to Google's
	Terms of Use,}
      \item{\code{footer}}{a character string of a html page footer:
	\code{</body>...</html>}.}
    }
  }
}
\references{
  Google Map  API: \url{http://code.google.com/apis/visualization/documentation/gallery/map.html}
}
\author{
Markus Gesmann \email{markus.gesmann@gmail.com},

Diego de Castillo \email{decastillo@gmail.com}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{

  See also \code{\link{print.gvis}}, \code{\link{plot.gvis}} for printing and plotting methods. 

}
\examples{

## Example with latitude and longitude information
 
states <- data.frame(state.name, state.area, 
       	  	     state.center=paste(state.center$y, state.center$x, sep=":"))

## state.center: list with components named x and y giving the
## approximate geographic center of each state in negative longitude
## and latitude. Alaska and Hawaii are placed just off the West Coast.  

G1 <- gvisMap(states, locationvar="state.center",
      			 tipvar="state.name", 
      		 options=list(showTip=TRUE,mapType='normal'))
\dontrun{
plot(G1) 
}


}

\keyword{ hplot }
