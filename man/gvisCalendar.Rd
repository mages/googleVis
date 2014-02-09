\name{gvisCalendar}
\alias{gvisCalendar}
\title{Google Calendar Chart with R
\Sexpr{googleChartName <- "Calendar"}
\Sexpr{gvisChartName <- "gvisCalendar"}}
\usage{
  gvisCalendar(data, datevar = "", numvar = "",
    options = list(), chartid)
}
\arguments{
  \item{data}{a \code{data.frame}. The data has to have at
  least two columns, one with date information
  (\code{datevar}) and one numerical variable.}

  \item{datevar}{column name of \code{data} which shows the
  date dimension. The information has to be of class
  \code{\link{Date}} or \code{POSIX*} time series.}

  \item{numvar}{column name of \code{data} which shows the
  values to be displayed against \code{datevar}. The
  information has to be \code{\link{numeric}}.}

  \item{options}{list of configuration options, see:

  % START DYNAMIC CONTENT

  \Sexpr[results=rd]{gsub("CHARTNAME", googleChartName,
  readLines(file.path(".", "inst", "mansections",
  "GoogleChartToolsURLConfigOptions.txt")))}

  \Sexpr[results=rd]{paste(readLines(file.path(".", "inst",
  "mansections", "gvisOptions.txt")))}}

  \item{chartid}{character. If missing (default) a random
  chart id will be generated based on chart type and
  \code{\link{tempfile}}.}
}
\value{
  \Sexpr[results=rd]{paste(gvisChartName)} returns list of
  \code{\link{class}}
  \Sexpr[results=rd]{paste(readLines(file.path(".", "inst",
  "mansections", "gvisOutputStructure.txt")))}
}
\description{
  A calendar chart is a visualization used to show activity
  over the course of a long span of time, such as months or
  years. They're best used when you want to illustrate how
  some quantity varies depending on the day of the week, or
  how it trends over time.
}
\section{Warning}{
  The calendar chart may be undergoing substantial
  revisions in future Google Charts releases.
}
\examples{
cl1 <- gvisCalendar(Cairo, datevar="Date", numvar="Temp")
plot(cl1)

cl2 <- gvisCalendar(Cairo, datevar="Date", numvar="Temp",
                    options=list(
                    title="Daily temperature in Cairo",
                    height=500,
                    calendar="{yearLabel: { fontName: 'Times-Roman',
                               fontSize: 32, color: '#1A8763', bold: true},
                    cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                    focusedCellColor: {stroke:'red'}}")
                  )
plot(cl2)
}
\author{
  Markus Gesmann \email{markus.gesmann@gmail.com},

  Diego de Castillo \email{decastillo@gmail.com}
}
\references{
  Google Chart Tools API:
  \Sexpr[results=rd]{gsub("CHARTNAME", googleChartName,
  readLines(file.path(".", "inst", "mansections",
  "GoogleChartToolsURL.txt")))}

  % END DYNAMIC CONTENT
}
\seealso{
  See also \code{\link{print.gvis}},
  \code{\link{plot.gvis}} for printing and plotting
  methods.
}
\keyword{iplot}

