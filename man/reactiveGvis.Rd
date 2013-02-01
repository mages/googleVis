\name{reactiveGvis}
\alias{reactiveGvis}
\title{
  Use a googleVis Chart as Shiny Output
}
\description{
  This function lets you use googleVis charts as Shiny output. Use it to wrap a
  googleVis-generating function that you assign to an \code{output} element in
  \code{server.R}; then create an \code{htmlOutput} with the same name in
  \code{ui.R}.
}
\usage{
reactiveGvis(func)
}
\arguments{
  \item{func}{
    A function that returns a gvis object.
  }
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
  Returns a function that can be assigned to a Shiny \code{output} element.
}  

\author{
Joe Cheng \email{joe@rstudio.com}
}

\examples{
\dontrun{
# To run this example:
shiny::runApp(system.file("shiny/", package="googleVis"))


# server.R
library(googleVis)

shinyServer(function(input, output) {
  datasetInput <- reactive(function() {
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  output$view <- reactiveGvis(function() {
    gvisScatterChart(datasetInput())
  })
})

# ui.R
shinyUI(pageWithSidebar(
  headerPanel("googleVis on Shiny"),
  sidebarPanel(
    selectInput("dataset", "Choose a dataset:", 
                choices = c("rock", "pressure", "cars"))
  ),
  mainPanel(
    htmlOutput("view")
  )
))
}
}

\keyword{ shiny }
