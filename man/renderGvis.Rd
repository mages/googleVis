\name{renderGvis}
\alias{renderGvis}
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
renderGvis(expr, env=parent.frame(), quoted=FALSE)
}
\arguments{
  \item{expr}{
    An expression that returns a gvis object.
  }
  \item{env}{
    The environment in which to evaluate \code{expr}.
  }
  \item{quoted}{
    Is \code{expr} a quoted expression (with \code{quote()})?
    This is useful if you want to save an expression in a variable.
  }
}
\details{
More information about shiny is available online: 
\url{http://www.rstudio.com/shiny/}. 

You find further examples with googleVis on shiny on mages' blog:
\url{http://lamages.blogspot.co.uk/search/label/shiny}
}
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
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  output$view <- renderGvis({
    gvisScatterChart(datasetInput(),
                    options=list(title=paste('Data:',input$dataset))))
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
