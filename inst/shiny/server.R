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