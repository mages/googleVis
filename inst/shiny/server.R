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
                     options=list(title=paste('Data:',input$dataset)))
  })
})