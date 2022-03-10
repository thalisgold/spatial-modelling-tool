ui <- fluidPage(
  selectInput("dataset", "Dataset", c("diamonds", "rock", "pressure", "cars")),
  conditionalPanel( condition = "output.nrows",
                    checkboxInput("headonly", "Only use first 1000 rows"))
)
server <- function(input, output, session) {
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  output$nrows <- reactive({
    nrow(datasetInput())
  })
  
  outputOptions(output, "nrows", suspendWhenHidden = FALSE)  
}

shinyApp(ui, server)