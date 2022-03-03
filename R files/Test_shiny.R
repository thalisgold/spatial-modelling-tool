# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)

# Load data --------------------------------------------------------------------

load("movies.RData")

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  br(),
  
  sidebarLayout(
    sidebarPanel(
      h4("Parameters for predictors"),
      sliderInput(
        inputId = "n_predictors",
        label = "Number of predictors:",
        value = 11,
        min = 2,
        max = 20,
        step = 1,
        width = "100%"
      ),
      
      selectInput(
        inputId = "nlm", label = "NLM:",
        choices = c("Random", "Gaussian random field", "Random cluster", "Edge Gradient"),
        selected = "Random"
      ),
      
      h4("Parameters for trainingdata"),
      numericInput(
        inputId = "n_trainingdata",
        label = "Number of sampling points:",
        value = 50,
        min = 50,
        max = 150,
        step = 50,
        width = "60%"
      ),
      selectInput(
        inputId = "dist_trainingdata", label = "Distribution of sampling points:",
        choices = c("Random", "Regular", "Weak clustering", "Strong clustering", "Uniform"),
        selected = "Regular"
      ),
      h4("Modelling"),
      radioButtons(
        inputId = "algorithm", label = "Choose algorithm for training:",
        choices = c("Random Forest", "Support Vector Machines"),
        selected = "Random Forest"
      ),
      selectInput(
        inputId = "cv_method", label = "Cross-validation method:",
        choices = c("Random", "Spatial"),
        selected = "Spatial"
      ),
      selectInput(
        inputId = "variableSelection", label = "Variable Selection:",
        choices = c("None", "FFS", "RFE"),
        selected = "None"
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "trainingdata"),
      dataTableOutput(outputId = "moviestable"),
      br()
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  output$trainindata <- renderPlot({
    ggplot(data = movies, aes_string(x = input$x, y = input$y)) +
      geom_point()
  })
  
  output$moviestable <- renderDataTable({
    nearPoints(movies, input$plot_hover) %>%
      select(title, audience_score, critics_score)
  })
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)