# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)

# Load data --------------------------------------------------------------------
# Create grids
dimgrid <- 100
rast_grid <- raster(ncols=dimgrid, nrows=dimgrid, xmn=0, xmx=dimgrid, ymn=0, ymx=dimgrid)
point_grid <- st_as_sf(rasterToPoints(rast_grid, spatial = TRUE))
# Create sampling areas
study_area <- st_as_sf(as(extent(rast_grid), "SpatialPolygons"))

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
        choices = c("Random" = "random",
                    "Regular" = "regular",
                    "Weak clustering" ="clust1",
                    "Strong clustering" ="clust2",
                    "Non-uniform" ="nonunif"),
        selected = "Random"
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
  output$trainingdata <- renderPlot({
    if(input$dist_trainingdata %in% c("clust1")){
      train_points <- clustered_sample(study_area, input$n_trainingdata/5, input$n_trainingdata*4/5, dimgrid*0.05)
    }else if(input$dist_trainingdata %in% c("clust2")){
      train_points <- clustered_sample(study_area, input$n_trainingdata/10, input$n_trainingdata*9/10, dimgrid*0.05)
    }else if(input$dist_trainingdata %in% c("nonunif")){
      nonuniform_areas <- nonuniform_sampling_polys(dgrid=dimgrid)
      train_points <- st_sample(filter(nonuniform_areas, sample=="Yes"), input$n_trainingdata, type = "random")
      train_points <- st_sf(geom=train_points)
    }else{
      train_points <- st_sample(study_area, input$n_trainingdata, type = input$dist_trainingdata)
      train_points <- st_sf(geom=train_points)
    }
    ggplot() +
      geom_sf(data = train_points, size = 1) +
      geom_sf(data = study_area,  alpha = 0) +
      theme_bw()
  })
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)