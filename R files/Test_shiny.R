# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(NLMR)
library(landscapetools)
library(raster)
library(caret)
library(CAST)
library(sf)
library(shinythemes)


# Load functions ---------------------------------------------------------------
#' Create stack from point layer
#' @description 
#' Function to create a stack of rasters from a sf point object with cell values included 
#' as columns.
#' @param sf_points A sf point object. Point data, typically a regular grid with numeric attributes
#' as columns, that we want to convert into a raster stack.
#' @param cols_indx Integer or vector of integers. Indices of the columns of the sf object from 
#' which to generate the raster values.
#' @param layers_names A string or vector of strings. Names to assign to the new layers of the
#'  stack.
#' @return A raster stack.
rasterise_and_stack <- function(sf_points, cols_indx, layers_names){
  
  # Make sure of equal length of indices and names
  if(length(cols_indx)!=length(layers_names)){
    stop("Colummns indeces and layer names must have the same length.")
  }
  
  # Create stack from first element and name it
  res_stack <- rasterFromXYZ(cbind(st_coordinates(sf_points), 
                                   as.matrix(as.data.frame(sf_points)[,cols_indx[1]], ncol=1)))
  names(res_stack) <- layers_names[1]
  
  # If there are more elements to be stacked, proceed
  if(length(cols_indx)>1){
    for(i in 2:length(cols_indx)){
      cindx <- cols_indx[i]
      temprast <- rasterFromXYZ(cbind(st_coordinates(sf_points), 
                                      as.matrix(as.data.frame(sf_points)[,cindx], ncol=1)))
      names(temprast) <- layers_names[i]
      res_stack <- stack(res_stack, temprast)
    }
  }
  return(res_stack)
}

#' Sandbox clustered sampling
#' @description 
#' Function to generate clustered samples by randomly simulating parent points and subsequently
#' random offspring points located within a radius of the parents.
#' @param area A sf polygon object representing the study area boundaries.
#' @param n1 Integer. Number of parents to simulate.
#' @param n2 Integer. Number of offspring to simulate.
#' @param radius Numeric. Radius of the buffer for offspring simulation.
#' @return A sf point object with the samples.
clustered_sample <- function(area, n1, n2, radius){
  
  # Calculate number of offspring per parent and round in case not integers are given
  nchild <- round(n2/n1, 0)
  n2 <- round(n2, 0)
  n1 <- round(n1, 0)
  
  # Simulate parents
  parents <- st_sf(geom=st_sample(area, n1, type="random"))
  res <- parents
  
  # Simulate offspring
  for(i in 1:nrow(parents)){
    
    # Generate buffer and cut parts outside of the area of study
    buf <- st_buffer(parents[i,], dist=radius)
    buf <- st_crop(buf, st_bbox(area))
    
    # Simulate children
    children <- st_sf(geom=st_sample(buf, nchild, type="random"))
    res <- rbind(res, children)
  }
  
  return(res)
}

#' Create a square polygon
#' @param xmin Numeric. Minimum x coordinate for square creation.
#' @param ymin Numeric. Minimum y coordinate for square creation.
#' @param ch_len Numeric. Side length of the square
#' @return A sf polygon object corresponding to a square.
#' @examples
#' plot(checkerpolys(0, 0, 10))
checkerpolys <- function(xmin, ymin, ch_len){	
  # Get maxs	
  xmax <- xmin + ch_len	  
  ymax <- ymin + ch_len	  
  
  # Create poly
  poly <- list(matrix(c(xmin,ymin,	
                        xmax,ymin,	  
                        xmax,ymax,	  
                        xmin,ymax,	   
                        xmin,ymin),ncol=2, byrow=TRUE)) 
  poly <- st_sf(geom=st_sfc(st_polygon(poly)))
  return(poly)
}

#' Non-uniform sampling areas generation
#' @description 
#' This functions partitions the study area into many squares, and randomly selects a subset of
#' them for non-uniform sampling.
#' @param dgrid Integer. Dimension of one side of the squared study area. 
#' @param blockside Integer. Number of squares per axis.
#' @param targetblock Integer. Number of squares from which we will sample. 
#' @return A polygon sf object consisting of squares with a column named "sample" taking two 
#' values: "Yes", if the square is considered for sampling; "No", otherwise.
#' @examples
#' plot(nonuniform_sampling_polys(100))
nonuniform_sampling_polys <- function(dgrid, blockside=5, targetblock=5){
  
  # Calculate right margin and size of the block
  size_block <- dgrid/blockside
  
  # Construct grid
  xmins <- seq(0, dgrid-size_block, size_block)
  ymins <- xmins
  coords_checker <- expand.grid(xmins=xmins, ymins=ymins)
  
  # Create 1st square poly
  checker_folds <- checkerpolys(coords_checker$xmins[1], coords_checker$ymins[1], size_block)
  # Iterate for the rest and add
  for(i in 2:nrow(coords_checker)){
    temp <- checkerpolys(coords_checker$xmins[i], 
                         coords_checker$ymins[i], size_block)
    checker_folds <- rbind(checker_folds,temp)
  }
  
  # Draw random blocks for sampling
  sampling_vector <- c(rep("Yes", targetblock), rep("No", blockside^2-targetblock))
  checker_folds$sample <- sample(sampling_vector, replace=FALSE)
  
  # Return object
  return(checker_folds)
}

# +, -, *, /, ^2
generate_random_function <- function(raster_stack) {
  operands = c("+", "-", "*", "^2 +", "^3 -", "^2 *", "^2 -", "^3 +")
  expression = ""
  for (i in 1:(nlayers(raster_stack)-1)){
    expression <- paste(expression, paste(as.character(substitute(raster_stack)), "$", names(raster_stack)[i], sep=""), sep = " ")
    expression <- paste(expression, sample(operands, 1), sep = " ")
  }
  expression <- paste(expression, paste(as.character(substitute(raster_stack)), "$", names(raster_stack)[nlayers(raster_stack)], sep=""), sep = " ")
  return(expression)
}

generate_sampling_points <- function(n_trainingdata, dist_trainingdata){
  if(dist_trainingdata %in% c("clust1")){
    train_points <- clustered_sample(study_area, n_trainingdata/5, n_trainingdata*4/5, dimgrid*0.05)
  }else if(dist_trainingdata %in% c("clust2")){
    train_points <- clustered_sample(study_area, n_trainingdata/10, n_trainingdata*9/10, dimgrid*0.05)
  }else if(dist_trainingdata %in% c("nonunif")){
    nonuniform_areas <- nonuniform_sampling_polys(dgrid=dimgrid)
    train_points <- st_sample(filter(nonuniform_areas, sample=="Yes"), n_trainingdata, type = "random")
    train_points <- st_sf(geom=train_points)
  }else{
    train_points <- st_sample(study_area, n_trainingdata, type = dist_trainingdata)
    train_points <- st_sf(geom=train_points)
  }
  return(train_points)
}

generate_predictors <- function(nlm){
  predictors <- stack()
  for (i in 1:length(nlm)) {
    if (nlm[i] %in% c("distance_gradient")){
      distance_gradient <- nlm_distancegradient(ncol = 100, nrow = 100,
                                                origin = c(80, 10, 40, 5))
      predictors$distance_gradient <- distance_gradient
    }
    else if(nlm[i] %in% c("edge_gradient")){
      edge_gradient <- nlm_edgegradient(ncol = 100, nrow = 100, direction = 30)
      predictors$edge_gradient <- edge_gradient
    }
    else if(nlm[i] %in% c("fbm_raster")){
      fbm_raster  <- nlm_fbm(ncol = 100, nrow = 100, fract_dim = 0.2)
      predictors$fbm_raster <- fbm_raster
    }
    else if(nlm[i] %in% c("gaussian_field")){
      gaussian_field <- nlm_gaussianfield(ncol = 100, nrow = 100,
                                          autocorr_range = 100,
                                          mag_var = 8,
                                          nug = 5)
      predictors$gaussian_field <- gaussian_field
    }
    else if(nlm[i] %in% c("mosaictess")){
      mosaictess <- nlm_mosaictess(ncol = 100, nrow = 100, germs = 50)
      predictors$mosaictess <- mosaictess
    }
    else if(nlm[i] %in% c("neigh_raster")){
      neigh_raster <- nlm_neigh(ncol = 100, nrow = 100, p_neigh = 0.75,
                                p_empty = 0.1, categories = 5, neighbourhood = 8)
      predictors$neigh_raster <- neigh_raster
    }
    else if(nlm[i] %in% c("planar_gradient")){
      planar_gradient <- nlm_planargradient(ncol = 100, nrow = 100)
      predictors$planar_gradient <- planar_gradient
    }
    else if(nlm[i] %in% c("random")){
      random <- nlm_random(ncol = 100, nrow = 100)
      predictors$random <- random
    }
    else if(nlm[i] %in% c("random_cluster")){
      random_cluster <- nlm_randomcluster(ncol = 100, nrow = 100,
                                          p = 0.4, ai = c(0.25, 0.25, 0.5))
      predictors$random_cluster <- random_cluster
    }
    else if(nlm[i] %in% c("random_rectangular_cluster")){
      random_rectangular_cluster <- nlm_randomrectangularcluster(ncol = 100,
                                                                nrow = 100,
                                                                minl = 5,
                                                                maxl = 10)
      predictors$random_rectangular_cluster <- random_rectangular_cluster
    }
  }
  return(predictors)
}

normalize <- function(x){(x-minValue(x))/(maxValue(x)-minValue(x))}

# Load data --------------------------------------------------------------------
# Create grids
dimgrid <- 100
rast_grid <- raster(ncols=dimgrid, nrows=dimgrid, xmn=0, xmx=dimgrid, ymn=0, ymx=dimgrid)
point_grid <- st_as_sf(rasterToPoints(rast_grid, spatial = TRUE))
# Create sampling areas
study_area <- st_as_sf(as(extent(rast_grid), "SpatialPolygons"))
# Creating coordinate points to include them in the surface_data
# coord_points <- point_grid
# coord_points$x <- st_coordinates(coord_points)[,1]
# coord_points$y <- st_coordinates(coord_points)[,2]
# coord_stack <- rasterise_and_stack(coord_points,
#                                    which(names(coord_points)%in%c("x","y")),
#                                    c("coord1", "coord2"))
# Define UI --------------------------------------------------------------------

ui <- navbarPage(title = "Remote Sensing Modeling Tool", theme = shinytheme("flatly"), 
  tabPanel("App",
    sidebarLayout(
      sidebarPanel(
        h4("Parameters for predictors"),
        # sliderInput(
        #   inputId = "n_predictors",
        #   label = "Number of predictors:",
        #   value = 11,
        #   min = 2,
        #   max = 20,
        #   step = 1,
        #   width = "100%"
        # ),
        
        selectInput(
          inputId = "nlm", label = "NLMs:",
          choices = c("Distance gradient" = "distance_gradient",
                      "Edge gradient" = "edge_gradient",
                      "Fractional brownian motion" = "fbm_raster",
                      "Gaussian random field" = "gaussian_field",
                      "Planar gradient" = "planar_gradient",
                      "Polygonal landscapes" = "mosaictess",
                      "Random" = "random",
                      "Random cluster" = "random_cluster",
                      "Random neighbourhood" = "neigh_raster",
                      "Random rectangular cluster" = "random_rectangular_cluster"),
          multiple = TRUE
        ),
        
        actionButton(
          inputId = "generate_predictors", label = "Generate selected predictors"
        ),
        
        p(),
        
        uiOutput("secondSelection"),
        
        actionButton(
          inputId = "sim_outcome", label = "Simulate outcome"
        ),
        
        h4("Parameters for training data"),
        numericInput(
          inputId = "n_sampling_points",
          label = "Number of sampling points:",
          value = 50,
          min = 50,
          max = 250,
          step = 50,
          width = "60%"
        ),
        
        selectInput(
          inputId = "dist_sampling_points", label = "Distribution of sampling points:",
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
        ),
        actionButton(
          inputId = "gen_prediction", label = "Generate prediction"
        ),
      ),
      
      mainPanel(
        h4("Predictors and sampling points"),
        wellPanel(
          fluidRow(
            column(6, plotOutput(outputId = "predictors")),
            column(6, plotOutput(outputId = "sampling_points"))
          )
        ),
        h4("Simulated outcome and prediction"),
        wellPanel(
          fluidRow(
            column(6, plotOutput(outputId = "outcome")),
            column(6, plotOutput(outputId = "prediction"))
          )
        ),
        h4("Absolute difference and mean absolute error"),
        wellPanel(
          fluidRow(
            column(6, plotOutput(outputId = "difference")),
            column(6, textOutput(outputId = "mae")),
          )
        ),
        h4("Area of Applicability and dissimilarity index"),
        wellPanel(
          fluidRow(
            column(6, plotOutput(outputId = "aoa")),
            column(6, plotOutput(outputId = "di")),
          )
        ),
        h4("Training data"),
        wellPanel(
          fluidRow(
            column(12, dataTableOutput(outputId = "training_data")),
          )
        ),
        br()
      )
    )
  ),
  tabPanel("Documentation"),
  tabPanel("Demo"),
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  output$secondSelection <- renderUI({
    selectInput("nlms_for_outcome", label = "Simulate outcome from following NLMs:", choices = input$nlm, multiple = TRUE)
    })
  
  predictors <- eventReactive(input$generate_predictors, {
    req(input$nlm)
    generate_predictors(input$nlm)
  })
  
  simulation <- eventReactive(input$sim_outcome, {
    if (length(input$nlms_for_outcome) >= 2) {
      nlms_for_outcome <- subset(predictors(), input$nlms_for_outcome)
      simulation <- raster()
      expression <- generate_random_function(nlms_for_outcome)
      simulation <- normalize(eval(parse(text=expression)))
      names(simulation) <- "outcome"
      return(simulation)
    }
  })
  
  sampling_points <- reactive({
    req(input$n_sampling_points, input$dist_sampling_points)
    generate_sampling_points(input$n_sampling_points, input$dist_sampling_points)
  })
  
  output$predictors <- renderPlot({
      show_landscape(predictors())
  })
  
  output$sampling_points <- renderPlot({
    ggplot() +
      geom_sf(data = sampling_points(), size = 1) +
      geom_sf(data = study_area,  alpha = 0) +
      theme_bw()
  })
  
  observeEvent(input$sim_outcome, {
    output$outcome <- renderPlot({
      show_landscape(simulation())
    })
  })
  
  observeEvent(input$gen_prediction, {
    if (input$sim_outcome >=1) {
      # Extracting all necessary information to create the training data
      # print(names(predictors()))
      all_stack <- stack(simulation(), predictors())
      pred <- names(predictors())
      training_data <- as.data.frame(raster::extract(all_stack, sampling_points()))
      output$training_data <- renderDataTable(expr = training_data)
      # id$areant_grid$area
      # 
      # Create default model
      model_default <- train(training_data[,pred],
                             training_data$outcome,
                             method = "rf",
                             importance = TRUE,
                             ntree = 500)
      print(varImp(model_default))
      print(model_default)
      # model_default
      prediction_default <- predict(all_stack, model_default)
    
      dif_default <- simulation() - prediction_default
      # result <- stack(prediction_default, dif_default)
      # print(names(result))
      # names(result) <- c("Prediction", "Difference")
      output$prediction <- renderPlot({
        show_landscape(prediction_default)
      })
      output$difference <- renderPlot({
        show_landscape(dif_default)
      })
      
      prediction_default_abs <- abs(prediction_default)
      MAE_default <- sum(raster::extract(prediction_default_abs, point_grid))/10000
      print(MAE_default)
      output$mae <- renderText({
        paste("MAE =", MAE_default, sep = " ")
      })
      aoa <- aoa(all_stack, model_default)
      # print(names(aoa))
      output$aoa <- renderPlot({
        show_landscape(aoa$AOA)
      })
      output$di <- renderPlot({
        show_landscape(aoa$DI)
      })
    }
  })
}
# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
