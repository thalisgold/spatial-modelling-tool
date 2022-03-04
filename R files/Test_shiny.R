# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(NLMR)
library(landscapetools)
library(raster)
library(caret)
library(sf)


# Load data --------------------------------------------------------------------
# Create grids
dimgrid <- 100
rast_grid <- raster(ncols=dimgrid, nrows=dimgrid, xmn=0, xmx=dimgrid, ymn=0, ymx=dimgrid)
point_grid <- st_as_sf(rasterToPoints(rast_grid, spatial = TRUE))
# Create sampling areas
study_area <- st_as_sf(as(extent(rast_grid), "SpatialPolygons"))

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
predictors <- stack(distance_gradient, edge_gradient)

# +, -, *, /, ^2
generate_random_function <- function (predictors) {
  operands = c("+", "-", "*", "^2 +", "^3 -")
  expression = ""
  for (i in 1:(nlayers(predictors)-1)){
    expression <- paste(expression, paste("predictors$", names(predictors)[i], sep=""), sep = " ")
    expression <- paste(expression, sample(operands, 1), sep = " ")
  }
  expression <- paste(expression, paste("predictors$", names(predictors)[nlayers(predictors)], sep=""), sep = " ")
  return(expression)
  # outcome <- expression[1]
  # return(outcome)
}
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
        choices = c("Distance gradient", "Edge gradient", "Fractional brownian motion",
                    "Gaussian random field", "Planar gradient", "Polygonal landscapes",
                    "Random", "Random cluster", "Random neighbourhood", "Random rectangular cluster"),
        multiple = TRUE,
        selected = c("Distance gradient", "Edge gradient")
      ),
      
      actionButton(
        inputId = "sim_outcome", label = "Simulate outcome"
      ),
      
      h4("Parameters for training data"),
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
      plotOutput(outputId = "predictors"),
      plotOutput(outputId = "trainingdata"),
      plotOutput(outputId = "outcome"),
      br()
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  predictors <- stack()
  
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
  output$predictors <- renderPlot({
    print(nlayers(predictors))
    for (i in 1:length(input$nlm)) {
      if (input$nlm[i] %in% c("Distance gradient")){
        distance_gradient <- nlm_distancegradient(ncol = 100, nrow = 100,
                                                  origin = c(80, 10, 40, 5))
        predictors$distance_gradient <- distance_gradient
      }
      else if(input$nlm[i] %in% c("Edge gradient")){
        edge_gradient <- nlm_edgegradient(ncol = 100, nrow = 100, direction = 30)
        predictors$edge_gradient <- edge_gradient
      }
      else if(input$nlm[i] %in% c("Fractional brownian motion")){
        fbm_raster  <- nlm_fbm(ncol = 100, nrow = 100, fract_dim = 0.2)
        predictors$fbm_raster <- fbm_raster
      }
      else if(input$nlm[i] %in% c("Gaussian random field")){
        gaussian_field <- nlm_gaussianfield(ncol = 100, nrow = 100,
                                            autocorr_range = 100,
                                            mag_var = 8,
                                            nug = 5)
        predictors$gaussian_field <- gaussian_field
      }
      else if(input$nlm[i] %in% c("Polygonal landscapes")){
        mosaictess <- nlm_mosaictess(ncol = 100, nrow = 100, germs = 50)
        predictors$mosaictess <- mosaictess
      }
      else if(input$nlm[i] %in% c("Random neighbourhood")){
        neigh_raster <- nlm_neigh(ncol = 100, nrow = 100, p_neigh = 0.75,
                                  p_empty = 0.1, categories = 5, neighbourhood = 8)
        predictors$neigh_raster <- neigh_raster
      }
      else if(input$nlm[i] %in% c("Planar gradient")){
        planar_gradient <- nlm_planargradient(ncol = 100, nrow = 100)
        predictors$planar_gradient <- planar_gradient
      }
      else if(input$nlm[i] %in% c("Random")){
        random <- nlm_random(ncol = 100, nrow = 100)
        predictors$random <- random
      }
      else if(input$nlm[i] %in% c("Random cluster")){
        random_cluster <- nlm_randomcluster(ncol = 100, nrow = 100,
                                            p = 0.4, ai = c(0.25, 0.25, 0.5))
        predictors$random_cluster <- random_cluster
      }
      else if(input$nlm[i] %in% c("Random rectangular cluster")){
        randomrectangular_cluster <- nlm_randomrectangularcluster(ncol = 100,
                                                                  nrow = 100,
                                                                  minl = 5,
                                                                  maxl = 10)
        predictors$randomrectangular_cluster <- randomrectangular_cluster
      }
    }
    observeEvent(input$sim_outcome, {
      print(nlayers(predictors))
      names = character()
      # print(names(predictors))
      for (i in 1:nlayers(predictors)) {
        names <- c(names, paste("pred_", i, sep=""))
      }
      # print(names)
      names(predictors) <- names
      print(names(predictors))
      expression <- generate_random_function(predictors)
      print(expression)
      out_true <- eval(parse(text=expression))
      
      output$outcome <- renderPlot({
        show_landscape(out_true)
      })
      
    })
    # print(names(predictors))
    # print(nlayers(predictors))
    if (nlayers(predictors) != 0) {
      show_landscape(predictors)
    }
    })
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
