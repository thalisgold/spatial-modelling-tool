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
library(gstat)
library(NNDM)

# Load functions ---------------------------------------------------------------

#' @author Carles Mila
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

#' @author Carles Mila
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

#' @author Carles Mila
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

#' @author Carles Mila
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
  
  # Add ID?
  checker_folds$ID <- c(1:25)

  # Draw random blocks for sampling
  sampling_vector <- c(rep("Yes", targetblock), rep("No", blockside^2-targetblock))
  checker_folds$sample <- sample(sampling_vector, replace=FALSE)
  
  # Return object
  return(checker_folds)
}

#' @author Thalis Goldschmidt
#' Generation of a random expression
#' @description 
#' This function generates a random expression in order to offset the predictors of a given 
#' stack to simulate an outcome.
#' @param raster_stack RasterStack. A stack with any number of layers
#' @return A string containing the expression to be evaluated. 
#' @examples
#' print(generate_random_function(predictors))
generate_random_function <- function(raster_stack) {
  # All possible operands (easy to extend the list)
  operands = c("+", "-", "*", "^2 +", "^3 -", "^2 *", "^2 -", "^3 +")
  expression = ""
  for (i in 1:(nlayers(raster_stack)-1)){ # (-1 so that the last argument of the stack does not have an operand attached to it)
    expression <- paste(expression, paste(as.character(substitute(raster_stack)), "$", names(raster_stack)[i], sep=""), sep = " ")
    expression <- paste(expression, sample(operands, 1), sep = " ")
  }
  expression <- paste(expression, paste(as.character(substitute(raster_stack)), "$", names(raster_stack)[nlayers(raster_stack)], sep=""), sep = " ")
  return(expression)
}

#' @author Thalis Goldschmidt
#' Generation of sampling points
#' @description 
#' This function generates sampling points depending on the chosen distribution and number. 
#' @param n_sampling_points Integer. Number of sampling points to be generated.
#' @param dist_sampling_points String. Name of the selected distribution.
#' @return A simple feature collection with n features of the type "POINT" and their geometry.
#' @examples
#' generate_sampling_points(50, "random")
generate_sampling_points <- function(n_sampling_points, dist_sampling_points){
  if(dist_sampling_points %in% c("nonunif")){
    nonuniform_areas <- nonuniform_sampling_polys(dgrid=dimgrid)
    sampling_points <- st_sample(filter(nonuniform_areas, sample=="Yes"), n_sampling_points, type = "random")
    sampling_points <- st_sf(geom=sampling_points)
  }else{
    sampling_points <- st_sample(study_area, n_sampling_points, type = dist_sampling_points)
    sampling_points <- st_sf(geom=sampling_points)
  }
  return(sampling_points)
}

#' @author Thalis Goldschmidt
#' Generation of predictors
#' @description 
#' This function generates predictors from a list of passed neutral landscape models.
#' @param nlms List of strings. Contains all the names of the nlms to be generated.
#' @return A stack of the generated nlms, i. e. of out predictors
#' @examples
#' generate_sampling_points(c("distance_gradient", "edge_gradient", "fbm_raster"))
generate_predictors <- function(nlms){
  predictors <- stack()
  for (i in 1:length(nlms)) {
    if (nlms[i] %in% c("distance_gradient")){
      distance_gradient <- nlm_distancegradient(ncol = 100, nrow = 100,
                                                origin = c(40, 40, 40, 40))
      predictors$distance_gradient <- distance_gradient
    }
    else if(nlms[i] %in% c("edge_gradient")){
      edge_gradient <- nlm_edgegradient(ncol = 100, nrow = 100, direction = 25)
      predictors$edge_gradient <- edge_gradient
    }
    else if(nlms[i] %in% c("fbm_raster")){
      fbm_raster  <- nlm_fbm(ncol = 100, nrow = 100, fract_dim = 0.4)
      predictors$fbm_raster <- fbm_raster
    }
    else if(nlms[i] %in% c("gaussian_field")){
      gaussian_field <- nlm_gaussianfield(ncol = 100, nrow = 100,
                                          autocorr_range = 40,
                                          mag_var = 8,
                                          nug = 5)
      predictors$gaussian_field <- gaussian_field
    }
    else if(nlms[i] %in% c("mosaictess")){
      mosaictess <- nlm_mosaictess(ncol = 100, nrow = 100, germs = 50)
      predictors$mosaictess <- mosaictess
    }
    else if(nlms[i] %in% c("neigh_raster")){
      neigh_raster <- nlm_neigh(ncol = 100, nrow = 100, p_neigh = 0.75,
                                p_empty = 0.1, categories = 5, neighbourhood = 8)
      predictors$neigh_raster <- neigh_raster
    }
    else if(nlms[i] %in% c("planar_gradient")){
      planar_gradient <- nlm_planargradient(ncol = 100, nrow = 100)
      predictors$planar_gradient <- planar_gradient
    }
    else if(nlms[i] %in% c("random")){
      random <- nlm_random(ncol = 100, nrow = 100)
      predictors$random <- random
    }
    else if(nlms[i] %in% c("random_cluster")){
      random_cluster <- nlm_randomcluster(ncol = 100, nrow = 100,
                                          p = 0.4, ai = c(0.25, 0.25, 0.5))
      predictors$random_cluster <- random_cluster
    }
    else if(nlms[i] %in% c("random_rectangular_cluster")){
      random_rectangular_cluster <- nlm_randomrectangularcluster(ncol = 100,
                                                                nrow = 100,
                                                                minl = 5,
                                                                maxl = 10)
      predictors$random_rectangular_cluster <- random_rectangular_cluster
    }
  }
  return(predictors)
}

#' @author Thalis Goldschmidt
#' Function to normalize raster values to values between 0 and 1
#' @description 
#' This function normalizes all values of a raster to values between 0 and 1.
#' @param raster RasterLayer. Layer to be normalized.
#' @return A normalized raster layer.
#' @examples
#' distance_gradient_normalized <- normalized(distance_gradient)
normalizeRaster <- function(raster){(raster-minValue(raster))/(maxValue(raster)-minValue(raster))}

execute_model_training <- function(algorithm, cv_method, training_data, predictors, variable_selection) {
  names_predictors <- names(predictors)
  # Create train control depending on cv strategy
  if (cv_method == "random_10_fold_cv"){
    ctrl <- trainControl(method="cv", number = 10, savePredictions = TRUE)
  }
  else if(cv_method == "loo_cv"){
    ctrl <- trainControl(method="cv", number = length(training_data[[1]]), savePredictions = TRUE)
  }
  else if(cv_method == "sb_cv"){
    indices <- CreateSpacetimeFolds(training_data,spacevar = "ID",k=length(unique(training_data$ID)))
    ctrl <- trainControl(method="cv", index = indices$index, savePredictions = TRUE)
  }
  else if(cv_method == "nndm_loo_cv"){
    training_data_as_sfc <- st_as_sf(training_data, coords = c("coord1", "coord2"), remove = F)
    predictors <- stack(predictors, coord_stack)
    predictors_df <- as.data.frame(extract(predictors, point_grid))
    predictors_as_sfc <- st_as_sf(predictors_df, coords = c("coord1", "coord2"), remove = F)
    training_data_sp_df <- training_data
    coordinates(training_data_sp_df)=~coord1+coord2
    empvar <- variogram(outcome~1, data = training_data_sp_df)
    fitvar <- fit.variogram(empvar, vgm(model="Sph", nugget = T), fit.sills = TRUE)
    outrange <- fitvar$range[2]
    # print(outrange)
    # output$test1 <- renderPlot(plot(empvar, fitvar,cutoff = 50, main = "Outcome semi-variogram estimation"))
    # Compute NNDM indices
    NNDM_indices <- nndm(training_data_as_sfc, predictors_as_sfc, outrange, min_train = 0.5)
    #> nndm object
    #> Total number of points: 155
    #> Mean number of training points: 153.88
    #> Minimum number of training points: 150
    # Plot NNDM functions
    # output$test2 <- renderPlot(plot(NNDM_indices))
    ctrl <- trainControl(method = "cv", savePredictions = T, index=NNDM_indices$indx_train, indexOut=NNDM_indices$indx_test)
  }
  # Train model depending on variable selection and algorithm
  if (variable_selection == "None"){
    model <- train(training_data[,names_predictors],
                   training_data$outcome,
                   tuneGrid=data.frame("mtry"=2),
                   method = algorithm,
                   importance = TRUE,
                   trControl=ctrl)
  }
  else if (variable_selection == "FFS" & algorithm == "rf"){
    model <- CAST::ffs(predictors = training_data[,names_predictors],
                            response = training_data$outcome,
                            method = algorithm,
                            trControl=ctrl)
  }
  if (variable_selection == "RFE" & algorithm == "rf"){
    model <- rfe(training_data[,names_predictors],
                 training_data$outcome,
                 method= algorithm,
                 metric = "RMSE",
                 rfeControl=rfeControl(method="cv", index = indices$index, functions = caretFuncs))
  }
  return(model)
}

# Load data --------------------------------------------------------------------

# Create grids
dimgrid <- 100
rast_grid <- raster(ncols=dimgrid, nrows=dimgrid, xmn=0, xmx=dimgrid, ymn=0, ymx=dimgrid)
point_grid <- st_as_sf(rasterToPoints(rast_grid, spatial = TRUE))
# print((point_grid))

# Create sampling areas
study_area <- st_as_sf(as(extent(rast_grid), "SpatialPolygons"))
# plot(study_area)

# Spatial blocks for cross validation
spatial_blocks <- nonuniform_sampling_polys(100, 5, 5)
spatial_blocks <- spatial_blocks[1:2]
# plot(spatial_blocks)

# Creating coordinate points to include them in the surface_data
coord_points <- point_grid
coord_points$x <- st_coordinates(coord_points)[,1]
coord_points$y <- st_coordinates(coord_points)[,2]
coord_stack <- rasterise_and_stack(coord_points, 
                                   which(names(coord_points)%in%c("x","y")), 
                                   c("coord1", "coord2"))
all_stack <- stack(coord_stack)


# Define UI --------------------------------------------------------------------

ui <- navbarPage(title = "Remote Sensing Modeling Tool", theme = shinytheme("flatly"), 
  tabPanel("App",
    sidebarLayout(
      sidebarPanel(
        
        # It is possible to plant a seed in order to always achieve the same results
        # and thus comparability.
        checkboxInput(
          inputId = "set_seed",
          label = "Set following seed to make your results reproducible:",
          value = FALSE
          ),
        
        conditionalPanel(condition = "input.set_seed",
                         numericInput(
                           inputId = "seed",
                           label = "",
                           value = 1,
                           min = 1,
                           max = 10000,
                           step = 1,
                           width = "30%"
                           )
                         ),
      
        h4("Parameters for predictors"),
        
        # Choose multiple NLMs to generate predictors.
        selectInput(
          inputId = "nlm",
          label = "Choose some NLMs as predictors:",
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
          multiple = TRUE,
          selected = c("distance_gradient", "edge_gradient", "fbm_raster")
        ),
        
        # If more than 2 were chosen, it is possible to generate the predictors.
        conditionalPanel(condition = "input.nlm.length >= 2",
                         actionButton(
                           inputId = "generate_predictors",
                           label = "Generate selected predictors"
                           )
                         ),

        p(),
        
        # Select from which of the already generated predictors the result should be simulated.
        uiOutput("nlms_for_outcome"),
        checkboxInput(inputId = "r_noise", label = "Add random noise", value = FALSE),
        checkboxInput(inputId = "s_noise", label = "Add spatially correlated noise", value = FALSE),
        
        # If more than 2 were chosen, it is possible to simulate the outcome.
        conditionalPanel(condition = "input.nlms_for_outcome.length >= 2",
                         actionButton(
                           inputId = "sim_outcome",
                           label = "Simulate outcome"
                           )
                         ),
        
        # Select the number and distribution of the sampling points.
        h4("Parameters for training data"),
        selectInput(
          inputId = "dist_sampling_points",
          label = "Distribution of sampling points:",
          choices = c("Clustered" = "clustered",
                      "Non-uniform" = "nonunif",
                      "Random" = "random",
                      "Regular" = "regular"),
          selected = "random"
        ),
        
        conditionalPanel(condition = "!output.clustered",
                         numericInput(
                           inputId = "n_sampling_points",
                           label = "Number of sampling points:",
                           value = 50,
                           min = 50,
                           max = 250,
                           step = 50,
                           width = "60%"
                           )
                         ),
        
        # If "clustered" is selected as the distribution, three sliders open to
        # determine further parameters.
        conditionalPanel(condition = "output.clustered",
                         sliderInput(
                           inputId = "n_parents",
                           label = "Number of parents:",
                           value = 5,
                           min = 1,
                           max = 20,
                           step = 1,
                           width = "100%"
                           ),
                         sliderInput(
                           inputId = "n_offsprings",
                           label = "Number of offsprings:",
                           value = 45,
                           min = 10,
                           max = 250,
                           step = 1,
                           width = "100%"
                           ),
                         sliderInput(inputId = "radius",
                           label = "Radius:",
                           value = 5,
                           min = 1,
                           max = 8,
                           step = 1,
                           width = "100%"
                           )
                         ),
        
        h4("Modelling"),
        radioButtons(
          inputId = "algorithm", 
          label = "Choose algorithm for training:",
          choices = c("Random Forest" = "rf", 
                      "Support Vector Machines" = "svmRadial"),
          selected = "rf"
        ),
        
        selectInput(
          inputId = "cv_method",
          label = "Cross-validation method:",
          choices = c("Random 10-fold CV" = "random_10_fold_cv",
                      "LOO CV" = "loo_cv",
                      "Spatial block CV" = "sb_cv",
                      "NNDM LOO CV" = "nndm_loo_cv"
                      ),
          multiple = TRUE,
        ),
        
        conditionalPanel(condition = "input.algorithm == 'rf'",
                         selectInput(
                           inputId = "variable_selection", label = "Variable selection:",
                           choices = c("None", "FFS", "RFE"),
                           selected = "None"
                           )
                         ),
        
        # When the result has been calculated, it is possible to make a prediction.
        uiOutput("gen_prediction"),
      ),
      
      mainPanel(
        # conditionalPanel(condition = "input.generate_predictors",
        #   h4("Predictors and sampling points"),
        fluidRow(
          column(6, conditionalPanel(condition = "input.generate_predictors",
                                     wellPanel(
                                       h4("Predictors"),
                                       plotOutput(outputId = "predictors")
                                       )
                                     )
                 ),
          column(6, conditionalPanel(condition = "input.sim_outcome",
                                     wellPanel(
                                       h4("Sampling points"),
                                       plotOutput(outputId = "sampling_points")
                                       )
                                     )
                 ),
          ),
        fluidRow(
          column(4, conditionalPanel(condition = "input.sim_outcome",
                                     wellPanel(
                                       h4("Simulated outcome"),
                                       plotOutput(outputId = "outcome")
                                       )
                                     )
                 ),
          column(4, conditionalPanel(condition = "input.gen_prediction",
                                     wellPanel(
                                       h4("Prediction"),
                                       plotOutput(outputId = "prediction")
                                       )
                                     )
                 ),
          column(4, conditionalPanel(condition = "input.gen_prediction",
                                     wellPanel(
                                       h4("Difference"),
                                       plotOutput(outputId = "difference")
                                       )
                                     )
                 ),
          ),
        conditionalPanel(condition = "input.gen_prediction",
                         wellPanel(
                           textOutput(outputId = "true_mae")
                           )
                         ),
        fluidRow(
          column(3, conditionalPanel(condition = "input.gen_prediction",
                                     wellPanel(
                                       h4("Random 10-fold CV"),
                                       tableOutput(outputId = "random_10_fold_cv")
                                       )
                                     )
                 ),
          column(3, conditionalPanel(condition = "input.gen_prediction",
                                     wellPanel(
                                       h4("LOO CV"),
                                       tableOutput(outputId = "loo_cv")
                                       )
                                     )
                 ),
          column(3, conditionalPanel(condition = "input.gen_prediction",
                                     wellPanel(
                                       h4("Spatial block CV"),
                                       tableOutput(outputId = "sb_cv")
                                       )
                                     )
          ),
          column(3, conditionalPanel(condition = "input.gen_prediction",
                                     wellPanel(
                                       h4("NNDM LOO CV"),
                                       tableOutput(outputId = "nndm_loo_cv")
                                       )
                                     )
                 ),
          ),
        fluidRow(
          column(6, conditionalPanel(condition = "input.gen_prediction",
                                     wellPanel(
                                       h4("Area of applicabilty"),
                                       plotOutput(outputId = "aoa")
                                       )
                                     )
                 ),
          column(6, conditionalPanel(condition = "input.gen_prediction",
                                     wellPanel(
                                       h4("Dissimilarity index"),
                                       plotOutput(outputId = "di")
                                       )
                                     )
                 ),
          ),
        plotOutput(outputId = "test1"),
        plotOutput(outputId = "test2"),
        br(),
        )
      )
    ),
  tabPanel("Documentation"),
  tabPanel("Demo"),
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  predictors <- eventReactive(input$generate_predictors, {
    req(input$nlm)
    if (input$set_seed){
      set.seed(input$seed)
    }
    generate_predictors(input$nlm)
  })
  
  output$predictors <- renderPlot({
    show_landscape(predictors())
    # all_stack <- stack(coord_stack, predictors())
    # predictors_surface <- as.data.frame(raster::extract(all_stack, point_grid))
    # print(predictors_surface)
    # ggplot(predictors_surface) +
    #   geom_raster(aes(x = coord1, y = coord2, fill = distance_gradient)) +
    #   # scale_fill_scico("", palette = 'roma') +
    #   geom_raster(aes(x = coord1, y = coord2, fill = edge_gradient)) +
    #   # scale_fill_scico("", palette = 'roma') +
    #   geom_raster(aes(x = coord1, y = coord2 , fill = fbm_raster)) +
    #   scale_fill_scico("", palette = 'roma') +
    #   xlab("") + ylab("") +
    #   theme_bw() + theme(legend.position = "bottom")
  })
  
  output$nlms_for_outcome <- renderUI({
    selectInput("nlms_for_outcome", label = "Simulate outcome from following NLMs:", choices = input$nlm, multiple = TRUE, selected = c("distance_gradient", "edge_gradient"))
  })
  
  simulation <- eventReactive(input$sim_outcome, {
    nlms_for_outcome <- subset(predictors(), input$nlms_for_outcome)
    simulation <- raster()
    if (input$set_seed){
      set.seed(input$seed)
    }
    expression <- generate_random_function(nlms_for_outcome)
    simulation <- eval(parse(text=expression))
    if (input$r_noise == TRUE){
      r_noise <- raster(ncols=dimgrid, nrows=dimgrid, xmn=0, xmx=dimgrid, ymn=0, ymx=dimgrid)
      if (input$set_seed){
        set.seed(input$seed)
      }
      vals <- rnorm(dimgrid*dimgrid, sd=1)
      vals <- vals * 0.05
      r_noise <- setValues(r_noise, vals)
      simulation <- simulation + r_noise
    }
    if (input$s_noise == TRUE){
      if (input$set_seed){
        set.seed(input$seed)
      }
      variog_mod <- vgm(model = "Sph", psill = 1, range = 40, nugget = 0)
      gstat_mod <- gstat(formula = z~1, dummy = TRUE, beta = 0, model = variog_mod, nmax = 100)
      s_noise <- predict(gstat_mod, point_grid, nsim = 1)
      s_noise <- rasterFromXYZ(cbind(st_coordinates(s_noise),
                                    as.matrix(as.data.frame(s_noise)[,1], ncol=1)))
      s_noise <- s_noise * 0.05
      simulation <- simulation + s_noise
    }
    output$gen_prediction <- renderUI({
      conditionalPanel(condition = "input.cv_method.length > 0",
        actionButton(
          inputId = "gen_prediction", label = "Generate prediction"
        )
      )
    })
      
    simulation <- normalizeRaster(simulation)
    names(simulation) <- "outcome"
    # print(simulation)
    return(simulation)
  })
  
  observeEvent(input$sim_outcome, {
    output$outcome <- renderPlot({
      show_landscape(simulation())
      # all_stack <- stack(coord_stack, simulation())
      # sim_outcome_surface <- as.data.frame(raster::extract(all_stack, point_grid))
      # ggplot(sim_outcome_surface) +
      #   geom_tile(aes(x = coord1, y = coord2, fill = outcome)) +
      #   xlab("") + ylab("") +
      #   theme_light() + theme(legend.position = "bottom") +
      #   scale_fill_distiller("", palette = "YlOrRd")
    })
  })
  
  distInput <- reactive({
    switch(input$dist_sampling_points,
           "random",
           "regular",
           "clustered",
           "nonunif")
  })
  
  output$clustered <- reactive({
    if (input$dist_sampling_points == "clustered")
      return(TRUE)
  })
  
  outputOptions(output, "clustered", suspendWhenHidden = FALSE) 
  
  
  sampling_points <- reactive({
    req(input$n_sampling_points, input$dist_sampling_points)
    if (input$set_seed){
      set.seed(input$seed)
    }
    if (input$dist_sampling_points != "clustered"){
      sampling_points <- generate_sampling_points(input$n_sampling_points, input$dist_sampling_points)
    }
    else{
      sampling_points <- clustered_sample(study_area, input$n_parents, input$n_offsprings, input$radius)
    }
    # print(sampling_points)
  })
  
  output$sampling_points <- renderPlot({
    ggplot() +
      geom_sf(data = sampling_points(), size = 1) +
      geom_sf(data = study_area,  alpha = 0) +
      theme_light()
  })
  
  observeEvent(input$gen_prediction, {
    predictors <- predictors()
    sampling_points <- sampling_points() # Generate sampling points
    all_stack <- stack(coord_stack, simulation(), predictors()) # Create a stack of the outcome and all predictors
    print(class(all_stack))
    pred <- names(predictors()) # Save all the names of the predictors so that the function knows which columns to use in the training
    sampling_points <- st_join(sampling_points, spatial_blocks) # Assign a spatial block to each sampling point
    training_data <- as.data.frame(extract(all_stack, sampling_points, sp = TRUE)) # Extract the informations of the predictors and the outcome on the positions of the sampling points
    # View(training_data)
    # training_data_as_sfc <- st_as_sf(training_data, coords = c("coord1", "coord2"), remove = F)
    # print(training_data_as_sfc)
    # predictors <- stack(predictors(), coord_stack)
    # predictors_df <- as.data.frame(extract(predictors, point_grid))
    # predictors_as_sfc <- st_as_sf(predictors_df, coords = c("coord1", "coord2"), remove = F)
    # print(predictors_as_sfc)
    # # predictors_as_sfc 
    # training_data_sp_df <- training_data
    # coordinates(training_data_sp_df)=~coord1+coord2
    # # print(class(training_data_sp_df))
    # empvar <- variogram(outcome~1, data = training_data_sp_df)
    # fitvar <- fit.variogram(empvar, vgm(model="Sph", nugget = T), fit.sills = TRUE)
    # # print(fitvar)
    # outrange <- fitvar$range[2]
    # output$test1 <- renderPlot(plot(empvar, fitvar,cutoff = 50, main = "Outcome semi-variogram estimation"))
    # 
    # # Compute NNDM indices
    # NNDM_indices <- nndm(training_data_as_sfc, predictors_as_sfc, outrange, min_train = 0.5)
    # #> nndm object
    # #> Total number of points: 155
    # #> Mean number of training points: 153.88
    # #> Minimum number of training points: 150
    # # Plot NNDM functions
    # output$test2 <- renderPlot(plot(NNDM_indices))
    
    models <- list()
    model_results <- list()
    # Create model and use two different cv_methods during training.
    # For the first passed cv-method create a prediction and aoa
    if (input$set_seed){
      set.seed(input$seed)
    }
    for (i in 1:length(input$cv_method)) {
      models[[i]] <- execute_model_training(input$algorithm, input$cv_method[i], training_data, predictors, input$variable_selection)
      if (input$algorithm == "rf") {
        model_results[[i]] <- models[[i]]$results[c("mtry", "RMSE", "Rsquared", "MAE")]
      # print(model_results[[i]])
      # dummy <- noquote(input$cv_method[i])
      # print(dummy)
      # print(output)
      # output$eval(input$cv_method[i]) <- renderTable(model_results[[i]])
      print(varImp(models[[i]]))
      }
      else {
        model_results[[i]] <- models[[i]]$results[c("C", "RMSE", "Rsquared", "MAE")]
      }
    }
    names(models) <- input$cv_method
    # print(models)
    # output$test1 <- renderPlot(plot_ffs(models[[1]]))
    # output$test2 <- renderPlot(plot_ffs(models[[1]] ,plotType="selected"))
    # print(models$optVariables)
    # print(models)
    # print(names(models))
    # print(length(input$cv_method))
    # print(model_results[[1]])
    # print(model_results[[2]])
    # print(model_results[[3]])
    output$random_10_fold_cv <- NULL
    output$loo_cv <- NULL
    output$sb_cv <- NULL
    output$nndm_loo_cv <- NULL
    for (i in 1:length(input$cv_method)) {
      if (names(models[i]) == "random_10_fold_cv"){
        # print(i)
        j <- i
        # print(names(models[i]))
        output$random_10_fold_cv <- renderTable(expr = model_results[[j]], striped = TRUE, digits = 4)
      }
      if (names(models[i]) == "loo_cv"){
        # print(i)
        k <- i
        # print(names(models[i]))
        output$loo_cv <- renderTable(expr = model_results[[k]], striped = TRUE, digits = 4)
      }
      if (names(models[i]) == "sb_cv"){
        # print(i)
        l <- i
        # print(names(models[i]))
        output$sb_cv <- renderTable(expr = model_results[[l]], striped = TRUE, digits = 4)
      }
      if (names(models[i]) == "nndm_loo_cv"){
        # print(i)
        m <- i
        # print(names(models[i]))
        output$nndm_loo_cv <- renderTable(expr = model_results[[m]], striped = TRUE, digits = 4)
      }
    }
    prediction <- predict(predictors(), models[[1]])
    names(prediction) <- "prediction"
    dif <- simulation() - prediction
    names(dif) <- "dif"
    aoa <- aoa(predictors(), models[[1]])
    
    all_stack <- stack(all_stack, prediction, dif, aoa)
    surface <- as.data.frame(raster::extract(all_stack, point_grid))
    # print(surface)
    
    # View(prediction)
    output$prediction <- renderPlot({
      # To rescale legend
      # surface$prediction[1] <- 0
      # surface$prediction[2] <- 1
      prediction[1] <- 0
      prediction[2] <- 1
      show_landscape(prediction)
      # ggplot(surface) +
      #   geom_tile(aes(x = coord1, y = coord2, fill = prediction)) +
      #   xlab("") + ylab("") +
      #   theme_light() + theme(legend.position = "bottom") +
      #   scale_fill_distiller("", palette = "YlOrRd")
    })
    output$difference <- renderPlot({
      show_landscape(dif)
      # ggplot(surface) +
      #   geom_raster(aes(x = coord1, y = coord2, fill = dif)) +
      #   xlab("") + ylab("") +
      #   theme_light() + theme(legend.position = "bottom") +
      #   scale_fill_distiller("", palette = "RdYlGn")
    })
    MAE <- round((sum(raster::extract(abs(dif), point_grid))/10000), digits = 4)
    output$true_mae <- renderText({
      paste("True MAE =", MAE,  sep = " ")
    })
    # print(names(aoa))
    output$aoa <- renderPlot({
      show_landscape(aoa$AOA)
      # ggplot(surface) +
        # geom_raster(aes(x = coord1, y = coord2, fill = AOA)) +
        # xlab("") + ylab("") +
        # theme_light() + theme(legend.position = "bottom") +
        # scale_fill_distiller("", palette = "YlOrRd")
    })
    output$di <- renderPlot({
      show_landscape(aoa$DI)
      # ggplot(surface) +
      #   geom_raster(aes(x = coord1, y = coord2, fill = DI)) +
      #   xlab("") + ylab("") +
      #   theme_light() + theme(legend.position = "bottom") +
      #   scale_fill_distiller("", palette = "YlOrRd")
    })
  })
}
# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
