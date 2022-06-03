# Load packages ----------------------------------------------------------------

library(shiny)
library(shinycssloaders)
library(shinybusy)
library(shinythemes)
library(caret)
library(sf)
library(ggplot2)
library(dplyr)
library(NLMR)
library(landscapetools)
library(raster)
library(CAST)
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
#' Generation of a random mathematical expression
#' @description 
#' This function generates a random expression to combine the predictors of a given stack to simulate a target variable.
#' @param raster_stack RasterStack. A stack with any number of layers.
#' @return A string containing the expression to be evaluated. 
#' @examples
#' print(generate_random_function(predictors))
generate_random_function <- function(raster_stack) {
  # All possible operands (easy to extend the list)
  operands_1 = c("+", "-", "*", "^2 +", "^2 -"," ^2 *", "^3 +", "^3 -", "^3 *")
  operands_2 = c("^2", "^3", "^4")
  expression = ""
  if (nlayers(raster_stack) == 1){
    expression <- paste(as.character(substitute(raster_stack)), "$", names(raster_stack)[1], sep="")
    expression <- paste(expression, sample(operands_2, 1), sep = " ")
  }
  else {
    for (i in 1:(nlayers(raster_stack)-1)){ # (-1 so that the last argument of the stack does not have an operand attached to it)
      expression <- paste(expression, paste(as.character(substitute(raster_stack)), "$", names(raster_stack)[i], sep=""), sep = " ")
      expression <- paste(expression, sample(operands_1, 1), sep = " ")
    }
    expression <- paste(expression, paste(as.character(substitute(raster_stack)), "$", names(raster_stack)[nlayers(raster_stack)], sep=""), sep = " ")
  }
  return(expression)
}

#' @author Thalis Goldschmidt
#' Simulation of the sample points
#' @description 
#' This function generates sample points depending on the chosen distribution and number. 
#' @param n_sample_points Integer. Number of sample points to be generated.
#' @param dist_sample_points String. Name of the selected distribution.
#' @return A simple feature collection with n features of the type "POINT" and their geometry.
#' @examples
#' simulate_sample_points(50, "random")
simulate_sample_points <- function(n_sample_points, dist_sample_points){
  if(dist_sample_points %in% c("nonunif")){
    nonuniform_areas <- nonuniform_sampling_polys(dgrid=dimgrid)
    sample_points <- st_sample(filter(nonuniform_areas, sample=="Yes"), n_sample_points, type = "random")
    sample_points <- st_sf(geom=sample_points)
  }else{
    sample_points <- st_sample(study_area, n_sample_points, type = dist_sample_points)
    sample_points <- st_sf(geom=sample_points)
  }
  return(sample_points)
}

#' @author Thalis Goldschmidt
#' Generation of nlms
#' @description 
#' This function generates nlms from a list of passed neutral landscape models.
#' @param nlms List of strings. Contains all the names of the nlms to be generated.
#' @return A stack of the generated nlms.
#' @examples
#' generate_nlms(c("distance_gradient", "edge_gradient"))
generate_nlms <- function(nlms, seed){
  temp <- stack()
  for (i in 1:length(nlms)) {
    if (nlms[i] %in% c("distance_gradient")){
      distance_gradient <- nlm_distancegradient(ncol = 100, nrow = 100,
                                                origin = c(40, 40, 40, 40))
      temp$distance_gradient <- distance_gradient
    }
    else if(nlms[i] %in% c("edge_gradient")){
      edge_gradient <- nlm_edgegradient(ncol = 100, nrow = 100, direction = 25)
      temp$edge_gradient <- edge_gradient
    }
    else if(nlms[i] %in% c("fractional_brownian_motion_05")){
      if (!is.null(seed)){
        set.seed(seed)
      }
      fractional_brownian_motion_05  <- nlm_fbm(ncol = 100, nrow = 100, fract_dim = 0.1)
      temp$fractional_brownian_motion_05 <- fractional_brownian_motion_05
    }
    else if(nlms[i] %in% c("fractional_brownian_motion_10")){
      if (!is.null(seed)){
        set.seed(seed+1)
      }
      fractional_brownian_motion_10  <- nlm_fbm(ncol = 100, nrow = 100, fract_dim = 0.2)
      temp$fractional_brownian_motion_10 <- fractional_brownian_motion_10
    }
    else if(nlms[i] %in% c("fractional_brownian_motion_20")){
      if (!is.null(seed)){
        set.seed(seed+2)
      }
      fractional_brownian_motion_20  <- nlm_fbm(ncol = 100, nrow = 100, fract_dim = 0.4)
      temp$fractional_brownian_motion_20 <- fractional_brownian_motion_20
    }
    else if(nlms[i] %in% c("fractional_brownian_motion_30")){
      if (!is.null(seed)){
        set.seed(seed+3)
      }
      fractional_brownian_motion_30  <- nlm_fbm(ncol = 100, nrow = 100, fract_dim = 0.6)
      temp$fractional_brownian_motion_30 <- fractional_brownian_motion_30
    }
    else if(nlms[i] %in% c("fractional_brownian_motion_40")){
      if (!is.null(seed)){
        set.seed(seed+4)
      }
      fractional_brownian_motion_40  <- nlm_fbm(ncol = 100, nrow = 100, fract_dim = 0.8)
      temp$fractional_brownian_motion_40 <- fractional_brownian_motion_40
    }
    else if(nlms[i] %in% c("fractional_brownian_motion_50")){
      if (!is.null(seed)){
        set.seed(seed+5)
      }
      fractional_brownian_motion_50  <- nlm_fbm(ncol = 100, nrow = 100, fract_dim = 1)
      temp$fractional_brownian_motion_50 <- fractional_brownian_motion_50
    }
    else if(nlms[i] %in% c("fractional_brownian_motion_60")){
      if (!is.null(seed)){
        set.seed(seed+6)
      }
      fractional_brownian_motion_60  <- nlm_fbm(ncol = 100, nrow = 100, fract_dim = 1.2)
      temp$fractional_brownian_motion_60 <- fractional_brownian_motion_60
    }
    else if(nlms[i] %in% c("fractional_brownian_motion_70")){
      if (!is.null(seed)){
        set.seed(seed+7)
      }
      fractional_brownian_motion_70  <- nlm_fbm(ncol = 100, nrow = 100, fract_dim = 1.4)
      temp$fractional_brownian_motion_70 <- fractional_brownian_motion_70
    }
    else if(nlms[i] %in% c("gaussian_random_field_05")){
      if (!is.null(seed)){
        set.seed(seed)
      }
      gaussian_random_field_05 <- nlm_gaussianfield(ncol = 100, nrow = 100,
                                                 autocorr_range = 5,
                                                 mag_var = 50,
                                                 nug = 2)
      temp$gaussian_random_field_05 <- gaussian_random_field_05
    }
    else if(nlms[i] %in% c("gaussian_random_field_10")){
      if (!is.null(seed)){
        set.seed(seed+1)
      }
      gaussian_random_field_10 <- nlm_gaussianfield(ncol = 100, nrow = 100,
                                                 autocorr_range = 10,
                                                 mag_var = 50,
                                                 nug = 2)
      temp$gaussian_random_field_10 <- gaussian_random_field_10
    }
    else if(nlms[i] %in% c("gaussian_random_field_20")){
      if (!is.null(seed)){
        set.seed(seed+2)
      }
      gaussian_random_field_20 <- nlm_gaussianfield(ncol = 100, nrow = 100,
                                                 autocorr_range = 20,
                                                 mag_var = 50,
                                                 nug = 2)
      temp$gaussian_random_field_20 <- gaussian_random_field_20
    }
    else if(nlms[i] %in% c("gaussian_random_field_30")){
      if (!is.null(seed)){
        set.seed(seed+3)
      }
      gaussian_random_field_30 <- nlm_gaussianfield(ncol = 100, nrow = 100,
                                                 autocorr_range = 30,
                                                 mag_var = 50,
                                                 nug = 2)
      temp$gaussian_random_field_30 <- gaussian_random_field_30
    }
    else if(nlms[i] %in% c("gaussian_random_field_40")){
      if (!is.null(seed)){
        set.seed(seed+4)
      }
      gaussian_random_field_40 <- nlm_gaussianfield(ncol = 100, nrow = 100,
                                                 autocorr_range = 40,
                                                 mag_var = 50,
                                                 nug = 2)
      temp$gaussian_random_field_40 <- gaussian_random_field_40
    }
    else if(nlms[i] %in% c("polygonal_landscapes")){
      if (!is.null(seed)){
        set.seed(seed)
      }
      polygonal_landscapes <- nlm_mosaictess(ncol = 100, nrow = 100, germs = 50)
      temp$polygonal_landscapes <- polygonal_landscapes
    }
    else if(nlms[i] %in% c("random_neighbourhood")){
      if (!is.null(seed)){
        set.seed(seed)
      }
      random_neighbourhood <- nlm_neigh(ncol = 100, nrow = 100, p_neigh = 0.75,
                                p_empty = 0.1, categories = 5, neighbourhood = 8)
      temp$random_neighbourhood <- random_neighbourhood
    }
    else if(nlms[i] %in% c("planar_gradient")){
      if (!is.null(seed)){
        set.seed(seed)
      }
      planar_gradient <- nlm_planargradient(ncol = 100, nrow = 100)
      temp$planar_gradient <- planar_gradient
    }
    else if(nlms[i] %in% c("random")){
      if (!is.null(seed)){
        set.seed(seed)
      }
      random <- nlm_random(ncol = 100, nrow = 100)
      temp$random <- random
    }
    else if(nlms[i] %in% c("random_rectangular_cluster")){
      if (!is.null(seed)){
        set.seed(seed)
      }
      random_rectangular_cluster <- nlm_randomrectangularcluster(ncol = 100,
                                                                 nrow = 100,
                                                                 minl = 5,
                                                                 maxl = 10)
      temp$random_rectangular_cluster <- random_rectangular_cluster
    }
  }
  return(temp)
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


#' @author Thalis Goldschmidt
#' Function that trains a model
#' @description 
#' This function trains a model.
#' @param algorithm String. Algorithm to be used during training (random forest or svm).
#' @param cv_method String. Name of the cv method to be used to evaluate the models performance. Various settings must be made for the folds.
#' @param training_data data frame. Data frame containing information about all predictors and the target variable on the locations of the sample points.
#' @param predictors RasterStack. Needed to extract the names of the predictors included in the training data.
#' @param variable_selection String. Describes whether and which variable selection is to be carried out (None, FFS or RFE)
#' @param nndm_loo_cv_folds List of numerics. Folds to be used if nndm loo cv is selected as cv method.
#' @return A trained model.
#' @examples
#' distance_gradient_normalized <- normalized(distance_gradient)
train_model <- function(algorithm, cv_method, training_data, predictors, variable_selection, nndm_loo_cv_folds) {
  names_predictors <- names(predictors)
  # Create train control depending on cv method
  if (cv_method == "random_10_fold_cv"){
    ctrl <- trainControl(method="cv", number = 10, savePredictions = TRUE)
    rfeCtrl <- rfeControl(method="cv", number = 10, functions = rfFuncs)
  }
  else if(cv_method == "loo_cv"){
    ctrl <- trainControl(method="cv", number = length(training_data[[1]]), savePredictions = TRUE)
    rfeCtrl <- rfeControl(method="cv", number = length(training_data[[1]]), functions = rfFuncs)
  }
  else if(cv_method == "sb_cv"){
    indices <- CreateSpacetimeFolds(training_data,spacevar = "ID",k=length(unique(training_data$ID)))
    ctrl <- trainControl(method="cv", index = indices$index, savePredictions = TRUE)
    rfeCtrl <- rfeControl(method="cv", index = indices$index, functions = rfFuncs)
  }
  else if(cv_method == "nndm_loo_cv"){
    NNDM_indices <- nndm_loo_cv_folds
    ctrl <- trainControl(method = "cv", savePredictions = T, index=NNDM_indices$indx_train, indexOut=NNDM_indices$indx_test)
    rfeCtrl <- rfeControl(method = "cv", functions = rfFuncs, index=NNDM_indices$indx_train, indexOut=NNDM_indices$indx_test)
  }
  # Train model depending on the algorithm
  if (variable_selection == "None" & algorithm == "rf"){
    model <- train(training_data[,names_predictors],
                   training_data$target_variable,
                   tuneGrid=data.frame("mtry"=2),
                   metric = "RMSE",
                   method = algorithm,
                   importance = TRUE,
                   ntree = 100,
                   trControl=ctrl)
  }
  else if (variable_selection == "None" & algorithm == "svmRadial"){
    model <- train(training_data[,names_predictors],
                   training_data$target_variable,
                   tuneGrid=data.frame("sigma" = 0.5, "C" = 1),
                   metric = "RMSE",
                   method = algorithm,
                   importance = TRUE,
                   ntree = 100,
                   trControl=ctrl)
  }
  # If random forest is selected, variable selection can be carried out
  else if (variable_selection == "FFS" & algorithm == "rf"){
    model <- ffs(predictors = training_data[,names_predictors],
                       response = training_data$target_variable,
                       tuneGrid=data.frame("mtry"=2),
                       metric = "RMSE",
                       method = algorithm,
                       importance = TRUE,
                       ntree = 100,
                       trControl=ctrl)
  }
  if (variable_selection == "RFE" & algorithm == "rf"){
    model <- rfe(training_data[,names_predictors],
                 training_data$target_variable,
                 # tuneGrid=data.frame("mtry"=2),
                 metric = "RMSE",
                 method = algorithm,
                 ntree = 100,
                 sizes = c(1:length(names_predictors)),
                 rfeControl= rfeCtrl)
  }
  return(model)
}

#' @author Thalis Goldschmidt
#' Function to find out if all values of a list are equal
#' @description 
#' This function finds out if all values of a list are the same.
#' @param x List.
#' @return A Boolean expression.
allSame <- function(x) length(unique(x)) == 1

# Load data --------------------------------------------------------------------
# Global variables that are neededin several places
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

# Creating coordinate points to include them in the surface_data
coord_points <- point_grid
coord_points$x <- st_coordinates(coord_points)[,1]
coord_points$y <- st_coordinates(coord_points)[,2]
coord_stack <- rasterise_and_stack(coord_points, 
                                   which(names(coord_points)%in%c("x","y")), 
                                   c("coord1", "coord2"))
