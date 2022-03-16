library(NLMR)
library(landscapetools)
library(ggplot2)
library(raster)
library(caret)
library(Metrics)
library(sf)
library(gstat)

# Test grids
# Create grids
# Testing all possible landscapes
################################################################################
# a)
# ?nlm_curds
# simulate random curdling
# (random_curdling <- nlm_curds(curds = c(0.5, 0.3, 0.6),
#                               recursion_steps = c(32, 6, 2)))
# 
# # simulate wheyed curdling
# (wheyed_curdling <- nlm_curds(curds = c(0.5, 0.3, 0.6),
#                               recursion_steps = c(32, 6, 2),
#                               wheyes = c(0.1, 0.05, 0.2)))
# plot(random_curdling)
# plot(wheyed_curdling)
# # Visualize the NLMs
# landscapetools::show_landscape(random_curdling)
# landscapetools::show_landscape(wheyed_curdling)

################################################################################
# b)
#?nlm_distancegradient
# simulate a distance gradient
distance_gradient <- nlm_distancegradient(ncol = 100, nrow = 100,
                                          origin = c(80, 10, 40, 5))

# visualize the NLM
# landscapetools::show_landscape(distance_gradient)

################################################################################
# c)
# simulate random curdling
edge_gradient <- nlm_edgegradient(ncol = 100, nrow = 100, direction = 30)

# visualize the NLM
# landscapetools::show_landscape(edge_gradient)

################################################################################
# d)
# simulate fractional brownian motion
#?nlm_fbm
set.seed(100)
fbm_raster  <- nlm_fbm(ncol = 100, nrow = 100, fract_dim = 0.2)

# visualize the NLM
# landscapetools::show_landscape(fbm_raster)

################################################################################
# e)
# simulate random gaussian field
#?nlm_gaussianfield
set.seed(100)
gaussian_field <- nlm_gaussianfield(ncol = 100, nrow = 100,
                                    autocorr_range = 100,
                                    mag_var = 8,
                                    nug = 5)

# visualize the NLM
# landscapetools::show_landscape(gaussian_field)

################################################################################
# f)
# simulate mosaic random field
# mosaic_field <- NLMR::nlm_mosaicfield(ncol = 100,
#                                 nrow = 100,
#                                 n = NA,
#                                 infinit = TRUE,
#                                 collect = FALSE)
# 
# # visualize the NLM
# landscapetools::show_landscape(mosaic_field)
#plot(mosaic_field)

################################################################################
# g)
# simulate neighborhood model
set.seed(100)
neigh_raster <- nlm_neigh(ncol = 100, nrow = 100, p_neigh = 0.75, p_empty = 0.1,
                          categories = 5, neighbourhood = 8)

# visualize the NLM
# landscapetools::show_landscape(neigh_raster)

################################################################################
# h)
# simulate percolation model
set.seed(100)
percolation <- nlm_percolation(ncol = 100, nrow = 100, prob = 0.5)

# visualize the NLM
# landscapetools::show_landscape(percolation)
# plot(percolation)
################################################################################
# i)
# simulate planar gradient
set.seed(100)
planar_gradient <- nlm_planargradient(ncol = 100, nrow = 100)

# visualize the NLM
# landscapetools::show_landscape(planar_gradient)

################################################################################
# j)
# simulate polygonal landscapes
# mosaicgibbs <- nlm_mosaicgibbs(ncol = 100,
#                                nrow = 100,
#                                germs = 20,
#                                R = 0.02,
#                                patch_classes = 12)
# 
# # visualize the NLM
# landscapetools::show_landscape(mosaicgibbs)

################################################################################
# k)
# simulate polygonal landscapes
set.seed(100)
mosaictess <- nlm_mosaictess(ncol = 100, nrow = 100, germs = 50)

# visualize the NLM
# landscapetools::show_landscape(mosaictess)

################################################################################
# l)
# simulate spatially random model
set.seed(100)
random <- nlm_random(ncol = 100, nrow = 100)

# visualize the NLM
# landscapetools::show_landscape(random)

################################################################################
# m)
# simulate random clustering
set.seed(100)
random_cluster <- nlm_randomcluster(ncol = 100, nrow = 100,
                                    p = 0.4,
                                    ai = c(0.25, 0.25, 0.5))

# visualize the NLM
# landscapetools::show_landscape(random_cluster)

################################################################################
# n)

# simulate midpoint displacement
# midpoint_displacement <- nlm_mpd(ncol = 102,
#                                  nrow = 102,
#                                  roughness = 0.7)
# 
# # visualize the NLM
# landscapetools::show_landscape(midpoint_displacement)
# midpoint_displacement
################################################################################
# o)
# simulate random rectangular cluster
set.seed(100)
randomrectangular_cluster <- nlm_randomrectangularcluster(ncol = 100,
                                                          nrow = 100,
                                                          minl = 5,
                                                          maxl = 10)
# visualize the NLM
# landscapetools::show_landscape(randomrectangular_cluster)

################################################################################
# create predictor stack
# Random noise
rnoise <- raster(ncols=dimgrid, nrows=dimgrid, xmn=0, xmx=dimgrid, ymn=0, ymx=dimgrid)
plot(rnoise)
vals <- rnorm(dimgrid*dimgrid, sd=0.5)
normalizeNum <- function(x){(x-min(x))/(max(x)-min(x))}
vals <- normalize(vals)
vals <- vals * 0.2
head(vals)
rnoise <- setValues(rnoise, vals)


# Spatially correlated noise
variog_mod <- vgm(model = "Sph", psill = 1, range = 40, nugget = 0)
gstat_mod <- gstat(formula = z~1, dummy = TRUE, beta = 0, model = variog_mod, nmax = 100) 
snoise <- predict(gstat_mod, point_grid, nsim = 1)
snoise <- rasterFromXYZ(cbind(st_coordinates(snoise), 
                              as.matrix(as.data.frame(snoise)[,1], ncol=1)))

plot(rnoise)
show_landscape(snoise)
# Create grids
dimgrid <- 100
rast_grid <- raster(ncols=dimgrid, nrows=dimgrid, xmn=0, xmx=dimgrid, ymn=0, ymx=dimgrid)
point_grid <- st_as_sf(rasterToPoints(rast_grid, spatial = TRUE))
n_train <- 50

# Extract information from stack in a data frame on the sample points
extr <- as.data.frame(raster::extract(predictors, train_points))
extr$geom <- train_points$geom
head(extr)
# Create study area
study_area <- st_as_sf(as(extent(rast_grid), "SpatialPolygons"))

# Defining stack of predictors
predictors <- stack(distance_gradient, edge_gradient, fbm_raster, 
                    gaussian_field, mosaictess, neigh_raster, 
                    planar_gradient, random, random_cluster,
                    randomrectangular_cluster)

# names(predictors) <- c("distance_gradient", "edge_gradient", "fbm_raster", 
#                       "gaussian_field", "mosaictess", "neigh_raster", 
#                       "planar_gradient", "random", "random_cluster",
#                       "randomrectangular_cluster")

# Changing the name of the predictors
names(predictors) <- c("pred_1", "pred_2", "pred_3", 
                       "pred_4", "pred_5", "pred_6", 
                       "pred_7", "pred_8", "pred_9",
                       "pred_10")

show_landscape(predictors)
train_data <- as.data.frame(raster::extract(predictors, train_points))
head(train_data)
pred <- names(c("pred_1", "pred_2"))
train_data[,pred]


# Computing the output with a random function
out_true <- predictors$pred_1*predictors$pred_2 + predictors$pred_10^2 - 
  predictors$pred_3*predictors$pred_7 - predictors$pred_5*predictors$pred_4^4 +
  predictors$pred_9*predictors$pred_5 + predictors$pred_6 -
  predictors$pred_8^2 + predictors$pred_3*predictors$pred_2

# Add some noise (later)
outcome <- out_true
names(outcome) <- "outcome"

show_landscape(outcome)

# Generating training points (depending on user input)
n_train = 50
# Training points: Regular
train_reg <- st_sample(study_area, n_train, type="regular")
train_reg <- st_sf(geom=train_reg)
ggplot() +
  geom_sf(data = train_reg, size = 1) +
  geom_sf(data = study_area,  alpha = 0) +
  theme_bw()

# Training points: Random
train_rand <-  st_sample(study_area, n_train, type="random")
train_rand <- st_sf(geom=train_rand)
ggplot() +
  geom_sf(data = train_rand, size = 1) +
  geom_sf(data = study_area,  alpha = 0) +
  theme_bw()

# Training points: Clust1
train_points <- clustered_sample(study_area, n_train/5, n_train*4/5, dimgrid*0.05)
train_points <- st_sf(geom=train_points)
ggplot() +
  geom_sf(data = train_points, size = 1) +
  geom_sf(data = study_area,  alpha = 0) +
  theme_bw()

# Training points: Clust2
train_clust2 <- clustered_sample(study_area, n_train/10, n_train*9/10, dimgrid*0.05)
train_clust2 <- st_sf(geom=train_clust2)
ggplot() +
  geom_sf(data = train_clust2, size = 1) +
  geom_sf(data = study_area,  alpha = 0) +
  theme_bw()

# Training points: Non-uniform
nonuniform_areas <- nonuniform_sampling_polys(dgrid=dimgrid)
train_nonunif <- st_sample(filter(nonuniform_areas, sample=="Yes"), n_train, type = "random")
train_nonunif <- st_sf(geom=train_nonunif)
ggplot() +
  geom_sf(data = train_nonunif, size = 1) +
  geom_sf(data = study_area,  alpha = 0) +
  theme_bw()

# "random"
set.seed(100)
train_points <- st_sample(study_area, 50, type = "random")
train_points <- st_sf(geom=train_points)
train_points

ggplot() +
  geom_sf(data = train_points, size = 1) +
  geom_sf(data = study_area,  alpha = 0) +
  theme_bw()

# Creating coordinate points to include them in the surface_data
coord_points <- point_grid
coord_points$x <- st_coordinates(coord_points)[,1]
coord_points$y <- st_coordinates(coord_points)[,2]
coord_stack <- rasterise_and_stack(coord_points, 
                                   which(names(coord_points)%in%c("x","y")), 
                                   c("coord1", "coord2"))

# Stack all predictors
all_stack <- stack(outcome, predictors)

# Extracting all necessary information to create the training data
train_data <- as.data.frame(raster::extract(all_stack, train_points))
vnames <- names(train_data)

# Adding the coord_stack to visualize it later
all_stack <- stack(outcome, predictors, coord_stack)

# Grid to predict surface, extract inner/outer grid indicator
surf_data <- as.data.frame(raster::extract(all_stack, point_grid))
surf_data$area <- point_grid$areant_grid$area

# Create default model
model_default <- train(train_data,
                       train_data$outcome,
                       method = "rf",
                       importance = TRUE,
                       ntree = 500)

model_default
prediction_default <- predict(all_stack, model_default)
show_landscape(prediction_default)
show_landscape(outcome)
dif_default <- outcome - prediction_default
show_landscape(dif_default)

prediction_default_abs <- abs(prediction_default)
MAE_default <- sum(raster::extract(prediction_default_abs, point_grid))/10000


# Model with training with random cross validation (nicht sinnvoll für Daten mit räumlichen Abhängigkeiten)
model_rcv <- train(train_data,
                   train_data$outcome,
                   method = "rf",
                   importance = TRUE,
                   ntree = 500,
                   trControl=trainControl(method="cv",number=3))

model_rcv
prediction_rcv <- predict(all_stack, model_rcv)
show_landscape(prediction_rcv)
dif_rcv <- outcome- prediction_rcv
show_landscape(dif_rcv)

prediction_rcv_abs <- abs(prediction_rcv)
MAE_rcv <- sum(raster::extract(prediction_rcv_abs, point_grid))/10000

# Difference between simulated outcome and prediction
dif_rcv <- prediction_rcv - outcome
show_landscape(dif_rcv)

# Surface figs
ggplot(surf_data) +
  geom_raster(aes(x = coord1, y = coord2, fill = outcome)) +
  geom_sf(data=train_points, alpha=0.7) +
  scale_fill_scico("Simulated\noutcome", palette = 'roma') +
  xlab("") + ylab("") + 
  theme_bw() + theme(legend.position = "bottom")


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
