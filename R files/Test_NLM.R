library(NLMR)
library(landscapetools)
library(ggplot2)
library(raster)
library(caret)

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
                                          origin = c(20, 30, 10, 15))

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
# Create grids
dimgrid <- 100
rast_grid <- raster(ncols=dimgrid, nrows=dimgrid, xmn=0, xmx=dimgrid, ymn=0, ymx=dimgrid)
point_grid <- st_as_sf(rasterToPoints(rast_grid, spatial = TRUE))

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
show_landscape(outcome)
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
all_cntrl <- trainControl(method="none", savePredictions=TRUE)

# Create default model
model_default <- train(train_data,
                       train_data$outcome,
                       method = "rf",
                       importance = TRUE,
                       ntree = 500)

model_default
prediction_default <- predict(all_stack, model_default)
show_landscape(prediction_default)
dif_default <- prediction_default - outcome
show_landscape(dif_default)


# Model with training with random cross validation (nicht sinnvoll für Daten mit räumlichen Abhängigkeiten)
model_rcv_default <- train(train_data,
                   train_data$outcome,
                   method = "rf",
                   importance = TRUE,
                   ntree = 500,
                   trControl=trainControl(method="cv",number=3))

model_rcv
prediction_rcv <- predict(all_stack, model_rcv)
show_landscape(prediction_rcv)
dif_rcv <- prediction_rcv - outcome
show_landscape(dif_rcv)

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

