library("NNDM")
library("caret")
library("sp")
library("sf")
library("knitr")
library("gstat")
library("gridExtra")

# Sample data
data("meuse")
meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, remove = F)

# AOI polygon
data("meuse.area")
meuse.area <- SpatialPolygons(list(Polygons(list(Polygon(meuse.area)), "area")))
meuse.area <- st_as_sf(meuse.area)
st_crs(meuse.area) <- 28992

# Prediction grid
data("meuse.grid")
meuse.grid <- st_as_sf(meuse.grid, coords = c("x", "y"), crs = 28992, remove = F)

# Fit model
trainControl_LOO <- trainControl(method = "LOOCV", savePredictions = T)
paramGrid <-  data.frame(mtry = 2, min.node.size = 5, splitrule = "variance")
mod_LOO <- train(zinc ~ x + y + dist + ffreq + soil,
                 method = "ranger",
                 trControl = trainControl_LOO,
                 tuneGrid = paramGrid, 
                 data = meuse, 
                 seed=12345)

# Estimate variogram on the outcome and return range
empvar <- variogram(zinc~1, data = meuse)
fitvar <- fit.variogram(empvar, vgm(model="Sph", nugget = T), fit.sills = TRUE)

(outrange <- fitvar$range[2]) # Outcome autocorrelation range in m
#> [1] 831.0127
plot(empvar, fitvar, cutoff=1500, main = "Outcome semi-variogram estimation")
# Estimate variogram on the residual and return range
meuse$res <- meuse$zinc - predict(mod_LOO, newdata=meuse)
empvar <- variogram(res~1, data = meuse)
fitvar <- fit.variogram(empvar, vgm(model="Sph", nugget = T))
plot(empvar, fitvar, cutoff=1500, main = "Residual semi-variogram estimation")

(resrange <- fitvar$range[2]) # Residual autocorrelation range in m
#> [1] 842.0515

# Compute bLOO indices
(bLOO_indices <- bLOO(meuse, resrange, min_train = 0.5))
#> bLOO object
#> Total number of points: 155
#> Mean number of training points: 110.01
#> Minimum number of training points: 92
#> Mean buffer radius: 842.05
# Plot for one CV iteration
plot(bLOO_indices, 53) +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

# Evaluate RF model using bLOO CV
trainControl_bLOO <- trainControl(method = "cv", savePredictions = T,
                                  index=bLOO_indices$indx_train,
                                  indexOut=bLOO_indices$indx_test)
mod_bLOO <- train(zinc ~ x + y + dist + ffreq + soil,
                  method = "ranger",
                  trControl = trainControl_bLOO,
                  tuneGrid = paramGrid, 
                  data = meuse, 
                  seed=12345)

# Compute NNDM indices
(NNDM_indices <- nndm(meuse, meuse.grid, outrange, min_train = 0.5))
#> nndm object
#> Total number of points: 155
#> Mean number of training points: 153.88
#> Minimum number of training points: 150
# Plot NNDM functions
plot(NNDM_indices)

# Evaluate RF model using NDM CV
trainControl_NNDM <- trainControl(method = "cv", savePredictions = T,
                                  index=NNDM_indices$indx_train,
                                  indexOut=NNDM_indices$indx_test)
mod_NNDM <- train(zinc ~ x + y + dist + ffreq + soil,
                  method = "ranger",
                  trControl = trainControl_NNDM,
                  tuneGrid = paramGrid, 
                  data = meuse, 
                  seed=12345)

library(devtools) # oder remotes

install_github("HannaMeyer/CAST")
install_github("carlesmila/NNDM", force = TRUE)