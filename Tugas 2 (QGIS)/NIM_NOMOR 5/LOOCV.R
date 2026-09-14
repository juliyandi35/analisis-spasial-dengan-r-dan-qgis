library(gstat)
library(sp)
library(sf)
library(raster)
library(tidyverse)
library(mapview)

sample <- read.csv("sebaran penyakit 2.csv")
str(sample)


library(caret)
set.seed(123)
train_index <- createDataPartition(y = sample$nilai, p = 0.85, list = FALSE)
data_train <- sample[train_index, ]
coordinates(data_train) <- c("long","lat")
crs <- CRS("+proj=longlat +datum=WGS84")
proj4string(data_train) <- crs
data_train <- spTransform(data_train, CRS("+proj=utm +zone=47 + south +datum=WGS84"))

data_test <- sample[-train_index, ]
coordinates(data_test) <- c("long","lat")
proj4string(data_test) <- crs
data_test <- spTransform(data_test, CRS("+proj=utm +zone=47 + south +datum=WGS84"))

map <- read_sf("Padang dan Pesisir Selatan.shp")
map <- st_zm(map,"ZM")
map <- as(map,"Spatial")
map <- spTransform(map, CRS("+proj=utm +zone=47 + south +datum=WGS84"))

# set parameter
neighbors <- length(data_train) - 1
beta <- 1

# build model
idw <- gstat(
  formula = nilai ~ 1, # intercept-only model
  data = data_train,
  nmax = neighbors,
  set = list(idp = beta)
)

crossval <- gstat.cv(idw)

MAPE <- function(actual,residuals) {mean(abs(residuals/actual)*100)}
MSE <- function(residuals) {sum((residuals)^2) / length(residuals)}
RMSE <- function(residuals) {sqrt(sum((residuals)^2) / length(residuals))}

crossval_mape <- MAPE(crossval$observed,crossval$residual)
crossval_mse <- MSE(crossval$residual)
crossval_rmse <- RMSE(crossval$residual)
crossval_GoF <- data.frame(MAPE=crossval_mape,MSE=crossval_mse,RMSE=crossval_rmse)
crossval_GoF 

cv_IDW <- function(spatialDF, stat.formula = NULL,
                   seqNeighbors = NULL, seqBeta = NULL,
                   evalGridSize = NULL,
                   evalRaster = NULL,
                   verbose = TRUE) {
  
  ### LOAD LIBRARIES ###
  library(sp)
  library(sf)
  library(gstat)
  library(raster)
  
  ### PROVIDE DEFAULT VALUES FOR FUNCTION ARGUMENTS ###
  if (is.null(seqNeighbors)) {
    seqNeighbors <- round(seq(3, length(spatialDF), length.out = 5))
  }
  if (is.null(seqBeta)) {
    seqBeta <- c(0.1, seq(0.5, 3, 0.5))
  }
  if (is.null(evalGridSize)) {
    x.interval <- extent(spatialDF)@xmax - extent(spatialDF)@xmin
    y.interval <- extent(spatialDF)@ymax - extent(spatialDF)@ymin
    evalGridSize <- round(min(x.interval, y.interval) * 0.05)
  }
  if (is.null(stat.formula)) {
    print("Please provide a formula!!")
    return()
  }
  if (is.null(evalRaster)) {
    extent.evalGrid <- extent(spatialDF)
  } else {
    extent.evalGrid <- extent(evalRaster)
  }
  
  ### BUILD A GRID FOR PARAMETER COMBINATIONS ###
  cv_Grid <- expand.grid(
    Beta = seqBeta,
    Neighbors = seqNeighbors
  )
  cv_Grid$RMSE <- NA
  cv_Grid$MSE <- NA
  cv_Grid$MAPE <- NA
  
  ### LOOP THROUGH ALL PARAMETER COMBINATIONS ###
  for (i in 1:nrow(cv_Grid)) {
    ### BUILD IDW MODEL ###
    idw <- gstat(
      formula = stat.formula,
      data = spatialDF,
      nmax = cv_Grid[i, "Neighbors"],
      set = list(idp = cv_Grid[i, "Beta"])
    )
    
    ### PERFORM LOOCV ###
    crossval <- gstat.cv(idw,
                         nmax = cv_Grid[i, "Neighbors"],
                         beta = v.Grid[i, "Beta"],
                         debug.level = 0
    )
    cv_Grid[i, "RMSE"] <- RMSE(residuals = crossval$residual)
    cv_Grid[i, "MSE"] <- MSE(residuals = crossval$residual)
    cv_Grid[i, "MAPE"] <- MAPE(actual = crossval$observed, residuals = crossval$residual)
    if (verbose) {
      print(paste("Function call", i, "out of", nrow(cv_Grid)))
      print(paste(
        "Evaluating beta =",
        cv_Grid[i, "Beta"],
        "and neighbors =",
        cv_Grid[i, "Neighbors"]
      ))
      print(paste("RMSE =", RMSE(residuals = crossval$residual)))
      print(paste("MSE =", MSE(residuals = crossval$residual)))
      print(paste("MAPE =", MAPE(actual = crossval$observed,residuals = crossval$residual)))
    }
  }
  
  ### GET BEST PARAMTER VALUES ###
  idx_min <- which.min(cv_Grid$MAPE)
  best_Beta <- cv_Grid$Beta[idx_min]
  best_Neighbors <- cv_Grid$Neighbors[idx_min]
  min_MAPE <- cv_Grid$MAPE[idx_min]
  
  ### BUILD IDW MODEL BASED ON BEST PARAMTER VALUES ###
  idw_best <- gstat(
    formula = stat.formula,
    data = spatialDF,
    nmax = best_Neighbors,
    set = list(idp = best_Beta)
  )
  
  ### PREPARE EVALUATION GRID ###
  grid_evalGrid <- expand.grid(
    x = seq(
      from = round(extent.evalGrid@xmin),
      to = round(extent.evalGrid@xmax),
      by = evalGridSize
    ),
    y = seq(
      from = round(extent.evalGrid@ymin),
      to = round(extent.evalGrid@ymax),
      by = evalGridSize
    )
  )
  
  coordinates(grid_evalGrid) <- ~ x + y
  proj4string(grid_evalGrid) <- proj4string(spatialDF)
  gridded(grid_evalGrid) <- TRUE
  
  ### INTERPOLATE VALUES FOR EVALUATION GRID USING THE BEST MODEL ###
  idw_best_predict <- predict(
    object = idw_best,
    newdata = grid_evalGrid,
    debug.level = 0
  )
  
  ### RETURN RESULTS AND OBJECTS ###
  return(list(
    "idwBestModel" = idw_best,
    "idwBestRaster" = idw_best_predict,
    "bestBeta" = best_Beta,
    "bestNeighbors" = best_Neighbors,
    "bestMAPE" = min_MAPE,
    "gridCV" = cv_Grid
  ))
}

my_IDW <- cv_IDW(
  spatialDF = data_train,
  stat.formula = formula(nilai ~ 1),
  evalGridSize=10000,
  evalRaster = map,
  verbose = TRUE
)
CV_result <- data.frame(my_IDW$gridCV)
CV_result
write.csv(CV_result,"CV Result.csv")

my_IDW[["bestBeta"]]
my_IDW[["bestNeighbors"]]
my_IDW[["bestMAPE"]]

# Best Training Model
idw_best <- gstat(
  formula = nilai ~ 1, # intercept-only model
  data = data_train,
  nmax = my_IDW[["bestNeighbors"]],
  set = list(idp = my_IDW[["bestBeta"]])
)

# GoF train
Actual_train <- crossval_best$observed
Pred_train <- crossval_best$var1.pred
Train_compare <- data.frame(Actual = Actual_train,
                           Prediction = Pred_train)
write.csv(Train_compare,"Train Compare.csv")

Pred_sd <- sd(Pred_train)
Pred_se <- Pred_sd / sqrt(length(Pred_train))
z <- qnorm(0.975)
lower_bound <- mean(Pred_train) - z * Pred_se
upper_bound <- mean(Pred_train) + z * Pred_se
Pred_CI <- c(lower_bound,upper_bound)
Train_result <- data.frame(SD=Pred_sd,SE=Pred_se,CI=Pred_CI)
write.csv(Train_result,"Train Result.csv")

# Test Model
extent.evalGrid <- extent(map)
grid_evalGrid <- expand.grid(
  x = seq(
    from = round(extent.evalGrid@xmin),
    to = round(extent.evalGrid@xmax),
    by = 10000
  ),
  y = seq(
    from = round(extent.evalGrid@ymin),
    to = round(extent.evalGrid@ymax),
    by = 10000
  )
)
coordinates(grid_evalGrid) <- ~ x + y
proj4string(grid_evalGrid) <- proj4string(data_train)
gridded(grid_evalGrid) <- TRUE

idw_test <- gstat(
  formula = nilai ~ 1, # intercept-only model
  data = data_test,
  nmax = my_IDW[["bestNeighbors"]],
  set = list(idp = my_IDW[["bestBeta"]])
)
crossval_test <- gstat.cv(idw_test)
cvtest_mape <- MAPE(crossval_test$observed,crossval_test$residual)
cvtest_mape

idw_test_predict <- predict(
  object = idw_test,
  newdata = grid_evalGrid,
  debug.level = 0
)

# GoF test
Actual_test <- crossval_test$observed
Pred_test <- crossval_test$var1.pred
Test_compare <- data.frame(Actual = Actual_test,
                           Prediction = Pred_test)
write.csv(Test_compare,"Test Compare.csv")

Pred_sd <- sd(Pred_test)
Pred_se <- Pred_sd / sqrt(length(Pred_test))
z <- qnorm(0.975)
lower_bound <- mean(Pred_test) - z * Pred_se
upper_bound <- mean(Pred_test) + z * Pred_se
Pred_CI <- c(lower_bound,upper_bound)
Test_result <- data.frame(SD=Pred_sd,SE=Pred_se,CI=Pred_CI)
write.csv(Test_result,"Test Result.csv")

# Visualize
Train_Interpolation <- plot(my_IDW[["idwBestRaster"]])
Test_Interpolation <- plot(idw_test_predict)