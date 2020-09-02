library(raster)
library(rgdal) 
library(ggplot2) 
library(plotly)
library(readr) 
library(dplyr)
library(reshape2)
library(shinyjs)
library(rasterVis)
library(rasclass)
library(h2o) ## A.I. library
library(rminer)
 
## read file
swir.file<-"/archivio/home/pirotti/Google Drive/RAD_3451-1_SWIR_384me_SN3155_22000us_2020-02-11T144756_raw_rad.tif"
myImages.SWIR<-raster::brick(swir.file)

swir.bands.wavelengths<-readr::parse_number(names(myImages.SWIR)) 

## training info
train.table <-  readRDS("/archivio/home/pirotti/tmp/train.table.rds")
head(train.table)

## nice empty matrix with reflectance/radiance data... 
## each ROW is a training point, and each COLUMN is a value of the image pixel
ss <-
  matrix(
    data = NA,
    nrow = nrow( train.table),
    ncol = length(swir.bands.wavelengths)
  )


for (i in 1:nrow( train.table)) {
  print("Reading  spect. signature @ point...")
  print(i)
  ss[i,] <-
    as.numeric(
      gdalUtils::gdallocationinfo(
        swir.file,
         train.table[i, "x"]  ,
         train.table[i, "y"],
         valonly = T
      )
    )
}


## here we create a table from the matrix with signatures above adding the "Class" column
train.data <-  as.data.frame(cbind(train.table[c("Class")], ss))
## here we make sure that "Class" is a "FACTOR" column...
train.data$Class <- as.factor(train.data$Class)

## this below is important... the training data must have the SAME column names 
## as names in image! Otherwise we can train but not predict
names(train.data) <- c("Class", names(myImages.SWIR))


############ START MODELING WITH H2O  ##############

localH2o <- h2o.init(nthreads = 10, max_mem_size = "50G")

 
train <-  as.h2o(train.data)
grid <-  as.h2o(as.data.frame(myImages.SWIR ))

y <- "Class"
x <- setdiff(names(train.data), y)
models <- list()


models[["RF"]] <- h2o.randomForest(
  x = x,
  y = y,
  ntrees = 10,
  max_depth = 5,
  min_rows = 10,
  binomial_double_trees = TRUE,
  training_frame = train
)
 
models[["NB"]]  <- h2o.naiveBayes(
  x = x,
  y = y,
  training_frame = train,
  laplace = 0,
  nfolds = 3,
  seed = 1234
) 

models[["GMB"]]  <- h2o.gbm(
  x = x,
  y = y,
  training_frame = train,
  ntrees = 10,
  max_depth = 3,
  min_rows = 2,
  learn_rate = 0.2,
  nfolds = 3,
  keep_cross_validation_predictions = TRUE,
  seed = 1
)
### Deep Learning Model 
models[["DL"]]  <- h2o.deeplearning(
  model_id = "Deep_Learning",
  # Destination id for this model
  training_frame = train,
  # Id of the training data frame
  #  validation_frame=valid,                    # Id of the validation data frame
  x = x,
  # a vector predictor variable
  y = y,
  # name of reponse vaiables
  standardize = TRUE,
  # standardize the data
  score_training_samples = 0,
  # training set samples for scoring (0 for all)
  activation = "RectifierWithDropout",
  # Activation function
  score_each_iteration = TRUE,
  hidden = c(200, 200, 200, 200),
  # 4 hidden layers, each of 200 neurons
  hidden_dropout_ratios = c(0.2, 0.1, 0.1, 0),
  # for improve generalization
  stopping_tolerance = 0.001,
  # tolerance for metric-based stopping criterion
  epochs = 100,
  # the dataset should be iterated (streamed)
  adaptive_rate = TRUE,
  # manually tuned learning rate
  l1 = 1e-6,
  # L1/L2 regularization, improve generalization
  l2 = 1e-6,
  max_w2 = 10,
  # helps stability for Rectifier
  nfolds = 3,
  # Number of folds for K-fold cross-validation
  fold_assignment = "Stratified",
  # Cross-validation fold assignment scheme
  keep_cross_validation_fold_assignment = TRUE,
  seed = 125,
  reproducible = TRUE,
  variable_importances = T
)

## empty container for predicted (classified) rasters
rasters<-list()
for (i in names(models)) {
  print("Predicting with...")
  print(i)
  ## save the model if you want
  ## h2o.saveModel(models[[i]], ....)
  
  ## Predict class at all pixels
  g.predict = as.data.frame(h2o.predict(object = models[[i]], newdata = grid))
  
  ## empty raster same size and resolution as original
  rasters[[i]] <-  raster(myImages.SWIR)
  
  ## here we make sure that all classes are factorized, as some results only have some of the
  ## classes and this messes up the Raster - see https://rdrr.io/cran/raster/man/factor.html
  rasters[[i]][]  <-    factor(g.predict$predict, levels = levels(train.data$Class))
}

#h2o.shutdown(prompt = F)
rb <- raster::brick(rasters)

levelplot(rb,  xlab = "X", ylab = "Y")

############ END  MODELING WITH H2O  ##############




############ START  MODELING WITH RMINER  ##############
rasters.rminer<-list()
models.rminer<-list()

models.rminer[["SVM"]] <- rminer::fit(Class~.,data=train.data, task="class",  model="ksvm", feature="s") 
rasters.rminer[["SVM"]] <- rminer::predict(  myImages.SWIR, models.rminer[["SVM"]] )


models.rminer[["RF"]] <- rminer::fit(Class~.,data=train.data, task="class",  model="randomForest") 
rasters.rminer[["RF"]] <- rminer::predict(  myImages.SWIR, models.rminer[["RF"]] )


models.rminer[["KNN"]] <- rminer::fit(Class~.,data=train.data, task="class",  model="knn") 
rasters.rminer[["KNN"]] <- rminer::predict(  myImages.SWIR, models.rminer[["KNN"]] )


models.rminer[["NAIVE"]] <- rminer::fit(Class~.,data=train.data, task="class",  model="naive") 
rasters.rminer[["NAIVE"]] <- rminer::predict(  myImages.SWIR, models.rminer[["NAIVE"]] )

rb.rminer <- raster::brick(rasters.rminer)
levelplot( rb.rminer,  xlab = "X", ylab = "Y")

# models[["rminerRPART"]] <- mining(Class~.,data=train.data,Runs=10,method=c("kfold",3,123),model="rpart")
# print(mmetric(M,metric="CONF"))
# print(mmetric(M,metric="AUC"))


############ END  MODELING WITH RMINER  ##############

