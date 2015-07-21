##  Function "ModelTr"
##  version: dv2
##  date: 12-9-2014
##  edit: 12-16-2014
##  edit: 01-28-2014
##  author: Diana
##  description : this function works almost the same as the "train" function
##  in the caret package, while adding PSS as a metric, also, can tune threshold
##  apllied SMOTE method
##  Arguments:
##            x: data frame that contains all the predictors
##            y: a factor vector containing the outcome for each sample
##            method: a string specifying which classification model to use
##            preProcess: a string vector that defines an pre-processing 
##            of the predictor data. Current possibilities are "BoxCox", 
##           "YeoJohnson", "expoTrans", "center", "scale", "range", 
##           "knnImpute", "bagImpute", "medianImpute", "pca", "ica" and 
##           "spatialSign". The default is no pre-processing. 
##            metric: a string that specifies what summary metric will 
##            be used to select the optimal model
##            maximize: a logical defines whether the metric be maximize or minimize
##            trControl:a list of values that define how this function acts.
##            tuneGrid: a data frame with the possible tuning values
##            tuneLength: an interger denoting the number of levels for each tuning parameters
##            SMOTE: a logical defines whether applies SMOTE method on the data
##            the dots: any parameters that can be passed to the train function
##                      but should be the same name as they are in the original function.
##  value:    same as the values returned by "train" function in caret package
##  depends: "caret","DMwR"
ModelTr <- function(x,y,method,
                    preProcess=NULL,
                    metric="ROC",
                    maximize=TRUE,
                    trControl=trainControl(),
                    tuneGrid=NULL,
                    tuneLength=3,
                    SMOTE=FALSE,...){
  library(caret)
  library(DMwR)
  
  ## update the metric
  if (metric=="PSS"){
    metric <- "PSS.Sens"
    maximize=TRUE
  }
  
  ## check training method
  if (method %in% c("thresh_rf","thresh_nnet","thresh_glm","thresh_knn","thresh_nb","thresh_lda")){
    listname <- c("thresh_rf","thresh_nnet","thresh_glm","thresh_knn","thresh_nb","thresh_lda")
    index <- which(method == listname)
    method <- threshmod[[index]]
 }
  
  ## check whether train method 
  if (SMOTE){
    smethod <- getModelInfo(method,regex = FALSE)[[1]]
    fitfuc <- smethod$fit
    
smethod$fit <- function(x, y, wts, param, lev, last, classProbs, ...){
      library(DMwR)
      tmp <- as.data.frame(x)
      tmp$Class <- y
      
smoted <- SMOTE(Class ~ ., data = tmp)
      x = smoted[, names(smoted) != "Class"]
  
y = smoted$Class
      fitfuc(x=x,y=y,
             wts=wts,
             param=param,
             lev=lev,
             last=last,
             classProb=classProb,
             ...)
    }
    method <- smethod
      
  }
  
  ## train model
  library(caret)
  train(x=x,y=y,
      method=method,
      metric=metric,
      maximize=maximize,
      preProcess=preProcess,
      trControl=trControl,
      tuneGrid=tuneGrid,
      tuneLength=tuneLength,
      ...)
  
}