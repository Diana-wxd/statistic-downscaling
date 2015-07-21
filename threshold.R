#  this file contains elements of model methods that can be used to 
#  tune threshold as a parameter in the train function.
#  the advantage to use these method is that when we predict
#  using the final model, the model automatically applied the
#  best threshold it select during train step to predict step.


############################ thresh_nnet####################
library(caret)

## Get the model code from the original code for nnet
thresh_nnet <- getModelInfo("nnet",regex = FALSE)[[1]]

## set the type only for classification
thresh_nnet$type <- c("Classification")

## add the threshold as another tuing parameter
threshold <- data.frame(parameter = c("threshold"),
                        class = c("numeric"),
                        label = c("Probability Cutoff"))
thresh_nnet$parameters <- rbind(thresh_nnet$parameters,threshold)

## the default tuning grid
thresh_nnet$grid <- function (x, y, len = NULL) {
  expand.grid(size = ((1:len) * 2) - 1, 
              decay = c(0, 10^seq(-1, -4, length = len - 1)),
              threshold = seq(0.01,0.99,length = len))
}

## we loop over the other two parameter while tuning the threshold in submodels
thresh_nnet$loop <- function(grid){
  library(plyr)
  loop <- ddply(grid,c("size","decay"),
                function(x) c(threshold = max(x$threshold)))
  submodels <- vector(mode = "list", length = nrow(loop))
  for (i in seq(along = loop$threshold)){
    index <- which(grid$size == loop$size[i] & grid$decay == loop$decay[i])
    cuts <- grid[index,"threshold"]
    submodels[[i]] <- data.frame(threshold = cuts[cuts != loop$threshold[i]])
  }
  list(loop = loop, submodels = submodels)
}

## fit the model only on two class classification
thresh_nnet$fit <- function (x, y, wts, param, lev, last, classProbs, ...) 
{
  if(length(levels(y)) != 2)
    stop("This works only for 2-class problems")
  dat <- x
  dat$.outcome <- y
  if (!is.null(wts)) {
    out <- nnet(.outcome ~ ., data = dat, weights = wts, 
                size = param$size, decay = param$decay, ...)
  }
  else out <- nnet(.outcome ~ ., data = dat, size = param$size, 
                   decay = param$decay, ...)
  out
}

## get a probability prediction
thresh_nnet$prob <- function (modelFit, newdata, submodels = NULL) 
{
  out <- predict(modelFit, newdata)
  if (ncol(as.data.frame(out)) == 1) {
    out <- cbind(out, 1 - out)
    dimnames(out)[[2]] <- rev(modelFit$obsLevels)
  }
  if(!is.null(submodels)) {
    probs <- out
    out <- vector(mode = "list", length = length(submodels$threshold)+1)
    out <- lapply(out, function(x) probs)
  }
  out
}

## use different thresholds to get the predicted class
thresh_nnet$predict <- function (modelFit, newdata, submodels = NULL) 
{
  out <- predict(modelFit, newdata)
  if (ncol(as.data.frame(out)) == 1) {
    out <- cbind(out, 1 - out)
    dimnames(out)[[2]] <- rev(modelFit$obsLevels)
  }
  class1Prob <- out[, modelFit$obsLevels[1]]
  out <- ifelse(class1Prob >= modelFit$tuneValue$threshold,
                modelFit$obsLevels[1],
                modelFit$obsLevels[2])
  if(!is.null(submodels)) {
    tmp2 <- out
    out <- vector(mode = "list", length = nrow(submodels) + 1)
    out[[1]] <- tmp2
    for(i in seq(along = submodels$threshold)) {
      out[[i+1]] <- ifelse(class1Prob >= submodels$threshold[[i]],
                           modelFit$obsLevels[1],
                           modelFit$obsLevels[2])
    }
  }
  out
}
############################ thresh_rf ######################
## tuning threshold random forest model
thresh_rf <- getModelInfo("rf",regex = FALSE)[[1]]

## set the model only for classification model
thresh_rf$type <- c("Classification")

## add the threshold as another tuing parameter
threshold <- data.frame(parameter = c("threshold"),
                        class = c("numeric"),
                        label = c("Probability Cutoff"))
thresh_rf$parameters <- rbind(thresh_rf$parameters,threshold)

## default tuning grid
thresh_rf$grid <- function(a,y,len=NULL){
  p <- ncol(x)
  expand.grid(mtry = floor(sqrt(p)),
              threshold = seq(.01, .99, length = len))
}

## loop over tuning parameter while set the threshold in submodels
thresh_rf$loop = function(grid) {
  library(plyr)
  loop <- ddply(grid, c("mtry"),
                function(x) c(threshold = max(x$threshold)))
  submodels <- vector(mode = "list", length = nrow(loop))
  for(i in seq(along = loop$threshold)) {
    index <- which(grid$mtry == loop$mtry[i])
    cuts <- grid[index, "threshold"]
    submodels[[i]] <- data.frame(threshold = cuts[cuts != loop$threshold[i]])
  }
  list(loop = loop, submodels = submodels)
}

## set the fit for only two class classification
thresh_rf$fit = function(x, y, wts, param, lev, last, classProbs, ...) {
  if(length(levels(y)) != 2)
    stop("This works only for 2-class problems")
  randomForest(x, y, mtry = param$mtry, ...)
}

## predicte the probs
thresh_rf$prob = function(modelFit, newdata, submodels = NULL) {
  out <- as.data.frame(predict(modelFit, newdata, type = "prob"))
  if(!is.null(submodels)) {
    probs <- out
    out <- vector(mode = "list", length = length(submodels$threshold)+1)
    out <- lapply(out, function(x) probs)
  }
  out
}

## make predictions 
thresh_rf$predict = function(modelFit, newdata, submodels = NULL) {
  class1Prob <- predict(modelFit,
                        newdata,
                        type = "prob")[, modelFit$obsLevels[1]]
  out <- ifelse(class1Prob >= modelFit$tuneValue$threshold,
                modelFit$obsLevels[1],
                modelFit$obsLevels[2])
  if(!is.null(submodels)) {
    tmp2 <- out
    out <- vector(mode = "list", length = length(submodels$threshold))
    out[[1]] <- tmp2
    for(i in seq(along = submodels$threshold)) {
      out[[i+1]] <- ifelse(class1Prob >= submodels$threshold[[i]],
                           modelFit$obsLevels[1],
                           modelFit$obsLevels[2])
    }
  }
  
  out
}

############################ thresh_glm #################################
## Get the model code from the original code for glm
thresh_glm <- getModelInfo("glm",regex = FALSE)[[1]]

## set the type only for classification
thresh_glm$type <- c("Classification")

## add the threshold as another tuing parameter
thresh_glm$parameters <- data.frame(parameter = c("threshold"),class = c("numeric"),label=c("Probability Cutoff"))

## the default tuning grid
thresh_glm$grid <- function(x,y,len=NULL){expand.grid(threshold = seq(.01,.99,length = len))}

## build one model and tuning threshold in submodels
thresh_glm$loop <- function(grid){
  grid <- grid[order(grid$threshold,decreasing = FALSE),,drop=FALSE]
  loop <- grid[1,,drop=FALSE]
  submodels <- list(grid[-1,,drop=FALSE])
  list(loop=loop,submodels = submodels)
}

## use different thresholds to get the predicted class
thresh_glm$predict = function(modelFit, newdata, submodels = NULL) {
  class1Prob <- predict(modelFit,
                        newdata,
                        type = "response")
  out <- ifelse(class1Prob >= modelFit$tuneValue$threshold,
                modelFit$obsLevels[1],
                modelFit$obsLevels[2])
  if(!is.null(submodels)) {
    tmp2 <- out
    out <- vector(mode = "list", length = nrow(submodels) + 1)
    out[[1]] <- tmp2
    for(i in seq(along = submodels$threshold)) {
      out[[i+1]] <- ifelse(class1Prob >= submodels$threshold[[i]],
                           modelFit$obsLevels[1],
                           modelFit$obsLevels[2])
    }
  }
  out
}

## get a probability prediction
thresh_glm$prob = function(modelFit, newdata, submodels = NULL) {
  out <- predict(modelFit, newdata, type = "response")
  out <- cbind(1 - out, out)
  dimnames(out)[[2]] <- modelFit$obsLevels
  
  if(!is.null(submodels)) {
    probs <- as.data.frame(out)
    out <- vector(mode = "list", length = length(submodels$threshold)+1)
    out <- lapply(out, function(x) probs)
  }
  out
}

############################ thresh_knn ##########################
## 
thresh_knn <- getModelInfo("knn",regex = FALSE)[[1]]

## model specific for classification
thresh_knn$type <- c("Classification")

## add the threshold as another tuing parameter
threshold <- data.frame(parameter = c("threshold"),
                        class = c("numeric"),
                        label = c("Probability Cutoff"))
thresh_knn$parameters <- rbind(thresh_knn$parameters,threshold)

## default tune grid 
thresh_knn$grid <- function(x,y,len=NULL){
  expand.grid(k = (5:((2 * len) + 4))[(5:((2 * len) + 4))%%2 > 0],
              threshold = seq(.01, .99, length = len))
}

## loop over the grid
thresh_knn$loop <- function(grid){
  library(plyr)
  loop <- ddply(grid,c("k"),
                function(x) c(threshold = max(x$threshold)))
  submodels <- vector(mode = "list",length=nrow(loop))
  for(i in seq(along = loop$threshold)){
    index <- which(grid$k == loop$k[i])
    cuts <- grid[index,"threshold"]
    submodels[[i]] <- data.frame(threshold=cuts[cuts != loop$threshold[i]])
  }
  list(loop = loop,submodels = submodels)
}

## fit the model
thresh_knn$fit <- function(x, y, wts, param, lev, last, classProbs, ...) {
  if(length(levels(y)) != 2)
    stop("This works only for 2-class problems")
  knn3(as.matrix(x), y, k = param$k, ...)
}

## predict the probability
thresh_knn$prob <- function (modelFit, newdata, submodels = NULL){
  out <- predict(modelFit, newdata, type = "prob") 
  if(!is.null(submodels)){
    probs <- out
    out <- vector(mode="list",length=length(submodels$threshold)+1)
    out <- lapply(out, function(x) probs)
  }
  out
} 

## make prediction based on different threshold
thresh_knn$predict <- function(modelFit, newdata, submodels = NULL){
  class1Prob <- predict(modelFit,
                        newdata,
                        type = "prob")[, modelFit$obsLevels[1]]
  
  out <- ifelse(class1Prob >= modelFit$tuneValue$threshold,
                modelFit$obsLevels[1],
                modelFit$obsLevels[2])
  if(!is.null(submodels)) {
    tmp2 <- out
    out <- vector(mode = "list", length = length(submodels$threshold)+1)
    out[[1]] <- tmp2
    for(i in seq(along = submodels$threshold)) {
      out[[i+1]] <- ifelse(class1Prob >= submodels$threshold[[i]],
                           modelFit$obsLevels[1],
                           modelFit$obsLevels[2])
    }
  }
  out
}

############################ thresh_lda ###########################
thresh_lda  <- getModelInfo("lda", regex = FALSE)[[1]]

## add threshold as a tuning parameter
thresh_lda$parameters <- data.frame(parameter = c("threshold"),
                                    class = c("numeric"),
                                    label = c("Probability Cutoff"))

## default tune grid
thresh_lda$grid <- function(x,y,len=NULL)
  data.frame(threshold = seq(.01, .99, length = len))

## loop function: build a model first then tune over different threshold
thresh_lda$loop <- function(grid){
  grid <- grid[order(grid$threshold, decreasing = FALSE), , drop = FALSE]
  loop <- grid[1, , drop = FALSE]
  submodels <- list(grid[-1, , drop = FALSE])
  list(loop = loop, submodels = submodels)
}

## fit the model
thresh_lda$fit = function(x, y, wts, param, lev, last, classProbs, ...) {
  if(length(levels(y)) != 2)
    stop("This works only for 2-class problems")
  lda(x,y,...)
}

## predict the probability
thresh_lda$prob <- function(modelFit, newdata, submodels = NULL){
  out <- predict(modelFit, newdata)$posterior
  if(!is.null(submodels)){
    probs <- out
    out <- vector(mode = "list", length = length(submodels$threshold)+1)
    out <- lapply(out, function(x) probs)
  }
  out
}

## make a prediction
thresh_lda$predict <- function(modelFit, newdata, submodels = NULL){
  class1Prob <- predict(modelFit, newdata)$posterior[, modelFit$obsLevels[1]]
  out <- ifelse(class1Prob >= modelFit$tuneValue$threshold,
                modelFit$obsLevels[1],
                modelFit$obsLevels[2])
  if(!is.null(submodels)) {
    tmp2 <- out
    out <- vector(mode = "list", length = length(submodels$threshold)+1)
    out[[1]] <- tmp2
    for(i in seq(along = submodels$threshold)) {
      out[[i+1]] <- ifelse(class1Prob >= submodels$threshold[[i]],
                           modelFit$obsLevels[1],
                           modelFit$obsLevels[2])
    }
  }
  out
}

############################ thresh_nb ####################
thresh_nb  <- getModelInfo("nb", regex = FALSE)[[1]]

## add thresh_hold as a tune parameter
threshold <- data.frame(parameter = c("threshold"),
                        class = c("numeric"),
                        label = c("Probability Cutoff"))
thresh_nb$parameters <- rbind(thresh_nb$parameters,threshold)

## default grid
thresh_nb$grid <- function (x, y, len = NULL){
  expand.grid(usekernel = c(TRUE, FALSE), fL = 0,
              threshold = seq(.01,.99,length=len))
}

## loop
thresh_nb$loop <- function(grid){
  library(plyr)
  loop <- ddply(grid, c("fL","usekernel"),
                function(x) c(threshold = max(x$threshold)))
  submodels <- vector(mode = "list", length = nrow(loop))
  for(i in seq(along = loop$threshold)) {
    index <- which(grid$fL == loop$fL[i]&grid$usekernel==loop$usekernel[i])
    cuts <- grid[index, "threshold"]
    submodels[[i]] <- data.frame(threshold = cuts[cuts != loop$threshold[i]])
  }
  list(loop = loop, submodels = submodels)
}

## fit the model
thresh_nb$fit = function(x, y, wts, param, lev, last, classProbs, ...) {
  if(length(levels(y)) != 2)
    stop("This works only for 2-class problems")
  NaiveBayes(x, y, usekernel = param$usekernel, fL = param$fL, 
             ...)
}

## predict the probability
thresh_nb$prob <- function(modelFit, newdata, submodels = NULL) {
  out <- predict(modelFit, newdata, type = "raw")$posterior
  
  if(!is.null(submodels)) {
    probs <- out
    out <- vector(mode = "list", length = length(submodels$threshold)+1)
    out <- lapply(out, function(x) probs)
  }
  out
}

## predict the classes
thresh_nb$predict = function(modelFit, newdata, submodels = NULL) {
  class1Prob <-predict(modelFit, newdata, type = "raw")$posterior[, modelFit$obsLevels[1]]
  
  out <- ifelse(class1Prob >= modelFit$tuneValue$threshold,
                modelFit$obsLevels[1],
                modelFit$obsLevels[2])
  if(!is.null(submodels)) {
    tmp2 <- out
    out <- vector(mode = "list", length = length(submodels$threshold))
    out[[1]] <- tmp2
    for(i in seq(along = submodels$threshold)) {
      out[[i+1]] <- ifelse(class1Prob >= submodels$threshold[[i]],
                           modelFit$obsLevels[1],
                           modelFit$obsLevels[2])
    }
  }
  out
}


threshmod <- list(thresh_rf,thresh_nnet,thresh_glm,thresh_knn,thresh_nb,thresh_lda)