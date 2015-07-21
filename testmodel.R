# task2_test.R
# author: Diana Wang
# created : 09/11/2014
# edit by Diana on 09/15/2014
# edit by Diana on 12/11/2014
# edit by Diana on 01/28/2014

# -----Beging defing function test that returns test stats based on the true observation
#     and the predict value.

# Args:
#     TrVal:  a vector that contains the true observations.
#     model:  a trained model that the user want to test the performance of predicting.
#     newdata: a data frame that contains all the predictors.
#     threshold:  a user specified cutoff value. If the model is a tuning threshold
#                 model, then the best threshold will be used, no matter what value 
#                 the user specified. The default value is 0.5.

# values:
#     prob: the predict probabilities for each class 
#     pred: the predict classes
#     sensitivity: the sensitivity of the classification(the first level of the outcome factor is used to define"possitive")
#     specificity: the specificity of the classification(the first level of the outcome factor is used to define"possitive")
#     PSS: the pierce skill score of the classification
#     ROC: the ROC of the classification
# Pakcage dependencies: caret, pROC
# + + + + + + + + + + + + + + + + 


TestModel <- function(model,newdata,Trval,threshold=NULL){
  lev <- levels(Trval)
  library(caret)
  library(pROC)
  prob <- predict(model,newdata,type="prob")
  class1prob <- prob[,1]
  if ('threshold' %in% names(model$best)){
    if (!is.null(threshold)) warning('threshold already specified in model')
    msg <- paste (" theshold is ", model$best["threshold"])
    pred <- predict(model,newdata)
    cat(msg)
  }
  else{
    if (threshold == NULL){
      warning (" default threshold is 0.5")
      pred <- predict(model,newdata)
    }
    
    pred <- ifelse(class1prob >= threshold,levels[Trval][1],levels[Trval][2])
  }
  sens <- sensitivity(pred,Trval)
  spec <- specificity(pred,Trval)
  pss <- sens-(1-spec)
  roc <- roc(Trval,class1prob,levels=rev(levels(Trval)))
  out <- list(prob=prob,pred=pred,sensitivity=sens,specificity=spec,PSS=pss,ROC=roc)
  out
}