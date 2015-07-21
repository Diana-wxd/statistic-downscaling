## tutorial document reproduce code
## author: Diana Wang
## date: 2/16/2015
## description: This peice of code is used to reproduce the
##              result in the tutorial document.

############################################################
# First step is to prepare the data 
# change the target values to dry  and wet classes
Tar1$class <- ifelse(Tar1$V1 <1,"dry","wet")
target <- Tar1$class
predictors <- GCM1

library(caret)
# then we split the data into train set and test set
set.seed(1) # set the seed so that when you reproduce this step on another computer will get the same result.
indextrain <- createDataPartition(target,p=3/4,list=FALSE)
head(indextrain) # this is the index of row numbers
trainpredictors <- predictors[indextrain,]
traintarget <- target[indextrain]
testpredictors <- predictors[-indextrain,]
testtarget <- target[-indextrain]

# check whether the partition of the two class are representable 
prop.table(table(traintarget))
prop.table(table(target))

##################tune parameters##########################
# don't forget to check whether you load the package and source
# the source file
source("train.R")
source("testmodel.R")
source("threshold.R")
source("summaryfunction_score.R")

trainpredictors <- predictors
traintarget <- target$class1
# train a simple model using logistic regression and tune the threshold
set.seed(101) # set seed in order to get the same result
simplemod <- ModelTr(x=trainpredictors,
                     y=as.factor(traintarget),
                     method="thresh_glm")

# we can set tune length
set.seed(101)
simplemod <- ModelTr(x=trainpredictors,
                     y=as.factor(traintarget),
                     metric="Accuracy",
                     method="thresh_glm",
                     tuneLength=5) # set tuneLength 
dev.off()
trellis.par.set(caretTheme())
plot(simplemod,metric="Kappa")
# we can also set our specific values to tune for the parameters
# combine your values as a tunegrid
# set a tune grid for"gbm" method
gbmGrid <- expand.grid(interaction.depth = c(1, 5, 9),
                       n.trees = c(50,100,200),
                       shrinkage = 0.1)
set.seed(101)
gbmmod <- ModelTr(x=trainpredictors, # load data
                  y=as.factor(traintarget), # load data
                  method="gbm", # choose method
                  tuneGrid=gbmGrid) #decide tune grid

trellis.par.set(caretTheme())
plot(gbmmod)

# using "thrsh_rf"
set.seed(101)
trfmod <- ModelTr(x=trainpredictors, # load data
                  y=as.factor(traintarget), # load data
                  method="thresh_rf", # choose method
                  metric="Accuracy",
                  tuneGrid=expand.grid(threshold=c(0.2,0.3,0.5,0.8),
                                       mtry=c(10,20))) #decide tune grid
# without tune threshold
set.seed(101)
rfmod <- ModelTr(x=trainpredictors, # load data
                  y=as.factor(traintarget), # load data
                  method="rf", # choose method
                  metric="Accuracy",
                  tuneGrid=expand.grid(mtry=c(10,20)))
                                       
#################alternate metric###########################
# using the score performance function
sc <- trainControl(classProbs = TRUE,
                   summaryFunction = score)
set.seed(101)
simplemod2 <- ModelTr(x=trainpredictors,
                     y=as.factor(traintarget),
                     metric="PSS", # now we can use PSS as the metric
                     method="thresh_glm",
                     trControl = sc,
                     
                     tuneLength=5)
set.seed(101)
trfmod2 <- ModelTr(x=trainpredictors, # load data
                  y=as.factor(traintarget), # load data
                  method="thresh_rf", # choose method
                  metric="PSS", # metric is PSS
                  trControl=sc,
                  tuneGrid=expand.grid(threshold=c(0.2,0.3,0.5,0.8),
                                       mtry=c(5,10))) #decide tune grid
# compare with when using accuracy as metric
par (mfrow=c(2,1))
trellis.par.set(caretTheme())
plot(trfmod)
plot(trfmod2, metric="PSS.Sens")
#################### resample method #######################
## set cross validation resampling method
cvcontrol <- trainControl(method="cv",#cross validation
                          number = 10,#number of fold
                          # we will stick to "PSS"metric,so still need to set 
                          # summaryFunction to "score"
                          summaryFunction=score,
                          classProb=TRUE)

## set bootstrap resampling method
btcontrol <- trainControl(method="boot",#bootstrap
                          number = 10,#number of sampling iterations
                          # we will stick to "PSS"metric,so still need to set 
                          # summaryFunction to "score"
                          summaryFunction=score,
                          classProb=TRUE)
set.seed(101)
cvSVM <- ModelTr(x=trainpredictors,
                 y=as.factor(traintarget),
                 method="svmRadial",
                 metric="PSS",
                 tuneGrid=expand.grid(sigma=c(0.005,0.01,0.05),C=c(0.25,0.5)),
                 preProcess=c('center',"scale"),
                 trControl=cvcontrol)
set.seed(101)
btSVM <- ModelTr(x=trainpredictors,
                 y=as.factor(traintarget),
                 method="svmRadial",
                 metric="PSS",
                 tuneGrid=expand.grid(sigma=c(0.005,0.01,0.05),C=c(0.25,0.5)),
                 preProcess=c('center',"scale"),
                 trControl=btcontrol)
set.seed(101)
smSVM <- ModelTr(x=trainpredictors,
                 y=as.factor(traintarget),
                 method="svmRadial",
                 metric="PSS",
                 tuneGrid=expand.grid(sigma=c(0.005,0.01,0.05),C=c(0.25,0.5)),
                 preProcess=c('center',"scale"),
                 trControl=trainControl(method="boot",
                                        summaryFunction=score,
                                        classProb=TRUE),
                 SMOTE=TRUE)

plot1 <- plot(cvSVM)
plot2 <- plot(btSVM)
rs<-resamples.default(x=list(CrossValidation = cvSVM,
                                 Bootstrap = btSVM))
resamps <- resamples(list(CrossValidation = cvSVM,
                          Bootstrap = btSVM))
summary(resamps)
trellis.par.set(theme1)
bwplot(resamps, layout = c(4, 1))
##################### preprocess ###########################
# set tune grid for nnet
netgrid <- expand.grid(size=c(5,10),decay=c(0.01,0.0001,0.001))

# center & scale
set.seed(101)
nnetmodel <- ModelTr(x=trainpredictors,
                     y=as.factor(traintarget),
                     method="nnet",
                     metric="PSS",
                     tuneGrid=netgrid,
                     preProcess=c('center',"scale"),
                     trControl=trainControl(summaryFunction=score,
                                            classProb=TRUE))
# PCA
nnetmodel2 <- ModelTr(x=trainpredictors,
                     y=as.factor(traintarget),
                     method="nnet",
                     metric="PSS",
                     preProcess="pca",
                     tuneGrid=netgrid,
                     trControl=trainControl(summaryFunction=score,
                                            classProb=TRUE,
                                            preProcOptions=list(thresh=0.95)))
###################### test model ##########################
ts <- TestModel(model=trfmod,#model we build
                newdata = testpredictors, #a newset of data contains all the predictors
                Trval=as.factor(testtarget))
############### cluster & multiclass ######################
# we cluster the preciptation into 3 clusters
cluster3 <- kmeans(Tar1$V1,3)

# make the clusters as three different class
target3 <- cluster3$cluster
traintarget3 <- target3[indextrain] #use the same partition as the predictors

set.seed(101)
trfmod3 <- ModelTr(x=trainpredictors, # load data
                  y=as.factor(traintarget3), # load data
                  method="rf", # choose method
                  metric="PSS",
                  tuneGrid=expand.grid(mtry=c(10,20)),
                  trControl=trainControl(
                                         summaryFunction=score,
                                         classProb=TRUE))

################# predictors ############################
corrmatrix <- cor(trainpredictors)
library(corrplot)

corrplot(corrmatrix, order = "hclust", tl.cex = .35)
