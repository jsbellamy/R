

library("doParallel")


#Register core backend, using 8 cores
cl <- makeCluster(4)
registerDoParallel(cl)

#list number of workers
getDoParWorkers()


#the important function is foreach (the parallel version of for)
#example

#lets calculate 30,000 linear regressions
#they are exactly the same but its just for example

#wrapping in system.time to get to time to rpcoess
system.time({
for(i in 1:30000)
  (
    lm(Petal.Width ~ Sepal.Width + Petal.Length, data=iris)
  )
})

#this is the multicore verison
system.time({
foreach(i=1:30000) %dopar%  lm(Petal.Width ~ Sepal.Width + Petal.Length, data=iris)
})

#use parallel for calculations that take a lot of time on each loop
#builtin functions are often vectorized and faster than multicore



#this is code for creating your own boot strap regression


#example comparing parallel vs single core (default: sequential) for loop
#running 10,000 bootstrap itterations and logistic regressions on iris data

#grabbing 2 levels of species factor (for logistic regression and sepal.length as predictor)
#iris is a dataset that 
x <- iris[which(iris[,5] != "setosa"), c(1,5)]

#10000 bootstraps
  trials <- 10000
  
  #capture time to run 10000 bootstrap logostic regressions across all cores
  #takes 3 seconds on my computer (8 core)
  multicoretime <- system.time({
    r <- foreach(icount(trials), .combine=cbind) %dopar% {
      
      ind <- sample(100, 100, replace=TRUE) #generate random row numbers for current bootstrap
      result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
      coefficients(result1)
      
      }
  })
  
  #capture time to run 10000 bootstrap logisitc regressions across one core
  #takes 25 seconds on my computer (8 Core)
  singlecoretime <- system.time({
    r <- foreach(icount(trials), .combine=cbind) %do% {
      
      ind <- sample(100, 100, replace=TRUE) #generate random row numbers for current bootstrap
      result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
      coefficients(result1)
      
    }
  })

#lets use the cores to train models  
#for complete list of models supported: https://topepo.github.io/caret/modelList.html
library(caret)
library(pROC)
  
setwd("Z:/SDSU/Courses/2016/Fall/MIS 620/Week 7")  


#lets use the credit dataset predicting defaulting on a loan
credit <- read.csv("credit.csv")

nrow(credit)

#create training 
inTrain<-createDataPartition(y=credit$default, p=.8, list=FALSE)


#create seperate test and training sets, making two versions (with and without factors)
#for models that don't like them (e.g., nnet)
credit.train <- credit[inTrain,]
credit.test <- credit[-inTrain,]


#the dummyVars object is used with predict function to create new data frame of dummy variables
#excluding the response factor default (column 17)
credit.dummy.train <- data.frame(predict(dummyVars("~ .", data=credit.train[-17], fullRank=TRUE), newdata=credit.train))
credit.dummy.test <- data.frame(predict(dummyVars("~ .", data=credit.test[-17], fullRank=TRUE), newdata=credit.test))

#add the response factor to the dummy variable training and test sets 
credit.dummy.train<-cbind(default=credit.train$default,credit.dummy.train)
credit.dummy.test<-cbind(default=credit.test$default,credit.dummy.test)

#all factor variables converted to numeric form
#not need for rpart but need for all other methods
str(credit.dummy.train)


#some parameters to control the sampling during parameter tuning and testing
ctrl <- trainControl(method="repeatedcv", number=10, repeats=2,
                     classProbs=TRUE,
                     #function used to measure performance
                     summaryFunction = twoClassSummary, #multiClassSummary for non binary
                     allowParallel = TRUE) #default looks for parallel backend


#twoClassSummary is built in function with ROC, Sensitivity and Specificity


#to see what parameters are to be tuned:
modelLookup("rpart")
m.rpart <- train(default ~ ., 
           trControl = ctrl,
           metric = "ROC", #using AUC to find best performing parameters
           preProc = c("range", "nzv"), #scale from 0 to 1 and from columns with zero variance
           data = credit.train, 
           method = "rpart")
m.rpart

##preprocess options, impute will use different methods for calculating missing values, 
##pca and ica will combine many columns into one (a type of factor analysis)
##"BoxCox", "YeoJohnson", "expoTrans", "center", "scale", "range", "knnImpute", "bagImpute", 
##"medianImpute", "pca", "ica", "spatialSign", "zv", and "nzv" 

#can plot the performance of different parameters affect on ROC
plot(m.rpart)


#the best performing model trained on the full training set is saved 
##preprocessing using predict function with caret train object will be applied to new data
p.rpart <- predict(m.rpart,credit.test)
confusionMatrix(p.rpart,credit.test$default)


#lets try a larger search of parameters tuneLength
m.rpart.8 <- train(default ~ ., 
                 trControl = ctrl,
                 metric = "ROC", #using AUC to find best performing parameters
                 preProc = c("range", "nzv"), #scale from 0 to 1 and from columns with zero variance
                 data = credit.train, 
                 tuneLength=8,
                 method = "rpart")
m.rpart.8
plot(m.rpart.8)

##neural Network
modelLookup("nnet")
m.nnet <- train(default~ ., 
                 trControl = ctrl,
                 metric = "ROC", #using AUC to find best performing parameters
                 preProc = c("range", "nzv"), #scale from 0 to 1 and from columns with zero variance
                 data = credit.dummy.train, 
                 method = "nnet")
m.nnet
plot(m.nnet)
p.nnet <- predict(m.nnet,credit.dummy.test)
confusionMatrix(p.nnet,credit.dummy.test$default)


##svm with radial kernel
modelLookup("svmRadial")
m.svm <- train(default~ ., 
                trControl = ctrl,
                metric = "ROC", #using AUC to find best performing parameters
                preProc = c("range", "nzv"), #scale from 0 to 1 and from columns with zero variance
                data = credit.dummy.train, 
                method = "svmRadial")
m.svm
plot(m.svm)
p.svm<- predict(m.svm,credit.dummy.test)
confusionMatrix(p.svm,credit.dummy.test$default)


#compare training performance
rValues <- resamples(list(rpart=m.rpart, nnet=m.nnet, svm=m.svm))

bwplot(rValues, metric="ROC")


#some meta-learning examples
##BAGGING - bootstrapping is used to create many training sets and simple models are trained on each and combined
##many small decision trees
library(ipred)

m.bag <- train(default~ ., 
               trControl = ctrl,
               metric = "ROC", #using AUC to find best performing parameters
               preProc = c("range", "nzv"), #scale from 0 to 1 and from columns with zero variance
               data = credit.train, 
               method = "treebag")
m.bag
p.bag<- predict(m.bag,credit.test)
confusionMatrix(p.bag,credit.test$default)

##caret support bagging of svm and neural network models using bag method "bag"
#also create a control function for bag parameters

##BOOSTING
##similar to bagging, resample of data to create many models and a final vote,
##but with boosting resampled data are constructed to support better learners and better models have greater weight in final vote

#adaboost or adaptive boosting is an algorithm that generates wak learners 
#iteratively that learn to classify more of the examples

library(ada)

#boosted decision trees
#using dummy codeds because this function internally does it and its better to handle it yourself (i.e., less error prone)
m.ada <- train(default~ ., 
               trControl = ctrl,
               metric = "ROC", #using AUC to find best performing parameters
               preProc = c("range", "nzv"), #scale from 0 to 1 and from columns with zero variance
               data = credit.dummy.train, 
               method = "ada")
m.ada
plot(m.ada)
p.ada<- predict(m.ada,credit.dummy.test)
confusionMatrix(p.ada,credit.dummy.test$default)

#random forest approach to many classification models created and voted on
#less prone to ovrefitting and used on large datasets
library(randomForest)


m.rf <- train(default~ ., 
               trControl = ctrl,
               metric = "ROC", #using AUC to find best performing parameters
               preProc = c("range", "nzv"), #scale from 0 to 1 and from columns with zero variance
               data = credit.train, 
               method = c("rf") )
m.rf
p.rf<- predict(m.rf,credit.test)
confusionMatrix(p.rf,credit.test$default)

#compare the performance of all models trained today
rValues <- resamples(list(rpart=m.rpart, nnet=m.nnet, svm=m.svm, bag=m.bag, boost=m.ada, rf=m.rf))

bwplot(rValues, metric="ROC")
bwplot(rValues, metric="Sens") #Sensitvity
bwplot(rValues, metric="Spec")

## you can quickly train many more models than we have here
#make sure to close you cluster (release your computers cores)
stopCluster(cl)
  