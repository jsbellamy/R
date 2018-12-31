setwd("C:/Users/preek/Desktop/SDSU MIS/MIS 620")
h1B <- read.csv("MasterH1BDataset.csv")
install.packages(c("doParallel", "caret", "mice", "klaR", "pROC", "VIM", "ipred", "ada", "randomForest"))
library("doParallel")
#will use book's package
library(ISLR)
library(ggplot2)
library(ggthemes)
library(gridExtra)
#memory.limit()
#memory.limit(size=8800)
#return to classification
#what happen if we try and predict default
#lets make a dummy variable out of default
#as.numeric converts lavels to numeric representation (No=1, Yes=2)
#we subtract 1 to make default yes =1 and no =0
h1b.dummy <- as.numeric(h1B$CASE_STATUS)-1

h1b.lm <- lm(h1b.dummy ~ . - ,data=h1B)
summary(h1b.lm)

#lets look at the predictions
hist(h1b.lm$fitted.values)

#using default plots add regression line
plot(h1b.dummy ~ h1B$CASE_STATUS)
abline(h1b.lm)

##lets fit a simple logistic regression
##don't need to recode the factor
h1b.loglm <- glm(CASE_STATUS ~ ., data=h1B, family=binomial) 
summary(h1b.loglm) #model summary
coef(h1b.loglm) #coefficents
exp(coef(h1b.loglm))

#linear discriminant analysis
library(MASS)

h1b.lda <- lda(CASE_STATUS ~ ., data=h1B )
h1b.lda
plot(h1b.lda)


library(caret)
library(ISLR)
library(pROC)

str(h1B)

#these data are unbalanced
h1B.table<- table(h1B$CASE_STATUS)
prop.table(h1B.table) # .03% of cases are postive class

#lets relevel the Default factor so Yes becomes the Positive class by default (Sensitivity)
h1B$CASE_STATUS <- relevel(h1B$CASE_STATUS, ref="CERTIFIED")

#converting to factors to dummy codes
#including DV because it should be a binary value too
h1B.dmodel <- dummyVars(~., data=h1B, fullRank=T)
h1B.d <- as.data.frame(predict(h1B.dmodel, h1B))
#NOT RUN NO SIZE OF 27.8GB PREVIOUS LINE
h1B.d$CASE_STATUS <- h1B$CASE_STATUS #copy back DV
str(h1B.d)

#creating test and training set as example
#if dataset is not very big avoid train/test splits high variation
#going use carets partition function to randomly split 70% of training set values
#proportional to class balance
set.seed(199)
trainIndex <- createDataPartition(h1B.d$CASE_STATUS, p=.7, list=F)
h1B.train <- h1B.d[trainIndex,]
h1B.test <- h1B.d[-trainIndex,]

###Moving straight to modeling, review Regression Week 6 Code for example of preprocessing
## and pre modeling visualization
#non-stratified
random.train <- sample(10,000,7000)
#setup control function for resampling and binary classification performance
#using 10 fold cross validation
ctrl <- trainControl(method = "cv", number=10, summaryFunction=twoClassSummary,
                     classProbs=T, savePredictions=T) #saving predictions from each resample fold

##logistic regression
set.seed(199)#ALWAYS USE same SEED ACROSS trains to ensure identical cv folds
h1B.log <-train(CASE_STATUS ~ ., data = h1B.train, method ="glm",family ="binomial",metric ="ROC", trControl=ctrl)
summary(h1B.log)
varImp(h1B.log)
getTrainPerf(h1B.log)
h1B.log
#calculate resampled accuracy/confusion matrix using extracted predictions from resampling
#take averages
confusionMatrix(h1B.log$pred$pred, h1B.log$pred$obs)
                  
                  
##linear discriminant analysis
set.seed(199)
h1B.lda <-  train(CASE_STATUS ~ .,data = h1B.train, method="lda", metric="ROC", trControl=ctrl)
h1B.lda
varImp(h1B.lda)
confusionMatrix(h1B.lda$pred$pred, h1B.lda$pred$obs) #take averages
                  
                  
##quadratic distriminant analysis
set.seed(199)
h1B.qda <-  train(CASE_STATUS ~ .,data = h1B.train, method="qda", metric="ROC", trControl=ctrl)
h1B.qda
                  
getTrainPerf(h1B.qda)
                
#k nearest neighbors classification
set.seed(199) 
kvalues <- expand.grid(k=1:20)
                
h1B.knn <-  train(CASE_STATUS ~ .,data = h1B.train, method="knn", metric="ROC", trControl=ctrl, tuneLength=10) #let caret decide 10 best parameters to search
h1B.knn
plot(h1B.knn)
getTrainPerf(h1B.knn)
            
confusionMatrix(h1B.knn$pred$pred, h1B.knn$pred$obs) #make sure to select resamples only for optimal parameter of K
                  
#really need test set to get more accurate idea of accuracy when their is a rare class
#can either use model on cross validation of complete training data or hold out test set
                  
#lets compare all resampling approaches
h1B.models <- list("logit"=h1B.log, "lda"=h1B.lda, "qda"=h1B.qda,
                  "knn"=h1B.knn)
d.resamples = resamples(h1B.models)
                  
                  
#plot performance comparisons
bwplot(h1B.resamples, metric="ROC") 
bwplot(h1B.resamples, metric="Sens") #predicting default dependant on threshold
bwplot(h1B.resamples, metric="Spec") 
                  
#calculate ROC curves on resampled data
                  
h1B.log.roc<- roc(response= h1B.log$pred$obs, predictor=h1B.log$pred$Yes)
h1B.lda.roc<- roc(response= h1B.lda$pred$obs, predictor=h1B.lda$pred$Yes)
h1B.qda.roc<- roc(response= h1B.qda$pred$obs, predictor=h1B.qda$pred$Yes)
#when model has parameters make sure to select final parameter value
h1B.knn.roc<- roc(response= h1B.knn$pred[h1B.knn$pred$k==23,]$obs, predictor=h1B.knn$pred[h1B.knn$pred$k==23,]$Yes) 
                
#build to combined ROC plot with resampled ROC curves
plot(h1B.log.roc, legacy.axes=T)
plot(h1B.lda.roc, add=T, col="Blue")
plot(h1B.qda.roc, add=T, col="Green")
plot(h1B.knn.roc, add=T, col="Red")
legend(x=.2, y=.7, legend=c("Logit", "LDA", "QDA", "KNN"), col=c("black","blue","green","red"),lty=1)
                  
#logit looks like the best choice its most parsimonious and equal ROC to LDA, QDA and KNN
              
#lets identify a more optimal cut-off (current resampled confusion matrix), low sensitivity
confusionMatrix(h1B.log$pred$pred, h1B.log$pred$obs)
                  
#extract threshold from roc curve  get threshold at coordinates top left most corner
h1B.log.Thresh<- coords(h1B.log.roc, x="best", best.method="closest.topleft")
h1B.log.Thresh #sensitivity increases to 88% by reducing threshold to .0396 from .5
                  
#lets make new predictions with this cut-off and recalculate confusion matrix
h1B.log.newpreds <- factor(ifelse(h1B.log$pred$Yes > h1B.log.Thresh[1], "Yes", "No"))
                  
#recalculate confusion matrix with new cut off predictions
confusionMatrix(h1B.log.newpreds, h1B.log$pred$obs)
                  
### TEST DATA PERFORMANCE
#lets see how this cut off works on the test data
#predict probabilities on test set with log trained model
test.pred.prob <- predict(h1B.log, h1B.test, type="prob")
              
test.pred.class <- predict(h1B.log, h1B.test) #predict classes with default .5 cutoff
                  
#calculate performance with confusion matrix
confusionMatrix(test.pred.class, h1B.test$default)
                  
#let draw ROC curve of training and test performance of logit model
test.log.roc<- roc(response= h1B.test$default, predictor=test.pred.prob[[1]]) #assumes postive class Yes is reference level
plot(test.log.roc, legacy.axes=T)
plot(h1B.log.roc, add=T, col="blue")
legend(x=.2, y=.7, legend=c("Test Logit", "Train Logit"), col=c("black", "blue"),lty=1)
                  
#test performance slightly lower than resample
auc(test.log.roc)
auc(h1B.log.roc)
                  
#calculate test confusion matrix using thresholds from resampled data
test.pred.class.newthresh <- factor(ifelse(test.pred.prob[[1]] > h1B.log.Thresh[1], "Yes", "No"))
                
#recalculate confusion matrix with new cut off predictions
confusionMatrix(test.pred.class.newthresh, h1B.test$default)
                  
              
#you have to adjust thresholds when dealing with unbalanced data
#don't ignore cost of FPR or FNR, falsely accusing may be expensive
                  
          
                  
###BONUS PLOTS to calibrate threshold LIFT AND CALIBRATION
#create a lift chart of logit test probabilities against
test.lift <- lift(h1B.test$default ~ test.pred.prob[[1]]) #Lift
plot(test.lift)
                  
test.cal <- calibration(h1B.test$default ~ test.pred.prob[[1]]) #Calibration 
plot(test.cal)
                  
                  
pp.thresh <- glm(h1B.test$default ~ test.pred.prob[[1]], family="binomial")
                  
                  
predict(pp.thresh, as.data.frame(test.pred.prob[[1]]))
              
                  
                  
library("randomForest")
library("party")
library("dplyr")
library("tidyr")
library("sqldf")
library("caret")
library("glmnet")
library("car")
library("ROCR")
                  
h1B <- read.csv("MasterH1BDataset.csv")
                  
h1B_1<-filter(h1B, CASE_STATUS ==c('CERTIFIED','DENIED')  & YEAR == 2016 )
h1B_1<-h1B_1[complete.cases(h1B_1),]
                
h1b.rf<-x22
h1B$CASE_STATUS<-ifelse(h1B,CASE_STATUS %in% c("CERTIFIED"),"1")                 

                  
                  