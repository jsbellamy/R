
##adapted from https://topepo.github.io/caret 
##Data Splitting
install.packages('caret')
install.packages('robustbase')
library('caret')

#always set seed to make code reproducable
set.seed(3456)

#Perserve class distribution and split 80% and 20% subsets
trainIndex <- createDataPartition(iris$Species, p = .8, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

#create training set subset (80%)
irisTrain <- iris[ trainIndex,]

#create training set subset (20%)
irisTest  <- iris[-trainIndex,]

table(irisTrain$Species)
table(irisTest$Species)

install.packages('earth')
#creating Dummy Variables (making factors into numeric form)
library(earth) #lets use the titanic dataset in earth package
data(etitanic)

str(etitanic) #notice factors
head(etitanic)

#model them as dummy variables
head(model.matrix(survived ~ ., data = etitanic))

#to convert to dummy variables use dummyVars to create a model
dummies <- dummyVars(survived ~ ., data = etitanic)

#apply that model with the predict function make sure and save to new data.frame
head(predict(dummies, newdata = etitanic))


#remove variables with zero or near zero variance
data(mdrr) #drug resistance data

data.frame(table(mdrrDescr$nR11)) #number of 11-membered rings have highly inbalanced

#identify variables with near or zero variance
nzv <- nearZeroVar(mdrrDescr, saveMetrics= TRUE) #saving metrics to view variable details
nzv[nzv$nzv,][1:10,]

dim(mdrrDescr) #342 variables to start

nzv <- nearZeroVar(mdrrDescr) #no metrics just return identified columns indexes
nzv

#remove or filter columns with nearZeroVar
filteredDescr <- mdrrDescr[, -nzv]
dim(filteredDescr) #297 columns remaining

##remove highly correlated variables

#first create correlation matrix of variables
descrCor <-  cor(filteredDescr)

#summarize the correlations some very high correlations
summary(descrCor[upper.tri(descrCor)])

#identified variables with correlation .75 or higher
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
highlyCorDescr

#filter out these variables from dataset
filteredDescr <- filteredDescr[,-highlyCorDescr]

#creaet new correlation matrix to verify
descrCor2 <- cor(filteredDescr)
summary(descrCor2[upper.tri(descrCor2)]) #no correlations greater than .75


##Putting it all together
#using the AppliedPredictiveModeling package for the book
library(AppliedPredictiveModeling)
data(schedulingData)
str(schedulingData)

#mix of categorical and numeric predictors
#we can do all preprocessing in one step!

#leaving out class variable
pp_hpc <- preProcess(schedulingData[, -8], 
                     method = c("center", "scale", "BoxCox"))
pp_hpc #can see results of processing

#now we juse need to apply the processing model to the data
transformed <- predict(pp_hpc, newdata = schedulingData[, -8])
head(transformed)

#numpending very low variance
mean(schedulingData$NumPending == 0) #76%=0

#could be an issue lets remove zero variance predictors

pp_no_nzv <- preProcess(schedulingData[, -8], 
                        method = c("center", "scale", "BoxCox", "nzv"))
pp_no_nzv #1 removed

#apply model to data and see new subset and processed variables
predict(pp_no_nzv, newdata = schedulingData[1:6, -8])

#pca and missing data
schedMissing<-schedulingData
#lets insert 4 random variables
schedMissing$Compounds[c(3,6,12,44)] <- c(NA,NA,NA,NA)

#lets process and impute missing values
schedMissing$Compounds[c(3,6,12,44)]

pp_no_impute <- preProcess(schedMissing[, -8], 
                        method = c("knnImpute")) ##automatically scales
pp_no_impute

schedMissing.imputed<-predict(pp_no_impute, newdata = schedMissing)

#view new values created
schedMissing.imputed$Compounds[c(3,6,12,44)]
scale(schedulingData$Compounds[c(3,6,12,44)])

#feature reduction using PCA

pp_no_pca <- preProcess(schedulingData[, -8], 
                        method = c("pca"))
pp_no_pca 

#apply model to data and see new subset and processed variables
head(predict(pp_no_pca, newdata = schedulingData[ -8]))



