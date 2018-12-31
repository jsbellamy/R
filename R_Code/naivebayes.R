#######################################################
# section 7.2 Naïve Bayes
#######################################################



##########################################
# section 7.2.5 Naïve Bayes in R
##########################################
setwd("C:/NextCloud/Academic/SDSU/Courses/2018/Fall/MIS 620/Week 6")
install.packages("e1071") # install package e1071
library(e1071) # load the library

sample <- read.table("sample1.csv",header=TRUE,sep=",")
print(sample)

# read the data into a table from the file
sample <- read.table("sample1.csv",header=TRUE,sep=",")
# define the data frames for the NB classifier
traindata <- as.data.frame(sample[1:14,])
testdata <- as.data.frame(sample[15,])
traindata
testdata

#calculate prior probabilities
tprior <- table(traindata$Enrolls)
tprior
tprior <- tprior/sum(tprior)
tprior


#calculate all conditional probabilities on training data predictors
ageCounts <- table(traindata[,c("Enrolls", "Age")])
ageCounts

ageCounts <- ageCounts/rowSums(ageCounts)
ageCounts

incomeCounts <- table(traindata[,c("Enrolls", "Income")])
incomeCounts <- incomeCounts/rowSums(incomeCounts)
incomeCounts

jsCounts <- table(traindata[,c("Enrolls", "JobSatisfaction")])
jsCounts <- jsCounts/rowSums(jsCounts)
jsCounts

desireCounts <- table(traindata[,c("Enrolls", "Desire")])
desireCounts <- desireCounts/rowSums(desireCounts)
desireCounts

#calculate naive bayes model against test data

prob_yes <-
  ageCounts["Yes",testdata[,c("Age")]]*
  incomeCounts["Yes",testdata[,c("Income")]]*
  jsCounts["Yes",testdata[,c("JobSatisfaction")]]*
  desireCounts["Yes",testdata[,c("Desire")]]*
  tprior["Yes"]

prob_no <-
  ageCounts["No",testdata[,c("Age")]]*
  incomeCounts["No",testdata[,c("Income")]]*
  jsCounts["No",testdata[,c("JobSatisfaction")]]*
  desireCounts["No",testdata[,c("Desire")]]*
  tprior["No"]

prob_yes
prob_no

#choose the higher of the probabilities as selected classification
max(prob_yes,prob_no) #YES

#try it using naivebayes function
model <- naiveBayes(Enrolls ~ Age+Income+JobSatisfaction+Desire,
                    traindata)

# display model
model

# predict with testdata
results <- predict (model,testdata)
# display results
results

# use the NB classifier with Laplace smoothing
model1 = naiveBayes(Enrolls ~., traindata, laplace=1)
# display model
model1

# predict with testdata
results1 <- predict (model1,testdata)
# display results
results1

#rpart

library("rpart")
library("rpart.plot")
model.tree <- rpart(Enrolls ~ Age+Income+JobSatisfaction+Desire,
                 traindata)
predict (model.tree,testdata)
rpart.plot(model.tree, type=4, extra=2, clip.right.labs=FALSE, varlen=0, faclen=3)



