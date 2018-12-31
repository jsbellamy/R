#######################################################
# section 7.1 Decision Trees
#######################################################

install.packages("rpart.plot") # install package rpart.plot
?rpart.plot
library
##########################################
# section 7.1.1 Overview of a Decision Tree
##########################################

library("rpart")
library("rpart.plot")

# Read the data
#set working folder to location of data files
setwd("C:/NextCloud/Academic/SDSU/Courses/2018/Fall/MIS 620/Week 5")

banktrain <- read.table("bank-sample.csv",header=TRUE,sep=",")

## drop a few columns to simplify the tree
drops<-c("age", "balance", "day", "campaign", "pdays", "previous", "month")
banktrain <- banktrain [,!(names(banktrain) %in% drops)]
summary(banktrain)


#review root prob/entropy
table(banktrain$subscribed)

1789/2000 #subscribed no
211/2000  #subscribed yes

#base entropy
-(.8945*log2(.8945) + .1055*log2(.1055))

# Make a simple decision tree by only keeping the categorical variables
fit <- rpart(subscribed ~ job + marital + education + default + housing + loan + contact + poutcome, 
             method="class", 
             data=banktrain,
             control=rpart.control(minsplit=1),
             parms=list(split='information'))
summary(fit)

#use basic rpart ploting
plot(fit)
text(fit, pretty =0) #still needs work and not great to look at

# Plot the tree using better plot function
#shows predicated class, predicted probability of subscription, percentage of observation in node
rpart.plot(fit)


#lets prune the tree to avoid overfitting using cross validation

printcp(fit) #display crossvalidated error for each tree size
plotcp(fit) #plot cv error

#select Complexity parameter (CP) with lowest crossvalidated error 
#cp restricts the size of tree based on the cost of adding a node being less than cp value


#we can grab this from the plotcp table automatically with 
opt.cp <- fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]

#lets prune the tree
fit.pruned <- prune(fit, cp=opt.cp)

#lets review the final tree
rpart.plot(fit.pruned)


##########################################
# section 7.1.2 The General Algorithm
##########################################

# Entropy of coin flips
set.seed(100)
x <- sort(runif(1000))
y <- data.frame(x=x, y=-x*log2(x)-(1-x)*log2(1-x))
plot(y, type="l", xlab="P(X=1)", ylab=expression("H"["X"]))
grid()

# include a numeric variable "duration" into the model
fit <- rpart(subscribed ~ job + marital + education + default + housing + loan + contact + duration + poutcome, 
             method="class", 
             data=banktrain,
             control=rpart.control(minsplit=1),
             parms=list(split='information'))
summary(fit)
# Plot the tree
rpart.plot(fit, type=4, extra=2, clip.right.labs=FALSE, varlen=0, faclen=3)

summary(fit)

# Predict
newdata <- data.frame(job="retired", 
                      marital="married", 
                      education="secondary",
                      default="no",
                      housing="yes",
                      loan="no",
                      contact = "cellular",
                      duration = 598,
                      poutcome="unknown")
newdata
predict(fit,newdata=newdata,type=c("prob"))


##########################################
# section 7.1.5 Decision Trees in R
##########################################

library("rpart") # load libraries
library("rpart.plot")

# Read the weather data


play_decision <- read.csv("DTdata.csv")
str(play_decision)



library(rpart)
library(rpart.plot)

play_decision <- read.table("DTdata.csv", header=TRUE, sep=",")
play_decision
summary(play_decision)

# build the decision tree
fit <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
             method="class",
             data=play_decision,
             control=rpart.control(minsplit=1),
             parms=list(split='information'))
summary(fit)

?rpart.plot
rpart.plot(fit, type=4, extra=1)
rpart.plot(fit, type=4, extra=1)
rpart.plot(fit, type=4, extra=2, clip.right.labs=FALSE,
           varlen=0, faclen=0)
rpart.plot(fit)


newdata <- data.frame(Outlook="rainy", Temperature="mild",
                      Humidity="high", Wind=FALSE)
newdata

predict(fit,newdata=newdata,type="prob")
predict(fit,newdata=newdata,type="class")

#Training data perf
predict(fit, newdata=play_decision, type="prob")
predclass <- predict(fit, newdata=play_decision, type="class")

table(play_decision$Play, predclass)

