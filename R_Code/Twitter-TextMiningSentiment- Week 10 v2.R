
setwd("C:/NextCloud/Academic/SDSU/Courses/2018/Fall/MIS 620/Week 10/")

library(tm) #for corpus and term document matrix creation/processing
library(SnowballC) #for stemming
library(wordcloud)
library(cluster)
library(rpart)

#read in subset of twitter
#stratified random sampled 68000 cases (equal positive and negative classes)
#saved as rdata file to save space and decrease loading times
twitter.sub <- read.csv("twittersubset.csv")

#convert tweet text to character, read.csv defaults to Factor
twitter.sub$Text <- as.character(twitter.sub$Text)



#gsub tutorial
#gsub looks for pattern and replaces gsub(pattern, replacement, text)
gsub("!", "", c("hi!", "hi hi hi!!!"))


str(twitter.sub)

head(twitter.sub$Text, 20)

#convert to text multibyte encoding to UTF form
#this was neccesary after importing on Ubuntu Server, but might not be for you
#encoding differences will often need to reconciled between platforms and editors
twitter.sub$Text <- iconv(twitter.sub$Text, to="utf-8",sub="")

##regular expression
## remove letters, digits, and punctuation haracters starting with @ remove usernames and replace with "USER"
twitter.sub$Text <- gsub("@\\w*"," USER",   twitter.sub$Text)




##Remove website links and replace with "URL"
twitter.sub$Text  <- gsub("http[[:alnum:][:punct:]]*"," WEBADDRESS",   tolower(twitter.sub$Text ))
twitter.sub$Text  <- gsub("www[[:alnum:][:punct:]]*"," WEBADDRESS",   tolower(twitter.sub$Text ))

#remove html entitties like &quot; starting with 
#note perfect but we will remove remaining punctation at later step
twitter.sub$Text<-gsub("\\&\\w*;","", twitter.sub$Text)


#remove any letters repeated more than twice (eg. hellooooooo -> helloo)
twitter.sub$Text  <- gsub('([[:alpha:]])\\1+', '\\1\\1', twitter.sub$Text)

#additional cleaning removing leaving only letters numbers or spaces
twitter.sub$Text <- gsub("[^a-zA-Z0-9 ]","",twitter.sub$Text)

#review tweets now
head(twitter.sub$Text,20)

#list of stopwords
stopwords("english")


#create corpus and clean up text before creating docu ent term matrix
Twitter_Corpus <- Corpus(VectorSource(twitter.sub$Text))

Twitter_Corpus <- tm_map(Twitter_Corpus, stemDocument)
Twitter_Corpus <- tm_map(Twitter_Corpus, removePunctuation)
Twitter_Corpus <- tm_map(Twitter_Corpus, removeNumbers)
Twitter_Corpus <- tm_map(Twitter_Corpus, removeWords, stopwords("english"))
Twitter_Corpus <- tm_map(Twitter_Corpus, stripWhitespace)  

  

#create term document matrix (terms as rows, documents as columns)
tdm <- TermDocumentMatrix(Twitter_Corpus)

#count row (i.e, terms)
#must convert to matrix to work with as dtm is stored as a memory efficient sparse matrix doesn't store
#empty fields
tdm$nrow 

#inspect the term document matrix, make sure to subset it is very large 
inspect(tdm[1:30, 1:30])

#there are over 30,000 terms and very high sparsity lets trim down and remove terms
#remove words that are over 98% sparse (i.e., do not appear in 98% of documents)
tdm <- removeSparseTerms(tdm, 0.98)
tdm$nrow #now 46 terms
tdm$ncol #51,800 tweets
inspect(tdm[1:46, 1:3])

inspect(tdm)


#now thats its manageable in size (the original dtm saved as a regular matrix requires 32GB of memory)

# define tdm as matrix
m = as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
d #lets see frequency of words

# plot wordcloud
set.seed(1)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#user (code for  @user included in tweet most frequent lets see what it is associated with)
#and web address 
findAssocs(tdm, terms = c("user", "webaddress"), corlimit = .0)

findAssocs(tdm, terms = c("love"), corlimit = .0) 

#normally set a limit for correlations to a reasonable r size, bui this is sparse data and we trimmed terms



#lets make a bar chart of frequent words
barplot(d[2:10,]$freq, las = 2, names.arg = d[2:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

#lets cluster the documents, but first find optimal k
wss <- numeric(15) 
for (k in 1:10) wss[k] <- sum(kmeans(tdm, centers=k)$withinss)
plot(wss, type="b") #seems like 2 or 3 will cover it

twitter.kmeans <- kmeans(tdm,2)
twitter.kmeans$cluster #lets looks at cluster membership

tdm$cluster <- twitter.kmeans$cluster
length(tdm[tdm$cluster==1,]$words)
tdm[tdm$cluster==2,]$words

#lets do some sentiment analysis with each word as a column
#document term matrix, 



#create Document Term Matrix (so terms are columns or attributes)
#create Document Term Matrix (so terms are columns or attributes)
dtm <- DocumentTermMatrix(Twitter_Corpus)
dtm <- removeSparseTerms(dtm, 0.98) #remove sparse terms
dtm
inspect(dtm[1:4,1:10])

#before looking at classifications, lets do some basic LDA topic modeling with gibbs sampling

library(topicmodels)

#filter tweets that have no words after processing
ui = unique(dtm$i)
dtm.nonempty <- dtm[ui,] #createing new document term matrix with only tweets with words


#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm.nonempty,k=5, method="Gibbs")


#top 10 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,10))


#CLASSIFICATION

#convert to matrix
labeledTerms <- as.data.frame(as.matrix(dtm))
labeledTerms$Sentiment <- twitter.sub$Sentiment #merge with labels

table(labeledTerms$Sentiment)


#model training on training set (first 50000 rows are balanced training set)
library(rpart)
library(rpart.plot)

#train decision tree on sentiment
sentiment.rpart = rpart(Sentiment ~., data=labeledTerms[1:50000,], method="class")
rpart.plot(sentiment.rpart, type=4, extra=2, clip.right.labs=FALSE, varlen=0, faclen=3)

#evaluate performance on test set

#performance testing on test set---------------
#prediction on of test set 
p.rpart <- predict(sentiment.rpart, labeledTerms[50001:51800,-47])

library(ROCR)

score <- p.rpart[, c("Positive")]
actual_class <- labeledTerms[50001:51800,]$Sentiment == 'Positive'
pred <- prediction(score, actual_class)
perf <- performance(pred, "tpr", "fpr")

plot(perf, lwd=2, xlab="False Positive Rate (FPR)",
     ylab="True Positive Rate (TPR)")
abline(a=0, b=1, col="gray50", lty=3)

## corresponding AUC score
auc <- performance(pred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc #not bad AUC of .6

#lets make a confusion matrix using caret
library(caret)
library(doParallel)

y.train <- labeledTerms[1:50000,]$Sentiment
x.train <- labeledTerms[1:50000,-47] 


y.test <- labeledTerms[50001:51800,]$Sentiment
x.test <- labeledTerms[50001:51800,-47] 

##Setup cluster
cl <- makeCluster(4)
registerDoParallel(cl)

#if you see an error when using parallel referencing "optimismBoot"
#its a bug in the current version of caret 6.0.77
#https://github.com/topepo/caret/issues/706
#install the dev release of caret for fix using:
#devtools::install_github('topepo/caret/pkg/caret') 

#set cv validation folds
set.seed(199)
final.folds<- createFolds(y=y.train,k=10) 

#this time we are creating our folds in advanced to ensure all models use same folds during training
#for apples to apples comparisons
ctrl <- trainControl(method = "cv", number=10, summaryFunction=twoClassSummary,
                     indexOut =final.folds,
                     classProbs=T, savePredictions=T) #saving predictions from each resample fold


set.seed(192)
m.rpart.15 <- train(y=y.train, x=x.train,
                    trControl = ctrl,
                    metric = "ROC", #using AUC to find best performing parameters
                    tuneLength=15, #search through 15 different complexity parameters for pruning (150 models, 10 CV x 15 parameters)
                    method = "rpart")
m.rpart.15
getTrainPerf(m.rpart.15)
plot(m.rpart.15)
p.rpart15<- predict(m.rpart.15,x.test)
confusionMatrix(p.rpart15,y.test) #calc accuracies with confuction matrix on test set





##Naive Bayes
modelLookup("nb") #we have some paramters to tune such as laplace correction
set.seed(192)
m.nb <- train(y=y.train, x=x.train,
              trControl = ctrl,
              metric = "ROC", #using AUC to find best performing parameters
              method = "nb")
m.nb
getTrainPerf(m.nb)

varImp(m.nb)
plot(m.nb)
p.nb<- predict(m.nb,x.test)
confusionMatrix(p.nb,y.test) #calc accuracies with confuction matrix on test set



##Logistic Regression (no parameters here, but will get cross validated perfomrance measures)
modelLookup("glm")
set.seed(192)
m.glm<- train(y=y.train, x=x.train,
              trControl = ctrl,
              metric = "ROC", #using AUC to find best performing parameters
              method = "glm")
m.glm

getTrainPerf(m.nb)

p.glm<- predict(m.glm,x.test)
confusionMatrix(p.glm,y.test)


#random forest
modelLookup("rf")
set.seed(192)

#manually specify randform parameters to search through
rf.grid <- expand.grid(mtry=c(7,14,20,30, 40))
m.rf<- train(y=y.train, x=x.train,
             trControl = ctrl, 
             tuneGrid=rf.grid,
             metric = "ROC", #using AUC to find best performing parameters
             method = "rf")
m.rf

getTrainPerf(m.rf)

p.rf<- predict(m.rf,x.test)
confusionMatrix(p.rf,y.test)

#make sure to clean up the cluster objects when finished
stopCluster(cl)

#compare training performance
#create list of cross validation runs (resamples)
rValues <- resamples(list(rpart=m.rpart, randomforest=m.rf, naivebayes=m.nb, logistic=m.glm))

summary(rValues)

#create plot comparing them
bwplot(rValues, metric="ROC")
bwplot(rValues, metric="Sens") #Sensitvity
bwplot(rValues, metric="Spec")