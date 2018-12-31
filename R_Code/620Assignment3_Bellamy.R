library(tm) #for corpus and term document matrix creation/processing
library(SnowballC) #for stemming
library(wordcloud)
library(cluster)
library(rpart)
library(readr)

spam <- read_tsv(file = 'SMSSpamCollection.tsv')

spam$Text <- as.character(spam$Text)
View(spam)

head(spam$Text, 20)

#remove any letters repeated more than twice (eg. hellooooooo -> helloo)
spam$Text  <- gsub('([[:alpha:]])\\1+', '\\1\\1', spam$Text)

#remove html entitties like &quot; starting with 
#note perfect but we will remove remaining punctation at later step
spam$Text<-gsub("\\&\\w*;","", spam$Text)

#additional cleaning removing leaving only letters numbers or spaces
spam$Text <- gsub("[^a-zA-Z0-9 ]","",spam$Text)

#create corpus and clean up text before creating docu ent term matrix
spam_Corpus <- Corpus(VectorSource(spam$Text))

spam_Corpus <- tm_map(spam_Corpus, stemDocument)
spam_Corpus <- tm_map(spam_Corpus, removePunctuation)
spam_Corpus <- tm_map(spam_Corpus, removeNumbers)
spam_Corpus <- tm_map(spam_Corpus, removeWords, stopwords("english"))
spam_Corpus <- tm_map(spam_Corpus, stripWhitespace)  

#create term document matrix (terms as rows, documents as columns)
tdm <- TermDocumentMatrix(spam_Corpus)

tdm$nrow 

inspect(tdm[1:30, 1:30])

tdm <- removeSparseTerms(tdm, 0.98)

tdm$nrow #now 59 terms
tdm$ncol #5568 msgs

inspect(tdm[1:59, 1:3])

inspect(tdm)

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
findAssocs(tdm, terms = c("call", "now"), corlimit = .0)

findAssocs(tdm, terms = c("come"), corlimit = .0) 

#lets cluster the documents, but first find optimal k
wss <- numeric(15) 
for (k in 1:10) wss[k] <- sum(kmeans(tdm, centers=k)$withinss)
plot(wss, type="b") #seems like 2 or 3 will cover it

spam.kmeans <- kmeans(tdm,3)
spam.kmeans$cluster #lets looks at cluster membership

tdm$cluster <- spam.kmeans$cluster
length(tdm[tdm$cluster==1,])
tdm[tdm$cluster==1,]$words
View(spam.kmeans)
