library(tm)
library(wordcloud)
library(stringr)
library(readr)
library(dplyr)
library(ggplot2)
library(syuzhet)
library(readxl)
library(syuzhet)
#library(twitteR)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggridges)
library(lubridate)
library(rtweet)
library(maps)
#install.packages("quanteda")
library(quanteda)
library(tidyverse)
library(wordcloud2)
library(RColorBrewer)
library(devtools)
# install_github("lchiffon/wordcloud2")
library(tm)
library(RTextTools)
library(readr)
library(e1071)
library(dplyr)
library(caret)
# install.packages("naivebayes")
library(naivebayes)
#install.packages("Rcpp")
# install.packages("RTextTools")
library(RTextTools)
library(SnowballC)
library(RColorBrewer)
library(ggplot2)
library(caTools)
library(rpart)
# install.packages("rpart.plot")
library(rpart.plot)
#install.packages('e1071', dependencies=TRUE)
# Install
# install.packages("tm")  # for text mining
# install.packages("SnowballC") # for text stemming
# install.packages("wordcloud") # word-cloud generator 
# install.packages("RColorBrewer") # color palettes
# install.packages("syuzhet") # for sentiment analysis
# install.packages("ggplot2") # for plotting graphs
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")
#install.packages('caret', dependencies=TRUE)


# Function to extract Hashtags and there frquency

extract.hashTag = function(Tweets){
  
  hash.pattern = "#[[:alpha:]]+"
  have.hash = grep(x = Tweets, pattern = hash.pattern)
  
  hash.matches = gregexpr(pattern = hash.pattern,
                          text = Tweets[have.hash])
  extracted.hash = regmatches(x = Tweets[have.hash], m = hash.matches)
  
  Data = data.frame(table(tolower(unlist(extracted.hash))))
  colnames(Data) = c("tag","freq")
  Data = Data[order(Data$freq,decreasing = TRUE),]
  return(Data)
}

#function to pre Process tweets

Preprocess.text = function(Tweets){
  Tweets2<-gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",Tweets)
  Tweets2<-gsub("http[^[:blank:]]+","",Tweets2)
  Tweets2<-gsub("@\\w+","",Tweets2)
  Tweets2<-gsub('[[:punct:]]', '',Tweets2)
  #Tweets2<-gsub("[^[:alnum:]]","",Tweets2)
  text<-tolower(Tweets2)
  #text<-data.frame(text)
  return(text)
}
#tweet preprocessing for Rahul Gandhi.


R_Gandhi.DF <- data.frame(read.csv("C:/Users/super/Downloads/english #RahulGandhi.csv", header = TRUE))
R_Gandhi.DF
R_Gandhi.DF$Tweet<-gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",R_Gandhi.DF$Tweet)
R_Gandhi_HashTags<-extract.hashTag(R_Gandhi.DF$Tweet)

tweet<-Preprocess.text(R_Gandhi.DF$Tweet)
R_Gandhi_Sent.matrix<-get_nrc_sentiment(tweet)
R_Gandhi_SM<-data.frame(colSums(R_Gandhi_Sent.matrix))
colnames(R_Gandhi_SM)<- "Score"
R_Gandhi_SM<- cbind("Sentiment"=rownames(R_Gandhi_SM),R_Gandhi_SM)
rownames(R_Gandhi_SM)<-NULL
R_Gandhi_SM
#write.csv(R_Gandhi_SM,"C:\\Users\\super\\OneDrive\\Desktop\\semester 2\\R_Gandhi_SM.csv", row.names = FALSE)



# Sentiment Analysis of user tweet on Rahul Gandhi.

ggplot(data=R_Gandhi_SM,aes(x=Sentiment,y=Score))+
  geom_bar(aes(fill=Sentiment), stat = "identity",width = .9)+
  theme(legend.position = "none")+
  xlab("Sentiment")+ylab("Score")+
  ggtitle("\nSentiment Analysis of user tweet on Rahul Gandhi.\n") 



# Class of user tweet on Rahul Gandhi.
sent.value<- get_sentiment(tweet)
#R_Gandhi.DF<-cbind(R_Gandhi.DF,R_Gandhi_Sent.matrix,sent.value)
#R_Gandhi.DF
#write.csv(R_Gandhi.DF,"C:\\Users\\super\\OneDrive\\Desktop\\R_Gandhi.DF.csv", row.names = FALSE)
indicFunc = function(value){
  if (value<=0){
    return (0)
  }
  else{
    return (1)}
}

sent.value<- get_sentiment(tweet)

sent2.value<-c()
for (item in sent.value)
{
  sent2.value<-append(sent2.value, indicFunc(item))
}

R_Gandhi.DF<-cbind(R_Gandhi.DF,R_Gandhi_Sent.matrix,sent.value, sent2.value)
#write.csv(R_Gandhi.DF,"C:\\Users\\super\\OneDrive\\Desktop\\R_Gandhi.DF.csv", row.names = FALSE)
Tweet_Class<-c("Postive","Negative","Neutral")
R_Gandhi.DF<- mutate(R_Gandhi.DF,class=sapply(R_Gandhi.DF$sent.value,
                                              function(x){
                                                if(x==0){
                                                  x<-"Neutral"
                                                  return(x)
                                                }
                                                ifelse(x<0,"Negative","Postive")
                                              }))
write.csv(R_Gandhi.DF,"C:\\Users\\super\\OneDrive\\Desktop\\R_Gandhi.DF.csv", row.names = FALSE)
Class_RG<-cbind(Tweet_Class,"Score"=c(sum(str_count(R_Gandhi.DF$class,"Postive")),
                                      sum(str_count(R_Gandhi.DF$class,"Negative")),
                                      sum(str_count(R_Gandhi.DF$class,"Neutral"))))
Class_RG<-as.data.frame(Class_RG)
Class_RG
ggplot(data=Class_RG,aes(x=Tweet_Class,y=Score))+
  geom_bar(aes(fill=Tweet_Class), stat = "identity",width = .8)+
  theme(legend.position = "none")+
  xlab("Sentiment")+ylab("Score")+
  labs(title="\n Class of user tweet on Rahul Gandhi. \n")
# word_cloud for the user tweet on Rahul Gandhi.

temp<-Preprocess.text(R_Gandhi.DF$Tweet)

docs <- Corpus(VectorSource(temp))
docs <- tm_map(docs, removeWords, 
               c("the", "a", "have", "is", "i","are", "will","and","that","his","with",
                 "and",  "that", "his","for","this" ,"but","who" , "not","has","had","was",
                 "were")) 
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq,scale=c(3,.3),min.freq = 1,
           max.words=200, random.order=FALSE, rot.per=0.25, 
           colors=brewer.pal(8, "Dark2"), shape = "traingle")
data=R_Gandhi.DF
head(data)

library(tidyverse) 
datas=data%>%select(Tweet,class)
head(datas) 
round(prop.table(table(datas$class)),2)
library(tm) 
library(SnowballC) 
corpus = VCorpus(VectorSource(datas$Tweet)) 
corpus = tm_map(corpus, content_transformer(tolower)) 
corpus = tm_map(corpus, removeNumbers) 
corpus = tm_map(corpus, removePunctuation) 
corpus = tm_map(corpus, removeWords, stopwords("english")) 
corpus = tm_map(corpus, stemDocument) 
corpus = tm_map(corpus, stripWhitespace) 
as.character(corpus[[1]])

dtm = DocumentTermMatrix(corpus) 
dtm
DTM=as.matrix(dtm) #Converting into a double matrix
DTM.totalfreq=colSums(DTM) # calculate the term frequencies
summary(DTM.totalfreq) #summary calculation
freq.matrix_RG <- as.matrix(dtm)
#freq.matrix
term.freq_RG <- sort(rowSums(freq.matrix_RG), decreasing = TRUE)
#term.freq
term.df_RG <- data.frame(term=names(term.freq_RG), freq=term.freq_RG)
#str(term.df)
plot_RG <- ggplot(subset(term.df_RG, term.df_RG$freq > 100 & term.df_RG$freq < 200), aes(term, freq, fill=freq)) + geom_bar(stat='identity') + labs(x='Terms', y='Count', title='Term Frequencies') 
plot_RG + coord_flip()
dim(dtm) 
dtm = removeSparseTerms(dtm, 0.999) 
dim(dtm)

#Finding out the most frequent words in Tweets 
#Sort by descending value of frequency
DTM_V=sort(DTM.totalfreq,decreasing=TRUE)
DTM_D=data.frame(word = names(DTM_V),freq=DTM_V)
# Display the top 10 most frequent words
head(DTM_D, 10)
# Plot the most frequent words
barplot(DTM_D[1:10,]$freq, las = 2, names.arg = DTM_D[1:10,]$word,
        col ="lightgreen", main ="Top 10 most frequent words in Tweets",
        ylab = "Word frequencies")

# Find associations 
findAssocs(DocumentTermMatrix(corpus), terms = c("narendramodi","bjp"), corlimit = 0.25)
#This script shows which words are associated with the most frequent words "narendramodi" and "bjp" with minimum correlation 25%.

#wordcloud requires RColorBrewer 
#positive <- subset(datas,class=="Positive") 
#wordcloud(positive$tweet, max.words = 100, colors = "blue") 
#negative <- subset(datas,class=="Negative") 
#wordcloud(negative$text, max.words = 100, colors = "purple") 
#neutral <- subset(datas,class=="Neutral") 
#wordcloud(neutral$text, max.words = 100, colors = "turquoise")


#preparing data for naive bayes
#As naive bayes algorithm excepts binary 
convert <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}  

datanaive = apply(dtm, 2, convert)

dataset = as.data.frame(as.matrix(datanaive))    
dataset$Class = datas$class
str(dataset$Class)
#write.csv(dataset,"C:/Users/Shridipta/Desktop/dataset.csv", row.names = FALSE)


#Data splitting
set.seed(31)
split = sample(2,nrow(dataset),prob = c(0.75,0.25),replace = TRUE)
train_set = dataset[split == 1,]
test_set = dataset[split == 2,] 

prop.table(table(train_set$Class))
prop.table(table(test_set$Class))


library(e1071)
library(caret)
#control= trainControl(method="repeatedcv", number=10, repeats=2)
#system.time( classifier_nb <- naiveBayes(train_set, train_set$Class, laplace = 1,trControl = control,tuneLength = 5) )


system.time( classifier_nb <- naiveBayes(train_set, train_set$Class, laplace = 1))
#system.time( NB_classifier <- naiveBayes(train_set, train_set$Class, laplace = 1) )
system.time( NB_pred <- predict( classifier_nb, newdata=test_set) )
table("Predictions"= NB_pred,  "Actual" = test_set$Class)
NB_conf.mat <- NA
u <- union(NB_pred, test_set$Class)
t <- table(factor(NB_pred, u), factor(test_set$Class, u))
confusionMatrix(t)
confusionMatrix(t)$overall['Accuracy']








