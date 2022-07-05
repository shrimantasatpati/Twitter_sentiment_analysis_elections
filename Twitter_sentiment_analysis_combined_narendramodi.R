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

#tweet preprocessing for narendramodi


N_modi.DF <- data.frame(read.csv("C:/Users/super/Downloads/english #narendramodi.csv", header = TRUE))
N_modi.DF$Tweet<-gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",N_modi.DF$Tweet)
N_modi_HashTags<-extract.hashTag(N_modi.DF$Tweet)


tweet<-Preprocess.text(N_modi.DF$Tweet)
N_modi_Sent.matrix<-get_nrc_sentiment(tweet)
N_modi_SM<-data.frame(colSums(N_modi_Sent.matrix))
colnames(N_modi_SM)<- "Score"
N_modi_SM<- cbind("Sentiment"=rownames(N_modi_SM),N_modi_SM)
rownames(N_modi_SM)<-NULL
#write.csv(N_modi_SM,"C:\\Users\\super\\OneDrive\\Desktop\\semester 2\\N_modi_SM.csv", row.names = FALSE)


# Sentiment Analysis of user tweet on narendra modi
ggplot(data=N_modi_SM,aes(x=Sentiment,y=Score))+
  geom_bar(aes(fill=Sentiment), stat = "identity",width = .9)+
  theme(legend.position = "none")+
  xlab("Sentiment")+ylab("Score")+
  ggtitle("\nSentiment Analysis of user tweet on narendra modi\n") 

# Class of user tweet on Narendra Modi.
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

N_modi.DF<-cbind(N_modi.DF,N_modi_Sent.matrix,sent.value, sent2.value)


Tweet_Class<-c("Postive","Negative","Neutral")
N_modi.DF<- mutate(N_modi.DF,class=sapply(N_modi.DF$sent.value,
                                          function(x){
                                            if(x==0){
                                              x<-"Neutral"
                                              return(x)
                                            }
                                            ifelse(x<0,"Negative","Postive")
                                          }))

write.csv(N_modi.DF,"C:/Users/super/OneDrive/Desktop/N_Modi.DF.csv", row.names = FALSE)

Class_NM<-cbind(Tweet_Class,"Score"=c(sum(str_count(N_modi.DF$class,"Postive")),
                                      sum(str_count(N_modi.DF$class,"Negative")),
                                      sum(str_count(N_modi.DF$class,"Neutral"))))
Class_NM<-as.data.frame(Class_NM)
ggplot(data=Class_NM,aes(x=Tweet_Class,y=Score))+
  geom_bar(aes(fill=Tweet_Class), stat = "identity",width = .8)+
  theme(legend.position = "none")+
  xlab("Sentiment")+ylab("Score")+
  labs(title="\n Class of user tweet on Narendra Modi.\n")




# word_cloud for the user tweet on Narendra Modi. 

temp_NM<-Preprocess.text(N_modi.DF$Tweet)

docs_NM <- Corpus(VectorSource(temp_NM))
docs_NM <- tm_map(docs_NM, removeWords, 
                  c("the", "a", "have", "is", "i","are", "will","and","that","his","with",
                    "and",  "that", "his","for","this" ,"but","who" , "not","has","had","was",
                    "were")) 
dtm_NM <- TermDocumentMatrix(docs_NM)
m_NM <- as.matrix(dtm_NM)
v_NM <- sort(rowSums(m_NM),decreasing=TRUE)
d_NM <- data.frame(word = names(v_NM),freq=v_NM)
set.seed(1234)
wordcloud(words = d_NM$word, freq = d_NM$freq,scale=c(3,.3),min.freq = 1,
           max.words=200, random.order=FALSE, rot.per=0.25, 
           colors=brewer.pal(8, "Dark2"), shape = "traingle")



data=N_modi.DF
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
freq.matrix_NM <- as.matrix(dtm)
#freq.matrix
term.freq_NM <- sort(rowSums(freq.matrix_NM), decreasing = TRUE)
#term.freq
term.df_NM <- data.frame(term=names(term.freq_NM), freq=term.freq_NM)
#str(term.df)
plot_NM <- ggplot(subset(term.df_NM, term.df_NM$freq > 100 & term.df_NM$freq < 200), aes(term, freq, fill=freq)) + geom_bar(stat='identity') + labs(x='Terms', y='Count', title='Term Frequencies') 
plot_NM + coord_flip()
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
#positive <- subset(datas,sentiment=="Positive") 
#wordcloud(positive$text, max.words = 100, colors = "blue") 
#negative <- subset(datas,sentiment=="Negative") 
#wordcloud(negative$text, max.words = 100, colors = "purple") 
#neutral <- subset(datas,sentiment=="Neutral") 
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
# model evaluation

nb_pred = predict(classifier_nb, type = 'class', newdata =  test_set)
confusionMatrix(factor(NB_pred, u), factor(test_set$Class, u))




















































































