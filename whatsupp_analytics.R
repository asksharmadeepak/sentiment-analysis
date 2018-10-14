#load Required Packages
library(ggplot2)
library(tm)
library(wordcloud)
library(syuzhet)


# get the date from whatsup chat 
texts <- readLines("WhatsAppChat.txt")

#let us create corpus

corpusData <- Corpus(VectorSource(texts))

               
#clean our chat data 
trans <- content_transformer(function(x , pattern) gsub(pattern , " ", x))
corpusData <- tm_map(corpusData , trans , "/")
corpusData <- tm_map(corpusData , trans , "@")
corpusData <- tm_map(corpusData , trans , "\\|")
corpusData <- tm_map(corpusData , content_transformer(tolower))
corpusData <- tm_map(corpusData , removeNumbers)
corpusData <- tm_map(corpusData , removeWords, stopwords("english"))
corpusData <- tm_map(corpusData , removePunctuation)
corpusData <- tm_map(corpusData , stripWhitespace)
corpusData <- tm_map(corpusData , stemDocument)

#create document term matrix
dtm <- TermDocumentMatrix(corpusData)
mat <- as.matrix(dtm)
v <- sort(rowSums(mat), decreasing = TRUE)


#Data Frame

d <- data.frame(word = names(v), freq=v)
head(d , 10)

#generate the word cloud 
set.seed(1056)
wordcloud(words = d$word, freq= d$freq, min.freq =1 , 
          max.words =200, random.order = FALSE, rot.per = 0.35,
          colors=brewer.pal(8 , "Dark2"))


#Fetch sentiments words from texts
sentiment <- get_nrc_sentiment(texts)
text <- cbind(texts, sentiment)


#count the sentiment word by category
TotalSentiment <- data.frame(colSums(text[,c(2:11)]))
names(TotalSentiment) <- "count"
TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment), TotalSentiment)
rownames(TotalSentiment) <- NULL


#total sentiment score of all texts
ggplot(data = TotalSentiment, aes(x = sentiment, y = count)) +  
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment score ")













































