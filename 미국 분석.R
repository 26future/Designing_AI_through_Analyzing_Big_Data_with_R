#USA-- Before 2.10
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("wordcloud2")

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(stringr)


#corpus
text_location<-'C:/Users/Wonseok/Documents/R/HMR/Before 2.10/USA/usa'
corpus_usa_before<-VCorpus(DirSource(text_location))

#preprocessing, dtm
usa_before_dtm<-DocumentTermMatrix(corpus_usa_before, control = list(tolower =TRUE, 
        stopwords = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, stemming = TRUE))
                                    

#word cloud
matrix<-as.matrix(usa_before_dtm) 
words<-sort(colSums(matrix),decreasing=TRUE) 
df<-data.frame(word=names(words),freq=words, index=1, stringsAsFactors = FALSE)

df<-df[-c(1,8),]  #said, the

View(df)
print(df$word)

set.seed(1004)
wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words=200, random.order=FALSE, 
          rot.per=0.35, colors=brewer.pal(8, "Dark2"))

#sentiments-bing
install.packages("tidytext")
install.packages("dplyr")
install.packages("ggplot2")
library(tidytext)
library(dplyr)
library(ggplot2)

usa_sentiments_before<-df %>%
  inner_join(get_sentiments("bing")) %>% 
  count(sentiment)
usa_sentiments_before<-as.data.frame(usa_sentiments_before)
usa_sentiments_before

#merge-bing
#usa_sentiment<-merge(usa_sentiments_before,usa_sentiments_after, by="sentiment")
#colnames(usa_sentiment)=c("sentiment","before", "after")
#usa_sentiment

ggplot(data=usa_sentiments_before, aes(x=c("negative","positive"), y=n, fill=c("neg","pos")))+
  ggtitle("USA")+
  xlab("before 2.10")+
  ylab("")+
  ylim(c(0,700))+
  theme(legend.position = "none")+
  coord_fixed(ratio =0.009)+
  geom_bar(stat = "identity", position = "dodge")




#sentiments-nrc
install.packages("syuzhet")
library(syuzhet)
install.packages("textdata")
library(textdata)


usa_nrc_before<-df %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(sentiment)
usa_nrc_before

usa_nrc_before<-as.data.frame(usa_nrc_before)
usa_nrc_before<-arrange(usa_nrc_before, desc(n))



ggplot(data=usa_nrc_before, aes(x=reorder(sentiment,-n), y=n))+
  ggtitle("USA")+
  xlab("before 2.10")+
  ylab("")+
  ylim(c(0,800))+
  theme(legend.position = "none")+
  #coord_fixed(ratio =0.009)+
  geom_bar(stat = "identity", position = "dodge")




#tf-idf

usa_before_freq<-DocumentTermMatrix(corpus_usa_before, control = list(tolower =TRUE, 
                                                                     stopwords = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, stemming = TRUE,
                                                                     weighting = function(x) weightTfIdf(x, normalize = TRUE)))

removeSparseTerms(usa_before_freq, as.numeric(0.99))  
word_freq_before<- colSums(as.matrix(usa_before_freq))
word_freq_before <- data.frame(word_freq_before)

word_freq_before$words<-rownames(word_freq_before)
word_freq_before<-arrange(word_freq_before,desc(word_freq_before))

View(word_freq_before)
#####################################################################################
######################################################################################
#after 2.10

#corpus
text_location1<-'C:/Users/Wonseok/Documents/R/HMR/After 2.10/USA/usa'
corpus_usa_after<-VCorpus(DirSource(text_location1))

#preprocessing, dtm
usa_after_dtm<-DocumentTermMatrix(corpus_usa_after, control = list(tolower =TRUE, 
       stopwords = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, stemming = TRUE))

#word cloud
matrix<-as.matrix(usa_after_dtm) 
print(matrix)
words<-sort(colSums(matrix),decreasing=TRUE) 
df1<-data.frame(word=names(words),freq=words, index=1, stringsAsFactors = FALSE)
View(df)


df1<-df1[-c(1,5,8),]  #said, the, will


set.seed(1004)
wordcloud(words = df1$word, freq = df1$freq, min.freq = 1, max.words=200, random.order=FALSE, 
          rot.per=0.35, colors=brewer.pal(8, "Dark2"))

#sentiments-bing
usa_sentiments_after<-df1 %>%
  inner_join(get_sentiments("bing")) %>% 
  count(sentiment) 

usa_sentiments_after<-as.data.frame(usa_sentiments_after)
usa_sentiments_after

usa_sentiments_after
View(usa_sentiments_after)

ggplot(data=usa_sentiments_after, aes(x=c("negative","positive"), y=n, fill=c("neg","pos")))+
  ggtitle("USA")+
  xlab("after 2.10")+
  ylab("")+
  ylim(c(0,700))+
  theme(legend.position = "none")+
  coord_fixed(ratio =0.009)+
  geom_bar(stat = "identity", position = "dodge")

#sentiments-nrc
usa_nrc_after<-df1 %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(sentiment)
usa_nrc_after

usa_nrc_after<-as.data.frame(usa_nrc_after)
usa_nrc_after<-arrange(usa_nrc_after, desc(n))



ggplot(data=usa_nrc_after, aes(x=reorder(sentiment,-n), y=n))+
  ggtitle("USA")+
  xlab("after 2.10")+
  ylab("")+
  ylim(c(0,800))+
  theme(legend.position = "none")+
  #coord_fixed(ratio =0.009)+
  geom_bar(stat = "identity", position = "dodge")

#tf-idf

usa_after_freq<-DocumentTermMatrix(corpus_usa_after, control = list(tolower =TRUE, 
                                                                      stopwords = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, stemming = TRUE,
                                                                      weighting = function(x) weightTfIdf(x, normalize = TRUE)))

removeSparseTerms(usa_after_freq, as.numeric(0.999))  
word_freq_after<- colSums(as.matrix(usa_after_freq))
word_freq_after <- data.frame(word_freq_after)
View(word_freq_after)

word_freq_after$words<-rownames(word_freq_after)
word_freq_after<-arrange(word_freq_after,desc(word_freq_after))

View(word_freq_after)



###################################################################################
######################################################################################
#total

#corpus
text_location2<-'C:/Users/Wonseok/Documents/R/HMR/Total/usa'
corpus_usa<-VCorpus(DirSource(text_location2))

#preprocessing, dtm
usa_dtm<-DocumentTermMatrix(corpus_usa, control = list(tolower =TRUE, 
                                                                   stopwords = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, stemming = TRUE))

#word cloud
matrix<-as.matrix(usa_dtm) 
words<-sort(colSums(matrix),decreasing=TRUE) 
df2<-data.frame(word=names(words),freq=words, index=1, stringsAsFactors = FALSE)
View(df)


df2<-df2[-c(1,5,10),]  #said, the, will


set.seed(1004)
wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words=200, random.order=FALSE, 
          rot.per=0.35, colors=brewer.pal(8, "Dark2"))


#sentiments-bing
usa_sentiments_total<-df2 %>%
  inner_join(get_sentiments("bing")) %>% 
  count(sentiment) 

usa_sentiments_total<-as.data.frame(usa_sentiments_total)
usa_sentiments_total

View(usa_sentiments_total)

ggplot(data=usa_sentiments_total, aes(x=c("negative","positive"), y=n, fill=c("neg","pos")))+
  ggtitle("USA")+
  xlab("")+
  ylab("")+
  ylim(c(0,700))+
  theme(legend.position = "none")+
  coord_fixed(ratio =0.009)+
  geom_bar(stat = "identity", position = "dodge")


#sentiments-nrc
usa_nrc_total<-df2 %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(sentiment)
usa_nrc_total

usa_nrc_total<-as.data.frame(usa_nrc_total)
usa_nrc_total<-arrange(usa_nrc_total, desc(n))



ggplot(data=usa_nrc_total, aes(x=reorder(sentiment,-n), y=n))+
  ggtitle("USA")+
  xlab("")+
  ylab("")+
  ylim(c(0,800))+
  theme(legend.position = "none")+
  #coord_fixed(ratio =0.009)+
  geom_bar(stat = "identity", position = "dodge")

#tf-idf

usa_total_freq<-DocumentTermMatrix(corpus_usa, control = list(tolower =TRUE, 
                                                                      stopwords = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, stemming = TRUE,
                                                                      weighting = function(x) weightTfIdf(x, normalize = TRUE)))

removeSparseTerms(usa_total_freq, as.numeric(0.999))  
word_freq_total<- colSums(as.matrix(usa_total_freq))
word_freq_total <- data.frame(word_freq_total)

word_freq_total$words<-rownames(word_freq_total)
word_freq_total<-arrange(word_freq_total,desc(word_freq_total))

View(word_freq_total)
