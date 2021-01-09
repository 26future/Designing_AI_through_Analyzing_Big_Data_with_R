#Thailand-- Before 2.10

library(tm)
library(SnowballC)

#corpus
text_location<-'C:/Users/Wonseok/Documents/R/HMR/Before 2.10/Thailand/Bangkok post'
corpus_thai_before<-VCorpus(DirSource(text_location))

#preprocessing, dtm
thai_before_dtm<-DocumentTermMatrix(corpus_thai_before, control = list(tolower =TRUE, 
    stopwords = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, stemming = TRUE))

#word cloud
matrix<-as.matrix(thai_before_dtm) 
words<-sort(colSums(matrix),decreasing=TRUE) 
df<-data.frame(word=names(words),freq=words, index=1, stringsAsFactors = FALSE)
View(df)

df<-df[-c(1,2,5),] #said, the, will

print(df$word)

set.seed(1004)
wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words=200, random.order=FALSE, 
          rot.per=0.35, colors=brewer.pal(8, "Dark2"))

#sentiments-bing
thai_sentiments_before<-df %>%
  inner_join(get_sentiments("bing")) %>% 
  count(sentiment) 

thai_sentiments_before

View(thai_sentiments_before)

ggplot(data=thai_sentiments_before, aes(x=c("negative","positive"), y=n, fill=c("neg","pos")))+
  ggtitle("Thailand")+
  xlab("before 2.10")+
  ylab("")+
  ylim(c(0,700))+
  theme(legend.position = "none")+
  coord_fixed(ratio =0.009)+
  geom_bar(stat = "identity", position = "dodge")

#sentiment-nrc
thai_nrc_before<-df %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(sentiment)
thai_nrc_before

thai_nrc_before<-as.data.frame(thai_nrc_before)
thai_nrc_before<-arrange(thai_nrc_before, desc(n))


ggplot(data=thai_nrc_before, aes(x=reorder(sentiment,-n), y=n))+
  ggtitle("Thailand")+
  xlab("before 2.10")+
  ylab("")+
  ylim(c(0,800))+
  theme(legend.position = "none")+
  geom_bar(stat = "identity", position = "dodge")


#tf-idf

thai_before_freq<-DocumentTermMatrix(corpus_thai_before, control = list(tolower =TRUE, 
                                                                          stopwords = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, stemming = TRUE,
                                                                          weighting = function(x) weightTfIdf(x, normalize = TRUE)))

removeSparseTerms(thai_before_freq, as.numeric(0.99))  
word_freq_before<- colSums(as.matrix(thai_before_freq))
word_freq_before <- data.frame(word_freq_before)

word_freq_before$words<-rownames(word_freq_before)
word_freq_before<-arrange(word_freq_before,desc(word_freq_before))

View(word_freq_before)



#####################################################################################
######################################################################################
#after 2.10

#corpus
text_location1<-'C:/Users/Wonseok/Documents/R/HMR/After 2.10/Thailand/Bangkok post'
corpus_thai_after<-VCorpus(DirSource(text_location1))

#preprocessing, dtm
thai_after_dtm<-DocumentTermMatrix(corpus_thai_after, control = list(tolower =TRUE, 
      stopwords = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, stemming = TRUE))


str(thai_after_dtm)

#word cloud
matrix<-as.matrix(thai_after_dtm) 
words<-sort(colSums(matrix),decreasing=TRUE) 
df<-data.frame(word=names(words),freq=words, stringsAsFactors = FALSE)
View(df)

df<-df[-c(1,2,3),] #said, the, will

print(df$word)

set.seed(1004)
wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words=200, random.order=FALSE, 
          rot.per=0.35, colors=brewer.pal(8, "Dark2"))

#sentiments-bing
thai_sentiments_after<-df %>%
  inner_join(get_sentiments("bing")) %>% 
  count(sentiment) 

thai_sentiments_after

View(thai_sentiments_after)

ggplot(data=thai_sentiments_after, aes(x=c("negative","positive"), y=n, fill=c("neg","pos")))+
  ggtitle("Thailand")+
  xlab("after 2.10")+
  ylab("")+
  ylim(c(0,700))+
  theme(legend.position = "none")+
  coord_fixed(ratio =0.009)+
  geom_bar(stat = "identity", position = "dodge")

#sentiments-nrc
thai_nrc_after<-df %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(sentiment)
thai_nrc_after

thai_nrc_after<-as.data.frame(thai_nrc_after)
thai_nrc_after<-arrange(thai_nrc_after, desc(n))

ggplot(data=thai_nrc_after, aes(x=reorder(sentiment,-n), y=n))+
  ggtitle("Thailand")+
  xlab("after 2.10")+
  ylab("")+
  ylim(c(0,800))+
  theme(legend.position = "none")+
  #coord_fixed(ratio =0.009)+
  geom_bar(stat = "identity", position = "dodge")

#tf-idf

thai_after_freq<-DocumentTermMatrix(corpus_thai_after, control = list(tolower =TRUE, 
                                                                        stopwords = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, stemming = TRUE,
                                                                        weighting = function(x) weightTfIdf(x, normalize = TRUE)))

removeSparseTerms(thai_after_freq, as.numeric(0.999))  
word_freq_after<- colSums(as.matrix(thai_after_freq))
word_freq_after <- data.frame(word_freq_after)

word_freq_after$words<-rownames(word_freq_after)
word_freq_after<-arrange(word_freq_after,desc(word_freq_after))

View(word_freq_after)


########################################################################################
#######################################################################################
#total

#corpus
text_location2<-'C:/Users/Wonseok/Documents/R/HMR/Total/thailand'
corpus_thai<-VCorpus(DirSource(text_location2))

#preprocessing, dtm
thai_dtm<-DocumentTermMatrix(corpus_thai, control = list(tolower =TRUE, 
                                                           stopwords = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, stemming = TRUE))

#word cloud
matrix<-as.matrix(thai_dtm) 
words<-sort(colSums(matrix),decreasing=TRUE) 
df<-data.frame(word=names(words),freq=words, index=1, stringsAsFactors = FALSE)
View(df)


df<-df[-c(1,2,4),]  #said, the, will


set.seed(1004)
wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words=200, random.order=FALSE, 
          rot.per=0.35, colors=brewer.pal(8, "Dark2"))

#sentiments-bing
thai_sentiments_total<-df %>%
  inner_join(get_sentiments("bing")) %>% 
  count(sentiment) 

thai_sentiments_total

View(thai_sentiments_total)

ggplot(data=thai_sentiments_total, aes(x=c("negative","positive"), y=n, fill=c("neg","pos")))+
  ggtitle("Thailand")+
  xlab("")+
  ylab("")+
  ylim(c(0,700))+
  theme(legend.position = "none")+
  coord_fixed(ratio =0.009)+
  geom_bar(stat = "identity", position = "dodge")

#sentiments-nrc
thai_nrc_total<-df %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(sentiment)
thai_nrc_total

thai_nrc_total<-as.data.frame(thai_nrc_total)
thai_nrc_total<-arrange(thai_nrc_total, desc(n))
thai_nrc_total


ggplot(data=thai_nrc_total, aes(x=reorder(sentiment,-n), y=n))+
  ggtitle("Thailand")+
  xlab("")+
  ylab("")+
  ylim(c(0,800))+
  theme(legend.position = "none")+
  geom_bar(stat = "identity", position = "dodge")


#tf-idf

thai_total_freq<-DocumentTermMatrix(corpus_thai, control = list(tolower =TRUE, 
                                                                  stopwords = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, stemming = TRUE,
                                                                  weighting = function(x) weightTfIdf(x, normalize = TRUE)))

removeSparseTerms(thai_total_freq, as.numeric(0.999))  
word_freq_total<- colSums(as.matrix(thai_total_freq))
word_freq_total <- data.frame(word_freq_total)

word_freq_total$words<-rownames(word_freq_total)
word_freq_total<-arrange(word_freq_total,desc(word_freq_total))

View(word_freq_total)
