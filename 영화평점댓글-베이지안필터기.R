#1. 베이지안 필터기
movie<-movieread.csv("movie-pang02/movie-pang02.csv", stringsAsFactors = FALSE)
str(movie)

movie$class<-factor(movie$class)
str(movie$class)
table(movie$class)

library(tm)
movie$text<-incov(enc2utif8(movie$text), sub='byte')
movie_corpus<-VCorpus(VectorSource(movie$text))

inspect(movie_corpus[1:3])

#소문자로 변경
corpus_clean<-tm_map(movie_corpus, content_transformer(tolower))
#숫자 제거
corpus_clean<-tm_map(corpus_clean, removeNumbers)
#stopwords 제거
corpus_clean<-tm_map(corpus_clean, content_transformer(removeWords), stopwords())
#punctuation 제거
corpus_clean<-tm_map(corpus_clean, removePunctuation)
#whitespace 제거
corpus_clean<-tm_map(corpus_clean, stripWhitespace)

#확인
lapply(movie_corpus[1], as.character)
lapply(corpus_clean[1], as.character)

#tokenization
movie_dtm<-DocumentTermMatrix(corpus_clean)

#train/test 데이터 분할
set.seed(123)
dim(movie)
movie_sample<-sample(2000,1400)

#movie_train<-movie[movie_sample,]
#movie_test<-movie[-movie_sample,]

movie_dtm_train<-movie_dtm[movie_sample,]
movie_dtm_test<-movie_dtm[-movie_sample,]
movie_dtm_train <- as.data.frame(as.matrix(movie_dtm_train))
movie_dtm_test <- as.data.frame(as.matrix(movie_dtm_test))

movie_train_label<-movie[movie_sample,'class']
movie_test_label<-movie[-movie_sample,'class']

#movie_corpus_train<-corpus_clean[movie_sample]
#movie_corpus_test<-corpus_clean[-movie_sample]


#모델 생성
library(e1071)
movie_classifier<-naiveBayes(movie_dtm_train,movie_train_label)
movie_pred <-predict(movie_classifier, movie_dtm_test)
library(gmodels)
CrossTable(movie_test_label, movie_pred,
           prop.t = FALSE, prop.r = FALSE,
           dnn = c("predicted","actual"))

#빈도수 5이상만 
movie_freq_words<-findFreqTerms(movie_dtm_train,5)
str(movie_freq_words)
class(movie_freq_words)

#열단위 전달
movie_dtm_freq_train<-movie_dtm_train[,movie_freq_words]
movie_dtm_freq_test<-movie_dtm_test[,movie_freq_words]

convert_counts<-function(x){
  x<- ifelse(x>0,"Yes","No")
}
#View(apply(movie_dtm_freq_train,MARGIN=2,convert_counts))
#View(apply(movie_dtm_train,MARGIN=2,convert_counts))

movie_train<-apply(movie_dtm_freq_train, MARGIN=2, convert_counts)
movie_test<-apply(movie_dtm_freq_test, MARGIN = 2, convert_counts)

#모델 생성
movie_classifier2<-naiveBayes(movie_train, movie_train_label)
movie_pred2<-predict(movie_classifier2,movie_test)
crossTable(movie_test_label,movie_pred2,
           prop.t = FALSE, prop.r = FALSE,
           dnn = c("predicted","actual"))

###############################################################
#2. POS/NEG 단위로 알파벳 문자(빈도수)

movie<-read.csv("movie-pang02/movie-pang02.csv")
dim(movie)
table(movie$class)
movie_pos<-movie[movie$class=='Pos']
