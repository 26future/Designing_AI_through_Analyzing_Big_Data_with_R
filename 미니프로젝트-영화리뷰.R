#############################1######################################
library(readr)
movie <- read_csv("IMDB Dataset.csv")
class(movie)
dim(movie)
View(movie)

#데이터 5000개 추출
set.seed(123)
movie_sample<-sample(50000,5000)
new_movie<-movie[movie_sample,]
dim(new_movie)
View(new_movie)

library(tm)
#코퍼스 만들기
movie_corpus<-VCorpus(VectorSource(new_movie$review))
inspect(movie_corpus[1:3])
lapply(movie_corpus[1],as.character)

#전처리
#소문자로 변경
#movie_corpus
#movie_corpus_clean<-tm_map(movie_corpus,content_transformer(tolower)) 
#inspect(movie_corpus_clean[1:3])
#lapply(movie_corpus_clean[1],as.character)

#숫자 제거
#movie_corpus_clean<-tm_map(movie_corpus_clean,content_transformer(removeNumbers))
#inspect(movie_corpus_clean[1:3])
#lapply(movie_corpus_clean[1],as.character)

#stop words 제거
#movie_corpus_clean<-tm_map(movie_corpus_clean,content_transformer(removeWords),stopwords())
#inspect(movie_corpus_clean[1:3])
#lapply(movie_corpus_clean[1],as.character)

#<br /><br /> 제거
remove_br<-function(x){
  return(gsub("<br /><br />"," ",x))
}
movie_corpus_clean<-tm_map(movie_corpus,content_transformer(remove_br))
lapply(movie_corpus_clean[1],as.character)
inspect(movie_corpus_clean[1:3])
class(movie_corpus_clean)

#punctuation 제거
#movie_corpus_clean<-tm_map(movie_corpus_clean,content_transformer(removePunctuation))
#lapply(movie_corpus_clean[1],as.character)

#어근 추출
#movie_corpus_clean<-tm_map(movie_corpus_clean,content_transformer(stemDocument))
#lapply(movie_corpus_clean[1],as.character)

#white space 제거
#movie_corpus_clean<-tm_map(movie_corpus_clean, content_transformer(stripWhitespace))
#lapply(movie_corpus_clean[1],as.character)

#DTM 생성
movie_dtm<-DocumentTermMatrix(movie_corpus_clean, control = list(tolower =TRUE, stopwords = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, stemming = TRUE))

#train/test data set 분리
set.seed(456)
movie_dtm_sample<-sample(5000,3500)
movie_dtm_train<-movie_dtm[movie_dtm_sample,]
movie_dtm_test<-movie_dtm[-movie_dtm_sample,]

movie_train_label<-new_movie[movie_dtm_sample,"sentiment"]
movie_test_label<-new_movie[-movie_dtm_sample,"sentiment"]

dim(movie_train_label)
dim(movie_test_label)
dim(movie_dtm_train)
dim(movie_dtm_test)

#10번 이상 사용된 단어만 추출
movie_freq_words<-findFreqTerms(movie_dtm_train,10)

class(movie_freq_words)
dim(movie_freq_words)
length(movie_freq_words)
movie_dtm_train
movie_freq_words

movie_dtm_freq_train<-movie_dtm_train[,movie_freq_words]
movie_dtm_freq_test<-movie_dtm_test[,movie_freq_words]



movie_dtm_freq_train<-as.data.frame(as.matrix(movie_dtm_freq_train))
movie_dtm_freq_test<-as.data.frame(as.matrix(movie_dtm_freq_test))


dim(movie_dtm_freq_train)
##################################################################
#bayes
#모델 생성
library(e1071)
classifier<-naiveBayes(movie_dtm_freq_train,movie_dtm_train_label)
movie_pred<-predict(classifier,movie_dtm_freq_test)
dim(movie_pred)
length(movie_pred)

library(gmodels)
CrossTable(movie_dtm_test_label,movie_pred,
           prop.t = FALSE, prop.r = FALSE,
           dnn = c("predicted","actual"))


##########################2######################################
#kmeans 기반 클러스터링

#표준화
#train_data<-as.data.frame(lapply(movie_dtm_freq_train,scale))
#test_data<-as.data.frame(lapply(movie_dtm_freq_test,scale))

#클러스터의 개수 결정
#library(NbClust)

#nc<-NbClust(movie_dtm,min.nc = 2,max.nc = 15, method = "kmeans")
#par(mfrow=c(1,1))
#barplot(table(nc$Best.n[1,]),
#        xlab="Numer of Clusters", ylab="Number of Criteria",
#        main="Number of Clusters Chosen")        

#DTM 생성
movie_dtm<-DocumentTermMatrix(movie_corpus_clean)

#10번 이상 사용된 단어만 추출
movie_freq_words<-findFreqTerms(movie_dtm,10)
movie_dtm_freq<-movie_dtm[,movie_freq_words]
dim(movie_dtm_freq)
View(movie_dtm_freq)

#모델 생성
movie_clusters<-kmeans(movie_dtm_freq, centers=2)

#클러스터 확인
movie_clusters$cluster
movie_clusters$centers
movie_clusters$size

#감성분석사전
install.packages("textdata")

get_sentiments('bing')
