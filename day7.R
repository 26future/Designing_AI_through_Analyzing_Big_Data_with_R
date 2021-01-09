#조건부 확률
#1. 서로 영향을 끼치지 않는 경우
#   P(B|A)=P(B) :A와 B가 서로 영향을 주지 않음
#2. 서로 영향을 주는 경우
#   P(AΠB)=P(A)P(B)
#   P(B|A)=P(AΠB)/P(B)
#

#데이터가 주어졌을때, ...가 발생 확률
#outlook(겉보기 날씨), 습도 => 테니스를 칠 확률
# 1. 테니스를 많이 친다
# 2. 테니스를 쳤을때, 날씨가 맑고, 습도가 좋은 조건이 자주 발생
#설명변수(날씨,습도)간의 독립을 가림
# =>테니스를 쳤을때, 날씨와 습도는 서로 상관관계가 없다고 가정
# =>독립이라고 가정
# =>계산 간단, data가 적은 경우에도 모델 생성 가능

#베이즈 정리
# 특성 상황이 만족되면, 계산을 이렇게 할 수 있다
# P(A|B)=P(AΠB)/P(B)
# =P(B|Ai)*P(Ai)/P(B|A1)*P(A1)+P(B|A2)*P(A2)+P(B|A3)*P(A3)
# 특정 상황: 1.A1~a4가 모두 배반 사건(교집합 없음, A1~A4의 합이 전체집합)
#P(AiΠB)/P(B)=P(AiΠB)/P(A1ΠB)+...+P(A4ΠB)
#            =P(Ai)*P(B|Ai)/P(A1)*P(B|A1)+...+P(A4)*P(B|A4)

#P(A1):테니스를 친다
#P(A2):테니스를 안친다
#P(B):날씨가 좋다
#=>P(Ai|B)=P(AiΠB)/P(B)
#         =P(B|Ai)*P(Ai)/P(BΠA1)+P(BΠA2)

#라플라스 추정량: 1로 설정

sms_raw<-read.csv("sms_spam_ansi.txt", stringsAsFactors = FALSE)
str(sms_raw)
sms_raw$type<-factor(sms_raw$type)
table(sms_raw$type)

#텍스트 데이터 정리, 표준화

#tm패키지:텍스트 마이닝 패키지
#설치
install.packages("tm")
library(tm)

#코퍼스: 단어 집합 생성 -> VCorpus()
#데이터 소스 객체 생성-> VectorSource()
sms_corpus<-VCorpus(VectorSource(sms_raw$text))
sms_corpus
inspect(sms_corpus[1:2])

sms_corpus[1]
sms_corpus[[1]]
as.character(sms_corpus[[1]])
#1번부터 5번까지 문서 내용 출력(lapply함수 이용)
lapply(sms_corpus[1:5],as.character)

sms_corpus_clean<-tm_map(sms_corpus,content_transformer(tolower))
#예전 R버전에서는 아래와 같이 작업할 것
#sms_corpus_clean<-tm_map(sms_corpus,content_transformer(tolower))
class(sms_corpus_clean)

as.character(sms_corpus_clean[[1]])
class(as.character(sms_corpus_clean[[1]]))

#메킨토시 운영체제의 경우 아래와 같은 옵션을 read.csv함수에 추가할 것
#fileEncoding="CP949", encoding="UTF-8

#숫자제거
sms_corpus_clean<-tm_map(sms_corpus_clean,removeNumbers)
inspect(sms_corpus_clean[1:5])

#구두점 제거
removePunctuation("hi.....hello...bye")
sms_corpus_clean<-tm_map(sms_corpus_clean,removePunctuation)
inspect(sms_corpus_clean[1:5])

#불용어(stop words) 제거
#불용어:to,and,but,or...
stopwords()
sms_corpus_clean<-tm_map(sms_corpus_clean,removeWords,stopwords())
inspect(sms_corpus_clean[1:5])

replacePunctuation<-function(x){
  gsub("[[:punct:]]+"," ",x) #x에 전달된 문자열에 대해 punctuation은 제거(" "로 변경)
}
replacePunctuation("hi+.{hello<;")

x="대한민국 대한 민국 대한민국"
gsub("대한민국","코리아",x)
gsub("한국","코리아",x)
gsub("우리나라","코리아",x)
gsub("조선","코리아",x)

#형태소 분석
install.packages("SnowballC")
library(SnowballC)
wordStem(c("learn","learned","learning","learns")) #단어의 어근을 추출

#stemDocument함수: 텍스트 문서의 전체 코퍼스에 wordstem을 적용
sms_corpus_clean<-tm_map(sms_corpus_clean,stemDocument)
inspect(sms_corpus_clean[1:5])

#추가 여백 제거(불필요한 공백 제거)
sms_corpus_clean<-tm_map(sms_corpus_clean,stripWhitespace)

lapply(sms_corpus[1:3],as.character)
#에러-> lapply(sms_corpus_clean[1:3], as.character)
inspect(sms_corpus_clean[1:3])

################################################################
################################################################

#토큰화(단어)
#DocumentTermMatrix(): sms 메세지 코퍼스 -> 토큰화
#행: sms메세지, 열: 단어
#DTM행렬, TDM행렬(행:단어, 열:sms메세지)
sms_dtm2<-DocumentTermMatrix(sms_corpus, 
                              control = list(tolower=TRUE, removeNumbers=TRUE,
                                        stopwords=TRUE,removePunctuation=TRUE,
                                        stemming=TRUE))
sms_dtm<-DocumentTermMatrix(sms_corpus_clean)
sms_dtm_train<-sms_dtm2[1:4169,]
sms_dtm_test<-sms_dtm2[4170:5559,]

sms_train_labels<-sms_raw[1:4169,]$type
sms_test_labels<-sms_raw[4170:5559,]$type

install.packages("wordcloud")
library(wordcloud)
wordcloud(sms_corpus_clean, scale=c(5,0.2), min.freq = 50, max.words = 100, 
          rot.per = 0.5, random.color = T, colors=brewer.pal(10,"Paired"), 
          random.order = FALSE)

spam<-subset(sms_raw, type=="spam")
ham<-subset(sms_raw, type=="ham")
wordcloud(spam$text, max.words = 40, scale=c(3,0.5))
wordcloud(ham$text, max.words = 40, scale=c(3,0.5))

sms_dtm_train
sms_freq_words<-findFreqTerms(sms_dtm_train,5) #최소 5번 이상 등장한 단어 출력
str(sms_freq_words)

convert_counts<-function(x){
  x<- ifelse(x>0,"Yes","No")
}

#행렬의 열 단위로 전달(apply, MARGIN=1 (행), 2(열))
sms_dtm_freq_train<-sms_dtm_train[,sms_freq_words]
sms_dtm_freq_test<-sms_dtm_test[,sms_freq_words]

#열 단위로 함수에 데이터를 전달
sms_train<-apply(sms_dtm_freq_train,MARGIN=2,convert_counts)
sms_test<-apply(sms_dtm_freq_test,MARGIN=2,convert_counts)

dim(sms_train)

#나이브 베이지안 필터기 생성(모델)
install.packages("e1071")
library(e1071)
#train
sms_classifier<-naiveBayes(sms_train, sms_train_labels)
#test
sms_test_pred<-predict(sms_classifier,sms_test)
sms_test_pred

library(gmodels)
CrossTable(sms_test_pred,sms_test_labels,
           prop.t = FALSE, prop.r = FALSE,
           dnn=c("predicted","actual"))

#laplace=1 적용
sms_classifier2<-naiveBayes(sms_train, sms_train_labels,laplace = 1)
sms_test_pred2<-predict(sms_classifier2,sms_test)
CrossTable(sms_test_pred2,sms_test_labels,
           prop.t = FALSE, prop.r = FALSE,
           dnn=c("predicted","actual"))
