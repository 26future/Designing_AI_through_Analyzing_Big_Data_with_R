#TFIDF= TF * IDF
#TF:문서에 등장한 단어의 개수
#IDF: N/(1+DF)
#N:문서의 개수
#DF:단어가 등장한 문서의 개수
#TFIDF값이 커지기 위해서는 TF가 커지면서,
#IDF가 커져야함(N이 커지거나 DF가 작아져야함.)


#말뭉치 구성
library(tm)
my.text.location<-"c:/gspark/ymbaek_papers"
mypaper<-VCorpus(DirSource(my.text.location))
mypaper
#meta():메타데이터 구성
summary(mypaper)
mypaper[[2]] #두번째 문서
mypaper[[2]]$content
mypaper[[2]]$meta
#meta: mypaper[[2]]를 설명하는 데이터
meta(mypaper[[2]], tag ='author')<-'g.d.hong'
mypaper[[2]]
mypaper[[2]]$meta

#tm_map(코퍼스, 사전처리함수)
library(stringr)
myfunc<-function(x){
  #특수기호(-, /,...) 전후의 단어를 확인
  str_extract_all(x,"[[:alnum:]]{1,}[[:punct:]]{1}[[:alnum:]]{1,}")
}
mypuncts<-lapply(mypaper,myfunc)
table(unlist(mypuncts))

myfunc<-function(x){
  #수치 자료 추출
  mydigits<-str_extract_all(x,"[[:digit:]]{1,}")
}
mydigits<-lapply(mypaper,myfunc)
table(unlist(mydigits))

#고유명사 추출(대문자로 시작)

myfunc<-function(x){
  #수치 자료 추출
  myuppers<-str_extract_all(x,"[[:upper:]]{1}[[:alpha:]]{1,}")
}
myuppers<-lapply(mypaper,myfunc)
table(unlist(myuppers))

mypaper[[2]]$content

#추가
mycorpus<-tm_map(mypaper,removeNumbers)

mytempfunc<-function(myobject,oldexp,newexp){
newobject<-tm_map(myobject, 
         content_transformer(
    function(x,pattern) gsub(pattern, newexp, 
                             x)),oldexp)
#x:myobject, pattern:-collar, newexp:collar
newobject
}
mycorpus<-mytempfunc(mypaper,"-collar","collar")
mycorpus<-mytempfunc(mypaper,"e\\.g\\.","for example")
mycorpus<-mytempfunc(mypaper,"and/or","and or")

mycorpus<-tm_map(mycorpus,removePunctuation)
mycorpus<-tm_map(mycorpus,stripWhitespace)
mycorpus<-tm_map(mycorpus,
                 content_transformer(tolower))
mycorpus<-tm_map(mycorpus,removeWords, 
                 words=stopwords("SMART"))
mycorpus<-tm_map(mycorpus,
                 stemDocument, language='en')
#어근 동일화

#문자 개수 계산 함수
mycharfunc<-function(x){
  str_extract_all(x, ".")
}
#단어수 계산 함수
mywordfunc<-function(x){
  str_extract_all(x, boundary("word"))
}
mychar<-lapply(mypaper, mycharfunc)
myuniquechar0<-length(table(unlist(mychar))) #79문자 사용
mytotalchar0<-sum(table(unlist(mychar)))#24765글자
myword<-lapply(mypaper,mywordfunc)
myuniqueword0<-length(table(unlist(myword))) #1151 개 종류 단어
mytotalword0<-sum(table(unlist(myword))) #총 3504 개 단어 사용
#전처리 이후
mychar<-lapply(mycorpus, mycharfunc)
myuniquechar1<-length(table(unlist(mychar))) #79문자 사용
mytotalchar1<-sum(table(unlist(mychar)))#24765글자
myword<-lapply(mycorpus,mywordfunc)
myuniqueword1<-length(table(unlist(myword))) #1151 개 종류 단어
mytotalword1<-sum(table(unlist(myword))) #총 3504 개 단어 사용

results.comparing<-rbind(
c(myuniquechar0, myuniquechar1),
#전처리 전 글자 종류:79, 전처리 후 : 41
c(mytotalchar0, mytotalchar1), #조금 후 수정
c(myuniqueword0, myuniqueword1),
#1151, 710
c(mytotalword0, mytotalword1))
#3504, 2060
results.comparing
colnames(results.comparing)<-c("before","after")
rownames(results.comparing)<-c("고유문자수","총문자수",
                               "고유단어수","총단어수")
results.comparing

#문서*단어 행렬 구성
dtm.e<-DocumentTermMatrix(mycorpus)
dtm.e

#가로줄 이름(문서 이름)
rownames(dtm.e[,])
#단어
colnames(dtm.e[,])
#행렬의 내용 참조
inspect(dtm.e[1:3,50:55])

#TF-IDF(토픽모델링)

#오타
dtm.e.tfidf<-DocumentTermMatrix(mycorpus,
                   control=list(weighting=function(x) weightTfIdf(x,normalize = FALSE)))
dtm.e.tfidf
inspect(dtm.e.tfidf[1:3,50:55])
#TF는 크지만, TFIDF는 작은 단어들 검출
value.tf.dtm<-as.vector(as.matrix(dtm.e[,]))
value.tf.dtm
value.tfidf.dtm<-as.vector(as.matrix(dtm.e.tfidf[,]))
value.tfidf.dtm

#dim(dtm.e[,])[1] #24개 문서, 703개 단어
word.label.dtm<-rep(colnames(dtm.e[,]),
    each=dim(dtm.e[,])[1])
doc.label.dtm<-rep(rownames(dtm.e[,]),
    dim(dtm.e[,])[2])
mydata<-data.frame(word.label.dtm, doc.label.dtm, 
           value.tf.dtm, value.tfidf.dtm)
mydata
colnames(mydata)<-c("word","doc","tf","tfidf")
mydata[120:130,]

cor.test(mydata$tf,mydata$tfidf,method="kendall")

cor.test(mydata$tf[mydata$tf>0],mydata$tfidf[mydata$tfidf>0],method="kendall")
#TF값의 순위가 높아도 TFIDF값의 순위가
#높지 않은 경우가 적지 않다.

#어떤 단어가 TF가 높고 TFIDF가 낮은지?
#1)TF, TFIDF가 모두 0보다 큰 데이터 추출
#2)TF값 > 중위수, TFIDF값< 중위수 => 단어확인
#class(mydata) => sub dataframe

#subset(mydata, 조건), #1
mydata2<-subset(mydata, tf>0 & tfidf>0)
#2
mydata3<-subset(mydata2, tf>median(mydata2$tf) & tfidf>median(mydata2$tfidf))
str(mydata3)

table(mydata3$word)[table(mydata3$word)>0]

#자연어처리
#웹 스크래핑 -> 문서, 단어 -> 토픽모델링
#유사한 문서, 카테고리화...

#R 웹 스크래핑 = rvest







#####내일 여기서 부터 코딩#####
as.matrix(dtm.e[,])
as.matrix(dtm.e.tfidf[,])

rep(1:3,each=10)
rep(1:3,times=10)
rep(1:3,10)#default : times
#tfidf 매트릭스
##############################
install.packages("KoNLP")
library('KoNLP')

#버전 오류
##############################
#인공신경망(ANN)
#필기체 인식, 음성 인식
#자율주행차, 드론, 스마티 장치 자동화


concrete<-read.csv("c:/gspark/concrete.csv")
str(concrete)

normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
concrete_norm<-as.data.frame
(lapply(concrete,normalize))
summary(concrete_norm)

concrete_train<-concrete_norm[1:773,]
concrete_test<-concrete_norm[774:1030,]
install.packages("neuralnet")
library(neuralnet)

concrete_model<-neuralnet(formula =strength~cement+slag+
            ash+water+superplastic+
            coarseagg+fineagg+age,
          data=concrete_train)
plot(concrete_model)


model_results<-compute(concrete_model,concrete_test[1:8])
str(model_results)

pre_str<-model_results$net.result
pre_str
cor(pre_str, concrete_test$strength)


concrete_model2<-neuralnet(formula =strength~cement+slag+
                            ash+water+superplastic+
                            coarseagg+fineagg+age,
                          data=concrete_train, hidden=5)

plot(concrete_model2)


model_results2<-compute(concrete_model2,concrete_test[1:8])
pre_str2<-model_results2$net.result
pre_str
cor(pre_str2, concrete_test$strength)

colnames(iris)
#Sepal.Length,Sepal.Width,Petal.Length
#Petal.Width
#Species는 제외
#data를 random shuffle
#70%:30% 분할(train/test)
#cor(hidden 레이어 수를 변경해가면서)
library(rJava)
library(KoNLP)
sentence <- "아버지가방에들어가신다"
extractNoun(sentence)



