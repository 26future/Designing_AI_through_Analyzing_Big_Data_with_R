#corpus 구성
library(tm)
my.text.location<-"C:/Users/student/Downloads/Python/ymbaek_papers"
mypaper<-VCorpus(DirSource(my.text.location))
mypaper

#meta(): 메타데이터 구성
summary(mypaper)
mypaper[[2]] #코퍼스의 두번째 문서
mypaper[[2]]$content #안에 있는 내용 확인
mypaper[[2]]$meta #데이터의 정보 확인

meta(mypaper[[2]],tag ='author')<-'g.d.hong' #메타 데이터 추가
mypaper[[2]]$meta

#tm_map(코퍼스, 사전처리 함수)
library(stringr)
myfunc<-function(x){
  #특수기호(-,/,...) 전후의 단어를 확인
  str_extract_all(x,"[[:alnum:]]{1,}[[:punct:]]{1}[[:alnum:]]{1,}")
}
mypuncts<-lapply(mypaper,myfunc)
table(unlist(mypuncts))

myfunc<-function(x){
  #수치 자료 추출
  str_extract_all(x,"[[:digit:]]{1,}")
}
mydigits<-lapply(mypaper,myfunc)
table(unlist(mydigits))

#고유명사 추출(대문자로 시작)
myfunc<-function(x){
  str_extract_all(x,"[[:upper:]]{1}[[:alpha:]]{1,}")
}
myuppers<-lapply(mypaper,myfunc)
table(unlist(myuppers))

mypaper[[2]]$content

mytempfunc<-function(myobject,oldexp,newexp){
newobject<-tm_map(myobject,content_transformer(
    function(x,pattern) gsub(pattern,newexp,x)),oldexp)
#x:mypaper, pattern:-collar, newexp:collar
newobject
}
mycorpus<-mytempfunc(mypaper,"-collar","collar")
mycorpus<-mytempfunc(mypaper,"e\\.g\\.","for example")
mycorpus<-mytempfunc(mypaper,"and/or","and or")

mycorpus<-tm_map(mycorpus,removePunctuation)
mycorpus<-tm_map(mycorpus,stripWhitespace)
mycorpus<-tm_map(mycorpus,content_transformer(tolower))
mycorpus<-tm_map(mycorpus,removeWords,words=stopwords("SMART"))
stopwords("SMART")
mycorpus<-tm_map(mycorpus,stemDocument, language='en') #어근동일화

#문자 갯수 계산 함수
mycharfunc<-function(x){
  str_extract_all(x,".")
}

#단어 수 계산 함수
mywordfunc<-function(x){
  str_extract_all(x,boundary("word"))
}

mychar<-lapply(mypaper, mycharfunc)
myuniquechar0<-length(table(unlist(mychar))) #79개의 문자 사용(종류)
mytotalchar0<-sum(table(unlist(mychar))) #총 사용된 문자: 24765

myword<-lapply(mypaper,mywordfunc)
myuniqueword0<-length(table(unlist(myword))) #단어 종류 수: 1151 
mytotalword0<-sum(table(unlist(myword))) #총 3504개의 단어 사용

#전처리 이후
mychar<-lapply(mycorpus, mycharfunc)
myuniquechar1<-length(table(unlist(mychar))) 
mytotalchar1<-sum(table(unlist(mychar))) #총 사용된 문자: 14867

myword<-lapply(mycorpus,mywordfunc)
myuniqueword1<-length(table(unlist(myword))) #단어 종류 수: 1151 
mytotalword1<-sum(table(unlist(myword))) #총 3504개의 단어 사용

#전처리 전 글자 종류:79, 전처리 후 글자 종류:41
c(myuniquechar0,myuniquechar1) 

c(mytotalchar0,mytotalchar1)
#전처리 전 단어 종류:1151, 전처리 후 단어 종류:725
c(myuniqueword0,myuniqueword1)
results.comparing<-rbind(c(myuniquechar0,myuniquechar1),c(mytotalchar0,mytotalchar1),
                         c(mytotalword0,mytotalword1))
results.comparing
#컬럼 이름 변경
colnames(results.comparing)<-c("before","after")
#행 이름 변경
rownames(results.comparing)<-c("고유문자수","총문자수",
                               "고유단어수","총단어수")
#문서*단어 행렬 구성
dtm.e<-DocumentTermMatrix(mycorpus)
dtm.e

#가로줄 이름(문서 이름) 추출
rownames(dtm.e[,])
#단어 추출
colnames(dtm.e[,])
#행렬의 내용 참조
inspect(dtm.e[1:3,50:55]) #1~3번째 문서의 55~55번째 단어

##############################################################
#Bag of Words(BOW): 단어의 출현 빈도
#Bag of words=Corpus
#DTM 구성: TF(term frequency)를 기반으로 구성 => 중요한 단어 분석X
#          =>TF-IDF 사용
#TF-IDF: 문장을 대표하는 단어(토픽모델링)
#TF-IDF(단어 빈도-역 문서 빈도) 
#       => DTM내의 각 단어들 마다 중요한 정도를 가중치로 주는 방법
#DTM생성 -> TF-IDF 생성
#TF-IDF 활용: 문서 간의 유사도, 검색 결과 중요도를 정할때
#TF-IDF: TF*IDF
#       문서: d
#       단어: t
#       문서의 총 개수: n
#TF(d,t): 특정 문서 d에서 특정 단어 t의 등장 횟수
#        => DTM 
#IDF(d,t):log(n/1+df(t))
#DF(t): 특정 단어 t가 등장한 문서의 개수
#       (단어의 등장 횟수에 관심X)
#TF-IDF: 모든 문서에서 자주 등장하는 단어는 중요도가 낮다
#        특정 문서에서 자주 등장하는 단어는 중요도가 높다
#       =>TF-IDF가 크면, 단어의 중요도가 크다

#TF-IDF(토픽모델링)
dtm.e.tfidf<-DocumentTermMatrix(mycorpus,
    control = list(weightning=function(x) weightTfIdf(x,normalize=FALSE)))
dtm.e.tfidf

#TF는 크지만, TF-IDF는 작은 단어들 검출
as.matrix(dtm.e[,])
as.matrix(dtm.e.tfidf[,])

install.packages("KoNLP")
library(KoNLP)
install.packages('Sejong')
install.packages('tau')
install.packages('hash')
install.packages('RSQLite')

sentence <- '아버지가 방에 들어가신다'
extractNoun(sentence)


##########################################################
#인공신경망(Artificial Neural Network-ANN)
#필기체 인식, 음성 인식
#자율주행차, 드론, 스마트 장치 자동화
############################################################
concrete<-read.csv("dataset_for_ml/concrete.csv")
str(concrete)

#정규화
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
concrete_norm<-as.data.frame(lapply(concrete,normalize))
summary(concrete_norm)

concrete_train<-concrete_norm[1:773,]                             
concrete_test<-concrete_norm[774:1030,]

#모델 생성
install.packages("neuralnet")
library(neuralnet)

concrete_model<-neuralnet(formula = strength~cement+slag+ash+water+superplastic+
            coarseagg+fineagg+age, data = concrete_train)

#시각화
plot(concrete_model)

#예측
model_results<-compute(concrete_model,concrete_test[1:8])
str(model_results)

pre_str<-model_results$net.result
pre_str
cor(pre_str, concrete_test$strength) #상관관계 구하기

#모델2 생성(hidden layer 5개)
concrete_mode2<-neuralnet(formula = strength~cement+slag+ash+water+superplastic+
                            coarseagg+fineagg+age, data = concrete_train,
                            hidden = 5)
plot(concrete_mode2)

#예측
model_results2<-compute(concrete_mode2,concrete_test[1:8])
pre_str2<-model_results2$net.result
cor(pre_str2, concrete_test$strength)
