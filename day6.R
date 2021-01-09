#클러스터 중심과 각 data간 거리의 합 최소화
#클러스터 중심간의 거리의 합 최대합
#1.k결정
#2.k개의 초기 중심 설정
#3.data와 중심(클러스터)이 가장 가까운 크러스터로 데이터 할당
#4.중심점이 update
#3,4번 반복(중심점이 바뀌지 않을때까지)


iris
str(iris)
head(iris)
colSums(is.na(iris)) #false:0, true:1 =>결측치 갯수 확인
panel.fun<-function(x,y,...){
  horizontal<-(par("usr")[1]+par("usr")[2])/2;
  vertical<-(par("usr")[3]+par("usr")[4])2;
  text(horizontal, vertical, format(abs(cor(x,y)),digits=2))
  
}
pairs(iris[1:4], pch=21, bg=c("red","green","blue")[unclass(iris$Species)],
      upper.panel = panel.fun, main="Scatter")

#산점도 그리기
#1.ggplot2패키지의 geom_point(): 변수 1개의 산점도 그리기
#2.corrplot패키지: 상관계수 행렬 그리기

str(airquality)
airquality_1<-airquality[,c(1:4)]
str(airquality_1)

colSums(is.na(airquality_1))
sum(is.na(airquality_1$Ozone))
cor(airquality_1)
#결측값이 있는 행을 제거
airquality_2<-na.omit(airquality_1)
str(airquality_2)
colSums(is.na(airquality_2))

pairs(USJudgeRatings, lower.panel = panel.smooth, upper.panel = panel.cor,
      gap=0, row1attop=FALSE)

pairs(iris[-5], log = "xy") # plot all variables on log scale
pairs(iris, log = 1:4, # log the first four
      main = "Lengths and Widths in [log]", line.main=1.5, oma=c(2,2,3,2))
?pairs

pairs(airquality_2, pch='*', main="scatter plot", lower.panel = panel.lm,
      upper.panel = panel.cor, diag.panel = panel.hist)

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.7, 0.7, txt, cex = cex.cor * r)
}
panel.lm<-function(x,y,col=par("col"), bg=NA, pch=par("pch"),
                   cex=1, col.smooth="black",...){
                     points(x,y,pch=pch, col=col, bg=bg, cex=cex)
                     abline(stats::lm(y-x), col=col.smooth,...)
                   }

pairs(airquality_2, pch='*', main="scatter plot", lower.panel = panel.lm,
      upper.panel = panel.cor, diag.panel = panel.hist)

pairs(iris[1:4], main = "Anderson's Iris Data -- 3 species",
      pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])

unclass(iris$Species) #factor->integer vector로 변환

panel.fun <-function(x,y,...){
  horizontal<-(par("usr")[1]+
                 par("usr")[2])/2;
  vertical<-(par("usr")[3]+
               par("usr")[4])/2;
  text(horizontal, vertical, 
       format(abs(cor(x,y)),digits=2))
}
pairs(iris[1:4],
      pch=21, bg=c("red","green","blue")[unclass(iris$Species)],
      upper.panel=panel.fun,
      main="Scatter")

library(ggplot2)
iris_plot<-ggplot(data = iris, aes(x=Petal.Length, y=Petal.Width, colour=Species))+
  geom_point(shape=19, size=4)

#annotation 추가하기
iris_plot2<-iris_plot+
  annotate("text", x=1.5,y=0.7,label="Setosa")+
  annotate("text", x=3.5,y=1.5,label="Veriscolor")+
  annotate("text", x=6,y=2.7,label="Virginica")

#영역 잡기
iris_plot2+
  annotate("rect", xmin=0,xmax=2.6,ymin=0,ymax=0.8,alpha=0.1,fill="red")+
  annotate("rect", xmin=2.6,xmax=4.9,ymin=0.8,ymax=1.5,alpha=0.1,fill="green")+
  annotate("rect", xmin=4.8,xmax=7.2,ymin=1.5,ymax=2.7,alpha=0.1,fill="blue")

iris_kmeans<-kmeans(iris[,c("Petal.Length","Petal.Width")],3)  
iris_kmeans
names(iris_kmeans$cluster)
iris_kmeans$size #각 클러스터의 크기
iris_kmeans$cluster
table(iris_kmeans$cluster)


#엔트로피: 복잡도, 혼잡도
#엔트로피가 높다=> 복잡하다
#Information Gain= Entropy before-Entropy after

curve(-x*log2(x)-(1-x)*log2(1-x),
      col="red", xlab="x", ylab="entropy", lwd=4)

credit<-read.csv("dataset_for_ml/credit.csv")
str(credit)
summary(credit$amount)
table(credit$default)
dim(credit)

set.seed(123)
train_sample<-sample(1000,900)
str(train_sample)
credit_train<-credit[train_sample,]
credit_test<-credit[-train_sample,]
View(credit)
table(credit_train$default)
table(credit_test$default)
install.packages("C50")
library(C50) 

#train
credit_model<-C5.0(credit_train[-17],credit_train$default) 
#17번째 열을 제외한 나머지
#names(credit_train)
credit_model
summary(credit_model)

#test
credit_pred<-predict(credit_model,credit_test[-17])
credit_pred
library(gmodels)
CrossTable(credit_test$default,credit_pred,
           prop.c = FALSE, prop.r = FALSE,
           dnn = c("actual","predicted"))

#부스팅: 의사결정 트리를 여러개 작성
#->각 의사결정 트리에서 나온 결과에 대해 투표
#성능이 약한 모델을 모아서 성능 개선

#train
credit_boost10<-C5.0(credit_train[-17],credit_train$default, trials = 10) 
#의사결정 트리의 개수:10개(가장 best)
credit_boost10
summary(credit_boost10)
#test
credit_boost_pred10<-predict(credit_boost10,credit_test[-17])
CrossTable(credit_test$default,credit_boost_pred10,
           prop.c = FALSE, prop.r = FALSE,
           dnn = c("actual","predicted"))

##타이타닉 분석##
train<-read.csv("titanic/train.csv")
test<-read.csv("titanic/test.csv")

str(train)
str(test)

install.packages("readr")
install.packages("rpart")
install.packages("rpart.plot")

library(readr)
library(rpart)
library(rpart.plot)
library(dplyr)
library(ggplot2)
Survived<-train$Survived
train$Survived<-NULL

str(bind_rows(train,test))
dim(bind_rows(train,test))

dataset<-bind_rows(train,test)
str(dataset)
summary(dataset)

dataset$PassengerId[is.na(dataset$PassengerId)==TRUE]

dataset$PassengerId[is.na(dataset$Fare)==TRUE]

dataset$Fare[dataset$PassengerId==1044]<-median(dataset$Fare, na.rm=TRUE)

summary(dataset$Age)
dataset$Age<-sapply(dataset$Age, FUN = function(x)
  {ifelse(is.na(x),median(dataset$Age, na.rm=TRUE),x)})
summary(dataset$Age)
table(dataset$Embarked)
table(dataset$Embarked)/sum(dataset$Embarked!="")

#dataset$Embarked가 ""인 승객의 id 추출
dataset$PassengerId[dataset$Embarked==""]
dataset$Embarked[c(62,830)]<-"S" #S에서 탑승한 것으로 변경

nrow(dataset) #행의 개수 출력
dim(dataset) #행,열의 개수 출력
sum(dataset$Cabin !="")/nrow(dataset)
1-sum(dataset$Cabin !="")/nrow(dataset) #누락된 비율
dataset$Cabin<-substr(dataset$Cabin,1,1) #첫번째 글자부터 한글자 추출
table(dataset$Cabin)
dataset$Cabin[dataset$Cabin==""]<-"H" #Cabin이 없는 값에 H입력
table(dataset$Cabin)

str(dataset)
factor_vars<-c('PassengerId','Pclass','Sex','Embarked','Cabin')

dataset[factor_vars]<-lapply(dataset[factor_vars], function(x) as.factor(x))
str(dataset)

train_cleaned<-dataset[1:891,]
test_cleaned<-dataset[892:1309,]
train_cleaned$Survived<-Survived

#4개의 컬럼을 가지고 survived를 예측할 수 있는 모델 만들기
#train
DT<-rpart(Survived~Pclass+Sex+Embarked+Cabin, train_cleaned, method = "class") 
summary(DT)
#test
predict_dt<-predict(DT, test_cleaned, type='class')

res<-data.frame(PassengerId=test_cleaned$PassengerId,
           Survived=predict_dt)
res
write.csv(res, file="result.csv", row.names = FALSE) #행 이름은 저장 안 함


