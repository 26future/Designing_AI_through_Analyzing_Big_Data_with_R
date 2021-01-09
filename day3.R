#텍스트 마이닝:text mining
#문장->형태소 분석->명사,동사...->빈도표->시각화
#java 설치:www.java.com

install.packages("rJava")
#install.packages("memoise")
#install.packages("rJava")
install.packages("KoNLP")
library()
install.packages("ggiraphExtra")
library(ggiraphExtra)

str(USArrests)
head(USArrests)

library(tibble)

crime<-rownames_to_column(USArrests, var='state')
head(crime)
crime$state<-tolower(crime$state)
crime$state
str(crime)

#미국 지도 데이터 준비
install.packages("maps")
library(maps)
library(ggplot2)
states_map<-map_data('state')
str(states_map)

install.packages("mapproj")
library(mapproj)
ggChoropleth(data = crime, #지도에 표시할 내용
             aes(fill=Murder,map_id=state),#색깔로 표시할 변수,지역 기준 방법
             map=states_map)#지도데이터

ggChoropleth(data = crime, 
             aes(fill=Murder,map_id=state),
             map=states_map,
             interactive = T)

install.packages('stringi')
install.packages("devtools")
devtools::install_github("cardiomoon/kormaps2014")

remotes::install_github("cardiomoon/kormaps2014")
install.packages("remotes")
remotes::install_local(path = "C:/Users/student/Downloads/kormaps2014-master")

library(devtools)
library(kormaps2014)
str(changeCode(korpop1))
head(changeCode(korpop1))

library(dplyr)
library(ggplot2)
library(maps)
library(mapproj)
library(ggiraphExtra)
korpop1<-rename(korpop1, pop="총인구_명",name="행정구역별_읍면동")
str(changeCode(korpop1))
ggChoropleth(data = korpop1, #지도에 표시할 데이터
             aes(fill=pop, map_id=code, tooltip=name),
             #색깔로 나타낼 변수, 지역기준 변수, 지도위에 표시할 지역명
             map=kormap1)
 
ggplot(korpop1,
       aes(map_id=code, fill="총인구_명"))+
  geom_map(map=kormap1, colour="black", size=0.1)+ #지역별 구분선
  expand_limits(x=kormap1$long, y=kormap1$lat)+
  scale_fill_gradientn(colours = c("white","orange","red"))+
  ggtitle("2015년 인구분포도")+
  coord_map()

devtools::install_github(cardiomoon/moonBook2)
ggChoropleth(korpop2,kormap2,fillvar="남자_명")
  
###############머신러닝####################
#통계적 기법, 연산 능력, 빅데이터
#데이터마이닝?

subject_name<-c("John","Jane","Steve")
temp<-c(37,35,33)
flu_status<-c(TRUE,FALSE,FALSE)
temp[2] #index가 1부터 시작
temp[-2] #2번을 제외하고 출력
temp[c(TRUE,FALSE,TRUE)] #TRUE에 해당하는 값만 출력
#팩터: 명목형 데이터를 표현

gender<-factor(c("M","F","M"))
gender

blood<-factor(c("O","AB","A"))
blood
blood<-factor(c("O","AB","A"), levels=c("O","AB","A","B"))
blood

factor(c("A","F","C"), levels=c("A","B","C","D","F"),
       ordered = TRUE)

subject_name
#리스트: 순서X, 타입이 다양

sb1<-list(fn=subject_name[1],
          temp=temp[1],
          flu=flu_status[1])
sb1
sb1$fn
class(sb1[1])
sb1[1]
class(sb1[[1]])

sb1[c("temp","flu")]

df<-data.frame(sb1, stringsAsFactors = FALSE)
#stringsAsFactors:팩터형으로 문자열을 읽을것인가?
str(df)

#apply 계열 함수: 함수 연산을 특정단위로 쉽게 할 수 있도록 지원
#for, while(소규모 반복연산)
#apply(대규모 반복연산)

iris_num<-NULL
iris
class(iris)
str(iris)
ncol(iris)
for(x in 1:ncol(iris)){
  if(is.numeric(iris[,x]))
    iris_num<-cbind(iris_num,iris[,x])
  print(iris[,x])
}
iris_num
class(iris_num)
iris_num<-data.frame(iris_num)

iris_num<-iris[,sapply(iris,is.numeric)]
iris_num
class(iris_num)

iris_num<-iris[1:10, 1:4]
set.seed(123)
sample(1:10,2) #1부터10까지의 임의의 수 2개
idx_r<-sample(1:10,2)
idx_c<-sample(1:4,2)

idx_r
idx_c
for (i in 1:2){
  iris_num[idx_r[i],idx_c[i]]<-NA
}

iris_num

#apply:행(1) 또는 열(2) 단위 연산 (MARGIN)
#입력: 배열,매트릭스(같은 변수형)
#출력: 매트릭스 또는 벡터

apply(iris_num,1,mean)
apply(iris_num,2,mean,na.rm=T)

apply(iris_num,2,function(x){x*2+1})

apply(iris_num,2,function(x){median(x*2+1)})
apply(iris_num,2,function(x){median(x*2+1, na.rm=T)})

#lapply: list + apply=>실행 결과가 list로 출력
#데이터프레임: 모든 변수가 벡터를 가져야 함
#리스트: 벡터, 매트릭스, 데이터프레임
apply(iris_num,2,mean,na.rm=T)
lapply(iris_num, mean, na.rm=T)  

#sapply:lapply와 비슷, 간단하게 기술
#연산결과가 벡터, 리스트(길이가 다른 경우)
class(sapply(iris_num, mean, na.rm=T, simplify = F))

#vapply:sapply+템플릿 지정
sapply(iris_num,fivenum)
vapply(iris_num,fivenum, c("Min"=0),c("lst"=0),c("med"=0),c("3rd"=0),c("max"=0))

usedcars<-read.csv("dataset_for_ml/usedcars.csv",stringsAsFactors = FALSE)
str(usedcars)
summary(usedcars$year)
summary(usedcars[c("price","mileage")])

diff(range(usedcars$price))
IQR(usedcars$price)
quantile(usedcars$price, seq(from=0,to=1,by=0.1))

boxplot(usedcars$price, main="Car prices", ylab="price($)")
boxplot(usedcars$mileage, main="Car mileage", ylab="odometer")

hist(usedcars$price, main="Car price",xlab = "price($)")
hist(usedcars$mileage, main="Car mileage",xlab = "odometer")

var(usedcars$price)
sd(usedcars$price)
#분산 (데이터-평균)의 제곱의 합의 평균(n-1)
#표준편차: 분산의 제곱근

table(usedcars$year)
table(usedcars$model)
table(usedcars$color)

c_table<-table(usedcars$color)
round(prop.table(c_table)*100,1) #반올림

#일변량 통계
#이변량 통계(두 변수의 관계)
#다변량 통계(두 개 이상의 변수 관계)
#산포도(이변량)

plot(x=usedcars$mileage, y=usedcars$price) #강한 음의 상관관계

usedcars$conservative<-usedcars$color %in% c("Black","Gray","Silver","White")
table(usedcars$conservative)

install.packages("gmodels",dependencies=TRUE, INSTALL_opts = '--no-lock')
library(gmodels)

CrossTable(x=usedcars$model,
           y=usedcars$conservative)

wbcd<-read.csv("dataset_for_ml/wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)
wbcd<-wbcd[-1]
str(wbcd)

table(wbcd$diagnosis)
str(wbcd)
wbcd$diagnosis<-factor(wbcd$diagnosis, levels = c("B","M"),
       labels = c("Benign","Malignant"))
wbcd$diagnosis
round(prop.table(table(wbcd$diagnosis))*100,1)
summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])

#정규화
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

normalize(c(1,2,3,4,5))

lapply(wbcd[2:31],normalize)
class(lapply(wbcd[2:31],normalize))
wbcd_n<-as.data.frame(lapply(wbcd[2:31],normalize))
class(wbcd_n)
summary(wbcd_n$area_mean)

wbcd_train<-wbcd_n[1:469,]
wbcd_test<-wbcd_n[470:569,]

wbcd_train_labels<-wbcd[1:469,1]
wbcd_test_labels<-wbcd[470:569,1]

library(class)
wbcd_test_pred<-knn(train = wbcd_train,
                    test = wbcd_test,
                    cl = wbcd_train_labels,
                    k = 21)
wbcd_test_pred
wbcd_test_labels

library(gmodels)

CrossTable(x=wbcd_test_labels, 
           y=wbcd_test_pred,
           prop.chisq = FALSE)

#정규화: 표준화는 최대/최소값이 없음
#그 값이 중심 방향으로 축소되지 않음

wbcd_z<-as.data.frame(scale(wbcd[-1])) #표준화
summary(wbcd_z$area_mean)

#모델->테스트->정확도

#iris
#data: 1000건
#700                           300
#490    210
#train/validation              test

#iris(1:35, 51:85, 101:135)=>train
#나머지 데이터=>test
