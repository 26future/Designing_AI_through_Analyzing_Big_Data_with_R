library(arules)
help(Epub)
data(Epub)
summary(Epub)

inspect(Epub[1:10])
itemFrequency(Epub[,1:10])
itemFrequencyPlot(Epub, support=0.01,main="item frequency") #지지도 0.01이상인 데이터
itemFrequencyPlot(Epub, topN=20,main="item frequency") #상위 20개의 데이터
image(sample(Epub,500))

epub_rule<-apriori(data=Epub, parameter = list(support=0.001,
                                               confidence=0.2,
                                               minlen=2))
summary(epub_rule)
inspect(epub_rule)

inspect(sort(epub_rule, by='lift')[1:20])
inspect(sort(epub_rule, by='support')[1:20])

rule_ins<-subset(epub_rule, items %in% c("doc_72f","doc_4ac"))
inspect(rule_ins)

rule_ins<-subset(epub_rule, lhs %in% c("doc_72f","doc_4ac")) #왼쪽
inspect(rule_ins)

rule_ins<-subset(epub_rule, rhs %in% c("doc_72f","doc_4ac")) #오른쪽
inspect(rule_ins)
#%in%는 적어도 하나의 제품이 존재하면 해당 규칙을 가져옴

rule_ins<-subset(epub_rule, items %pin% c("60e","doc_4ac")) #부분적으로 일치
inspect(rule_ins)

rule_ins<-subset(epub_rule, lhs %ain% c("doc_6e8","doc_6e9")) #온전히 일치
inspect(rule_ins)

rule_ins<-subset(epub_rule, items %pin% c("60e"), & confidence>0.25) 
inspect(rule_ins)

install.packages("arulesViz")
library(arulesViz)
plot(epub_rule)
plot(sort(epub_rule, by="support")[1:20],method="grouped")

plot(epub_rule, method = "graph",control = list(type="items"))
plot(epub_rule, method = "graph",control = list(type="items"),
     vertex.label.cex=0.7, #점의 크기(기본:1)
     edge.arrow.size=0.3, #화살표의 크기
     edge.arrow.width=2) #화살표의 넓이 
#원의 크기: 지지도에 비례
#원의 색깔: 향상도(lift)에 비례
#화살표: lhs->rhs

#클러스터링
#타겟 마케팅, 고객 data, 구매 패턴
#무단 네트워크 침입 (패턴: 포트 스캐닝)

teens<-read.csv("dataset_for_ml/snsdata.csv")
str(teens)
table(teens$gender, useNA = "ifany") #NA 포함
summary(teens$age)

teens$age<-ifelse(teens$age>=13 & teens$age<20,teens$age,NA)
summary(teens$age)

teens$female<-ifelse(teens$gender=="F" & !is.na(teens$gender),1,0) 
#NA를 연산대상에 포함
teens$no_gender<-ifelse(is.na(teens$gender),1,0)
table(teens$gender, useNA = "ifany")
table(teens$female)
table(teens$no_gender)

mean(teens$age, na.rm=TRUE)
myagg<-aggregate(data=teens, age~gradyear,mean) #그룹(졸업연도)에 대한 통계(평균) 계산
                                         #default na.rm=TRUE
class(aggregate(data=teens, age~gradyear,mean))

avg_age<-ave(teens$age, teens$gradyear, FUN=function(x) mean(x,na.rm=TRUE))
avg_age
class(avg_age)
teens$age<-ifelse(is.na(teens$age),avg_age,teens$age)
summary(teens$age)


interests<-teens[5:40]
set.seed(2345) #동일한 난수 발생

interests_z<-as.data.frame(lapply(interests, scale))
head(interests_z)
teen_clusters<-kmeans(interests_z,5) #clustering: 5개로 나눔
teen_clusters$size
teen_clusters$centers
teens$cluster<-teen_clusters$cluster
teens[1:5,]
teens[1:5,c("cluster","gender","age","friends")]

#클러스터 단위로 나이 평균
aggregate(data=teens, age~cluster,mean)
aggregate(data=teens, friends~cluster,mean)
teens$female

#아이리스 data(3개 그룹), 시각화,표준화,비교분석

#=>3개 그룹(정확도?)