install.packages("arules")
library(arules)

groceries<-read.csv("dataset_for_ml/groceries.csv") 
#read.csv에서는 자동적으로 ,로 나누어짐
groceries
str(groceries) 

#item의 개수가 일정하지 않은 경우
groceries<-read.transactions("dataset_for_ml/groceries.csv", sep=",")
groceries
summary(groceries)
#0에 해당하는 셀의 비율:98
#169:거래 내역에서 전체 상품의 종류의 개수
#1~32:거래 당 구매 상품의 개수

inspect(groceries[1:5]) #희소 행렬 안의 내용 확인

itemFrequency(groceries[,1:3]) #전체 거래 중 해당 아이템의 구매 비율
itemFrequency(groceries[,150:169])
itemFrequencyPlot(groceries, support=0.1) #지지도 0.1이상인 아이템 시각화
itemFrequencyPlot(groceries, topN=20) #상위 20개 아이템 시각화
image(groceries[1:5]) #희소행렬 형태로 시각화
image(sample(groceries,100)) #임의의 데이터 100개를 추출해서 희소행렬로 나타냄
                             #데이터가 어느쪽에 밀집되어있는지(경향) 확인
apriori(groceries) #default 최소지지도 0.1, 신뢰도 0.8 
                   #=>조건을 만족하는 데이터 0건
groceryRules<-apriori(groceries, 
                      parameter = list(support=0.006, confidence=0.25, minlen=2))
                      #min=2는 2개 미만의 아이템을 갖는 규칙은 제거
groceryRules
summary(groceryRules)
inspect(groceryRules[1:3])

inspect(sort(groceryRules, by="lift")[1:5])
#3.956477의 의미는 대략4
#허브를 산 사람들이 채소를 살 가능성이
#채소를 산 일반고객보다 4배가 더 높다

#berry가 포함되어 있는 모든 규칙 찾기
berryRules<-subset(groceries, items %in% "berries","yogurt")
inspect(berryRules)

#파일로 저장
write(groceryRules, file='groceryRules.csv', sep=',')
class(groceryRules)
grdf<-as(groceryRules, "data.frame")
grdf
str(grdf)

data(Epub) #data load
help(Epub)
summary(Epub)







