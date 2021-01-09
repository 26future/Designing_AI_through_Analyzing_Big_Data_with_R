#모집단의 요약값(모수: 평균, 분산)
#확률변수: 근원사건들에 실수값을 대응시킨 함수
#           ex) 동전 앞면의 개수
#이항분포: 베르누이 시행을 독립적으로 여러번 수행했을 때, 성공한 횟수의 분포

#############################################################################
#회귀 모델
#선형: x와 y의 관계가 선형이라고 가정
#비선형
#단순 선형 회귀분석: 변수(x)가 1개 => y를 예측
#다중 선형 회귀분석: 변수(x)가 2개 이상 => y를 예측
###########################################################################
#보험금 예측
insurance<-read.csv('dataset_for_ml/insurance.csv')
str(insurance)
#회귀모델을 구축하기 전에 정규성 확인
#종속변수가 정규분포를 따르는 경우, 모델이 잘 만들어짐

summary(insurance$expenses)
hist(insurance$expenses)
str(insurance)

table(insurance$region)

#변수 상관 관계(상관행렬): 변수간 상관계수로 이루어진 행렬 => cor 함수
cor(insurance[c('age','bmi','children','expenses')])

pairs(insurance[c('age','bmi','children','expenses')])

install.packages("psych")
library(psych)
pairs.panels(insurance[c('age','bmi','children','expenses')])

#모델 생성
ins_model<-lm(expenses~age+children+bmi+sex+smoker+region, data = insurance)
ins_model<-lm(expenses~ . , data = insurance)
ins_model

summary(ins_model)

#p값이 작다 => 회귀계수가 0이 아닐 가능성 낮다
#R-squared(결정계수, r제곱값)
#모델이 종속변수 값을 얼마나 잘 설명하는가
#0.7509 => 75% 설명하고 있다

#독립/종속 변수: 선형 가정

#비선형 관계: 높은 차수 항을 모델 추가
insurance$age2<-insurance$age^2 #제곱한 값
#lm(expenses~age+age2)

#BMI 30을 기준
insurance$bmi30<-ifelse(insurance$bmi>=30,1,0)

ins_model2<-lm(expenses~age+age2+children+bmi+sex+bmi30*smoker+region, 
               data=insurance)
summary(ins_model2)
