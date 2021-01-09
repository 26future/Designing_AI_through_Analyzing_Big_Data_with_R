colnames(iris)
#"Sepal.Length" "Sepal.Width"  "Petal.Length"
#"Petal.Width"
#species는 제외
#data를 random shuffle
#70:30 분할(train/test)
#cor(hidden layer 수를 변경해가면서)
###########################################################
View(iris)
iris<-iris[,-5]

#정규화
normalize<-function(x){
  return(x-min(x))/(max(x)-min(x))
}
iris_norm<-as.data.frame(lapply(iris,normalize))
summary(iris_norm)

#train/test
set.seed(123)
dim(iris_norm)
iris_sample<-sample(150,105)
iris_train<-iris_norm[iris_sample,]
iris_test<-iris_norm[-iris_sample,]

#모델 생성
library(neuralnet)
iris_model<-neuralnet(formula = Petal.Width~Sepal.Length+Sepal.Width+Petal.Length,
                      data = iris_train)

#시각화
plot(iris_model)

#예측
model_results<-compute(iris_model,iris_test)
str(model_results)

pre_iris<-model_results$net.result
pre_iris

#상관관계 구하기 0.9632448
cor(pre_iris,iris_test$Petal.Width)
##################################################################
#hidden layer=5   
#모델 생성
iris_model5<-neuralnet(formula = Petal.Width~Sepal.Length+Sepal.Width+Petal.Length,
                      data = iris_train, hidden = 5)

#예측
model_results5<-compute(iris_model5,iris_test)
str(model_results5)
pre_iris5<-model_results5$net.result
pre_iris5

#상관관계 구하기 0.9565131
cor(pre_iris5,iris_test$Petal.Width)

####################################################################
#hidden layer=4
#모델 생성
iris_model4<-neuralnet(formula = Petal.Width~Sepal.Length+Sepal.Width+Petal.Length,
                       data = iris_train)

#예측
model_results4<-compute(iris_model4, iris_test)
model_results4

pre_iris4<-model_results4$net.result
pre_iris4

#상관관계 분석  0.9632581
cor(pre_iris4,iris_test$Petal.Width)

###################################################################
#hidden layer=3
#모델 생성
iris_model3<-neuralnet(formula = Petal.Width~Sepal.Length+Sepal.Width+Petal.Length,
          data=iris_train)
iris_model3

#예측
model_results3<-compute(iris_model3,iris_test)
model_results3

pre_iris3<-model_results3$net.result
pre_iris3

#상관관계 분석  0.9632621
cor(pre_iris3,iris_test$Petal.Width)

#####################################################################
#hidden layer=2
#모델 생성
iris_model2<-neuralnet(formula = Petal.Width~Sepal.Length+Sepal.Width+Petal.Length,
          data = iris_train)

#예측
model_results2<-compute(iris_model2, iris_test)
model_results2

pre_iris2<-model_results2$net.result
pre_iris2

#상관관계 분석  0.9632691
cor(pre_iris2,iris_test$Petal.Width)

##############################################################
#hidden layer=6
#모델 생성
iris_model6<-neuralnet(formula = Petal.Width~Sepal.Length+Sepal.Width+Petal.Length,
          data = iris_train)

#예측하기
model_results6<-compute(iris_model6, iris_test)
model_results6

pre_iris6<-model_results6$net.result
pre_iris6

#상관관계 분석  0.9630955
cor(pre_iris6,iris_test$Petal.Width)
