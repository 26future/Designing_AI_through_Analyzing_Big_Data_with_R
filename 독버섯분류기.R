mushrooms<-read.csv("dataset_for_ml/mushrooms.csv")
mushrooms
str(mushrooms)
dim(mushrooms)
View(mushrooms)

mushrooms[,1]
table(mushrooms[,1])

table(is.na(mushrooms))

set.seed(123)
train_sample<-sample(8124,5687)
mushrooms_train<-mushrooms[train_sample,]
mushrooms_test<-mushrooms[-train_sample,]

table(mushrooms_train$type)
table(mushrooms_test$type)

library(C50)
#train
mushrooms_model<-C5.0(mushrooms_train[-1],mushrooms_train$type)
mushrooms_model
summary(mushrooms_model)

#test
mushrooms_pred<-predict(mushrooms_model,mushrooms_test[-1])
mushrooms_pred

library(gmodels)
CrossTable(mushrooms_test$type,mushrooms_pred,
           prop.c = FALSE, prop.r = FALSE,
           dnn = c("actual","predicted"))


library(readr)
library(rpart)
library(rpart.plot)
library(dplyr)
library(ggplot2)

DT<-rpart(type~.,mushrooms_train, method = "class")
DT
summary(DT)

predict_dt<-predict(DT, mushrooms_test, type="class")

CrossTable(mushrooms_test$type,predict_dt,
           prop.c = FALSE, prop.r = FALSE,
           dnn = c("actual","predicted"))
mushrooms_train


mushrooms<-mushrooms[-17]
mushrooms
View(mushrooms)

set.seed(123)
mushrooms_sample<-sample(8124,5687)
mushrooms_train<-mushrooms[mushrooms_sample,]
mushrooms_test<-mushrooms[-mushrooms_sample,]

#train
mushrooms_model<-C5.0(mushrooms_train[-1],mushrooms_train$type)
mushrooms_model
summary(mushrooms_model)

#test
mushrooms_pred<-predict(mushrooms_model,mushrooms_test[-1])
mushrooms_pred

CrossTable(mushrooms_test$type,mushrooms_pred,
           prop.c = FALSE, prop.r = FALSE,
           dnn = c("actual","predicted"))
