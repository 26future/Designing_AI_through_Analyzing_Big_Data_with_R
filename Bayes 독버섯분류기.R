mushrooms<-read.csv("dataset_for_ml/mushrooms.csv")
View(mushrooms)
dim(mushrooms)

is.null(mushrooms)

set.seed(123)
mushrooms_sample<-sample(8124,5687)
mushrooms_train<-mushrooms[mushrooms_sample,]
mushrooms_test<-mushrooms[-mushrooms_sample,]

mushrooms_train_label<-mushrooms[mushrooms_sample,1]
mushrooms_test_label<-mushrooms[-mushrooms_sample,1]


library(e1071)
mushroom_classifier<-naiveBayes(mushrooms_train, mushrooms_train_label)
mushroom_test_pred<-predict(mushroom_classifier,mushrooms_test)
mushroom_test_pred

library(gmodels)
CrossTable(mushroom_test_pred,mushrooms_test_label,
           prop.t = FALSE, prop.r = FALSE,
           dnn=c("predicted","actual"))

#laplace=1 적용
mushroom_classifier2<-naiveBayes(mushrooms_train, mushrooms_train_label, laplace = 1)
mushroom_test_pred2<-predict(mushroom_classifier2,mushrooms_test)
mushroom_test_pred2

CrossTable(mushroom_test_pred2, mushrooms_test_label,
           prop.t = FALSE, prop.r = FALSE,
           dnn = c("predicted","actual"))
