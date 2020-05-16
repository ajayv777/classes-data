library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(party)
library(RColorBrewer)
library(ROCR)
library(class)
library(rpart)
library(rattle)
library(rpart.plot)
install.packages("randomForest")
library(randomForest)
install.packages("pROC")
library(pROC)
library(e1071)

hr_data<- read.csv(file.choose(), header = T)
summary(hr_data)
str(hr_data)
dim(hr_data)

hr_data1 = hr_data[,-c(9,10)]
str(hr_data1)

shuffle_index<-sample(1:nrow(hr_data))
hr_data <- hr_data[shuffle_index,]


hr_data1$role_code = as.factor(hr_data$role_code)
hr_data1$salary.code= as.factor(hr_data$salary.code)

head(hr_data1)


set.seed(1234)
create_train_test <-
  function(hr_data1,size = 0.7, train = TRUE ) {
    n_row = nrow(hr_data1)
    total_row = size * n_row
    train_sample <- 1:total_row
    if (train ==  TRUE)  {
      return (hr_data1[train_sample,])
    }
    else {
      return (hr_data1[-train_sample,])
    }
  }

hrdata_train <- create_train_test(hr_data1, 0.7, train = TRUE)
hrdata_test<- create_train_test(hr_data1, 0.7,train = FALSE)


print((table(hrdata_train$left)))
print(table(hrdata_test$left))
dim(hrdata_train)
dim(hrdata_test)




#random forrest
modelrf <- randomForest(as.factor(left)~.,data = hrdata_train, do.trace = T)
?importance
importance(modelrf)
varImpPlot(modelrf)
summary(modelrf)


hrdata_train$predleft <- predict(modelrf, hrdata_train)
hrdata_test$predtest <- predict(modelrf, hrdata_test)


confusionMatrix(as.factor(hrdata_train$predleft),as.factor(hrdata_train$left))
confusionMatrix(table(hrdata_train$predleft,hrdata_train$left))

confusionMatrix(table(hrdata_test$predtest,hrdata_test$left))



aucrf_tr <- plot.roc(as.numeric(hrdata_train$predleft),as.numeric(hrdata_train$left), ci = TRUE)
aucrf_ts <- plot.roc(as.numeric(hrdata_test$predtest),as.numeric(hrdata_test$left), ci = TRUE)

summary(aucrf_tr)

plot.roc(aucrf_tr, ylim=c(0,1), print.thres=TRUE, main=paste('Random Forest AUC:',round(aucrf_tr$auc[[1]],3)),col = 'blue')
plot.roc(aucrf_ts, ylim=c(0,1), print.thres=TRUE, main=paste('Random Forest AUC:',round(aucrf_ts$auc[[1]],3)),col = 'blue')


round(aucrf_tr$auc[[1]],5)
