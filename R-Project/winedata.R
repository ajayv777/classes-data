winedata<- read.csv(file.choose(), header = T)
winedata
summary(winedata)
head(winedata)


train = sample(1:nrow(winedata),nrow(winedata)/2)

wine_train <- winedata[train,]
wine_test <- winedata[-train,]
summary(wine_train)
head(wine_train)
str(wine_train)


fit = lda(Type~., data = wine_train)


pred_train = predict(fit,wine_train)
pred_test = predict(fit,wine_test)

dim(wine_train)
dim(wine_test)
pred_cltr = pred_train$class
pred_clts = pred_test$class
library(caret)
library("pROC")
library("e1071")

confusionMatrix(table(pred_clts, wine_test$Type))

confusionMatrix(table(pred_cltr, wine_train$Type))

table(pred_clts, wine_test$Type)
