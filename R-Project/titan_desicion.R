library(caret)
titan<- read.csv(file.choose(), header = T)
head(titan)
shuffle_index<-sample(1:nrow(titan))


titan <- titan[shuffle_index,]
head(titan)
View(titan)

install.packages("dplyr")
library(dplyr)
clean_titan<- titan %>%
  select(-c(home.dest,cabin,name,x,ticket))%>%  
  mutate(pclass= factor(pclass,levels = c(1,2,3),labels = c('Upper','Middle','Lower')),  
        survived = factor(survived,levels = c(0,1), labels = c('no','yes'))) %>%  
  na.omit()


head(clean_titan)
glimpse(clean_titan)
str(clean_titan)

summary(clean_titan)

clean_titan$age<-as.numeric(clean_titan$age)

str(clean_titan)
median(clean_titan$age)

clean_titan$age<-ifelse(clean_titan$age=="?",33,clean_titan$age)
summary(clean_titan)



View(clean_titan)
#replace question mark

idx<-clean_titan$embarked == "?"
is.na(clean_titan$embarked)<- idx
clean_titan = na.omit(clean_titan)
summary(clean_titan)
str(clean_titan)


set.seed(1234)
create_train_test <-
  function(clean_titan,size = 0.8, train = TRUE ) {
    n_row = nrow(clean_titan)
    total_row = size * n_row
    train_sample <- 1:total_row
    if (train ==  TRUE)  {
      return (clean_titan[train_sample,])
    }
    else {
      return (clean_titan[-train_sample,])
    }
  }

titan_train <- create_train_test(clean_titan, 0.8, train = TRUE)
titan_test<- create_train_test(clean_titan, 0.8,train = FALSE)
dim(clean_titan)
dim(titan_train)
dim(titan_test)



##check randomization

prop.table(table(titan_test$survived)
prop.table(table(titan_train$survived))



library(rpart)
library(rpart.plot)
fit<-rpart(survived~.,data = titan_train, method = 'class')
summary(fit)
plot(fit)
text(fit)
rpart.plot(fit, uniform =  TRUE, extra = 106)


titan_train$score=predict(fit,newdata=titan_train,type = 'class')
titan_train$score
library(lattice)
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
confusionMatrix(table(titan_train$score,titan_train$survived),positive = "yes")


titan_test$score=predict(fit,newdata=titan_test,type='class')
table_mat = table(titan_test$survived,titan_test$score)
confusionMatrix(table(titan_test$survived,titan_test$score),positive = "yes")

# https://www.guru99.com/r-decision-trees.html
#R in action
#practical data science with R

#accuracy test

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

#Tune the hyper-parameter
library(rpart)
rpart.control(minbucket = round(20/3), maxdepth = 30)      
