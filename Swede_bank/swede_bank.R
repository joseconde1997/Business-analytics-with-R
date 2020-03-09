library(readxl)
library(tidyverse)
library(randomForest)
library(adabag)
library(class)
library(rpart)
library(rpart.plot)
library(caret)
library(nnet)
d <- read_excel("swede_bank.xlsx")
str(d)
d <- d %>% filter(complete.cases(.))
d <- d %>% mutate_all(as.factor)

d$amount <- as.numeric(d$amount)
d$duration <- as.numeric(d$duration)
d$age <- as.numeric(d$age)

#tree
train <- sample(length(d$check),0.8*length(d$check))
train_t <- d[train,]
test_t <- d[-train,]
d_tree <- rpart(default~.,data=train_t,method="class")
prp(d_tree, type=2, extra=1, varlen=0, split.font=2)
pred <- predict(d_tree,train_t)
pred <- as.data.frame(pred)
prediction <- ifelse(pred$no>0.5,"no","yes")
confusionMatrix(as.factor(prediction),as.factor(train_t$default),dnn=c("Predicted","Actual"))
pred <- predict(d_tree,test_t)
pred <- as.data.frame(pred)
prediction <- ifelse(pred$no>0.5,"no","yes")
confusionMatrix(as.factor(prediction),as.factor(test_t$default),dnn=c("Predicted","Actual"))

#random forest
rf <- randomForest(as.factor(default)~.,data=train_t,ntree=10000,importance=T)
pred <- predict(rf,train_t)
confusionMatrix(as.factor(pred),as.factor(train_t$default),dnn=c("Predicted","Actual"))
pred <- predict(rf,test_t)
confusionMatrix(as.factor(pred),as.factor(test_t$default),dnn=c("Predicted","Actual"))

#knn
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
d <- read_excel("swede_bank.xlsx")
str(d)
d <- d %>% filter(complete.cases(.))
d <- d %>% mutate_all(as.factor)
d <- d %>% mutate_all(as.numeric)
d <-lapply(d, normalize)
d <- as.data.frame(d)
train_t <- d[train,]
test_t <- d[-train,]
set.seed(123)
knn <- knn(train = train_t[,-17],test = test_t[,-17],cl=train_t[,17],k=30)
actual <- test_t[,17]
confusionMatrix(as.factor(knn),as.factor(actual),dnn=c("Predicted","Actual"))
#nnet
d <- read_excel("swede_bank.xlsx")
str(d)
d <- d %>% filter(complete.cases(.))
d <- d %>% mutate_all(as.factor)
d <- d %>% mutate_all(as.numeric)
d <-lapply(d, normalize)
d <- as.data.frame(d)

train_t <- d[train,]
test_t <- d[-train,]
mean(test_t[,17])

nnet <- neuralnet::neuralnet(default~.,data=train_t,linear.output=F,hidden=30,err.fct="sse",threshold=0.05)

pred <- neuralnet::compute(nnet,data.frame(test_t[,1:16]))
mean(pred$net.result)

predicted.class <- ifelse(pred$net.result>mean(pred$net.result),1,0)
confusionMatrix(as.factor(predicted.class),as.factor(test_t[,17]),dnn=c("Predicted","Actual"))

#
