library(rpart.plot)
library(readxl)
library(rpart)
library(tidyverse)
library(caret)
library(randomForest)
library(class)
library(adabag)

#import data
d <- read_excel("wisc_bc_data.xlsx")
View(d)
tree <- d[,2:32]
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
tree$diagnosis <- ifelse(tree$diagnosis=="B",0,1)
tree <- as.data.frame(lapply(tree,normalize))
#Reduction of data 

#look for colinearity in 1st
tree <- select(tree,-c("perimeter_mean","area_mean","radius_worst","perimeter_worst","area_worst"))
#look for colinearity in 2nd
tree <- select(tree,-c("texture_worst"))
#look for colinearity in 3rd
tree <- select(tree,-c("concavity_mean"))
#look for colinearity in se
tree <- select(tree,-c("perimeter_se","area_se"))
#look for colinearity in 
tree <- select(tree,-c("concavity_worst"))
tree <- select(tree,-c("points_worst"))



#Decission tree
train <- sample(569,569*0.8)
train_t <- tree[train,]
test_t <- tree[-train,]
set.seed(123)
tree <-rpart(diagnosis~.,data=train_t,method = "class",control = rpart.control(maxdepth = 5))
tree
prp(tree,type=4,extra=1,split.font=1,varlen=-10)
pred <- predict(tree,train_t,type="class")
conf <- confusionMatrix(pred,as.factor(train_t$diagnosis),dnn=c("Predicted","Actual"))
conf
#test data
pred <- predict(tree,test_t,type="class")
conf <- confusionMatrix(pred,as.factor(test_t$diagnosis),dnn=c("Predicted","Actual"))
conf

#random forest
rf <-randomForest(as.factor(diagnosis)~.,data=train_t,
                  ntree=500,importance=T)
pred <- predict(rf,train_t)
conf <- confusionMatrix(pred,as.factor(train_t$diagnosis),dnn = c("Predicted","Actual"))
conf
#testing data
pred <- predict(rf,test_t)
conf <- confusionMatrix(pred,as.factor(test_t$diagnosis),dnn = c("Predicted","Actual"))
conf
plot(randomForest(as.factor(diagnosis) ~ ., data=train_t, keep.forest=FALSE, ntree=500), log="y")
getTree(rf, 1, labelVar=TRUE)

#Knn
set.seed(123)
pred <- knn(train=train_t[,-1],test=test_t[,-11],cl=train_t[,1],k=8)
pred
actual <- test_t[,1]
table(x=pred,y=actual)

#boosting trees
train_t[,1]<- factor(train_t[,1])
test_t[,1]<- factor(test_t[,1])
boost <- boosting(diagnosis~.,mfinal=120,data=train_t)
pred <- predict(boost,test_t)
confusionMatrix(as.factor(pred$class),test_t$diagnosis,dnn=c("Predicted","Actual"))
