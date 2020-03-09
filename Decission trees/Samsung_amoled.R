library(tidyverse)
library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)
library()
smart <- read.csv("https://raw.githubusercontent.com/wdecisions/ba/master/smartphones.csv")
smart2 <- smart %>% filter(complete.cases(.))
d <- select(smart2,weight:release)
d$amoled <- factor(ifelse(smart2$display=="AMOLED","AMOLED","NO AMOLED"))
d$samsung <- factor(ifelse(smart2$brand=="Samsung",1,0))
d <- data.frame(d)
train_sample <- sample(1774,1395)
d_train <- d[train_sample,]
d_test <- d[-train_sample,]
set.seed(123)
tree <- rpart(amoled~.,data=d_train,method = 'class',control=rpart.control(maxdepth = 4))
tree
prp(tree,type = 4,extra = 1,split.font = 1,varlen=-10)
tree <- rpart(amoled~.,data=d_train,method = 'class')
tree
pred <- predict(tree,d_train,type = "class")
conf <- confusionMatrix(pred,d_train$amoled,dnn=c("Predicted","Actual"))
conf
