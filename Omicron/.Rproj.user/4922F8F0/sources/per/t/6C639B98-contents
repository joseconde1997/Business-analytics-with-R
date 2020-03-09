library(readxl)
library(tidyverse)
library(class)
library(descr)
library(knitr)
library(readr)
library(tibble)
library(randomForest)
library(caret)
library(tree)
library(mlbench)
library(tidyverse)
library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)
library(adabag)
library(kknn)
library(dplyr)
d <- read_excel("omicron.xlsx",sheet = "Data")
mod <- lm(CHURN~.-ID,d)
summary(mod)
#linear regression
mod <- lm(CHURN~ACLENGTH+INTPLAN+DATAPLAN+as.factor(DATAGB)+OMMIN+OMCALL+OTMIN+OTCALL+NGMIN+IMIN+as.factor(ICALL)+as.factor(CUSCALL),d)
summary(mod)
pred <- predict(mod,d)
median(pred)
pred <- ifelse(pred>median(pred),1,0)
cor(pred,d$CHURN)
View(pred)
CHURN <- as.factor(d$CHURN)
pred <- as.factor(pred)
multi <- pred*CHURN
confusionMatrix(pred,CHURN,dnn=c("Predicted","Actual"))

#KNN
d_IDless <- d[,2:15]
CHURN  <- ifelse(d_IDless$CHURN==1,"Churn","NO_CHURN")
d_IDless$DATAGB <- ifelse(d_IDless$DATAGB==0,0,1)
d_IDless <- as_tibble(scale(d_IDless[1:13]))
d_IDless <- d_IDless %>% filter(complete.cases(.))
d_IDless$CHURN <- CHURN
set.seed(123)
training <- sample(5000,5000*0.8)
d_train <- d_IDless[training,]
d_test <- d_IDless[-training,]
train <- d_train[,-14]
test <- d_test[,-14]
cl <- d_train[,14]
pred <-knn(train,test,cl,k=20)
actual <- d_test[,11]
CrossTable(x=prediction,y=actual,prop.chisq = F)
#knn
d_IDless <- d[,c(2,3,13)]
d_IDless <- as_tibble(scale(d_IDless))
d_IDless$CHURN <- ifelse(d$CHURN==1,"CHURN","NO_CHURN")
training <- sample(5000,5000*0.8)
d_train <- d_IDless[training,]
d_test <- d_IDless[-training,]
train <- d_train[,-4]
test <- d_test[,-4]
cl <- d_train[,4]
pred <-knn(train,test,cl,k=20)
actual <- d_test[,11]

# randm forest
rf <- randomForest(as.factor(CHURN)~.,data=d_train,n_tree=500,importance=T)
pred <- predict(rf,d_train)
actual <- d_train[,14]
actual$pred <- pred
confusionMatrix(pred,d_test$CHURN,dnn=c("Predicted","Actual"))
#boosted trees
boost <- boosting(CHURN~.,mfinal=10,data=d_train)
#logistic regression
d_sel <- d[,c(2,3,4,6,7,8,9,12,13,14,15)]
d_sel
d_sel$ACLENGTH <- ifelse(d_sel$ACLENGTH>mean(d_sel$ACLENGTH),1,0)
d_sel$OMMIN <- ifelse(d_sel$OMMIN>mean(d_sel$OMMIN),1,0)
d_sel$OMCALL <- ifelse(d_sel$OMCALL>mean(d_sel$OMCALL),1,0)
d_sel$OTMIN <- ifelse(d_sel$OTMIN>mean(d_sel$OTMIN),1,0)
d_sel$OTCALL <- ifelse(d_sel$OTCALL>mean(d_sel$OTCALL),1,0)
d_sel$IMIN <- ifelse(d_sel$IMIN>mean(d_sel$IMIN),1,0)
d_sel$IMIN <- ifelse(d_sel$IMIN>mean(d_sel$IMIN),1,0)
d_sel$ICALL <- ifelse(d_sel$ICALL>mean(d_sel$ICALL),1,0)
mod <- lm(CHURN~.,d_sel)
summary(mod)
d_sel <- d[,c(2,3,4,6,7,8,9,12,13,14,15)]
d_sel
d_sel$ACLENGTH <- ifelse(d_sel$ACLENGTH>median(d_sel$ACLENGTH),1,0)
d_sel$OMMIN <- ifelse(d_sel$OMMIN>median(d_sel$OMMIN),1,0)
d_sel$OMCALL <- ifelse(d_sel$OMCALL>median(d_sel$OMCALL),1,0)
d_sel$OTMIN <- ifelse(d_sel$OTMIN>median(d_sel$OTMIN),1,0)
d_sel$OTCALL <- ifelse(d_sel$OTCALL>median(d_sel$OTCALL),1,0)
d_sel$IMIN <- ifelse(d_sel$IMIN>median(d_sel$IMIN),1,0)
d_sel$IMIN <- ifelse(d_sel$IMIN>median(d_sel$IMIN),1,0)
d_sel$ICALL <- ifelse(d_sel$ICALL>median(d_sel$ICALL),1,0)
mod <- lm(CHURN~.,d_sel)
summary(mod)
#Average by group
group <- d %>% group_by(CUSCALL) %>% summarise(mean(CHURN))
d_num <- select_if(d,is.numeric)
d_num
cor(d_num)
View(cor(d_num))
#Roberto
d_ro <- d[,c(3,6,14,15)]
d_ro
d_ro$OMMIN <- ifelse(d_ro$OMMIN>200,1,0)
d_ro$CUSCALL <- ifelse(d_ro$CUSCALL>3,1,0)
mod <- lm(CHURN~.,d_ro)
summary(mod)
pred_ro <- predict(mod,d_ro)                     
pred_ro <- ifelse(pred_ro>mean(pred_ro),1,0)
confusionMatrix(as.factor(pred),as.factor(d_ro$CHURN),dnn=c("Predicted","Actual"))
