library(readr)
library(tidyverse)
library(class)
library(descr)

smart<- read.csv(file="https://raw.githubusercontent.com/wdecisions/ba/master/smartphones.csv")
smart2 <- smart %>% dplyr::filter(complete.cases(.))
smart_num <- select(smart2,weight:release)
smart_num_sd <- as_tibble(scale(smart_num))
d1 <- cbind(model=smart2$model,smart_num_sd)
d1 <- d1 %>% 
  filter(model=="iPhone 7 Plus"|model=="iPhone 7"|model=="Lumia 950 XL")
d1 <- select(d1,model,ppi,size)
d1
d1 <- as_tibble(scale(select(smart2,weight:release)))
d1$model <- smart2$model
d1 <- filter(d1,model=="iPhone 7 Plus"|model=="iPhone 7"|model=="Lumia 950 XL")
d1 <- column_to_rownames(d1,var = "model")
round(dist(d1),2)
round(dist(d1),2)
d <- smart_num_sd
d$amoled <- factor(ifelse(smart2$display=="AMOLED","AMOLED","NO AMOLED"))
d <- data.frame(d)
View(d)
set.seed(123)
train_sample <- sample(1744,1395)
length(train_sample)
d_train <- d[train_sample,]
d_test <- d[-train_sample,]
prediction <- knn(train = d_train[,-11],test = d_test[,-11],cl=d_train[,11],k=20)
actual <- d_test[,11]
CrossTable(x=prediction,y=actual,prop.chisq = F)
