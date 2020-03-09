library(class)
library(descr)
d <- smart_num_sd
d$amoled <- factor(ifelse(smart2$display=="AMOLED","AMOLED","NO AMOLED"))
d <- data.frame(d)
set.seed(123)
train_sample <- sample(1744,1395)
length(train_sample)
d_train <- d[train_sample,]
d_test <- d[-train_sample,]
set.seed(123)
prediction <- knn(train = d_train[,-11],test = d_test[,-11],cl=d_train[,11],k=20)
actual <- d_test[,11]
CrossTable(x=prediction,y=actual,prop.chisq = F)
