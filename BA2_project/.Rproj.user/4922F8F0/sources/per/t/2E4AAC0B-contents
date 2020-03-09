library(readxl)
library(tidyverse)
library(lubridate)
library(dplyr)
library(forecast)
library(gtable)
d_2 <- read_excel("amarcord.xlsx",sheet = "Wires By Month")
#rearrange data
names(d_2)<- c("Month","O7","O8","O9","O10","O11","O12","O13")
d_2$index <- month(d_2$Month)
d_2[11,5] <- 396.812
d_2 [8,5] <- d_2 [8,5]-37
d_2[,2:9]<- lapply(d_2[,2:9], function(x) as.numeric(as.character(x)))
d_2[7,3] <- NA
d_2[4,7] <- NA
d_2[12,2] <- NA
all_years <- c(d_2$O7,d_2$O8,d_2$O9,d_2$O10,d_2$O11,d_2$O12,d_2$O13)
all_years <- data.frame(month=c(1:12),transactions=as.numeric(all_years),year=year)
train <- all_years[1:72,]
non_na <- train %>% filter(complete.cases(.))
non_na <- non_na[-12,]
non_na$index <- c(1:length(non_na$month))
#Getting the trend
lm_trend <- lm(transactions~month+year,non_na)
summary(lm_trend)
plot(predict(lm_trend),col="blue",type = "l",ylim=c(300,600))
lines(non_na$transactions)
non_na$trend <- predict(lm_trend)
non_na$sub <- non_na$transactions-non_na$trend
season <- non_na %>% group_by(month) %>% summarise(Season=mean(sub))
season
non_na <- left_join(non_na,season)
non_na$predict <- non_na$trend+non_na$Season
plot(non_na$transactions,type = "l")
lines(non_na$predict,col="blue")
non_na
cor(non_na$pred,non_na$transactions)^2
ggplot(data=non_na) + geom_line(aes(y=transactions,x=index,color="Actual"),size=1) +
  geom_line(aes(y=predict,x=index,color="Predicted"))+scale_color_manual(values=c("black", "blue"))


test <- all_years[73:84,]
test$index <- c(73:84)
test$trend <- predict(lm_trend,test)
test$sub <- test$transactions/test$trend
test <- left_join(test,season)
test$predict <-test$trend+test$Season
plot(test$transactions,type = "l", ylim=c(350,600))
lines(test$predict,col="blue")
cor(test$predict,test$transactions)^2
rms <-(1/length(test$predict))*(test$predict-test$transactions)^2
rms <- sqrt(rms)
sum(rms)
#Applied to all years
all_years$trend <-predict(lm_trend,all_years)
all_years$sub <- all_years$transactions-all_years$trend
all_years <- left_join(all_years,season)
all_years$predict <- all_years$trend+all_years$Season
all_years$index <- c(1:84)
all_years$upper <-all_years$predict +1.654*sd(all_years$predict-all_years$transactions,na.rm=T)
all_years$lower <- all_years$predict - 1.654*sd(all_years$predict-all_years$transactions,na.rm=T)

cor(all_years$transactions,all_years$predict,use="complete.obs")^2
ggplot(data=all_years) + geom_line(aes(y=transactions,x=index,color="Actual"),size=1) +
  geom_line(aes(y=predict,x=index,color="Predicted")) + 
  geom_ribbon(aes(ymin=lower,ymax=upper,x=index),alpha=0.2)+scale_color_manual(values=c("black", "blue"))

