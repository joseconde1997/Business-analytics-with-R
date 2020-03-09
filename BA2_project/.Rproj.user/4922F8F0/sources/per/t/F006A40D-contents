library(readxl)
library(tidyverse)
library(lubridate)
library(dplyr)
library(forecast)
library(ggplot2)
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
year <-rep(c(1:7), times = c(12,12,12,12,12,12,12))
all_years <- c(d_2$O7,d_2$O8,d_2$O9,d_2$O10,d_2$O11,d_2$O12,d_2$O13)
all_years <- data.frame(month=c(1:12),transactions=as.numeric(all_years),year=year)

train_all <- all_years[1:72,]
test_all <- all_years[73:84,]
train.ts <- ts(train_all[,2],frequency = 12)
test.ts <- ts(test_all[,2],frequency = 12)
d.ts <-ts(all_years[,2],frequency = 12)
arima_all <- auto.arima(train.ts,seasonal = T)
pred <- forecast(arima_all,h=12)
plot(train.ts,type="l",xlim=c(0,8))
lines(pred$fitted,type="l",col="blue")
autoplot(forecast(arima_all,h=12,level = c(90))) +
  autolayer(d.ts, series="Data") 
cor(test.ts,pred$mean)^2

cor(train.ts,arima_all$fitted,use="complete.obs")^2
p_train <- data.frame(Actual=train.ts,Predicted=arima$fitted)
coefficient<- 1.645
div_sd <- sd(all_years$predict)
adjusted_sd <- length(all_years$predict)
p_train$upper <-p_train$Predicted +coefficient*sd(p_train$Predicted-p_train$Actual,na.rm=T)
p_train$lower <-p_train$Predicted -coefficient*sd(p_train$Predicted-p_train$Actual,na.rm=T)
fore <- forecast(arima_all,h=12,level=c(90))
zero <- rep(c(NA), times = 72)
fore <- c(zero,fore$mean)
zero <- rep(c(NA), times = 12)
train <-c(p_train$Actual,zero)
predicted <- c(p_train$Predicted,zero)
upper <- c(p_train$upper,zero)
lower <- c(p_train$lower,zero)
zero <- rep(c(NA), times = 72)
test <- c(zero,test.ts)
t_upper <- c(fore+coefficient*sd(fore-test,na.rm = T))
t_lower <-c(fore-coefficient*sd(fore-test,na.rm = T))
p_train <- data.frame(Actual=train,Predicted=predicted,Test=test,Forecast=fore,Index=c(1:84),
                      Upper=upper,Lower=lower,t_Upper=t_upper,t_Lower=t_lower)
p_train
rms <-(1/length(p_train$Forecast[73:84]))*(p_train$Forecast[73:84]-p_train$Test[73:84])^2
rms <- sqrt(rms)
sum(rms)
ggplot(data=p_train) + geom_line(aes(y=Actual,x=Index,color="Actual")) +
  geom_line(aes(y=Predicted,x=Index,color="Predicted")) + 
  geom_line(aes(y=Forecast,x=Index,color="Forecast")) + 
  geom_line(aes(y=Test,x=Index,color="Test")) + 
  geom_ribbon(aes(ymin=Lower,ymax=Upper,x=Index),alpha=0.2)+
  geom_ribbon(aes(ymin=t_Lower,ymax=t_Upper,x=Index),alpha=0.2)+
  scale_color_manual(values=c("Black", "Blue","Blue","Black"))


