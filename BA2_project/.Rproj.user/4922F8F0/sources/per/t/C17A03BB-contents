library(readxl)
library(tidyverse)
library(lubridate)
library(dplyr)
library(forecast)
#Wires November
d <- read_excel("amarcord.xlsx",sheet = "November 2010 Wires")
names(d) <- c("Amount","Date","time","ID")
d$week <- week(d$Date)
d$hour <- hour(d$time)
d$minutes <- minute(d$time)
d$seconds <- second(d$time)
d$day <- day(d$Date)
#Wires December
d_2 <- read_excel("amarcord.xlsx",sheet = "Wires By Month")
#rearrange data
names(d_2)<- c("Month","O7","O8","O9","O10","O11","O12","O13")
d_2$index <- month(d_2$Month)
d_2[11,5] <- 396.812
d_2[,2:9]<- lapply(d_2[,2:9], function(x) as.numeric(as.character(x)))
d_2[7,3] <- NA
d_2[4,7] <- NA
d_2[12,2] <- NA
year <-rep(c(1:7), times = c(12,12,12,12,12,12,12))
all_years <- c(d_2$O7,d_2$O8,d_2$O9,d_2$O10,d_2$O11,d_2$O12,d_2$O13)
all_years <- data.frame(month=c(1:12),transactions=as.numeric(all_years),year=year)
train <- all_years[1:72,]
non_na <- train %>% filter(complete.cases(.))
non_na <- non_na[-12,]
#Getting the trend
lm_trend <- lm(transactions~.,non_na)
summary(lm_trend)
non_na$trend <- predict(lm_trend)
all_years$trend <- predict(lm_trend,all_years)
#getting the seasonality
non_na$multi <- non_na$transactions/non_na$trend
season <- non_na %>% group_by(month) %>% summarise(mean(multi))
non_na <- left_join(non_na,season)
names(non_na) <- c("month","transactions","year","trend","multi","season")
season_month <- non_na$season[1:11]
season_month <- c(season_month,non_na$season[20])
all_years$season <-rep(season_month,7)
#prediction with season*trend
non_na$predict <- non_na$season*non_na$trend
non_na$error <- non_na$transactions-non_na$predict
plot(non_na$transactions,type="l",col="black")
lines(non_na$predict,type = "l",col="blue")
error_by_month_na <- non_na %>% group_by(month) %>% summarize(mean(abs(error)))
error_by_month_na
all_years$predict <- all_years$season*all_years$trend
all_years$error <- all_years$transactions-all_years$predict
plot(all_years$transactions,type="l",col="black")
lines(all_years$predict,type = "l",col="blue")
error_by_month_all <- all_years %>% group_by(month) %>% summarize(mean(abs(error),na.rm = T))
error_by_month_all
#90% confidence calculations for multiplicative trend:seasonal
coefficient<- 1.645
div_sd <- sd(all_years$predict)
adjusted_sd <- length(all_years$predict)
multi_CI <- adjusted_sd*coefficient
all_years$upper <- all_years$transactions+multi_CI
all_years$lower <- all_years$transactions - multi_CI
plot(all_years$transactions,type="l",col="black",ylim = c(0,700))
lines(all_years$upper, lty = 'dashed', col = 'red')
lines(all_years$lower, lty = 'dashed', col = 'red')
lines(all_years$predict,type = "l",col="blue")
error_by_year_all <- all_years %>% group_by(year) %>% summarize(mean(abs(error),na.rm = T))
cor(all_years[73:84,2],all_years[73:84,6])^2
cor(all_years[,2],all_years[,6],use="complete.obs")^2
#ARIMA 
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

#holt-winters
data <- all_years$transactions
data[12]<-NA
data <- data.frame(actual=data)
data
for (i in 1:84) {
  if(is.na(data[i,1])){
    data[i,1] <- all_years$predict[i]
  }
}
d.ts <- ts(data,frequency = 12)
train.ts <-window(d.ts,start=c(1,1),end=c(1,72))
test.ts <- window(d.ts,start=c(1,73),end=c(1,84))
hw <- ets(train.ts,model = "MAA")
hw
cor(hw$fitted,train.ts)^2
plot(forecast(hw, h=12, level=c(90)),xlim=c(0,10))
fore <-forecast(hw, h=12, level=c(90))
lines(test.ts,col="black",type="l")
cor(fore$mean,test.ts)^2
hw_all <- ets(d.ts,model="MAM")
cor(hw_all$fitted,d.ts)^2
plot(hw_all$fitted,col="blue",xlim=c(0,10),ylim=c(340,500))
#hw final
train.ts <- arima_all$fitted
hw <- ets(train.ts,model = "MAM")
cor(hw$fitted,train.ts)^2
fore <- forecast(hw, h=12, level=c(90))
cor(test.ts,fore$mean)^2

