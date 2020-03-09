library(readxl)
library(tidyverse)
library(forecast)
d <- read_excel("guarini.xlsx",sheet = "Data")
d$lag <- lag(d$SALES)
d$lag2 <- lag(d$lag)
d$lag3 <- lag(d$HOLIDAY)
train <- round(104*0.8)
test <- length(d$SALES)-train
plot(d$SALES,type="l")
abline(lm(d$SALES~d$WEEK))
cor(d[,c(3,6,7,8)])
mod <- lm(SALES~as.factor(MONTH)+as.factor(WEEKMONTH)+as.factor(HOLIDAY)+as.factor(SUMMER)+lag+lag2,d[1:train,])
summary(mod)
pred <- predict(mod,d[(train+1):(train+test),])
x <- pred
y <- d$SALES[(train+1):(train+test)]
error <- x-y
cor(x,y)^2
plot(y,type = "l")
lines(pred,col="blue",type = "l")
plot(x,y,bty="l")
#lag
d_filter <- d %>% filter(WEEKMONTH!=5)
d_filter$lag <- lag(d_filter$SALES)
d.ts <- ts(d_filter$SALES,frequency = 4)
d.ts
Acf(d.ts,lag.max=4,main="",bty="l")
mod1 <- lm(SALES~lag,d_filter)
summary(mod1)
arm <- arima(d.ts,order=c(1,0,0))
arm
arm <- auto.arima(d.ts,seasonal = T)
arm
plot(d.ts,bty="l")
lines(arm$fitted,col="blue",lwd=1)
lines(pred,col="red",lwd=1)
cor(d.ts,arm$fitted)^2
ntrain <- round(length(d_filter$SALES)*0.8)
ntest <- length(d_filter$SALES)-ntrain
train.ts <- window(d.ts,start=c(1,1),end=c(1,ntrain))
test.ts <- window(d.ts,start=c(1,ntrain+1),end=c(1,ntrain+ntest))
arm <- auto.arima(train.ts,seasonal = T)
pred <- forecast(arm,h=ntest)
arm
cor(test.ts,pred$mean)^2
arm <- Arima(train.ts,order=c(1,0,0))
arm
pred <- forecast(test.ts,h=ntest)
cor(test.ts,pred$mean)^2
names(d_mean_month)<- c("MONTH","SALES")
ggplot(d_mean_month,aes(MONTH,SALES,fill=MONTH)) + geom_point()
d_numeric <- dplyr::select_if(d, is.numeric)
cor(d_numeric,use = "pairwise.complete.obs")
