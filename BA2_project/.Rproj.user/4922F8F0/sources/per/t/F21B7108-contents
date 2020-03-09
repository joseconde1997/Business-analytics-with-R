library(readxl)
library(tidyverse)
library(lubridate)
library(dplyr)
library(forecast)
library(ggplot2)
library(seewave)

d_2 <- read_excel("amarcord.xlsx",sheet = "Wires By Month")
#rearrange data
names(d_2)<- c("Month","O7","O8","O9","O10","O11","O12","O13")
d_2$index <- month(d_2$Month)
d_2[11,5] <- 396.812
d_2[,2:9]<- lapply(d_2[,2:9], function(x) as.numeric(as.character(x)))
d_2 [8,5] <- d_2 [8,5]-37
d_2[7,3] <- NA
d_2[4,7] <- NA
d_2[12,2] <- NA
year <-rep(c(1:7), times = c(12,12,12,12,12,12,12))
all_years <- c(d_2$O7,d_2$O8,d_2$O9,d_2$O10,d_2$O11,d_2$O12,d_2$O13)
all_years <- data.frame(month=c(1:12),transactions=as.numeric(all_years),year=year)
#Trend seasonal prediction

trend_seasonal <- as.data.frame(all_years$predict)
trend_seasonal

#holt-winters
data <- all_years$transactions
data[12]<-NA
data <- data.frame(actual=data)
data
for (i in 1:84) {
  if(is.na(data[i,])){
    data[i,] <- trend_seasonal[i,]
  }
}
d.ts <- ts(data,frequency = 12)
train.ts <-window(d.ts,start=c(1,1),end=c(1,72))
test.ts <- window(d.ts,start=c(1,73),end=c(1,84))
hw <- ets(train.ts,model = "MAA")
hw
cor(hw$fitted,train.ts)^2
fore <- forecast(hw,h=12,level=c(90))
cor(fore$mean,test.ts)^2
fore$mean


# average month
data <- all_years$transactions
data[12]<-NA
data <- data.frame(actual=data)
data
average <- all_years[1:72,] %>% group_by(month) %>% summarise(Average=mean(transactions,na.rm = T))
average
data$month <- c(1:12)
data <- left_join(data,average)
for (i in 1:84) {
  if(is.na(data[i,1])){
    data[i,1] <- data[i,3]
  }
}
data$actual
d.ts <- ts(data$actual,frequency = 12)
train.ts <-window(d.ts,start=c(1,1),end=c(1,72))
test.ts <- window(d.ts,start=c(1,73),end=c(1,84))
hw <- ets(train.ts,model="MAA")
plot(hw$fitted,col="blue")
lines(train.ts,col="black")
cor(hw$fitted,train.ts)^2
fore <- forecast(hw,h=12,level=c(90))
fore$mean
cor(fore$mean,test.ts)^2
#rms formula


#data frame transformation
coefficient<- 1.645
zero <- rep(c(NA),12)
train <- c(train.ts,zero)
hwint <- c(hw$fitted,zero)
upper <-hw$fitted +coefficient*sd(hw$fitted-train.ts,na.rm=T)
upper <- c(upper,zero)
lower <-hw$fitted -coefficient*sd(hw$fitted-train.ts,na.rm=T)
lower <- c(lower,zero)
zero <- rep(c(NA),72)
test <- c(zero,test.ts)
forehw <- c(zero,fore$mean)
upper_t <- c(zero,fore$upper)
lower_t <- c(zero,fore$lower)
hw.d <- data.frame(Actual=train,Predicted=hwint,Upper=upper,Lower=lower,Test=test,
                   Forecast=forehw,Upper_t=upper_t,Lower_t=lower_t,Index=c(1:84))
View(hw.d)
rms <-(1/length(hw.d$Actual[1:72]))*(hw.d$Predicted[1:72]-hw.d$Actual[1:72])^2
rms <- sqrt(rms)
sum(rms)
#Plot
ggplot(data=hw.d) + geom_line(aes(y=Actual,x=Index,color="Actual "),size=1) +
  geom_line(aes(y=Predicted,x=Index,color="Predicted")) + 
  geom_line(aes(y=Forecast,x=Index,color="Forecast 2013")) + 
  geom_line(aes(y=Test,x=Index,color="Test 2013"),size=1) + 
  geom_ribbon(aes(ymin=Lower,ymax=Upper,x=Index),alpha=0.2)+
  geom_ribbon(aes(ymin=Lower_t,ymax=Upper_t,x=Index),alpha=0.2)+
  scale_color_manual(values=c("Black","Blue","Blue","Black"))

ggplot(data=hw.d) + geom_line(aes(y=Actual,x=Index,color="Actual"),size=1) +
  geom_line(aes(y=Predicted,x=Index,color="Predicted")) + 
  geom_line(aes(y=Forecast,x=Index,color="Forecast 2013")) + 
  geom_line(aes(y=Test,x=Index),size=1) + 
  geom_ribbon(aes(ymin=Lower,ymax=Upper,x=Index),alpha=0.2)+
  geom_ribbon(aes(ymin=Lower_t,ymax=Upper_t,x=Index),alpha=0.2)+
  scale_color_manual(values=c("Black", "Red","Blue","Black"))+xlab('Months')
ggplot(data=hw.d) + geom_line(aes(y=Actual,x=Index,color="Actual"),size=1) +
  geom_line(aes(y=Predicted,x=Index,color="Predicted")) + 
  geom_ribbon(aes(ymin=Lower,ymax=Upper,x=Index),alpha=0.2)+
  scale_color_manual(values=c("Black", "Blue"))+xlab('Months')


