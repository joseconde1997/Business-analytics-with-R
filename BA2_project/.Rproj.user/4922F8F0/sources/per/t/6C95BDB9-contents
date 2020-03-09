
#prediction with season*trendlibrary(readxl)
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
all_years$upper <-all_years$predict +2*sd(all_years$predict-all_years$transactions,na.rm=T)
all_years$lower <- all_years$predict - 2*sd(all_years$predict-all_years$transactions,na.rm=T)
lines(all_years$upper, lty = 'dashed', col = 'red')
lines(all_years$lower, lty = 'dashed', col = 'red')
lines(all_years$predict,type = "l",col="blue")
legend(1, 200, legend=c("Actual", "Predicted","Upper limit","Lower limit"),
       col=c("red", "blue"), lty=1:2, cex=0.8)
all_years$index <- c(1:84)
ggplot(data=all_years) + geom_line(aes(y=transactions,x=index,color="Actual"),size=1) +
  geom_line(aes(y=predict,x=index,color="Predicted")) + 
  geom_ribbon(aes(ymin=lower,ymax=upper,x=index),alpha=0.2)+scale_color_manual(values=c("black", "blue"))

error_by_year_all <- all_years %>% group_by(year) %>% summarize(mean(abs(error),na.rm = T))
cor(all_years[73:84,2],all_years[73:84,6])^2
cor(all_years[,2],all_years[,6],use="complete.obs")^2
