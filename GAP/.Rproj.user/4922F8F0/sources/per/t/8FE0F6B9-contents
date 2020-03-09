library(readxl)
library(tidyverse)
library(forecast)
d_year <- read_excel("gap.xlsx",sheet = "Yearly sales")
d_quarter <- read_excel("gap.xlsx",sheet = "Quarterly sales")
d_year <- d_year %>% drop_na()
cor(d_year)
mod_y <- lm(Sales~GDP,d_year)
summary(mod_y)
pred <- predict(mod_y,d_year)
d_year$forecast <- pred
plot(d_year$Sales,type="l")
lines(d_year$forecast,type="l",col="blue")
mod_y <- lm(Sales~lag(Sales),d_year)
summary(mod_y)
pred <- predict(mod_y,d_year)
d_year$forecast <- pred
plot(d_year$Sales,type="l")
lines(d_year$forecast,type="l",col="blue")
mod_y <- lm(Sales~GDP+lag(Sales),d_year)
summary(mod_y)
pred <- predict(mod_y,d_year)
d_year$forecast <- pred
plot(d_year$Sales,type="l")
lines(d_year$forecast,type="l",col="blue")
#time series
d.ts <- ts(d_quarter$Sales,frequency = 4)
plot(d.ts,bty="l")
hw <- ets(d.ts,model="MAA")
hw
plot(d.ts,bty="l")
lines(hw$fitted,col="blue",type="l")
ntrain <- round(length(d.ts)*0.8)
ntest <- length(d.ts)-ntrain
train.ts <- window(d.ts,start=c(1,1),end=c(1,ntrain))
test.ts <- window(d.ts,start=c(1,ntrain+1),end=c(1,ntrain+ntest))
#GDP growth
lag(d_year$GDP)
