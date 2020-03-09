# Guarini Sales Trend

# libraries
library(readxl)
library(tidyverse)

# data
d <- read_xlsx("guarini.xlsx", sheet="Data")

# plot
plot(d$SALES, type="l")
abline(lm(d$SALES ~d$WEEK))

# correlation
cor(d[, c(3,6,7,8)])

# weekmonth effect
d %>% group_by(WEEKMONTH) %>% 
  summarise(sales=mean(SALES))

# regression 1
d$WEEKMONTH <- factor(d$WEEKMONTH)
reg1 <- lm(SALES ~ WEEK+WEEKMONTH+XMAS+SUMMER+HOLIDAY, data=d)
summary(reg1)           

# actual vs prediction
plot(d$SALES,type="l")
lines(predict(reg1),type="l",col="blue")

# lagged variables
lagged <- data.frame(d$SALES,lag(d$SALES,1),lag(d$SALES,2),lag(d$SALES,3))
cor(lagged,use = "pairwise.complete.obs")

# regression 2 (lagged variables)
reg2 <- lm(SALES ~ WEEKMONTH+XMAS+SUMMER+HOLIDAY+
             lag(SALES,1)+lag(SALES,2)+lag(SALES,3), data=d)
summary(reg2)           

# actual vs prediction
plot(d$SALES[-c(1:3)],type="l")
lines(predict(reg2),type="l",col="blue")

# forecast (example remove last obs SEP and OCT for testing)
train <- d[1:95,] # filter first 95 weeks
test <- d[96:104,] # filter last  9 weeks

reg3 <- lm(SALES ~ WEEK+WEEKMONTH+XMAS+SUMMER+HOLIDAY, data=train)
summary(reg3)       

pred <- predict(reg3, test)

plot(test$SALES, type="l")
lines(pred,type="l",col="blue")
