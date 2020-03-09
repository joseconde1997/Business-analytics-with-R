library(readxl)
library(tidyverse)
d <- read_excel("guarini_coupon.xls",sheet = "Data")
mod <- lm(SALES~lag(PROMO)+as.factor(WEEKMONTH)+XMAS+HOLIDAY+lag(SALES)+lag(lag(SALES))+WEEK+SUMMER,d)
summary(mod)
mod_promo <- lm(SALES ~PROMO+lag(PROMO)+lag(lag(PROMO)),d)
summary(mod_promo)
plot(d$PROMO,d$SALES,type="l")
plot(d$SALES[1:length(d$SALES)],type="l")
abline(lm(d$SALES~d$WEEK),col="dark green")
lines(predict(mod,d[1:length(d$SALES),]),col="blue",type="l")
error <- d$SALES - predict(mod,d)
which(error==max(error,na.rm = T))
d$error <- error
plot(error,type="l")
error_sum_abs <- d[3:104,] %>% group_by(MONTH) %>% summarise(abs(mean(error)))
error_sum_abs
error_sum <-  d[3:104,] %>% group_by(MONTH) %>% summarise(mean(error))
error_sum
d_factor <- d
d_factor$PROMO <- ifelse(d_factor$PROMO>946,1,0)
mod_fact <- lm(SALES~PROMO+lag(PROMO)+as.factor(WEEKMONTH)+XMAS+HOLIDAY+lag(SALES)+lag(lag(SALES))+WEEK+SUMMER,d_factor)
summary(mod_fact)
mod_promo <- lm(SALES ~PROMO+lag(PROMO)+lag(lag(PROMO))+lag(lag(lag(PROMO))),d_factor)
summary(mod_promo)
d_cor <- d %>%na.omit(.)
cor(dplyr::select_if(d_cor,is_numeric))
train <- d[1:95,]
test <- d[95:104,]
reg_4 <- lm(mod <- lm(SALES~lag(PROMO)+as.factor(WEEKMONTH)+XMAS+HOLIDAY+lag(SALES)+lag(lag(SALES))+WEEK+SUMMER,train))
summary(reg_4)
test$forecast <- predict(reg_4,test)
cor(test$SALES,test$forecast,"pairwise.complete.obs")^2
