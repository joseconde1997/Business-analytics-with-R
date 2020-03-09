library(readxl)
library(ggplot2)
library(tidyverse)
library(knitr)
return_auto <-c()
return_bank <- c()
return_capgoods <- c()
return_consumer <- c()
return_fast <- c()
return_health <-c()
return_info <- c()
return_metals <- c()
return_oil <- c()
return_power <- c()
return_teleco <- c()
return_CNX <- c()
kapoor <- read_excel("kapoor.xlsx")
auto <- kapoor$TATAAUTO
bank <- kapoor$HDFC
capgoods <- kapoor$LARSEN
consumer <- kapoor$TKK
fast <- kapoor$DABUR
#11 sector return
for (i in 2:length(kapoor$CNX)) {
  return_CNX <-c(return_CNX,(kapoor$CNX[i]/kapoor$CNX[i-1])-1)
}
mean_CNX <- mean(return_CNX)
sd_CNX <- sd(return_CNX)
for (i in 2:length(auto)) {
  return_auto <-c(return_auto,(auto[i]/auto[i-1])-1)
}
mean_auto <- mean(return_auto)
sd_auto <- sd(return_auto)
for (i in 2:length(bank)) {
  return_bank <-c(return_bank,(bank[i]/bank[i-1])-1)
}
mean_bank <- mean(return_bank)
sd_bank <- sd(return_bank)
for (i in 2:length(capgoods)) {
  return_capgoods <-c(return_capgoods,(capgoods[i]/capgoods[i-1])-1)
}
mean_capgoods <- mean(return_capgoods)
sd_capgoods <- sd (return_capgoods)
for (i in 2:length(consumer)) {
  return_consumer <-c(return_consumer,(consumer[i]/consumer[i-1])-1)
}
mean_consumer <- mean(return_consumer)
sd_consumer <- sd(return_consumer)
for (i in 2:length(fast)) {
  return_fast <-c(return_fast,(fast[i]/fast[i-1])-1)
}
mean_fast <- mean(return_fast)
sd_fast <- sd(return_fast)
for (i in 2:length(kapoor$SUN)) {
  return_health <-c(return_health,(kapoor$SUN[i]/kapoor$SUN[i-1])-1)
}
mean_health <- mean(return_health)
sd_health <- sd (return_health)
for (i in 2:length(kapoor$WIPRO)) {
  return_info <-c(return_info,(kapoor$WIPRO[i]/kapoor$WIPRO[i-1])-1)
}
mean_info <- mean(return_info)
sd_info <- sd(return_info)
for (i in 2:length(kapoor$TATASTEEL)) {
  return_metals <-c(return_metals,(kapoor$TATASTEEL[i]/kapoor$TATASTEEL[i-1])-1)
}
mean_metals <- mean(return_metals,na.rm = TRUE)
sd_metals <- sd(return_metals)
for (i in 2:length(kapoor$INDIAN)) {
  return_oil <-c(return_oil,(kapoor$INDIAN[i]/kapoor$INDIAN[i-1])-1)
}
mean_oil <- mean(return_oil)
sd_oil <- sd(return_oil)
for (i in 2:length(kapoor$NTPC)) {
  return_power <-c(return_power,(kapoor$NTPC[i]/kapoor$NTPC[i-1])-1)
}
mean_power <- mean(return_power)
sd_power <- sd(return_power)
for (i in 2:length(kapoor$BHARTI)) {
  return_teleco <-c(return_teleco,(kapoor$BHARTI[i]/kapoor$BHARTI[i-1])-1)
}
return_auto <-unlist(return_auto)
mean_teleco <- mean(return_teleco)
sd_teleco <- sd(return_teleco)
daily_return <- data.frame()
daily_return <- data.frame(CNX=return_CNX,auto=return_auto,bank=return_bank,capsgoods=return_capgoods,
                           consumer=return_consumer,fast=return_fast,health=return_health,
                           info=return_info,metals=return_metals,oil=return_oil,
                           power=return_power,teleco=return_teleco)

kable (daily_return %>%
         summarize_if(is.numeric,funs(mean,sd,max,median,min),na.rm= TRUE) %>%
         gather(stat,val) %>%
         separate(stat,into=c("var","stat"),sep="_") %>% 
         spread(stat,val) %>% 
         select(var,mean,sd,min,median,max), bookstabs=TRUE,digits=4)
