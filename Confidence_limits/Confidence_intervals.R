#Confidence intervals
library(tidyverse)
#Setting data
y15 <- smart2 %>% dplyr::filter(release==2015)
y16 <- smart2 %>% dplyr::filter(release==2016)
price15 <- data.frame(price=y15$price)
price16 <- data.frame(price=y16$price)
price15$year <- '2015'
price16$year <- '2016'
mean15 <- mean(y15$price)
mean16 <- mean(y16$price)
sd15 <- sd(y15$price)
sd16 <- sd(y16$price)
#Representation of data
tibble(
  year=c(2015,2016),
  n=c(length(y15$price), length(y16$price)),
  mean=c(mean15,mean16),
  sd=c(sd15,sd16)
)
#Confidence interval
CI15_pos95 <- mean15+2*sd15/sqrt(length(y15$price))
CI15_neg95 <- mean15-2*sd15/sqrt(length(y15$price))
CI15 <- c(CI15_neg95,CI15_pos95)
CI16_pos95 <- mean16+2*sd16/sqrt(length(y16$price))
CI16_neg95 <- mean16-2*sd16/sqrt(length(y16$price))
CI16 <- c(CI16_neg95,CI16_pos95)
#Probability plot
bind_ys <- rbind(price15,price16)
ggplot(bind_ys,aes(price,fill=year)) + geom_density(alpha=0.3)
#Mean distribution
x15<-rnorm(10000,mean=mean15, sd=sd15)
x16<-rnorm(10000,mean = mean16, sd=sd16)
x15<-data.frame(val=x15)
x16<-data.frame(val=x16)
x15$year <-'2015'
x16$year <-'2016'
bind_mean <- rbind(x15,x16)
ggplot(bind_mean,aes(val,fill=year)) + geom_density(alpha=0.3)
