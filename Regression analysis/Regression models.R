library(readr)
library(ggplot2)
library(tidyverse)
smart <- read.csv("https://raw.githubusercontent.com/wdecisions/ba/master/smartphones.csv")
smart2 <- smart %>% dplyr::filter(complete.cases(.))
d <- select(smart2,price,ppi)
mod <- lm(price~ppi,data=d)
summary(mod)
ggplot(d,aes(ppi,price)) + geom_point(size=0.8,alpha=0.8)+
  geom_smooth(method = "lm", se= FALSE, color="brown",size=1)
d$pred <- predict(mod,data=d)
d$resid <- d$price-d$pred
stats::quantile(d$resid)
hist(d$resid,xlab = "Residuals",main = "")
paste(c("Error mean",mean(d$resid)))
cor(d$price,d$pred)
var_pred <- var(d$pred)
paste(c("var pred",var_pred))
var_actual <- var(d$price)
paste(c("var actual",var_actual))
r_sqr <- var_pred/var_actual
paste(c("R-square",r_sqr))
dif <- d$pred-d$price
dif_sqr <- dif*dif
rmse <- sqrt( (1/1773)*sum(dif_sqr))
rmse
sd(d$resid)
stats::quantile(d$resid,probs=c(0.025,0.5,0.975))
