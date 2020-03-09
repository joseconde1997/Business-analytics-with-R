library(readr)
library(tidyverse)
library(ggplot2)
#pdf and cdf
smart <- read.csv("https://raw.githubusercontent.com/wdecisions/ba/master/smartphones.csv")
smart2 <- smart %>% dplyr::filter(complete.cases(.))
quantile(smart2$price, probs=seq(0,1,0.05))
qplot(smart2$price,probs=seq(0,1,0.05))
ggplot(smart) + geom_histogram(aes(price),color=I("green"),fill=I("blue"))
ggplot(smart,aes(price)) + stat_ecdf(geom="step")
#Normal distribution
set.seed(9912)
x <- replicate(100000,{
  dice <- rdunif(100,6,a=1)
  mean(dice)
  }) 
d <- data.frame(x=x)
ggplot(d,aes(x)) + geom_histogram(aes(y=..density..),binwidth = 0.009,fill="brown",
                                  color="grey40",alpha=0.3) + geom_density() + xlim(2,5)

