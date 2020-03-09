library(tidyverse)
library(dplyr)
library(ggplot2)
library(StatMatch)
smart <- read.csv("https://raw.githubusercontent.com/wdecisions/ba/master/smartphones.csv")
smart2 <- smart %>% filter(complete.cases(.))
d <- cbind(brand=as.character(smart2$brand),smart2[,11:20])
d <- d %>% 
  group_by(brand) %>%
  summarize_all(mean)
brand <- as.character(d$brand)
d <- as.tibble(sapply(d[,2:11], scale))
d <- as.tibble(cbind(brand,d))
d$brand  <- as.character(d$brand)
small <- d[c(16,20,27,37,50),c(1,7,9)]
small
eco <- d[27,c(7,9)]
A <- (d[37,c(7,9)] + d[50,c(7,9)])/2
D <- as.matrix(eco-A)
dist_D <- sqrt(D %*% t(D))
as.numeric(dist_D)

B <- (d[16,c(7,9)] + d[20,c(7,9)])/2
D_1 <- as.matrix(eco-B)
dist_D_1 <- sqrt(D_1 %*% t(D_1))
as.numeric(dist_D_1)
