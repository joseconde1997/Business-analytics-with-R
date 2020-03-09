library(tidyverse)
library(dplyr)
library(ggplot2)
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
d
round(data.frame(mean=sapply(d[,2:11], mean),sd=sapply(d[,2:11],sd)),3)
ggplot(d,aes(size,price)) + geom_text(label=brand,size=2,check_overlap = T)
