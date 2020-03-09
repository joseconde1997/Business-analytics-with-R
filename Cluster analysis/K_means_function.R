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
options(digits = 2)
d <- select(d,size,weight,vol,battery,mem,ram,camera,ppi,price,release)
set.seed(1239990101)
km <- kmeans(d,5,nstart = 50,iter.max = 10)
km
km$centers
d$cluster <- km$cluster
ggplot(d,aes(size,price,color=as.factor(cluster))) + geom_text(label=brand,size=2,check_overlap = T)
