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
d[1:2,]
#Euclidian
xi <- t(as.vector(d[1,-1]))
xj <- t(as.vector(d[2,-1]))
euclidian_dist <- t(xi-xj) %*% (xi-xj)
euclidian_dist <- as.numeric(sqrt(euclidian_dist))
d <- column_to_rownames(d,var = "brand")
distance_euclid <- dist(d,method = "euclidean")
as.matrix(distance_euclid)
as.matrix(distance_euclid)[1:9,1:9]
#Mahalanobis
xi <- t(as.vector(d[1,]))
xj <- t(as.vector(d[2,]))
s=cov(d)
dist_maha <- t(xi-xj) %*%solve(s)%*%(xi-xj)
dist_maha <- as.numeric(sqrt(dist_maha))
d <- column_to_rownames(d,var = "brand")
distance_mahalanobis <- mahalanobis.dist(d)
as.matrix(distance_mahalanobis)
as.matrix(distance_mahalanobis)[1:9,1:9]
#Manhattan distance
manhat_dist <- dist(d,method = "manhattan")
as.matrix(manhat_dist)[1:9,1:9]
