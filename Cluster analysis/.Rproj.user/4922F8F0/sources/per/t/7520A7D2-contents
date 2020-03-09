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
pcomp <- prcomp(d,center = T,scale. = T)
summary(pcomp)
pcomp$rotation
d.pca <- as.data.frame(pcomp$x[,1:2])
head(d.pca)
tail(d.pca)
set.seed(1239990101)
km <- kmeans(d.pca,5,nstart = 50,iter.max = 10)
km
cluster <- as.data.frame(km$cluster)
names(cluster)[1] <- paste("cluster")
d.pca <- cbind(d.pca,cluster)
d.pca <- rownames_to_column(d.pca,var="brand")
d <- as.tibble(d.pca[,1:4])
hull <- d%>%
  group_by(cluster) %>%
  slice(chull(PC1,PC2))
ggplot(d,aes(PC1,PC2,color=factor(cluster),fill=factor(cluster))) +
  geom_text(aes(label=brand),size=2.1,check_overlap = T)+
  geom_polygon(data = hull,alpha=0.1,color="grey90",size=0.0) +
  theme(legend.position = "top")
