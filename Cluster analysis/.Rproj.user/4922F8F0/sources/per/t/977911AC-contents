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
#Euclidian denogram
d <- column_to_rownames(d,var = "brand")
distance_euclid <- dist(d,method = "euclidean")
cluster1 <- hclust(distance_euclid,method ="single")
plot(as.dendrogram(cluster1),horiz = T)
#heatmap
heatmap(as.matrix(d),cexRow = 0.7,cexCol = 1,Colv = NA,hclustfun = hclust,
        col=rev(colorspace::sequential_hcl(palette = "Mint",n=10)))
