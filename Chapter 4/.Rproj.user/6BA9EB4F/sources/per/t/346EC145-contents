library(ggbiplot)
library(dplyr)
library(ggplot2)
library(knitr)
library(tidyverse)
m <- round(cov(select(smart_num,weight,vol)),2)
cor(m)
ggplot(smart_num,aes(weight,vol)) + geom_point(size=1,color=I("blue"))
two.pca <- prcomp(select(smart_num,weight,vol),
                  center=TRUE,scale. = TRUE)
summary(two.pca)
pc1 <- two.pca$x[,1]
pc2 <- two.pca$x[,2]
ggplot(data.frame(pc1,pc2), aes(pc1,pc2)) + geom_point(size=0.3,alpha=0.8)
#Application (10)
round(cov(smart_num),2)
smart_num_sd <- as_tibble(scale(smart_num))
round(smart_num_sd,2)
kable(smart_num_sd %>%
        summarize_if(is.numeric,funs(mean,sd,max,median,min),na.rm=T) %>%
        gather(stat,val) %>%
        separate(stat,into = c("var", "stat"),sep="_") %>%
        spread(stat,val) %>%
        select(var,mean,sd,min,median,max),booktabs=T,digits=2)
round(cov(smart_num_sd),2)
smart.pca <- prcomp(smart_num_sd, center = TRUE,scale. = TRUE)
summary(smart.pca)
round(smart.pca$rotation,2)
round(head(smart.pca$x,6),2)
round(tail(smart.pca$x,6),2)
sd(smart.pca$x[,1])
var(smart.pca$x[,1])
sd(smart.pca$x[,2])
var(smart.pca$x[,2])
ggbiplot(smart.pca,varname.adjust=3,varname.size=2.5,alpha=0.05,size=0.4) + 
  ylim(-2,2.5)+xlim(-3,2)+ theme(axis.title = element_text(size=9))
