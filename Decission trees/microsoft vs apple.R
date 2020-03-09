library(tidyverse)
library(dplyr)
library(rpart)
library(rpart.plot)
smart <- read.csv("https://raw.githubusercontent.com/wdecisions/ba/master/smartphones.csv")
smart2 <- smart %>% filter(complete.cases(.))
d <- smart2
d$amoled <- factor(ifelse(smart2$display=="AMOLED","AMOLED","NO AMOLED"))
d <- d %>% filter(brand=="Apple"|brand=="Microsoft",size<7,
                       release>2010,!model %in% c("iPhone 4 CDMA","Lumia 535",
                                                  "Lumia 640 Dual SIM","Lumia 640 LTE","Lumia 640 XL")) %>%
  select(brand,model,ppi,size,amoled,release,camera)
print(d)
plot <-ggplot(d,aes(ppi,size,color=brand)) + geom_point(size=0.6,alpha=1,position=position_jitter(width = 12,height=0.1))+
  ylim(3.4,6.1)+
  xlim(210,580)+
  scale_color_manual(values = c("grey","orange"))+
  geom_hline(yintercept = 4.9)+
  geom_vline(xintercept = 349)
plot
d <- as.data.frame(d)
d$brand <- factor(d$brand)
tree <- rpart (brand~size+ppi,data=d,method="class",control = rpart.control(minsplit = 1))
tree
prp(tree,type=4,extra=1,split.font=1,varlen=-10)
