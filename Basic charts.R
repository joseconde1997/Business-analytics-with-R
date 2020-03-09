library(visdat)
library(GGally)
#BAR CHARTS
#Absolute count
ggplot(data = smart) + 
  geom_bar(aes(x=os,fill=os))
#proportion
ggplot(data = smart, fill=os) + 
  geom_bar(aes(x=os,y=..prop.., group=1, fill=factor(..x..))) + 
  theme(legend.position = "none")
#LINE CHARTS
d <- smart %>% group_by(release) %>%
  summarize(avgprice=mean(price))
ggplot(data=d) + geom_line(aes(release,avgprice))
#SCATTER PLOTS
ggplot(data=smart) +
  geom_point(aes(size,battery),alpha=0.5,size=1)
#HISTOGRAMS
ggplot(smart, aes(ram)) +
  geom_histogram(binwidth = 1, fill=1,color="grey40",alpha=0.3)
#BOX PLOTS
ggplot(smart,aes(as.factor(ram),price, color=as.factor(ram),fill=as.factor(ram))) + 
  geom_boxplot(alpha=0.6)
#Release histogram
nas <- smart %>% filter(is.na(ram)==TRUE)
ggplot(nas,aes(release)) +
  geom_histogram(binwidth = 1,fill=1,color="grey40",alpha=0.3) + 
  scale_x_continuous(breaks = c(2001:2017))
#VIOLIN PLOTS
ggplot(smart,aes(as.factor(ram),price,color=as.factor(ram),
                 fill=as.factor(ram))) +geom_violin(alpha=0.6)
#HEATMAPS
heat <- round(cor(dplyr::select(smart,weight:release),use = "pairwise.complete.obs"),2)
heat <- reshape::melt(heat)
ggplot(heat, aes(X1,X2,fill=value)) +
  geom_tile() + geom_text(aes(X1,X2, label=value))
#Missing data heatmap
vis_miss(smart) + theme(axis.text.x = element_text(angle = -90))
#Multidimensional Visualization
ggpairs(select(smart,price:ppi))
