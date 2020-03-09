library(tidyverse)
library(ggplot2)
library(GGally)
library(dplyr)
library(readxl)
laptops <- read_excel("C:\\Users\\conde_000\\Documents\\Business analytics 1\\Session1\\laptops.xlsx")
fres <- readLines("C:\\Users\\conde_000\\Documents\\Business analytics 1\\Session1\\laptops_1.csv")
fresl <- read.csv("C:\\Users\\conde_000\\Documents\\Business analytics 1\\Session1\\laptops_2.txt")
#brands numbers
d <- fresl %>%
  group_by(ï..company) %>%
  summarise(count=n())
d <- left_join(fresl,d)
#Bar chart companies
ggplot(data = d) + geom_bar(aes(x=ï..company,fill=ï..company)) + 
  theme(legend.position="none",
        axis.text.x=element_text(angle=90, hjust=1,size=11))
#Bar chart OS
ggplot(data = d) + geom_bar(aes(x=opsys,fill=opsys)) + 
  theme(legend.position="none",
        axis.text.x=element_text(angle=90, hjust=1,size=11))
#Ram 
ggplot(fresl,aes(as.factor(ram),price,color=as.factor(ram),
                 fill=as.factor(ram))) + geom_boxplot(alpha=0.6)
#price boxes
ggplot(fresl,aes(as.factor(type),price,color=as.factor(type),
                 fill=as.factor(type))) + geom_boxplot(alpha=0.6)
#correlations
ggpairs(select(fresl,weight:mem1))
#Variability
quantile(fresl$price)
#Price weight plot
ggplot(data=fresl) + geom_point(aes(weight,price),alpha=0.5,size=1)
#Ghz weight
ggplot(data=fresl) + geom_point(aes(weight,ghz),alpha=0.5,size=1)
#inches weight plot
ggplot(data=fresl) + geom_point(aes(inches,weight),alpha=0.5,size=1)
#Histogram
ggplot(laptops,aes(price)) + 
  geom_histogram(bindwidth=1,fill=1,color="grey40",alpha=0.3)
ggplot(laptops,aes(weight)) + 
  geom_histogram(bindwidth=1,fill=1,color="grey40",alpha=0.3)
