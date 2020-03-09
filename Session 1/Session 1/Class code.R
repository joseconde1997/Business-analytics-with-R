library(gmodels)
library(readxl)
library (tidyverse)
laptops <- read_xlsx("C:\\Users\\conde_000\\Documents\\Business analytics 1\\Session1\\laptops.xlsx")
#by firm
laptops %>%
  group_by(company) %>%
  summarise(price=mean(price),inches=mean(inches),ram=mean(ram),weight=mean(weight),
            ghz=mean(ghz),mem1=mean(mem1),mem2=mean(mem2))
#Categorical
table(laptops$company,laptops$type)
table(laptops$company,laptops$opsys)
#numeric:descriptive stats
summary(laptops)
mean(laptops$price)
sd(laptops$price)
#numeric histogram
hist(laptops$price,breaks = 25)
hist(laptops$weight,breaks = 25)
ggplot(laptops,aes(price)) + geom_histogram(bindwidth=1,color="grey40",alpha=0.3)

#scatter plots
ggplot (laptops, aes(weight,price)) + geom_point(alpha=0.5,size=1)
ggplot(laptops, aes(weight,ghz)) + geom_point(alpha=0.5,size=1)

#text labels
ggplot(laptops,aes(weight,price,color=company)) + geom_point(size=1.5,alpha=0.9)
ggplot(laptops,aes(weight,price,color=company,label=company)) + geom_text(aes(label=company),check_overlap = TRUE)
ggplot(laptops,aes(ghz,price,color=company,label=company)) + geom_text(aes(label=company),check_overlap = TRUE)
write.csv(laptops,"laptops_fromR.csv")
