#Notebooks (B)

# load libraries
library(readxl)
library(tidyverse)

# import data 
laptops <- read_xlsx("laptops.xlsx")

# numeric data SELECT
d <- select(laptops, company,price,inches,ram,weight,ghz,mem1,mem2)

# filter companies FILTER
apple <- filter(laptops, company == "Apple")

# Question 1 --------------------------------------------------------------

# scatter plot
ggplot(d, aes(weight,price))+geom_point(size=1.5,alpha=0.4)

ggplot(d, aes(weight,price))+
  geom_point(size=1.5,alpha=0.4)+ geom_smooth(method="lm",se=F)

# Question 2 --------------------------------------------------------------

# correlation
cor(d[,-1])

cor(d$weight,d$price)

# regression 
mod1 <- lm(price ~ weight, data=d)
summary(mod1)

# Question 3 --------------------------------------------------------------

ggplot(d, aes(weight,ram,color=company))+ geom_point()

ggplot(d, aes(weight, ram, color=company, label=company))+ 
  geom_text(aes(label=company), size=4 ,check_overlap = TRUE)

# Question 4 --------------------------------------------------------------

mod2 <- lm(price ~ weight+ram, data=d)
summary(mod2)

mod3 <- lm(price ~ weight+ram+ghz+mem1+inches, data=d) #multicollinearity weight and inches
summary(mod3)

mod4 <- lm(price ~ weight+ram+ghz+mem1, data=d)
summary(mod4)


# Question 5 --------------------------------------------------------------

laptops %>% 
  group_by(company) %>% 
  summarize(price_mean=mean(price))

laptops$apple <- ifelse(d$company=="Apple",1,0)

# apple premium 
mod5 <- lm(price ~ apple, data=laptops)
summary(mod5)

# apple premium with controls
mod6 <- lm(price ~ apple+ram+weight+ghz+mem1, data=laptops)
summary(mod6)

# apple premium with controls + type
mod7 <- lm(price ~ apple+ram+weight+ghz+mem1+type, data=laptops)
summary(mod7)
