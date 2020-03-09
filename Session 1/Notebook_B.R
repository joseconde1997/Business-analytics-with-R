#1) CUrvilinear
#2)The price will increase
#3) There is a strong correlation between inches,weight and price
#4) Yes,apple sucks
library(readxl)
library(ggplot2)
library(tidyverse)
laptops <- read_excel("laptops.xlsx")
#price weight
#mod_pw <- lm(price~weight+I(weight^2)+I(weight^3),data=laptops)
mod_pw <- lm(price~weight,data=laptops)
summary(mod_pw)
predict_data <- data.frame(weight=laptops$weight+1)
predict_data$price <- predict(mod_pw,predict_data)
predict_data$weight <- data.frame(laptops$weight-1)
predict_data$price1 <- predict(mod_pw,predict_data)
View(predict_data)
ggplot(laptops,aes(weight,price)) + geom_point(alpha=0.5,size=0.5) +
  geom_smooth(method="lm", formula="y~poly(x,2)",color="brown",size=0.6,se= FALSE)
#weight ram
mod_wr <- lm(weight~inches+I(inches^2),data=laptops)
summary(mod_wr)
View(laptops_pred)
ggplot(laptops,aes(inches,weight)) + geom_point(alpha=0.5,size=0.5) +
  geom_smooth(method="lm", formula="y~poly(x,2)",color="brown",size=0.6,se= FALSE)
#weight price
mod_wp <- lm(weight ~ price+I(price^2),data=laptops)
summary(mod_wp)
ggplot(laptops,aes(price,weight)) + geom_point(alpha=0.5,size=0.5) +
  geom_smooth(method="lm", formula="y~poly(x,2)",color="brown",size=0.6,se= FALSE)
#price_company
laptops_norazer <- laptops %>% filter(company !="Razer")
mod_pb <- lm(price~as.factor(company)+ghz+mem1,data=laptops)
summary(mod_pb)
