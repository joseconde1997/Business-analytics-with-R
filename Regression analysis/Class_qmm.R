library(tidyverse)
library(readxl)
library(ggplot2)
qmm <- read_excel("qmm-tn-02.xlsx")
#1 variable
mod_qmm <- lm(SALES~PRICE,data=qmm)
#2 variables
mod_qmm <-lm(SALES~PRICE+WEEK,data=qmm)
summary(mod_qmm)
qmm$predict <- predict(mod_qmm,data=qmm)
qmm$error <- qmm$SALES-qmm$predict
qmm$percent_err <- qmm$error/qmm$SALES
ggplot(qmm,aes(PRICE,SALES,color=I("BLACK"),xlab="Price (NZ dollars)",ylab="Sales(thousands)")) + geom_point(alpha=0.84,size=0.4) + 
  geom_smooth(method="lm", se=FALSE,color="brown",size=0.6)
#Calculating the R square
var(qmm$predict)/var(qmm$SALES)

#new data
new_data <- runif(100,min=0.9,max=2.1)
data_l <- data.frame(PRICE=new_data)
data_l$predict <-predict(mod_qmm,data_l)
