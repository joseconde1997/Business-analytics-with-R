library(readxl)
library(ggplot2)
library(tidyverse)
hot_dog <- read_excel("hotdog.xlsx",sheet="data")
#marketshare_price
ggplot(hot_dog,aes(WEEK,MSHARE)) + geom_line(size=0.6)
hot_dog_sd <- select(hot_dog,MSHARE:PBPALL)
hot_dog$difball <- hot_dog$PBPREG - hot_dog$PBPALL
ggplot(hot_dog,aes(PDUB,MSHARE)) + geom_point(size=0.3) +  
  geom_smooth(method = "lm", formula = "y~poly(x,2)", se=FALSE,color="brown",size=0.4)

#relationship between price and marketshare
mod_dub <- lm(MSHARE~PDUB,data=hot_dog)
summary(mod_dub)
round(cor(hot_dog_sd),2)

#Oscar Mayer vs Duburque

ggplot(hot_dog,aes(PMAY,MSHARE)) + geom_point(size=0.3,alpha=0.3) + 
  geom_smooth(method = "lm", formula = "y~poly(x,1)", se=FALSE,color="brown",size=0.4)
mod_DubMay <- lm(MSHARE~PMAY,data=hot_dog)
summary(mod_DubMay)
#everytime the price increases the marketshare increases

#Duburke vs BallPark

mod_DubBallReg <- lm(MSHARE~PBPREG, data = hot_dog)
summary(mod_DubBallReg)

#Everytime DubBall increases their market share Duburke gains market share
mod_comp <- lm(MSHARE~PDUB+PMAY+PBPREG,data=hot_dog)
summary(mod_comp)

#Ball park is the leading competition!

#Beef price change

mod_DubBallBeef <- lm(MSHARE~PBPALL, data = hot_dog)
summary(mod_DubBallBeef)


#How much marketshare are they expected to lose
pred_reg_change <- predict(mod_DubBallReg,newdata=data.frame(PBPREG=1.45))
pred_PBPALL_high <- predict(mod_DubBallBeef,newdata=data.frame(PBPALL=1.95))
pred_PBPALL_low <- predict(mod_DubBallBeef,newdata=data.frame(PBPALL=1.55))
