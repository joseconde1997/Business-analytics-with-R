# Restaurant Grades

# library
library(readxl)
library(tidyverse)

# data
d <- read_xlsx("916702-XLS-ENG.xlsx", sheet ="data")

names(d) <- c("treat","views","calls","res","id","type")

# random assignment of treatments
table(d$type, d$treat)

# dummy variables
d$type_chain <- ifelse(d$type=="chain",1,0)
d$type_ind   <- ifelse(d$type=="independent",1,0)
d$treat_1    <- ifelse(d$treat==1,1,0)
d$treat_2    <- ifelse(d$treat==2,1,0)

# correlation
round(cor(select(d,treat_1,treat_2,views,calls,res,type_chain)),3)

# pivot tables
d %>% group_by(treat) %>% 
  summarise(views=mean(views),call=mean(calls),res=mean(res))

d %>% group_by(type) %>% 
  summarise(views=mean(views),call=mean(calls),res=mean(res))

d %>% group_by(treat,type) %>% 
  summarise(views=mean(views),call=mean(calls),res=mean(res)) 

# factorize treat and type 
d$treat <- factor(d$treat)
d$type  <- factor(d$type, levels = c("independent","chain"))
d$type  <- factor(d$type, levels = c("chain","independent"))

mod1 <- lm(views ~ treat, data=d)
summary(mod1)

# regression views
mod2 <- lm(views ~ treat + type, data=d)
summary(mod2)

# regression views x type
mod3 <- lm(views ~ treat  + type + treat:type, data=d)
summary(mod3)

# regression reservations
mod4 <- lm(res ~ treat + type, data=d)
summary(mod4)

# regression reservations x type
mod5 <- lm(res ~ treat + type + treat:type, data=d)
summary(mod5)



