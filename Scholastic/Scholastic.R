library(readxl)
library(tidyverse)
library(lubridate)
library(pls)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
d <- read_excel("Scholastic.xlsx",sheet = "Exhibit 1") 
#group by state
state <- d %>% group_by(Group.State) %>% summarise(mean(Retained.in.2012.))
#count NAs
n_occur <- data.frame(table(d$Special.Pay))
mod <- lm(Retained.in.2012.~.,d)
summary(mod)
d_complete <- d %>% filter(complete.cases(.))
d_num <- select_if(d,is.numeric)
d_num <- d_num[,-1]
d_factor <- select_if(d,is.character)
#pcr baby
#PC1:Total.Pax FPP Num.of.Non_FPP.PAX Total.Discount.Pax FRP.Active	
#PC5: Days
#PC6: Is.Non.Annual. SingleGradeTripFlag
pcr_model <- pcr(Retained.in.2012.~.,data=d_num,scale=T,validation="CV")
View(abs(pcr_model$coefficients))
mod_pcr <- lm(Retained.in.2012.~Total.Pax+FPP+Num.of.Non_FPP.PAX+Total.Discount.Pax+FRP.Active+Days+Is.Non.Annual.+
                SingleGradeTripFlag,d)
summary(mod_pcr)
d_complete <- d_num %>% filter(complete.cases(.))
d_complete$FRP.Active <- ifelse(d_complete$FRP.Active>mean(d_complete$FRP.Active),1,0)
mod_factored <- lm(Retained.in.2012.~FRP.Active+Is.Non.Annual.+
                     SingleGradeTripFlag,d_complete)
summary(mod_factored)
pred <- predict(mod_factored,d_complete)
pred <- as.numeric(pred)
pred <- ifelse(pred>mean(pred),1,0)
confusionMatrix(as.factor(pred),as.factor(d_complete$Retained.in.2012.),dnn=c("Predicted","Actual"))
#decission trees
d$From.Grade <- as.numeric(d$From.Grade)
d$To.Grade <- as.numeric(d$To.Grade)
d_complete <- d %>% filter(complete.cases(.))
d_complete$range <- d_complete$To.Grade - d_complete$From.Grade
tree <- rpart(Retained.in.2012.~ FRP.Active+Is.Non.Annual.+
                SingleGradeTripFlag+as.factor(Group.State)+as.factor(range)+as.factor(Special.Pay)+
                as.factor(Poverty.Code),data=d_complete,method = "class")
tree <- rpart(Retained.in.2012.~ FRP.Active+Is.Non.Annual.+
                SingleGradeTripFlag,data=d_complete,method = "class")

tree
prp(tree,type=4,extra=1,split.font=1,varlen=-10)
pred <- predict(tree,d_complete,type = "class")
confusionMatrix(as.factor(pred),as.factor(d_complete$Retained.in.2012.),dnn=c("Predicted","Actual"))

#Roberto import
#import libraries
library(readxl)
library(tidyverse)

#import data
library(readxl)
d <- read_excel("Scholastic.xlsx", sheet = "Exhibit 1",na="NA")
View(d)
View(d$Special.Pay)
#tidy and remove dates
d$Special.Pay[is.na(d$Special.Pay)] <- "NAA"
d <- d[ , -c(9:11,17,18,21,39,40)]
str(d)

d <- d %>% mutate_if(is.character,as.factor)

#convert columns to factors
d$From.Grade <- as.factor(d$From.Grade)
d$To.Grade <- as.factor(d$To.Grade)
d$CRM.Segment <- as.factor(d$CRM.Segment)
d$MDR.High.Grade <- as.factor(d$MDR.High.Grade)


#Keep only top 10 factor levels for each variable
d <- d %>% mutate_if(is.factor, ~fct_lump(.x, n=10, other_level ="Other"))

#keep complete cases (get rid of real NA locations- clean data)
d <- na.omit(d)


#logistic regression

#logistic model 1
mod1 <- lm(Retained.in.2012. ~ ., data=d)
summary(mod1)

#correlation 
cor(d$FPP,d$Total.Pax)

#confusion matrix

d$pred <- predict(mod1)
d$pred_b <- ifelse(d$pred>0.5,1,0)
d[,32:34]

conf <- table(d$pred_b,d$Re)