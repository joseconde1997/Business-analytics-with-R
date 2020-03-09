library(readxl)
library(tidyverse)
library(ggplot2)
library(viridis)
library(RColorBrewer)
drugs <- read_excel("drugs.xlsx")
drugs_no_letter <- select(drugs,time,dosage,events)
drugs_cardio_5 <- filter(drugs,dosage<15)
ggplot(drugs_cardio_5,aes(dosage,events,label=patient,color=as.factor(time))) + geom_text(aes(label=patient),size=3,alpha=1,check_overlap = T) 
drugs_A <- drugs %>% filter(patient=="A")
cor(select(drugs_A,time,dosage,events))
View(round(cor(drugs_no_letter),2))
ggplot(drugs,aes(events,fill=as.factor(time),color=as.factor(time))) + geom_density(alpha=0.05)
ggplot(drugs,aes(events,dosage,label=patient,color=as.factor(time))) + geom_text(aes(label=patient),size=3,alpha=1,check_overlap = T) 
# pivot tables
drugs %>% group_by(time) %>% 
  summarise(dosage=mean(dosage),events=mean(events))

d %>% group_by(type) %>% 
  summarise(views=mean(views),call=mean(calls),res=mean(res))

d %>% group_by(treat,type) %>% 
  summarise(views=mean(views),call=mean(calls),res=mean(res)) 
#Linear models
mod <- lm(events~dosage+as.factor(time),drugs)
summary(mod)
mod_dosage <- lm(events~dosage,drugs)
summary(mod_dosage)
mod_patient <- lm(events~as.factor(patient),drugs)
summary(mod_patient)
#min dosages
drugs_68_min <- filter(drugs,dosage<(mean(drugs$dosage)-sd(drugs$dosage)))
mean(drugs_68_min$events)
drugs_95_min <- filter(drugs,dosage<(mean(drugs$dosage)-2*sd(drugs$dosage)))
mean(drugs_95_min$events)
#max dosages
drugs_68_max <- filter(drugs,dosage<(mean(drugs$dosage)+sd(drugs$dosage)))
mean(drugs_max$events)
drugs_95_max <- filter(drugs,dosage<(mean(drugs$dosage)+2*sd(drugs$dosage)))
mean(drugs_95_max$events)
#Confident limits 
mean_dosage <- mean(drugs$dosage)
sd_dosage <- sd(drugs$dosage)
length_dosage <- as.numeric(length(drugs$dosage))
CI_95 <-c(mean_dosage-(2*sd_dosage/sqrt(length_dosage)),
      mean_dosage+(2*sd_dosage/sqrt(length_dosage)))
CI_68 <- c(mean_dosage-(sd_dosage/sqrt(length_dosage)),
           mean_dosage+(sd_dosage/sqrt(length_dosage)))
drugs$dossage_dif <- drugs$dosage - lag(drugs$dosage,3)
drugs$event_dif <- drugs$events - lag(drugs$events,3)
drugs_4 <- filter(drugs,time==4)
View(drugs_4)
mod_4 <- lm (event_dif~dossage_dif)
summary(mod_4)
