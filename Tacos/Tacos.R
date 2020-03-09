library(readxl)
library(tidyverse)
library(pls)
library(dplyr)
library(stringi)
d <- read_excel("tacos.xlsx",sheet = "Data")
View(cor(d[2:40]))
rest <- d %>% group_by(Restaurant) %>% summarise(Rating=mean(Rating))
rest
mod <- lm(Rating~.-Restaurant,data=d)
summary(mod)
d_num <- select_if(d,is.numeric)
d_num
mod_pcr <-  pcr(Rating~., data = d_num, scale = TRUE, validation = "CV")
summary(mod_pcr)
mod_pcr$Xmeans
View(abs(mod_pcr$coefficients))
d <- d %>% mutate_all(as.factor) %>% mutate_all(as.numeric)
str(d)
mod_pcr <-  pcr(Rating~., data = d, scale = TRUE, validation = "CV")
summary(mod_pcr)
View(abs(mod_pcr$coefficients))
#training and testing
train <- sample(length(d$Restaurant),0.8*length(d$restaurant.1))
d_train <- d[train,]
d_test <- d[-train,]
mod_pcr <-  pcr(Rating~., data = d_train, scale = TRUE, validation = "CV")
summary(mod_pcr)
pcr_pred <- predict(mod_pcr, d_test, ncomp = 10)
cor(pcr_pred,d_test$Rating)^2
d_test$pred <- pcr_pred
d_test$error <- d_test$Rating - d_test$pred
mean(abs(d_test$error))
quantile(d_test$error)

#Analize outliers
d <- read_excel("tacos.xlsx",sheet = "Data")
d <- d %>% filter(stri_detect_fixed(Restaurant,"Donna"))
d
mean_donna <-d[,2:40] %>%summarise_all("mean")
mean_donna <- t(mean_donna)
mean_donna <- data.frame(mean_donna)
max(mean_donna,5)
mean_donna[order(mean_donna)]

d <- read_excel("tacos.xlsx",sheet = "Data")
d <- d %>% filter(stri_detect_fixed(Restaurant,"Arriba Arriba Sunnyside"))
d
View(cor(d[2:40]))
mean_arriba <-d[,2:40] %>%summarise_all("mean")
mean_arriba <- t(mean_arriba)
mean_arriba <- data.frame(mean_arriba)
mean_donna[order(mean_donna)]

#drinks
d <- read_excel("tacos.xlsx",sheet = "Data")
drinks <- d %>% filter(drink==1)
mean(drinks$Rating)

