library(outliers)
library(readxl)
library(tidyverse)
library(lubridate)
library(writexl)
library(ggplot2)
library(stringi)
library(car)
library(outliers)
d <- read_excel("amarcord.xlsx",sheet = "November 2010 Wires")
names(d) <- c("Amount","Date","time","ID")
d$week <- week(d$Date)
d$hour <- hour(d$time)
d$minutes <- minute(d$time)
d$seconds <- second(d$time)
d$day <- day(d$Date)
d$Amount <- as.numeric(d$Amount)
mean(d$Amount)
d <- d %>% drop_na()
d <- d%>% filter(Amount<1000)
d <- d%>% filter(Amount>-10)
d_IN <- d %>% filter(stri_detect_fixed(ID,"INC"))
d_out <- d %>% filter(stri_detect_fixed(ID,"OUT"))
d_out$Amount <- d_out$Amount*1
d_out$Amount
d <- rbind(d_IN,d_out)

boxplot(Amount ~ day, data=d, main="Data reading across November")
#Cook distance
n <- nrow(d)
mod <- lm(Amount ~ day+hour, data=d)
cooksd <- cooks.distance(mod)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
str(cooksd)
influential_obs <- as.numeric(names(cooksd)[(cooksd > (4/n))])
influential_obs
d_inf <- d[influential_obs,]
d_inc <- d_inf %>% filter(stri_detect_fixed(ID,"INC"))
d_out <- d_inf %>% filter(stri_detect_fixed(ID,"OUT"))
n_occur <- data.frame(table(d_inc$ID))
repeating_freq <- n_occur[n_occur$Freq > 1,]
reapeting_freq
n_occur
d_inf <- d[influential_obs,]
tab
plot(tab$Freq,type="l")
#Outliers
Outlier <- outlier(d$Amount,opposite = T)
Outlier
#scores
scores(d$Amount)
outliers<-scores(d$Amount, type="chisq", prob=0.9)  
View(outliers)
d$outliers <- scores(d$Amount, type="chisq", prob=0.9)  
View(d)
table(d$outliers)
d$outliers <- as.character(d$outliers)
d_outliers <- d %>% filter(stri_detect_fixed(outliers,"TRUE"))
day <- d_outliers %>% group_by(day) %>% summarise(Transactions=sum(Amount))
View(day)
mean(day$Transactions)
plot(day,type="l")
abline(h =mean(day$Transactions) , col="red")  # add cutoff line
day <- day %>% filter(Transactions>mean(Transactions))
d_20 <- d_outliers %>% filter(day==20)
d_20 <- d_20 %>% group_by(hour) %>% group_by(minutes)
View(d_20)

out <- kable(d_20) %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  kable_styling(full_width = F)
#excel
tmp <-writexl::write_xlsx(d_20)
