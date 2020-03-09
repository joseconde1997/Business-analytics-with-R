library(readxl)
library(tidyverse)
library(lubridate)
library(writexl)
library(ggplot2)
library(stringi)
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
d_IN <- d %>% filter(stri_detect_fixed(ID,"INC"))
d_IN <- d_IN %>% filter(!stri_detect_fixed(ID,"$C"))
d_IN <- d_IN%>% filter(Amount<1000)
d_IN <- d_IN %>% filter(Amount>-10)
sum(d_IN$Amount)
d_biggie <- d_IN %>% filter(!stri_detect_fixed(ID,"$C"))
d_f <-d_biggie%>% filter(Amount<1000)
d_f <- d_f %>% filter(Amount>-10)
daily <- d_f %>% group_by(day) %>% summarise(Transactions=abs(mean(Amount)))
View(daily)
plot(daily$Transactions,type="l")
daily.ts <- ts(daily$Transactions,frequency = 7)
hw_daily <- ets(daily.ts,model="MAA")
plot(daily.ts,type="l")
lines(hw_daily$fitted,col="blue")
November_mega <- data.frame(Actual=daily.ts,Predicted=hw_daily$fitted)
November_mega$upper <- November_mega$Predicted +coefficient*sd(November_mega$Predicted-November_mega$Actual,na.rm=T)
November_mega$lower <- November_mega$Predicted -coefficient*sd(November_mega$Predicted-November_mega$Actual,na.rm=T)
plot(November_mega$Actual,type="l",ylim=c(40,70))
lines(November_mega$Predicted,col="blue")
lines(November_mega$upper,col="red",lty = 'dashed')
lines(November_mega$lower,col="red",lty = 'dashed')
sum(d_f$Amount)
mean(d$Amount)
ID_transactions <- d %>% group_by(d$ID) %>% summarise(mean(d$Amount))
n_occur <- data.frame(table(d$ID))
repeating_freq <- n_occur[n_occur$Freq > 1,]
sum(repeating_freq$Freq)
repeating_d <- d %>% filter(d$ID=="AAANSVUTJO7500OUTWTKAQDDC" | d$ID=="CSYECEFIIX7200OUTWTAXKRJA" | 
                            d$ID=="DMOVBFTKOA7500OUTWTWIHZQS" | d$ID=="LHBRDRZYJR6600OUTWTRPCZTZ" |
                            d$ID=="XTCWUXARTC8400OUTWTOBVCES" | d$ID=="YWHBLCKAFN8800OUTWTVTBOME" | 
                            d$ID=="ZRCXWIGEKW8400OUTWTWGRJTP")
groups <- repeating_d %>% group_by(ID) %>% summarise(sum(Amount))

ggplot(repeating_d,aes(day,Amount))+ geom_text(aes(label=ID),size=2.5,check_overlap = T)
d_clean <- d %>% filter(!stri_detect_fixed(ID,"$C"))
week_d <- d %>% group_by(week) %>% summarise(sum(week))
#gibberish 1
d_small <- d_clean
d_small <-d_clean%>% filter(Amount<1000)
d_small <- d_small %>% filter(Amount>0)

#OUT
d_OUT <- d_small %>% filter(stri_detect_fixed(ID,"OUT"))
#IN
d_IN <- d_small %>% filter(stri_detect_fixed(ID,"INC"))
out <- d_OUT$Amount
inc <- d_IN$Amount
sum(inc)
Total <- sum(inc)-sum(out)
sum(d_small$Amount)
#gibberish 2
d_small <-d_clean %>% filter(Amount<50)
d_small <- d_small %>% filter(Amount>0)
sum(d_small$Amount)

