# 1) As it can  be seen in the graph the loyal customers were more satisfied than
#the non loyal ones. Also the mean indicates so.
#2) Looking at CI_lo and CI_nonlo it can be concluded that 95% of the customers 
#that weren't satisfied had a score between 2.9 and 3.08 while the satisfied customers
#had an score of 3,24 and 3.4 95% of the time
#3) The confidence intervals can be used in t distributions and as this sample is
#bigger than 50 it should have an small error
library(readxl)
library(ggplot2)
library(knitr)
mwc <- read_xlsx("C:\\Users\\conde_000\\OneDrive - IESE Business School - University of Navarra\\MiM\\Business analytics R\\movile world congress\\mwc-data.xlsx")
non_loyal <- mwc$No_loyal
loyal <- mwc$Loyal
loyal <- data.frame(loyal)
non_loyal <- data.frame(loyal=non_loyal)
loyal$loyalty <- 'loyal'
non_loyal$loyalty <-'no loyal'
data_mwc <- rbind(loyal,non_loyal)
ggplot(data=data_mwc,aes(loyal,fill=loyalty)) + geom_density(alpha=0.2)

non_lo <- data_mwc %>% filter(loyalty=='no loyal')
lo <- data_mwc %>% filter(loyalty=='loyal')
mean_lo <- mean(lo$loyal)
mean_non_lo <- mean(non_lo$loyal,na.rm = TRUE)
sd_lo <- sd(lo$loyal)
sd_non_lo <- sd(non_lo$loyal,na.rm = TRUE)
#Confidence limits table
tibble(
  classification=c("loyal","non loyal"),
  n=c(length(lo$loyal),length(non_lo$loyal[!is.na(non_lo$loyal)])),
  mean=c(mean_lo,mean_non_lo),
  sd=c(sd_lo,sd_non_lo))
#Calculating  95% confidence interval
CI_loup <- mean_lo + 2*sd_lo/sqrt(161)
CI_lodown <- mean_lo - 2*sd_lo/sqrt(161)
CI_lo <- c(CI_lodown,CI_loup)
CI_nonloup <- mean_non_lo + 2*sd_lo/sqrt(161)
CI_nonlodown <- mean_non_lo - 2*sd_lo/sqrt(161)
CI_nonlo <- c(CI_nonlodown,CI_nonloup)