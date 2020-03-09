#Sampling
set.seed(9912)
mean_loyal <- replicate(100000,{
  loyal <- sample(mwc$Loyal,10)
  mean(loyal)
}) 
hist(mean_loyal)
set.seed(9912)
mean_Noloyal <- replicate(100000,{
  Non_loyal <- sample(mwc$No_loyal,10)
  mean(Non_loyal)
}) 
hist(mean_Noloyal)

mean_loyal_data <- data.frame(grade=mean_loyal) 
mean_Noloyal_data <- data.frame(grade=mean_Noloyal)
mean_loyal_data$loyalty <- 'loyal'
mean_Noloyal_data$loyalty <- 'No loyal'
loyalty_sample <- rbind(mean_loyal_data,mean_Noloyal_data)
ggplot(loyalty_sample,aes(grade,fill=loyalty)) + geom_density(alpha=0.2) + 
  ggtitle("Density of loyalty") + theme(plot.title = element_text(hjust = 0.5))
CI_loyal <- c((mean(mean_loyal)-sd(mean_loyal)/sqrt(length(mean_loyal))),
              (mean(mean_loyal)+sd(mean_loyal)/sqrt(length(mean_loyal))))
CI_NoLoyal <- c((mean(mean_Noloyal,na.rm= TRUE)-sd(mean_Noloyal,na.rm = TRUE)/sqrt(length(mean_Noloyal[!is.na(mean_Noloyal)]))),
                (mean(mean_Noloyal,na.rm= TRUE)+sd(mean_Noloyal,na.rm=TRUE)/sqrt(length(mean_Noloyal))))
