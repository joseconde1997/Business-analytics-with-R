library(kableExtra)
#R squares test
trend_seasonalR <- 0.7156595
ArimaR <- 0.7038627
HWR <-0.7269356
Rs <-c(trend_seasonalR,ArimaR,HWR)

#rms test
trend_seasonalrms <- 56.82068
Arimarms <- 57.04532
hwrms <- 62.68024
RMS <- c(trend_seasonalrms,Arimarms,hwrms)
rows <- c("Trend Seasonal","Arima","Holt winters")
#accuaracy table
acc <- cbind(Method=rows,R_squared=Rs,RMSE=RMS)
acc <- as.matrix(acc)
acc <-kable(acc) %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  kable_styling(full_width = F)
acc

#holt winters
conf <- hw.d
conf$Actual[73:84] <- hw.d$Test[73:84]
conf$Predicted[73:84] <- hw.d$Forecast[73:84]
conf$Upper[73:84] <- hw.d$Upper_t[73:84]
conf$Lower[73:84] <- hw.d$Lower_t[73:84]
conf <- conf[,1:4]
conf$Inside <- ifelse(conf$Actual<conf$Upper,"In","Out")
conf$Inside <- ifelse(conf$Actual>conf$Lower,conf$Inside,"Out")
conf$Inside <- factor(conf$Inside)
conf$index <- c(1:84)

View(conf)
table(conf$Inside)
12/84
al <- kable(conf) %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  kable_styling(full_width = F)
al
in_out <- kable(conf[73:84,]) %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  kable_styling(full_width = F)
in_out
conf$Inside
out <- conf [c(5,9,11,23,27,31,43,53,57,59,75,83),]
out <- data.frame(out)
out <- kable(out) %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  kable_styling(full_width = F)
out 
