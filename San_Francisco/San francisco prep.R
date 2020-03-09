library(tidyverse)
library(readxl)
learning <- read_excel("Learning_data.xlsx")
testing <- read_excel("Testing data.xlsx")
San_francisco <- read_excel("San_francisco.xlsx")
View(San_francisco)
mod_price <- lm(Price~Squarefeet+Lotsize+as.factor(Neighborhood) + 
                  Bedrooms+Dayslisted+Year,data=learning)
summary(mod_price)
View(-sort(residuals(mod_price)))
San_francisco$predict <- predict(mod_price,San_francisco)
San_francisco$err <- San_francisco$Price - San_francisco$predict
testing$pred <- predict(mod_price,testing)
testing$err <- testing$Price-testing$pred
ggplot(testing,aes(pred,Price)) + geom_point(alpha=0.6, size=0.3) +
  geom_smooth(method="lm",color="brown",size=0.4,se=FALSE)
