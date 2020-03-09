#Libraries used
library(ggplot2)
library(tidyverse)
#Linear model and the summary
jaya <- readxl::read_excel("jayalaxmi.xlsx")
mod_ln <- lm(income_per_acre ~ loan_amount + crop_insured +
                training_on_sericulture + own_vermi_compost + 
                bio_fertilizers+ mechanization + mulberry_diseases + 
                affected_by_pest + rearing_cost + temp_mgmt + chawki_bivol
              ,data=jaya)
summary(mod_ln) 
mod <- lm(income_per_acre ~ rearing_cost,jaya)
summary(mod)
mod <- lm(income_per_acre~chawki_bivol,jaya)
#Predict vs actual plot
predict <- data.frame(prediction=predict(mod_ln),actual=jaya$income_per_acre)
predict$error <- predict$actual-predict$prediction
ggplot(predict,aes(prediction,actual)) + geom_point(size=0.3,alpha=0.4) + 
  geom_smooth(method="lm",formula="y~poly(x,1)",se=F,color="blue",size=0.4,)
#temperature management and mechanization
jaya_no_tmp <- jaya %>% filter(temp_mgmt<1)
mean(jaya_no_tmp$mechanization)
mean(jaya$mechanization)
mod_mech_tmp <- lm(income_per_acre~mechanization+temp_mgmt,data=jaya)
summary(mod_mech_tmp)
