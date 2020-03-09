#linear regression
library(ggplot2)
library(tidyverse)
library(writexl)
jaya <- readxl::read_excel("jayalaxmi.xlsx")
jaya <- select(jaya,c(-25))
jaya$years_loan <- jaya$loan_amount/(jaya$years_of_exp_in_sericulture+1)
View(jaya)
#jaya$loan_sub <- jaya$loan_amount-jaya$seri_total_subsidy
#jaya$loan_amount <- ifelse(jaya$loan_amount>(mean(jaya$loan_amount)),1,0)
#jaya$rearing_cost <- ifelse(jaya$rearing_cost>mean(jaya$rearing_cost),1,0)
jaya_filter <- jaya %>% filter(!is.na(rearing_cost_missing))
View(round(abs(cor(jaya_filter)),2))
View(round(cor(jaya),2))

as.table(round(abs(cor(jaya_filter)),2))
linear_learning <- jaya[1:470,]
linear_testing <- jaya[470:508,]
mod_ln <- lm(income_per_acre~as.factor(mechanization) + as.factor(temp_mgmt) + as.factor(chawki_bivol)+
               as.factor(mulberry_diseases) + loan_amount + as.factor(crop_insured) + 
               as.factor(own_vermi_compost) + as.factor(bio_fertilizers) + as.factor(affected_by_pest)+
             rearing_cost + as.factor(temp_mgmt)+as.factor(training_on_sericulture)+as.factor(borewell_recharge)
             ,data=jaya)
summary(mod_ln)
predict <- data.frame(prediction=predict(mod_ln),actual=jaya$income_per_acre)
predict$error <- predict$actual-predict$prediction
plot(predict$prediction,predict$actual)
ggplot(predict,aes(prediction,actual)) + geom_point(size=0.3,alpha=0.4) + 
  geom_smooth(method="lm",formula="y~poly(x,1)",se=F,color="blue",size=0.4,)
ggplot(predict,aes(actual)) + geom_density()
ggplot(predict,aes(prediction)) + geom_density()
cor(predict$prediction,predict$actual)
cor(predict$prediction,predict$actual)^2
write_xlsx(jaya_filter,"jaya_filter.xlsx")
mod_ln_2 <- lm(income_per_acre ~ loan_amount +  crop_insured  + 
               training_on_sericulture +  borewell_recharge  + own_vermi_compost  + bio_fertilizers 
             + mechanization + mulberry_diseases + affected_by_pest + rearing_cost +  temp_mgmt + chawki_bivol
             ,data=jaya)
summary(mod_ln_2)
mod_all <- lm(income_per_acre ~ loan_amount + loan_repaid + crop_insured + years_of_exp_in_sericulture + 
                     training_on_sericulture + krishi_pond + borewell_recharge + rain_harvesting + 
                     own_compost_manure + own_vermi_compost + trenching_mulching + bio_fertilizers 
                   + mechanization + mulberry_diseases + affected_by_pest + rearing_cost + 
                     instrument_mgmt_cost + temp_mgmt + humidity_mgmt + airvent_temp_mgmt + 
                     rotary_mounting + seri_total_subsidy + chawki_bivol+ rearing_cost_missing
                   ,data=jaya)
summary(mod_all)
mod_training <- lm(income_per_acre ~ training_on_sericulture ,data=jaya)
summary(mod_training)
mod_training_m <- lm(income_per_acre ~ training_on_sericulture+mechanization ,data=jaya)
summary(mod_training_m)
mod_training_tmp <- lm(income_per_acre ~ training_on_sericulture+temp_mgmt ,data=jaya)
summary(mod_training_tmp)
mod_training_y <- lm(income_per_acre ~ training_on_sericulture + years_of_exp_in_sericulture ,data=jaya)
summary(mod_training_y)
mod_training_chawki <- lm(income_per_acre ~ training_on_sericulture +chawki_bivol  ,data=jaya)
summary(mod_training_chawki)

mod_training_comb <- lm(income_per_acre ~ training_on_sericulture + mechanization + temp_mgmt + own_vermi_compost +
                          years_of_exp_in_sericulture + chawki_bivol  ,data=jaya)
summary(mod_training_comb)
mod_ln6 <- lm(income_per_acre ~ loan_amount + crop_insured +
                training_on_sericulture + own_vermi_compost + 
                bio_fertilizers+ mechanization + mulberry_diseases + 
                affected_by_pest + rearing_cost + temp_mgmt + chawki_bivol
              ,data=jaya)
summary(mod_ln6)              
mod_tmp <- lm(income_per_acre~temp_mgmt,data=jaya)
summary(mod_tmp)
jaya_no_tmp <- jaya %>% filter(temp_mgmt<1)
jaya_tmp <- jaya %>% filter(temp_mgmt>0)
mean(jaya$mechanization)
mean(jaya$own_vermi_compost)
mean(jaya_no_tmp$mechanization)
mean(jaya_no_tmp$own_vermi_compost)
mod_notmp <- lm(income_per_acre~mechanization,data=jaya_no_tmp)
summary(mod_notmp)
quantile(jaya_tmp$income_per_acre)
quantile(jaya_tmp$mechanization)
quantile(jaya_no_tmp$income_per_acre)
quantile(jaya_no_tmp$mechanization)

