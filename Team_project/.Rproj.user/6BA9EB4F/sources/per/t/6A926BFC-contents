#Learning and testing
library(ggplot2)
jaya <- readxl::read_excel("jayalaxmi.xlsx")
mod_all <- lm(income_per_acre ~ loan_amount + loan_repaid + crop_insured + years_of_exp_in_sericulture + 
                training_on_sericulture + krishi_pond + borewell_recharge + rain_harvesting + 
                own_compost_manure + own_vermi_compost + trenching_mulching + bio_fertilizers 
              + mechanization + mulberry_diseases + affected_by_pest + rearing_cost + 
                instrument_mgmt_cost + temp_mgmt + humidity_mgmt + airvent_temp_mgmt + 
                rotary_mounting + seri_total_subsidy + chawki_bivol+ rearing_cost_missing
              ,data=jaya)
summary(mod_all)
mod_ln6 <- lm(income_per_acre ~ loan_amount + crop_insured +
                training_on_sericulture + own_vermi_compost + 
                bio_fertilizers+ mechanization + mulberry_diseases + 
                affected_by_pest + rearing_cost + temp_mgmt + chawki_bivol
              ,data=jaya)
summary(mod_ln6)  
R_sampling_modall <- c()
for (i in 1:1000) {
  testing_index <- sample(1:508,30)
  learning_data <- jaya[-testing_index,]
  testing_data <- jaya [testing_index,]
  mod_all <- lm(income_per_acre ~ loan_amount + loan_repaid + crop_insured + years_of_exp_in_sericulture + 
                  training_on_sericulture + krishi_pond + borewell_recharge + rain_harvesting + 
                  own_compost_manure + own_vermi_compost + trenching_mulching + bio_fertilizers 
                + mechanization + mulberry_diseases + affected_by_pest + rearing_cost + 
                  instrument_mgmt_cost + temp_mgmt + humidity_mgmt + airvent_temp_mgmt + 
                  rotary_mounting + seri_total_subsidy + chawki_bivol+ rearing_cost_missing
                ,data=learning_data)
  
  predict <- data.frame(prediction=predict(mod_all,testing_data),actual=jaya$income_per_acre[testing_index])
  predict$error <- predict$actual-predict$prediction
  R_sampling_modall <- c(R_sampling_modall,cor(predict$prediction,predict$actual)^2)
  
}
mean(R_sampling_modall)

predict <- data.frame(prediction=predict(mod_ln6),actual=jaya$income_per_acre)
predict$error <- predict$actual-predict$prediction
plot(predict$prediction,predict$actual)
ggplot(predict,aes(prediction,actual)) + geom_point(size=0.3,alpha=0.4) + 
  geom_smooth(method="lm",formula="y~poly(x,1)",se=F,color="blue",size=0.4,)

R_sampling_mod6 <- c()
for (i in 1:1000) {
  testing_index <- sample(1:508,30)
  learning_data <- jaya[-testing_index,]
  testing_data <- jaya [testing_index,]
  mod_learning <-  lm(income_per_acre ~ loan_amount + crop_insured +
                        training_on_sericulture + own_vermi_compost + 
                        bio_fertilizers+ mechanization + mulberry_diseases + 
                        affected_by_pest + rearing_cost + temp_mgmt + chawki_bivol
                      ,data=learning_data)
  
  predict <- data.frame(prediction=predict(mod_ln6,testing_data),actual=jaya$income_per_acre[testing_index])
  predict$error <- predict$actual-predict$prediction
  R_sampling_mod6 <- c(R_sampling_mod6,cor(predict$prediction,predict$actual)^2)
  
}
mean(R_sampling_mod6)
ggplot(predict,aes(prediction,actual)) + geom_point(size=3,alpha=0.4) + 
  geom_smooth(method="lm",formula="y~poly(x,1)",se=F,color="blue",size=0.4,)

