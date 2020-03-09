#Linear regression
library(neuralnet)
library(readxl)
library(plotly)
neural_data <- read_excel ("jayalami_filter.xlsx")
attach(neural_data)
normalize <- function(x){
  return((x-min(x))/(max(x) - min(x)))
}
maxmindf <- as.data.frame((lapply(neural_data,normalize)))
set.seed(123)
learning <- maxmindf[1:420,]
testing <- maxmindf[421:488,]
nn <- neuralnet (income_per_acre~ mechanization + temp_mgmt + chawki_bivol +
                   mulberry_diseases + loan_amount + crop_insured + 
                   own_vermi_compost + bio_fertilizers + affected_by_pest +
                   rearing_cost + temp_mgmt + rearing_cost_missing,
                 data=learning,linear.output = F,hidden =c(1,2),err.fct = "sse")
pred <- compute(nn,testing[,1:25])$net.result
actual <- testing[,23]
pred_actual = data.frame(pred=pred,actual=actual)
cor(pred,actual)^2
ggplot(pred_actual,aes(pred,actual)) + geom_point() + geom_smooth(size=0.4,method="lm",formula ="y~poly(x,1)",se=F,color="brown")
cor_nn2 <- c()
for (i in 1:15) {
  nn_2 <- neuralnet (income_per_acre~ mechanization + temp_mgmt + chawki_bivol +
                       mulberry_diseases + loan_amount + crop_insured + 
                       own_vermi_compost + bio_fertilizers + affected_by_pest +
                       rearing_cost + temp_mgmt + rearing_cost_missing + loan_repaid+years_of_exp_in_sericulture + 
                       training_on_sericulture + krishi_pond + borewell_recharge + rain_harvesting +
                       own_compost_manure + trenching_mulching + instrument_mgmt_cost + humidity_mgmt + 
                       airvent_temp_mgmt + rotary_mounting + seri_total_subsidy,
                     data=learning,linear.output = F,hidden =c(100),err.fct = "sse")
  
  pred <- compute(nn_2,learning[,1:25])$net.result
  actual <- testing[,23]
  cor_nn2 <-c(cor_nn2,cor(pred,actual)^2)
  
}
nn_2 <- neuralnet (income_per_acre~ mechanization + temp_mgmt + chawki_bivol +
                     mulberry_diseases + loan_amount + crop_insured + 
                     own_vermi_compost + bio_fertilizers + affected_by_pest +
                     rearing_cost + temp_mgmt + rearing_cost_missing + loan_repaid+years_of_exp_in_sericulture + 
                     training_on_sericulture + krishi_pond + borewell_recharge + rain_harvesting +
                     own_compost_manure + trenching_mulching + instrument_mgmt_cost + humidity_mgmt + 
                     airvent_temp_mgmt + rotary_mounting + seri_total_subsidy,
                   data=learning,linear.output = F,hidden =c(50),err.fct = "sse")

pred <- compute(nn_2,testing[,1:25])$net.result
actual <- testing[,23]
cor(pred,actual)^2

								
