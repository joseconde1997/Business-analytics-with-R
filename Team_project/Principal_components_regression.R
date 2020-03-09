#Principal components regression
library(pls)
library(tidyverse)
library(ggplot2)
jaya <- readxl::read_excel("jayalaxmi.xlsx")
jaya_pr <- select(jaya,income_per_acre,mechanization, temp_mgmt,chawki_bivol,
                    mulberry_diseases, loan_amount,crop_insured, 
                    own_vermi_compost, bio_fertilizers, affected_by_pest,
                    rearing_cost,temp_mgmt,training_on_sericulture,borewell_recharge)
pr_all <- jaya[1:508,]
pr_learning <- jaya_pr[1:470,]
pr_testing <- jaya_pr[470:508,c(-1)]
pr_testing_y <- jaya_pr[470:508,c(1)]

set.seed(1)
fit <- pcr(income_per_acre~.,data=pr_learning,scale=TRUE,validation="CV",na.action=na.omit)
summary(fit)
fit$coefficients
fit_data <- as_tibble(fit$coefficients) 
validationplot(fit)
validationplot(fit,val.type = "MSEP")
validationplot(fit,val.type = "R2")
predplot(fit)
fit_pred <- predict(fit,pr_learning,ncomp=12)
cor(pr_learning[,1],fit_pred,use = "pairwise.complete.obs")^2
set.seed(10)
fit_all <- pcr(income_per_acre~.,data=jaya,scale=TRUE,validation="CV",na.action=na.omit)
summary(fit_all)
View(abs(fit_all$coefficients))
fit_data <- as_tibble(fit$coefficients) 
validationplot(fit_all)
validationplot(fit,val.type = "MSEP")
validationplot(fit,val.type = "R2")
predplot(fit)
mod_pc <- lm(income_per_acre~chawki_bivol+mechanization+temp_mgmt+loan_amount+training_on_sericulture+loan_repaid+mulberry_diseases+affected_by_pest,data=jaya)
summary(mod_pc)
