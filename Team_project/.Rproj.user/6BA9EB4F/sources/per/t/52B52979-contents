#Principal component
#Covariance 2 decimals
library(tidyverse)
library(knitr)
library(ggbiplot)
library(tidyverse)
View(round(cov(jaya),2))
jaya_sd <- as_tibble(scale(jaya))
jaya_no_miss <- jaya %>% dplyr::filter(complete.cases(.))
jaya_sd <- as_tibble(scale(jaya_filter))
View(round(jaya_sd,2))
#Weighted covariance table
View(round(cor(jaya_sd),2))
#Principal components
jaya_sd.pca <- prcomp(jaya_sd,center = TRUE, scale. = TRUE)
jaya_pc <- as.data.frame(abs(round(t(jaya_sd.pca$rotation),2)))
jaya_pc_rotation <- as.data.frame(abs(round((jaya_sd.pca$rotation),2)))
rownames(jaya_pc_rotation) <- c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11",
                                "PC12","PC13","PC14","PC15","PC16","PC17","PC18","PC19","PC20","PC21","PC22","PC23",
                                "PC24","PC25")
View(jaya_pc_rotation)
jaya_sd.pca$rotation
sum(jaya_pc_rotation$PC1)
View(jaya_pc_rotation)
sum(jaya_pc_rotation)
View(summary(jaya_sd.pca))
View(abs(t(jaya_sd.pca$rotation)))
round(jaya_sd.pca$rotation,2)
View(round(jaya_sd.pca$x))
ggbiplot(jaya_sd.pca, varname.adjust = 3,varname.size = 2.5,alpha=0.5,size=0.4) +
  ylim(-2,2.5)+xlim(-3,2) + theme(axis.title = element_text(size=9))
jaya_sd.pca <- prcomp(select(jaya,income_per_acre,mechanization, temp_mgmt ,chawki_bivol,
                               mulberry_diseases, loan_amount,crop_insured, 
                               own_vermi_compost, bio_fertilizers,affected_by_pest,rearing_cost,temp_mgmt,training_on_sericulture,borewell_recharge),center = TRUE, scale. = TRUE)
summary(jaya_sd.pca)
jaya_sd.pca <- prcomp(income_per_acre~.,jaya,center = TRUE, scale. = TRUE)
summary(jaya_sd.pca)

