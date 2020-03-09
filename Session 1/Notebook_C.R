library(tidyverse)
library(knitr)
library(ggbiplot)
d <- laptops %>% select(inches:mem2)
d <- d %>% select(1,2,4:7)
laptops_sd <- as_tibble(scale(d))
View(round(laptops_sd,2))
#Weighted covariance table
#Principal components
laptops_sd.pca <- prcomp(laptops_sd,center = TRUE, scale. = TRUE)
summary(laptops_sd.pca)
round(laptops_sd.pca$rotation,2)
laptops_sd.pca$x
view(laptops_sd.pca$rotation)
view(round(laptops_sd.pca$x))
ggbiplot(laptops_sd.pca, varname.adjust = 3,varname.size = 2.5,alpha=0.2,size=0.4) +
  ylim(-2,2.5)+xlim(-3,2) + theme(axis.title = element_text(size=9))
firms <- laptops %>% group_by (company) %>%
  dplyr::summarise(price = mean(price),inches = mean(inches),ghz=mean(ghz),ram=mean(ram),
                   weight=mean(weight),mem1=mean(mem1))
firms <- column_to_rownames(firms,var="company")
brand.pca <- prcomp(firms, center=TRUE,scale. = TRUE)
summary(brand.pca)
brand.pca$rotation
ggbiplot(brand.pca, labels = rownames(firms),labels.size = 3.5,size=0.5) +
  ylim(-2,2.5)+xlim(-3,2) + theme(axis.title = element_text(size=9))
