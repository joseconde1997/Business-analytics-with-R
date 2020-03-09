library(tidyverse)
library(ggbiplot)
smart_num <- select(smart2,brand,weight:release) %>%
  dplyr::filter(brand!="Nvidia")
firms <- smart_num %>%
  group_by(brand) %>%
  dplyr::summarise(weight = mean(weight),mem=mean(mem),ram=mean(ram),
                   camera=mean(camera),battery=mean(battery),price=mean(price),
                   vol=mean(vol),size=mean(size),ppi=mean(ppi),release=mean(release))
#list the top brands by average price
firms %>% dplyr::filter(price>300) %>% arrange(-price)
#use brands as rownames
firms <- column_to_rownames(firms,var="brand")
brand.pca <- prcomp(firms,center= TRUE,scale. = TRUE)
ggbiplot(brand.pca,labels = rownames(firms),labels.size = 2,size=0.5) + ylim(-1.5,3.9)+
  xlim(-2.5,3) + theme(axis.title = element_text(size=9))
