library(readxl)
library(tidyverse)
library(ggbiplot)
quest <- read_excel("hrbarney_pca.xlsx")
quest_sd <- as_tibble(scale(quest))
quest_pc <- prcomp(quest,center = TRUE)
summary(quest_pc)
quest_pc$x
quest_pc$rotation
ggbiplot(bcp_pr, varname.adjust = 3,varname.size = 2.5,alpha=0.5,size=0.4) +
  ylim(-2,2.5)+xlim(-3,2) + theme(axis.title = element_text(size=9))
