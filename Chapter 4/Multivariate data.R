library(tidyverse)
library (readr)
library(knitr)
smart <- read.csv(file="https://raw.githubusercontent.com/wdecisions/ba/master/smartphones.csv")
table(smart$jack,smart$gps)
smart2 <- smart %>% dplyr::filter(complete.cases(.))
smart2
kable(smart %>%
        summarize_if(is.numeric,funs(mean,sd,max,median,min),na.rm=T) %>%
        gather(stat,val) %>%
        separate(stat,into = c("var", "stat"),sep="_") %>%
        spread(stat,val) %>%
        select(var,mean,sd,min,median,max),booktabs=T,digits=2)
smart_num <- select(smart2,weight:release)
round(cor(smart_num),2)
