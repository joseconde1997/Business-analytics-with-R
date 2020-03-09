library(tidyverse)
library(knitr)
kable(smart %>% summarize(
  n_brands=n_distinct(brand),
  n_models=n_distinct(model)
  ,n_displays=n_distinct(display)
  ,n_os=n_distinct(os),booktabs=TRUE))
ggplot(smart,aes(ppi,price,label=model)) +
  geom_text(aes(label=model),size=2.5,check_overlap = TRUE)
samsung <- smart %>% filter(brand=="Samsung") 
ggplot(samsung,aes(ppi,price,label=model)) + 
  geom_text(aes(label=model),size=2.5, check_overlap = TRUE)     
ggplot(smart) + geom_bar(aes(x=brand, fill=brand)) +
  theme(legend.position = "none",axis.text.x = element_text(angle=90,hjust = 1,size = 8))
ggplot(filter(d,count>20)) + geom_bar(aes(x=brand, fill=brand)) +
  theme(legend.position = "none", axis.text.x=element_text(angle = 90, hjust = 1, size = 11))
