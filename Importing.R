library(tidyverse)
smart <- read_csv(file="https://raw.githubusercontent.com/wdecisions/ba/master/smartphones.csv")
smart_3 <- smart %>% dplyr::select(brand,model,price)


