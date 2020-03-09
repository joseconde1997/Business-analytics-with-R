library(readxl)
library(tidyverse)
library(lubridate)
d_quarter <- read_excel("gap.xlsx",sheet = "Quarterly sales")
quarter <- quarter(d_quarter$Quarter)
quarters <- c()
i <- 1
for (i in 1:112) {
  if(i%%4!=0){
    print(i%%4)
    quarters<- c(quarters,i%%4)
  }
  else{
    quarters<- c(quarters,4)
  }
  
}
quarters
d_quarter$index <- quarters
quarter_mean <- d_quarter %>% group_by(index) %>% summarise(mean(Sales))
quarter_mean
