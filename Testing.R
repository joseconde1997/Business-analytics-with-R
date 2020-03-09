library(tidyverse)
cities <- c("Munich","Madrid","Rome")
age <- c(20,80,42)
lista <- list(cities=cities,ages=age)
myTib<- tibble(y=age,x=cities)
m <- matrix(c(1,2,3,4),ncol=2)
rows_m <- m[1,]
cols_m <- m[,2]
t_m <- t(m)
inv_m <- solve(m)
m_m <- m %*% inv_m

