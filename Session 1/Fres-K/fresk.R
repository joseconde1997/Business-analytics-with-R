library(tidyverse)
library(gmodels)
library(readxl)
library(ggplot2)
library(plotly)
fresk <- read_excel("fresk.xlsx")
ggplot(fresk,aes(k101,score)) + geom_point(size=3)
ggplot(fresk,aes(k905,score)) + geom_point(size=3)
ggplot(fresk,aes(k101,k905)) + geom_point(size=3)
fresk[match(max(fresk$score),fresk$score),2]
fresk[match(max(fresk$score),fresk$score),3]
max_score <- c(fresk[match(max(fresk$score),fresk$score),2],fresk[match(max(fresk$score),fresk$score),3])
plot_ly(x=fresk$score, y=fresk$k101, z=fresk$k905, type="scatter3d", mode="markers", color="grey40")
