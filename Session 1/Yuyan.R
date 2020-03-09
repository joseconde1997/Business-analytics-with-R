library(readxl)
library(tidyverse)
library(ggplot2)
library(gmodels)
library(devtools)


# import data
d <- read_xlsx("laptops.xlsx")
summary(d)

# numberic data
data <- select(d, company, price, inches, ram, weight, ghz, mem1, mem2)

# all 1303 obs
pca <- prcomp(data[ , -c(1,8)], center=TRUE, scale.=TRUE)

