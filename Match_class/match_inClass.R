# Match.com

# load libraries
library(writexl)
library(tidyverse)
library(readxl)

# load data
match <- as.data.frame(read_xlsx("C:\\Users\\conde_000\\OneDrive - IESE Business School - University of Navarra\\MiM\\Business analytics R\\Match_class\\data_mim20.xlsx"))

# omit NA's and shorten names
match <- match %>% 
  na.omit() %>% 
  mutate(name=substr(name, 0, 12))

# use first column variable for rownames
rownames(match) <- match[ , 1]
match <- match[ , -1] # remove first column

# standardize variables (x - mean(x)) / sd(x) 
match <- scale(match)

# Distance across Students ------------------------------------------------

# compute distances
dist <- round(dist(match, diag = T, upper = T),1)
dist

# averages
summary(as.matrix(dist))

# check euclidean distance manually
sqrt(sum((match[1,]-match[2,])^2))

# save distances matrix to excel
write_xlsx(as.data.frame(as.matrix(dist)),"distances_test.xlsx")
