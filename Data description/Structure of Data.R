#Structure of Data
str(smart, width=150, strict.width="wrap")
summary(select(smart, weight:release))
smart %>% 
  filter(ppi>=807) %>%
  select(brand,model,release,price,ppi,display)
#??????
knitr::include_graphics("dfsummary.png")
view(dfSummary.png)
#Categorical variables
table(smart$os)
prop.table(table(smart$os))
table(smart$jack)
table(smart$os, smart$display)
#central tendency
mean(smart$price)
median(smart$price)
#Variability
quantile(smart$price)
