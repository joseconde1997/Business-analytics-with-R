library(ggplot2)
library(readxl)
scandia <- read_excel("scandia.xlsx")
scandia_age <- scandia %>% filter(EXPERIENCE>15 & EXPERIENCE<23)
scandia_age_male <- scandia_age %>% filter(GENDER=='Male')
scandia_age_female <- scandia %>% filter(GENDER=='Female')
t.test(scandia_age_male$SALARY,scandia_age_female$SALARY)
#plots
ggplot(scandia_age,aes(as.factor(EXPERIENCE),SALARY,color=GENDER)) + geom_point()
