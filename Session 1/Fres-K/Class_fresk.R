library(readxl)
library(writexl)
library(plotly)
d <- read_xlsx("fresk.xlsx")

summary(d)

cor(d)

plot(d$k101,d$score)
plot(d$k905,d$score)

plot_ly(x=d$k905,y=d$k101,z=d$score,
        color= I("black"),alpha=I(1),size=I(100))
d$dif <-abs(d$k101-d$k905)
cor(d)

plot(d$dif,d$score)

ggplot(d,aes(dif,score)) + geom_point(size=3,color="grey40",alpha=1)
mod <- lm(score~dif,data = d)
summary(mod)
write_xlsx(d,"difference_fesk.xls")
