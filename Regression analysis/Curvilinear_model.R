#Change d depending of the graph you want to display
d <- select(smart2,price,size)
mod <- lm(price~ppi+I(ppi^2),data=d)
summary(mod)
ggplot(d,aes(ppi,price)) + geom_point(size=0.8,alpha=0.4) +
  geom_smooth(method="lm",formula = y~poly(x,2),se=FALSE,color="brown",size=0.6)
ggplot(d,aes(size,price)) + geom_point(size=0.8,alpha=0.4) +
  geom_smooth(method="lm",formula = y~poly(x,2),se=FALSE,color="brown",size=0.6)
mod <- lm(price~size+I(size^2),data=d)
summary(mod)
#Interaction terms
mod <-lm(price~camera+battery+camera:battery,data=smart2)
summary(mod)
