#Returns without for loops

kapoor$cnx <- (kapoor$CNX-lag(kapoor$CNX)*100/lag(kapoor$CNX))
view(kapoor)
kapoor$larsen <- (kapoor$LARSEN-lag(kapoor$CNX))*100/lag(kapoor$LARSEN)
kapoor <- kapoor[-1,]
cor(kapoor$cnx,kapoor$larsen)
camp <- lm(larsen~cnx,data=kapoor)
summary(camp)
kapoor$predict_larsen <- predict(camp)
kapoor$resid_larsen <- kapoor$larsen-kapoor$predict_larsen
#K
cor(kapoor$larsen,kapoor$predict_larsen)^2
var(kapoor$predict_larsen)/var(kapoor$larsen)
ggplot(kapoor,aes(cnx,larsen)) + geom_point() + 
  geom_smooth(method="lm", se=FALSE, size=0.5,alpha=0.6)
