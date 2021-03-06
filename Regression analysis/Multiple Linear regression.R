library(tidyverse)
apple <- smart %>% filter(brand=="Apple")
apple2 <- apple %>% dplyr::filter(complete.cases(.))
d <- select(smart2,weight:release)
d_app <- select(apple2,weight:release)
mod <-lm(price~weight+mem+ram+camera+battery+vol+size+ppi+release,data=d)
summary(mod)
mod_app <- lm(price~weight+mem+ram+camera+battery+vol+size+ppi+release,data=d_app)
summary(mod_app)              
smart_2016 <- smart %>% filter(release=="2016")
apple_2016 <- apple %>% filter(release=="2016")
smart2_2016 <- smart_2016 %>% dplyr::filter(complete.cases(.))
apple2_2016 <-apple_2016 %>% dplyr::filter(complete.cases(.))
mean(apple2_2016$price)-mean(smart2_2016$price)
mean(apple2_2016$ram)-mean(smart2_2016$ram)
mean(apple2_2016$camera)-mean(smart2_2016$camera)
mean(apple2_2016$battery)-mean(smart2_2016$battery)
samsung <- smart %>% filter (brand=="Samsung")
app_sam <- rbind(apple,samsung)
app_sam <- app_sam %>% filter(release=="2016")
ggplot(app_sam, aes(price, fill = brand)) + geom_density(alpha = 0.2)
ggplot(app_sam,aes(mem,price,label=brand)) + geom_text(aes(label=model,size=0.5,check_overlap=TRUE))+
  geom_smooth(method="lm",se=FALSE,color="brown",size=0.6)
ggplot(app_sam,aes(ram,price,label=brand)) + geom_text(aes(label=model,size=0.5,check_overlap=TRUE))+
  geom_smooth(method="lm",se=FALSE,color="brown",size=0.6)
ggplot(app_sam,aes(camera,price,label=brand)) + geom_text(aes(label=model,size=0.5,check_overlap=TRUE))+
  geom_smooth(method="lm",se=FALSE,color="brown",size=0.6)
ggplot(app_sam,aes(ppi,price,label=brand)) + geom_text(aes(label=model,size=0.5,check_overlap=TRUE))+
  geom_smooth(method="lm",se=FALSE,color="brown",size=0.6)
