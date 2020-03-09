d <- select(smart2,weight:ppi,brand,release,os)
mod <-lm(price~weight+mem+ram+camera+battery+vol+size+ppi+as.factor(os)+as.factor(release)+as.factor(brand),data=d)
summary(mod)
