library(readxl)
library(arules)
library(tidyverse)
library(recommenderlab)
d <- read_excel("netflix_1m.xlsx")
str(d)
d %>% summarise(n_users=n_distinct(userId),
                n_movies=n_distinct(movieId),
                min_rating=min(rating),
                max_rating=max(rating))
d %>% group_by(userId) %>%
  summarise(count=n()) %>%
  arrange(-count)
#collaborative filtering
ratings <- d %>%
  select(title,userId,rating) %>%
  spread(title,rating) %>%
  column_to_rownames(var="userId") %>%
  as.matrix
ratings <- ratings %>% as("realRatingMatrix")
#recommender
Rec.user <- Recommender(ratings,"UBCF",param=list(method="Pearson"))
pred <- predict(Rec.user,ratings[3,],n=2)
as(pred,"list")
#pred
pred <- predict(Rec.user,ratings[3,],type=c("ratings"))
mv <- (as(pred,"list"))
View(mv$`49`)
