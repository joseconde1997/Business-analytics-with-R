# Tacos (Yelp)

library(rvest)    # web scraping
library(stringr)  # manipulate text
library(readxl)
library(writexl)
library(tidyverse)

# web scraping ------------------------------------------------------------

# website
url1 <- 'https://www.yelp.es/biz/villalobos-montclair?osq=villalobos'
url2 <- 'https://www.yelp.es/biz/donna-brooklyn?osq=donna'
url3 <- 'https://www.yelp.es/biz/poketeria-new-york'
url4 <- 'https://www.yelp.es/biz/bennys-burritos-new-york'
url5 <- 'https://www.yelp.es/biz/la-esquina-soho-new-york?osq=La+esquina'
url6 <- 'https://www.yelp.es/biz/arriba-arriba-sunnyside-sunnyside-111?osq=Arriba+Arriba+Sunnyside'
url7 <- 'https://www.yelp.es/biz/dylan-murphys-new-york?osq=dylan+murphys'

url <- c(url1,url2,url3,url4,url5,url6,url7)

data <-  data.frame()

for (url in url) {
  
  # read HTML content
  webpage <- read_html(url)
  
  # get title
  title_html <- html_nodes(webpage,'h1')
  title <- html_text(title_html)
  title <- str_trim(title)
  
  # get address
  address_html <- html_nodes(webpage,'address')
  address <- html_text(address_html)
  address <- str_trim(address)[1]
  
  # get rating
  rate_html <- html_nodes(webpage, "[class='lemon--div__373c0__1mboc border-color--default__373c0__2oFDT']")
  rate <- str_match(rate_html,"[0-9].[0-9] estrellas")
  rate <- str_trim(rate)[1]
  
  # data frame
  d <- data.frame(restaurant = title, address=address, rating = rate)
  data <- rbind(data,d)
  
}

data

data$rating <- substr(data$rating, 0, 3) # take first three characters

# export as xlsx 
write_xlsx(data,"scrape_test.xlsx")

# data analysis -----------------------------------------------------------

# count words
d <- read_xlsx("tacos.xlsx", sheet ="Reviews")
d$Food <-  str_count(d$Review, c("Food"))
d$Good <-  str_count(d$Review, c("Good"))

# import data
d <- read_xlsx("tacos.xlsx", sheet="Data")
d <- d %>% rename(rest=Restaurant, rating=Rating)

# descriptive
hist(d$rating, breaks = seq(0,5))
table(d$rating)
d %>% group_by(rest) %>% summarise(rating=mean(rating))

# rating by restaurant - table
x <- d %>% 
  group_by(rest) %>% 
  summarise(table=list(table(rating)))
x$table

# rating by restaurant - histogram
histogram <- d %>% 
  group_by(rest) %>% 
  summarise(histogram=list(hist(rating, plot=TRUE)))

# correlation with rating
correl <- round(cor(d[,-1])[,1],2)
View(sort(correl))

# competitor profiling
profile <- d %>% 
  split(.$rest) %>% 
  map(select, -c(rest)) %>% 
  map(cor) # correlation split by restaurant
profile_df <- round(as.data.frame(profile),3)
profile_df[c(1,40,79,118,157)] # select rating only
write.csv(profile_df, "cors.csv")
data
