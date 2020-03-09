library(tidyverse)
library (igraph)
library(treemap)
#Network
ebay <- read.csv("https://raw.githubusercontent.com/wdecisions/ba/master/eBayNetwork.csv")
ebay <- ebay[,1:2]
ebay[,1] <- as.factor(ebay[,1])
ebay[,2] <- as.factor(ebay[,2])
graph.edges <- as.matrix(ebay[,1:2])
g <- graph.edgelist (graph.edges, directed= FALSE)
isBuyer <- V(g)$name %in% graph.edges[,2]
plot(g,vertex.label=NA,
     vertex.color= ifelse(isBuyer,"black","orange"),
     vertex.size= ifelse(isBuyer, 5, 9))
#Treemaps
tree.df <- read.csv("https://raw.githubusercontent.com/wdecisions/ba/master/eBayTreemap.csv")
head(tree.df[-1,c(1,3:5),10])
#morosity
tree.df$negative.feedback <- 1*(tree.df$Seller.Feedback<0)
#draw treemap
treemap(tree.df, index = c("Category","Sub.Category","Brand"),
        vSize = "High.Bid",vColor = "negative.feedback",
        fun.aggregate = "mean",align.labels = list(c("left", "top"),c("right","bottom"),
                                                   c("center","center")),
                                                   palette=rev(gray.colors(3)), type="manual",title="")
