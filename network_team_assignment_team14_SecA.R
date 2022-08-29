#1.
getwd()
products <- read.csv("products.csv")
copurchase <- read.csv("copurchase.csv")
products1 <- products[products$group == "Book", ]
Book <- subset(products1, salesrank <= 150000 & salesrank != -1)
Book$downloads <- NULL
copurchase1 <- subset(copurchase, Source %in% Book$id & Target %in% Book$id)


#2.
library(igraph)
network <- graph.data.frame(copurchase1, directed = T)
indegree <- degree(network, mode = 'in')

#3.
outdegree <- degree(network, mode = 'out')


#4.
alldegree <- degree(network, mode = 'total')
max(alldegree)
which(alldegree==53)
sub_all <- subcomponent(network, "33",'all')


#5.
#directed
newgraph<-subgraph(network,sub_all)
diameter(newgraph, directed=F, weights=NA)
diameter(newgraph, directed = T)
diam <- get_diameter(newgraph, directed=T)
diamF <- get_diameter(newgraph, directed = F)
as.vector(diam)
V(newgraph)$color<-"green"
V(newgraph)$size<-2
V(newgraph)[diam]$color<-"black"
V(newgraph)[diam]$size<-5
par(mar=c(4,4,2,2))
plot.igraph(newgraph,
            vertex.label=NA,
            edge.arrow.size=0.2,
            layout=layout_nicely)


#undirected
as.vector(diamF)
V(newgraph)$color<-"skyblue"
V(newgraph)$size<-2
V(newgraph)[diamF]$color<-"darkblue"
V(newgraph)[diamF]$size<-3
par(mar=c(4,4,2,2))
plot.igraph(newgraph,
            vertex.label=NA,
            edge.arrow.size=0.0001,
            layout=layout_nicely)


#6.
#Degree distribution
deg1 <- degree(newgraph, mode = "all")
deg.dist.all <- degree_distribution(newgraph, cumulative=T, mode="all")
centr_degree(newgraph,mode="all",normalized=T)
plot( x=0:max(deg1), y=1-deg.dist.all, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")

deg2 <- degree(newgraph, mode = "in")
deg.dist.in <- degree_distribution(newgraph, cumulative=T, mode="in")
centr_degree(newgraph,mode="in",normalized=T)
plot( x=0:max(deg2), y=1-deg.dist.in, pch = 19, cex=1, col="blue",      
      xlab="Degree", ylab="Cumulative Frequency")

deg3 <- degree(newgraph, mode = "out")
deg.dist.out <- degree_distribution(newgraph, cumulative=T, mode="out")
centr_degree(newgraph,mode="out",normalized=T)
plot( x=0:max(deg3), y=1-deg.dist.out, pch=19, cex=1.2, col="red", 
      xlab="Degree", ylab="Cumulative Frequency")

#Density
edge_density(newgraph, loops=F)

#Closeness
closeness<-closeness(newgraph, mode="all", weights=NA)
centr_clo(newgraph,mode="all",normalized=T)

summary(closeness)

#Betweeness
betwenness<-betweenness(newgraph, directed=T, weights=NA)
edge_betweenness(newgraph,directed = T, weights=NA)
centr_betw(newgraph,directed = T,normalized=T)

#Hub/Authority score
hub<-hub.score(newgraph)$vector
auth<-authority.score(newgraph)$vector
plot(newgraph,
vertex.size=hub*4,
main = 'Hubs',
vertex.color.hub = "green",
vertex.label=NA,
edge.arrow.size=0.00001,
layout = layout_nicely)
plot(newgraph,
vertex.size=auth*10,
main = 'Authorities',
vertex.color.auth = "green",
vertex.label=NA,
edge.arrow.size=0.00001,
layout = layout_nicely)

#7&8.
library("dplyr")

sub_all1 <-as_ids(sub_all)
Book$id <- as.character(Book$id)
filtered <- Book[Book$id %in% sub_all1,]
copurchase$Target <- as.character(copurchase$Target)

mean_values <- inner_join(copurchase, filtered, by = c("Target"="id")) %>%
  group_by(Target) %>%
  summarise(nghb_mn_rating = mean(rating),
            nghb_mn_salesrank = mean(salesrank),
            nghb_mn_review_cnt = mean(review_cnt))

in_degree1 <- as.data.frame(deg2)
in_degree1 <- cbind(id = rownames(in_degree1), in_degree1)
out_degree1 <- as.data.frame(deg3)
out_degree1 <- cbind(id = rownames(out_degree1), out_degree1)

closeness1 <- as.data.frame(closeness)
closeness1 <- cbind(id = rownames(closeness1), closeness1)
betweenness1 <- as.data.frame(betwenness)
betweenness1 <- cbind(id = rownames(betweenness1), betweenness1)

hub_score2 <- as.data.frame(hub)
hub_score2 <- cbind(id = rownames(hub_score2), hub_score2)
authority_score2 <- as.data.frame(auth)
authority_score2 <- cbind(id = rownames(authority_score2), authority_score2)


merge.1 = merge(x=hub_score2,y=betweenness1,by="id")

merge.2 = merge(x=merge.1,y=authority_score2,by="id")

merge.3 = merge(x=merge.2,y=closeness1,by="id")

merge.4 = merge(x=merge.3,y=in_degree1,by="id")

merge.5 = merge(x=merge.4,y=out_degree1,by="id")

merge.6 = merge(x=merge.5,y=Book,by="id")

new_df1 = merge(x=merge.6,y=mean_values,by.x="id",by.y="Target")
str(new_df1)
new_df2 <- subset(new_df1,select = c(id, hub, betwenness, auth, closeness, deg2, deg3, salesrank, review_cnt, rating,nghb_mn_rating, nghb_mn_salesrank, nghb_mn_review_cnt))

options(scipen=100, digits=2)
summary(salesrank_prediction<- glm(salesrank ~ review_cnt + rating + hub + betwenness + 
                                     auth + closeness + deg2 + deg3 + nghb_mn_rating + nghb_mn_review_cnt + nghb_mn_salesrank, family="poisson", data=new_df2))
str(new_df2)
new_df3 <- subset(new_df2, select = -c(id))

library(ggplot2)
library(reshape2)
corr <- cor(new_df3)
melted_corr <- melt(corr)
head(melted_corr)
ggplot(data = melted_corr,aes(x=Var1, y=Var2, fill = value))+
  geom_tile()

summary(salesrank_prediction_nonghb <- glm(salesrank ~ review_cnt + rating + hub + betwenness + 
                                     auth + closeness + deg2 + deg3 + nghb_mn_salesrank, family="poisson", data=new_df2))

summary(salesrank_prediction_noreive <- glm(salesrank ~ hub + betwenness + 
                                     auth + closeness + deg2 + deg3 + nghb_mn_rating + nghb_mn_review_cnt + nghb_mn_salesrank, family="poisson", data=new_df2))
