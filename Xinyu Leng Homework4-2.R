library(tidyverse)
library(ggplot2)
library(psych)
library(cluster)
library(NbClust)
library(flexclust)
library(fMultivar)
library(rattle)
library(knitr)
library(lattice)
library(grid)
library(gridExtra)
library(mosaic)
library(quantmod)
library(foreach)
library(biganalytics)



social_marketing <- read.csv("C:/Users/Administrator/Desktop/social_marketing.csv", stringsAsFactors=TRUE,row.names=1)

#data pre-processing
# There are 4 unwanted categories, 
# chatter, uncategorized, adult and spam 
#These four variables will not help in clustering.
myvars<- names(social_marketing) %in% c("chatter","uncategorized","adult","spam")
newdata<- social_marketing[!myvars]
#scale the variables
newdata.scaled <- scale(newdata)

# Customer Segments 


# clustering using Kmeans
head(newdata)
df <- newdata.scaled
wssplot<-function(data,nc=15,seed=1234){
  wss<-(nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i]<-sum(kmeans(data,centers=i)$withinss)}
  plot(1:nc,wss,type="b",xlab="number of clusters",ylab="within groups sum of squares")}
wssplot(df)
library(NbClust)
set.seed(1234)
devAskNewPage(ask=TRUE)
nc<-NbClust(df ,min.nc=2, max.nc = 15,method="kmeans")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters")
#we choose k=6
set.seed(1234)
fit.km <-kmeans(df,6,nstart=25)
fit.km$siza
fit.km$centers
aggregate(newdata, by=list(cluster=fit.km$cluster),mean)



## according to the graph, the number of clusters we choose is 6
#PCA

pca1= prcomp(newdata.scaled, scale=TRUE, rank=2)
loadings1 = pca1$rotation
scores1 = pca1$x

o11 = order(loadings1[,1], decreasing=TRUE)
colnames(social_marketing_clean)[head(o1,5)]
colnames(social_marketing_clean)[tail(o1,5)]

o21 = order(loadings1[,2], decreasing=TRUE)
colnames(social_marketing_clean)[head(o2,5)]
colnames(social_marketing_clean)[tail(o2,5)]

{plot(loadings1, col="red", pch=19, cex=2, xlim = c(0,0.5), ylim = c(-0.45,0.3))
  text(loadings1, labels=rownames(loadings1), cex=0.4, font=1, pos=1)}
#KNN
sc_mkt = scale(newdata.scaled, center=TRUE, scale=TRUE) # cluster on measurables
k_grid = seq(2, 20, by=1)
SSE_grid = foreach(k = k_grid, .combine='c') %do% {
  cluster_k = kmeans(sc_mkt, k, nstart=50)
  cluster_k$tot.withinss
}
plot(k_grid, SSE_grid, xlab = "K")
clust6 = kmeans(sc_mkt, 6, nstart=50)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
qplot(scores1[,1], scores1[,2], color=clust6$cluster, xlab='Component 1', ylab='Component 2', main="6 Clusters")

#as we can see in the graph, we divide these variables into 6 groups
#Group1(parent group)
# sports_fandom parenting_religion food school family
#Group2(media group)
#cooking beauty sharing photo_sharing 
#Group3(fitness group)
#outdoors personal_fitness health_nutrition
#Group4(travel group)
#politics news travel automotive computers current_events dating home_and_garden
#Group5(student group)
#online_gaming college_uni tv_film shopping sports_playing
#Group6(business group)
#small_business business eco 

# Recommendation
#Group1(parent group)
# Tell them the beneficial of this products for their children compare with others.
#Group2(media group)
# A good way in marketing, improve brand influence.
#Group3(fitness group)
# Tell them the products ingredients and could help them to achieve goals.
#Group4(travel group)
# Tell them to bring this products in order to have a better sleep in traveling
#Group5(student group)
# Tell them the nutrients of the products would have positive influence on their body growth.
#Group6(business group)
# The ingredients of product could improve their energy and this brand has investment value.








