install.packages("psych")
install.packages("cluster")
install.packages("NbClust")
install.packages("flexclust")
install.packages("fMultivar")
install.packagesy("rattle")

library(tidyverse)
library(ggplot2)
library(psych)
library(cluster)
library(NbClust)
library(flexclust)
library(fMultivar)
library(rattle)

wine <- read.csv("C:/Users/Administrator/Desktop/wine.csv", stringsAsFactors=TRUE)
test_Data <- wine[,1:11]
# PCA
# the number of the nfactors(principal components)

fa.parallel(test_Data, fa="pc", n.iter=100)
# according to the graph, we choose the number of nfactors(principal components)=3

pca<- principal(test_Data, nfactors=3, rotate="varimax", scores=TRUE)
pca

head(pca$scores)
round(unclass(pca$weights),2)

wine1 <- read.csv("C:/Users/Administrator/Desktop/wine1.csv", stringsAsFactors=TRUE)
cor(wine1$color, pca$scores)
#wine 1 和wine唯一的不同是把原数据color red 赋值成1， color white赋值成2

cor(wine$quality, pca$scores)

# we could see the correlation between the color, quality and scores.
# There is no correlation or the correlation is very low between the color, quality and scores
# the PCA way make no sense in this dataset.

#cluster
# Kmeans
head(test_Data)
df <- scale(test_Data)
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
set.seed(1234)
fit.km <-kmeans(df,3,nstart=25)
fit.km$siza
fit.km$centers
aggregate(test_Data, by=list(cluster=fit.km$cluster),mean)

## according to the graph, the number of clusters we choose is 3.
ct.km <- table(wine$color, fit.km$cluster)
ct.km
randIndex(ct.km)

# randIndex=0.4517721

#围绕中心点划分(pam)
library(cluster)
set.seed(1234)
fit.pam <- pam(test_Data, k=3, stand= TRUE)
fit.pam$medoids
clusplot(fit.pam, main="Bivariate Cluster Plot")

ct.pam <-table(wine$color, fit.pam$clustering)
randIndex(ct.pam)

#randIndex=0.4376432 lower than the outcome in Kmeans

#Question 2 quality

# K 均值
ct.km1 <- table(wine$quality, fit.km$cluster)
ct.km1
randIndex(ct.km1)
#randIndex=0.01799552
# pam
ct.pam1 <-table(wine$quality, fit.pam$clustering)
randIndex(ct.pam1)
#randIndex= 0.01713892

#Conclusion  clustering(kmeans) makes more sense to me for this data.
#             kmeans is more significant in explaining color than quality


