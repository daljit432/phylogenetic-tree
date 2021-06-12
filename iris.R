data(iris)
plot(iris)
iris1 <- scale(iris[, -5])
iris1

#k means clustering
fitk <- kmeans(iris1, 3)
fitk
str(fitk)
plot(iris1, col = fitk$cluster)

k <- list()
for(i in 1:10){
  k[[i]] <- kmeans(iris1, i)
}
k

betweens_totss <- list()
for(i in 1:10){
  betweens_totss[[i]]<- k[[i]]$betweenss/k[[i]]$totss
}

plot(1:10, betweens_totss, type = "b",
     ylab = "between SS / Total SS", xlab = "cluster(k)")

for(i in 1:4){
  plot(iris,col = k[[i]]$cluster)
}

# hiereical clustering 
d <- dist(iris1)
fitH <- hclust(d, "ward.D2")
plot(fitH)
rect.hclust(fitH, k = 3, border = "blue")
clusters <- cutree(fitH, 3)
clusters
plot(iris1, col = clusters)

#model based clustering
library(mclust)
fitM <- Mclust(iris1)
fitM
plot(fitM)
#x axis no of clusters

intstall.packages("dbscan")
library(dbscan)
kNNdistplot(iris1, k = 3)
abline(h = 0.7,col = "purple",lty = 2)
fitD <- dbscan(iris1, eps = 0.7,minPts = 5)
fitD
plot(iris1, col = fitD$cluster)
