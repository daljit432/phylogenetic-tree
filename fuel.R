library("xlsx")

GD <- read.csv(choose.files(), header = TRUE)
GD
plot(mpg~cylinders,GD)
with(GD,text(mpg~cylinders,labels=horsepower,pos=1,cex=0.8))
data2 <- GD[,-c(2,6)]
data2

mean_data <- apply(data2,2,mean)
mean_data

std <-apply(data2,2,sd)
std

data1<-scale(data2,mean_data,std)
data2 

distance <- dist(data2)
distance
print(distance, digits = 9)


hc <- hclust(distance, "ave")
plot(hc, labels = GD$displacement)

library("dendroxtras")
clust1 <- color_clusters(hclust(distance), 5)
plot(clust1)
