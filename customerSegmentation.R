library(purrr) #Elbow method
library(cluster) #Silhouette method
library(gridExtra) #Silhouette method
library(grid) #Silhouette method
library(NbClust) #Silhouette method
library(factoextra) #Silhouette method

data <- read.csv("Mall_Customers.csv")

#Determine optimal number of clusters
#Intra-cluster Sum of Squares (Elbow Method)
iss <- function(k) {
  kmeans(data[,3:5], k, iter.max = 100, nstart = 100, algorithm = "Lloyd")$tot.withinss
}

kValues <- 1:10

issValues <- map_dbl(kValues, iss)

plot(kValues, issValues, type = "b", xlab = "Number of Clusters (K)", ylab = "Total Intra-Clusters")

#4-6 clusters

#Silhouette Method
k2 <- kmeans(data[,3:5], 2, iter.max = 100, nstart = 50, algorithm = "Lloyd")
k3 <- kmeans(data[,3:5], 3, iter.max = 100, nstart = 50, algorithm = "Lloyd")
k4 <- kmeans(data[,3:5], 4, iter.max = 100, nstart = 50, algorithm = "Lloyd")
k5 <- kmeans(data[,3:5], 5, iter.max = 100, nstart = 50, algorithm = "Lloyd")
k6 <- kmeans(data[,3:5], 6, iter.max = 100, nstart = 50, algorithm = "Lloyd")
k7 <- kmeans(data[,3:5], 7, iter.max = 100, nstart = 50, algorithm = "Lloyd")
k8 <- kmeans(data[,3:5], 85, iter.max = 100, nstart = 50, algorithm = "Lloyd")
k9 <- kmeans(data[,3:5], 9, iter.max = 100, nstart = 50, algorithm = "Lloyd")
k10 <- kmeans(data[,3:5], 10, iter.max = 100, nstart = 50, algorithm = "Lloyd")
#6

par(mfrow = c(3, 3))
plot(silhouette(k2$cluster, dist(data[,3:5], "euclidean")))
plot(silhouette(k3$cluster, dist(data[,3:5], "euclidean")))
plot(silhouette(k4$cluster, dist(data[,3:5], "euclidean")))
plot(silhouette(k5$cluster, dist(data[,3:5], "euclidean")))
plot(silhouette(k6$cluster, dist(data[,3:5], "euclidean")))
plot(silhouette(k7$cluster, dist(data[,3:5], "euclidean")))
plot(silhouette(k8$cluster, dist(data[,3:5], "euclidean")))
plot(silhouette(k9$cluster, dist(data[,3:5], "euclidean")))
plot(silhouette(k10$cluster, dist(data[,3:5], "euclidean")))

fviz_nbclust(data[,3:5], kmeans, method = "silhouette")
#6

#Gap Statistic Method
gap <- clusGap(data[, 3:5], FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap)
#6

clusterSummary <- prcomp(data[ ,3:5])
summary(clusterSummary)

ggplot(data, aes(x = Annual.Income..k.., y = Spending.Score..1.100.)) + geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) + scale_color_discrete(name = " ", breaks = c(1:6), labels = c("C1", "C2", "C3", "C4", "C5", "C6")) + ggtitle("Segments of Mall Customers", subtitle = "K-means Clustering")
