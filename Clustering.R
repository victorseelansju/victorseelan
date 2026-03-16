#==================== K-means clustering=============

# Sample dataset
data <- data.frame(
  Income = c(30,32,35,80,82,85),
  Spending = c(40,42,45,85,88,90)
)

# K-means clustering
model <- kmeans(data, centers = 2)

# View clusters
model$cluster

# View centroids
model$centers

# Plot clusters
plot(data, col = model$cluster, pch = 19)
points(model$centers, col = 1:2, pch = 8, cex = 2)



#=================== Hierarchical clustering=================
# Sample dataset
data <- data.frame(
  Income = c(30,32,35,80,82,85),
  Spending = c(40,42,45,85,88,90)
)

# Distance matrix
d <- dist(data, method = "euclidean")

# Hierarchical clustering
hc <- hclust(d, method = "ward.D2")

# Plot dendrogram
plot(hc)

# Cut tree into 2 clusters
clusters <- cutree(hc, k = 2)

clusters