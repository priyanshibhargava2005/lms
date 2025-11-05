# ------------------------------------------
# Title: Clustering Analysis on Breast Cancer Data
# ------------------------------------------

# Install required packages if missing (run only when needed)
pkgs <- c("mlbench", "ggplot2", "factoextra", "cluster")
new <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(new)) install.packages(new)

# Load libraries
suppressPackageStartupMessages({
  library(mlbench)
  library(ggplot2)
  library(factoextra)
  library(cluster)
})

# ------------------------------------------
# Step 1: Load and Prepare the Data
# ------------------------------------------
data("BreastCancer")
bc <- BreastCancer

# Remove ID column
bc <- bc[, -1]

# The dataset encodes missing values as "?" in some columns.
# Replace "?" with NA and then convert to numeric.
bc[] <- lapply(bc, function(col) {
  if (is.factor(col) || is.character(col)) {
    # replace "?" with NA then return
    col <- as.character(col)
    col[col == "?"] <- NA
    return(col)
  } else {
    return(col)
  }
})

# Convert columns 1:9 to numeric (safe conversion)
bc[, 1:9] <- lapply(bc[, 1:9], function(x) as.numeric(as.character(x)))

# Remove rows with missing values
bc <- na.omit(bc)

# Verify cleaned data
cat("Rows after cleaning:", nrow(bc), "\n")
str(bc)

# Create numeric matrix and scaled data frame
bc_num <- bc[, 1:9]

# scale returns a matrix — convert to data.frame for some functions
bc_scaled_mat <- scale(bc_num)
bc_scaled <- as.data.frame(bc_scaled_mat)

# ------------------------------------------
# Step 2: K-Means Clustering
# ------------------------------------------
set.seed(123)

# Elbow method to choose k (WSS)
# Use as.data.frame(bc_scaled) to satisfy fviz_nbclust input
fviz_nbclust(bc_scaled, FUNcluster = kmeans, method = "wss") +
  ggtitle("Elbow Method for Optimal Clusters")

# Run K-means with k = 2 (you can change centers after seeing elbow)
kmeans_model <- kmeans(bc_scaled, centers = 2, nstart = 25)

cat("\nK-means cluster sizes:\n")
print(kmeans_model$size)

# Add kmeans cluster result to original data
bc$Cluster_KMeans <- factor(kmeans_model$cluster)

# Visualize K-means result
fviz_cluster(kmeans_model, data = bc_scaled, geom = "point",
             ellipse.type = "norm",
             main = "K-Means Clustering of Breast Cancer Data")

# ------------------------------------------
# Step 3: Hierarchical Clustering
# ------------------------------------------

# Distance matrix (Euclidean)
dist_matrix <- dist(bc_scaled, method = "euclidean")

# Hierarchical clustering with Ward's method
hc_model <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc_model, labels = FALSE, main = "Hierarchical Clustering Dendrogram")

# Cut tree into 2 clusters
hc_clusters <- cutree(hc_model, k = 2)
bc$Cluster_HC <- factor(hc_clusters)

# Visualize Hierarchical Clustering
fviz_cluster(list(data = bc_scaled, cluster = hc_clusters),
             geom = "point",
             ellipse.type = "norm",
             main = "Hierarchical Clustering of Breast Cancer Data")

# ------------------------------------------
# Step 4: Compare Cluster Means
# ------------------------------------------
cat("\nCluster-wise Mean Values (K-Means):\n")
print(aggregate(bc_num, by = list(Cluster_KMeans = bc$Cluster_KMeans), FUN = mean))

cat("\nCluster-wise Mean Values (Hierarchical):\n")
print(aggregate(bc_num, by = list(Cluster_HC = bc$Cluster_HC), FUN = mean))

# ------------------------------------------
# Optional: Silhouette width to evaluate clustering quality
# ------------------------------------------
sil_kmeans <- silhouette(kmeans_model$cluster, dist_matrix)
cat("\nAverage silhouette width (kmeans):", mean(sil_kmeans[, 3]), "\n")
fviz_silhouette(sil_kmeans) + ggtitle("Silhouette Plot - KMeans")

sil_hc <- silhouette(hc_clusters, dist_matrix)
cat("Average silhouette width (hierarchical):", mean(sil_hc[, 3]), "\n")
fviz_silhouette(sil_hc) + ggtitle("Silhouette Plot - Hierarchical")
