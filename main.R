library(readxl)
library(cluster)
library(NbClust)

# Specify the file path
file_path <- "C:\\Users\\dlahi\\OneDrive\\Desktop\\ML\\ML\\ML\\CW\\Whitewine_v6.xlsx"

# Read the Excel file
wine_data <- read_excel(file_path)

# View the first few rows of the imported data
head(wine_data)

# Assuming your dataset is named 'wine_data'
# Remove the 'Quality' column
wine_data <- wine_data[, -which(names(wine_data) == "quality")]

# perfomr outlier removal
# Assuming your dataset is named 'wine_data' without the 'Quality' column
# Scale the data
scaled_data <- scale(wine_data)

# Assuming your scaled dataset is named 'scaled_data'
# Define a function to detect outliers using the IQR method
detect_outliers <- function(x, threshold = 1.5) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - threshold * iqr
  upper_bound <- q3 + threshold * iqr
  return(x < lower_bound | x > upper_bound)
}

# Apply the outlier detection function to each column
outlier_indices <- apply(scaled_data, 2, detect_outliers)

# Remove outliers
cleaned_data <- scaled_data[!apply(outlier_indices, 1, any), ]

# Scale the data
scaled_data <- scale(cleaned_data)

# Perform PCA
pca_result <- prcomp(scaled_data)

# Extract eigenvalues and eigenvectors
eigenvalues <- pca_result$sdev^2
eigenvectors <- pca_result$rotation

# Calculate cumulative score per principal component
cumulative_score <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2)) * 100

# Print eigenvalues and eigenvectors
print("Eigenvalues:")
print(eigenvalues)
print("Eigenvectors:")
print(eigenvectors)

# Print cumulative score per principal component
print("Cumulative Score per Principal Component:")
print(cumulative_score)

# Determine the number of principal components with cumulative score > 85%
num_components <- sum(cumulative_score > 85)

# Select principal components
selected_components <- pca_result$x[, 1:num_components]

# Create a transformed dataset with selected principal components
transformed_data <- as.data.frame(selected_components)

# Print the transformed dataset
print("Transformed Dataset with Principal Components:")
# print(transformed_data)


# Apply NBclust to the PCA-based dataset
nb_clusters_pca <- NbClust(transformed_data, min.nc = 2, max.nc = 10, method = "kmeans")
print("NBclust Results:")
print(nb_clusters_pca$Best.nc)

# Apply the Elbow method to the PCA-based dataset
wcss_pca <- numeric(10)
for (i in 1:10) {
  kmeans_model_pca <- kmeans(transformed_data, centers = i)
  wcss_pca[i] <- kmeans_model_pca$tot.withinss
}
plot(1:10, wcss_pca, type = "b", xlab = "Number of Clusters", ylab = "Within-Cluster Sum of Squares (WCSS)", main = "Elbow Method for PCA-based Dataset")
points(elbow_index + 1, wcss_pca[elbow_index + 1], col = "red", pch = 19)


# Apply the Elbow method to the PCA-based dataset
# Initialize variables
wcss_pca <- numeric(10)
wcss_diff <- diff(wcss_pca)
curvature <- numeric(length(wcss_diff) - 1)

# Calculate curvature
for (i in 1:(length(wcss_diff) - 1)) {
  curvature[i] <- (wcss_diff[i] - wcss_diff[i + 1]) / wcss_diff[i]
}

# Find the index of the maximum curvature
elbow_index <- which.max(curvature)

# Plot the Elbow method with the marked elbow point
plot(1:10, wcss_pca, type = "b", xlab = "Number of Clusters", ylab = "Within-Cluster Sum of Squares (WCSS)", main = "Elbow Method for PCA-based Dataset")
points(elbow_index + 1, wcss_pca[elbow_index + 1], col = "red", pch = 19)


# Apply Gap statistics to the PCA-based dataset
gap_stat_pca <- clusGap(transformed_data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
print("Gap Statistics Results:")
print(gap_stat_pca$Tab[,"gap"])

# Apply the Silhouette method to the PCA-based dataset
silhouette_score <- function(k){
  km <- kmeans(transformed_data, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(transformed_data))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)


optimal_k <- 3
# Perform k-means clustering with the most favored k
kmeans_model_pca <- kmeans(transformed_data, centers = optimal_k)

# Print kmeans model output
print("K-means Model Output:")
print(kmeans_model_pca)

# Calculate between-cluster sum of squares (BSS)
bss_pca <- sum(kmeans_model_pca$betweenss)

# Calculate within-cluster sum of squares (WSS)
wss_pca <- sum(kmeans_model_pca$withinss)

# Calculate total sum of squares (TSS)
tss_pca <- wss_pca + bss_pca

# Calculate ratio of BSS over TSS
bss_tss_ratio_pca <- bss_pca / tss_pca

# Print BSS, WSS, and BSS/TSS ratio
cat("Between-Cluster Sum of Squares (BSS) for PCA-based Dataset:", bss_pca, "\n")
cat("Within-Cluster Sum of Squares (WSS) for PCA-based Dataset:", wss_pca, "\n")
cat("Total Sum of Squares (TSS) for PCA-based Dataset:", tss_pca, "\n")
cat("Ratio of BSS over TSS for PCA-based Dataset:", bss_tss_ratio_pca, "\n")

# Print centers
cat("Cluster Centers for PCA-based Dataset:\n")
print(kmeans_model_pca$centers)

# Print clustered results
cat("Clustered Results for PCA-based Dataset:\n")
table(kmeans_model_pca$cluster)

# Plot clusters
plot(transformed_data[,1], transformed_data[,2], col = kmeans_model_pca$cluster,
     pch = 19, main = "Cluster Plot for PCA-based Dataset",
     xlab = "Principal Component 1", ylab = "Principal Component 2")

# Add cluster centers to the plot
points(kmeans_model_pca$centers[,1], kmeans_model_pca$centers[,2], col = 1:length(kmeans_model_pca$centers),
       pch = 8, cex = 2)


silhouette_score <- function(k){
  km <- kmeans(transformed_data, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(transformed_data))
  mean(ss[, 3])
}

k <- 2:10
avg_sil <- sapply(k, silhouette_score)

# Plot silhouette plot
plot(k, avg_sil, type='b', xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)

# Calculate and print the maximum silhouette score and corresponding k
max_sil_score <- max(avg_sil)
optimal_k_sil <- k[which.max(avg_sil)]
cat("Maximum Average Silhouette Score:", max_sil_score, "\n")
cat("Optimal number of clusters based on silhouette method:", optimal_k_sil, "\n")



library(fpc)

# Perform k-means clustering with the most favored k
kmeans_model_pca <- kmeans(transformed_data, centers = optimal_k)

# Calculate the Calinski-Harabasz Index
ch_index <- cluster.stats(transformed_data, kmeans_model_pca$cluster)$ch

# Print the Calinski-Harabasz Index
cat("Calinski-Harabasz Index:", ch_index, "\n")


km <- kmeans(transformed_data,2)
ch_index <- round(calinhara(transformed_data,km$cluster),digits=2)

# Print the Calinski-Harabasz Index
cat("Calinski-Harabasz Index:", ch_index, "\n")