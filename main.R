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
print(transformed_data)


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
