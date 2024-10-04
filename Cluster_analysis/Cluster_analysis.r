library(readxl)
library(ggplot2)

# Load the data
data <- read_excel("Customer_Data.xlsx")  # Ensure the file path is correct

# Normalize the data
data_normalized <- scale(data)

# Set seed for reproducibility
set.seed(123)

# Run K-means clustering with 2 clusters (you can change the number of clusters)
kmeans_result <- kmeans(data_normalized, centers = 3)

# View clustering results
print(kmeans_result)

# Add cluster information to the data
data$cluster <- as.factor(kmeans_result$cluster)

# Plot clusters
ggplot(data, aes(x = Trust, y = Social_Influence, color = cluster)) +
    geom_point(size = 4) +
    labs(title = "K-means Clustering", x = "Trust", y = "Social Influence") +
    theme_minimal()
ggplot(data, aes(x = Customer_Satisfaction, y = Loyalty, color = cluster)) +
  geom_point(size = 4) +
  labs(title = "Customer Segmentation: Satisfaction vs. Loyalty ",
       x = "Customer_Satisfaction", y = "Loyalty ") + 
  theme_minimal()
table(data$cluster)
# Load necessary libraries
library(factoextra)

# Run k-means clustering (example code)
kmeans_result <- kmeans(data_normalized, centers = 3, nstart = 25)

# Visualize the clusters
fviz_cluster(kmeans_result, data = data_normalized, geom = "point")
set.seed(123)
data<- data.frame(
  Customer_Satisfaction = rnorm(100, mean = 7, sd = 2),  # Satisfaction score (1-10)
  Social_Influence = rnorm(100, mean = 10, sd = 4),    # Purchases per month
  Loyalty = rnorm(100, mean = 50, sd = 15) # Loyalty score (0-100)
)
scaled_data <- scale(data)
wcss <- vector()
for (k in 1:5) {
  kmeans_result <- kmeans(scaled_data, centers = k, nstart = 25)
  wcss[k] <- kmeans_result$tot.withinss
}
plot(1:5, wcss, type = "b", main = "Elbow Method for Optimal K",
     xlab = "Number of Clusters", ylab = "WCSS (Within-Cluster Sum of Squares)",
     col = "blue", pch = 19, cex = 1.5, lwd = 2)
# Enhanced visualization of clusters using factoextra
fviz_cluster(kmeans_result, data = scaled_data,
             geom = "point",
             stand = FALSE,
             ellipse.type = "euclid",
             show.clust.cent = TRUE,
             palette = "jco",
             ggtheme = theme_minimal(),
             main = "K-means Clustering of Customer Data")