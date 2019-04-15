# Importing the data
url <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_1903/datasets/WisconsinCancer.csv"
df <- read.csv(url)

#Initial inspection of dataset
head(df)
summary(df)
colnames(df)

# Excluding the id and target column
data <- as.matrix(df[,3:32])
row.names(data) <- df$id
diagnosis <- as.numeric(df$diagnosis == 'M')
head(data)

#Check for PCA,Scaling if necessary
colMeans(data)
apply(data, 2, sd)
scale(data)
data_pca <- prcomp(data, scale = TRUE, center = TRUE )
summary(data_pca)

#Exploring PCs
biplot(data_pca)
data_pca$x
plot(data_pca$x[,c(1,2)], col = (diagnosis + 1), xlab = 'PC1', ylab = 'PC2')
plot(data_pca$x[,c(1,3)], col = (diagnosis + 1), xlab = 'PC1', ylab = 'PC2')

#Plotting the PCs explained variance
par(mfrow = c(1,2))
var <- data_pca$sdev^2
var_explained <- var/sum(var)

plot(var_explained, xlab = 'Principal Component', ylab = 'Proportion of Variance Explained', ylim = c(0,1), type = 'b')
plot(cumsum(var_explained), xlab = 'Principal Component', ylab = 'Proportion of Variance Explained', ylim = c(0,1), type = 'b')

# Applying Hierarchical clustering
data_scaled <- scale(data)
data_dist <- dist(data_scaled)
data_hclust <- hclust(data_dist, method = 'complete')
plot(data_hclust)
# Pruning 
data_hclust_clusters <- cutree(data_hclust, k = 4)
table(data_hclust_clusters, diagnosis)

# Applying KMeans clustering
data_km <- kmeans(data_scaled, centers = 2, nstart = 20)
table(data_km$cluster,diagnosis)
#Comparing with hclust
table(data_km$cluster, data_hclust_clusters)

# Using PCA applied data

data_pca_hclust <- hclust(dist(data_pca$x[,1:7]), method = "complete")
data_pca_hclust_clusters <- cutree(data_pca_hclust, k = 4)
# Comparing with actuals and non-PCA datasets
table(data_pca_hclust_clusters, diagnosis)
table(data_pca_hclust_clusters, data_hclust_clusters)

