df = read.csv("~/Documents/UP/24 25/2ºsemestre/EAD/projeto2/fertilizer_recommendation_dataset.csv", sep=",")
df = read.csv("/home/barbara/MDS/EAD/EAD/fertilizer_recommendation_dataset.csv", sep=",")
summary(df)
dim(df) # 3100 12

library(factoextra)
library(cluster)
library(fpc)
library(fastDummies)
library(mclust)
library(ggplot2)
library(dplyr)

set.seed(123)

############################## Preprocessing ################################

df = df[,1:9]
df

for (col in names(df)[1:8]) {
  par(mfrow = c(1, 1))
  boxplot(df[[col]], main = paste("Boxplot of", col))
}

for (col in names(df)[1:8]) {
  hist(df[[col]], main = paste("Histogram of", col), xlab = col)
  par(mfrow = c(1, 1))
}

# remove wrong values 

df <- df %>%
  mutate(across(
    c(Rainfall, Phosphorous, Potassium, Carbon),
    ~ if_else(. <= 0, 0, .)
  ))

summary(df)
for (col in names(df)[1:8]) {
  par(mfrow = c(1, 1))
  boxplot(df[[col]], main = paste("Boxplot of", col))
}

# remove outliers

cap_iqr_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  x[x < lower] <- lower
  x[x > upper] <- upper
  return(x)
}

df <- df %>%
  mutate(across(
    c(Carbon, Nitrogen, Phosphorous, Potassium, Rainfall, Temperature, Moisture, PH),
    cap_iqr_outliers
  ))

for (col in names(df)[1:8]) {
  par(mfrow = c(1, 1))
  boxplot(df[[col]], main = paste("Boxplot of", col))
}

# one hot encoding

one_hot_df <- dummy_cols(df, remove_first_dummy = TRUE, remove_selected_columns = TRUE)

dim(one_hot_df)  # 2798   12
head(one_hot_df)

# scaling  

scaled_df <- scale(one_hot_df)
summary(scaled_df)


###################### K-Means clustering ########################

# Choosing K

# Elbow method
fviz_nbclust(scaled_df, kmeans, method = "wss", k.max=15) + 
  theme_minimal() + 
  ggtitle("Elbow Method")
# k = 6

# Silhouette Method
fviz_nbclust(scaled_df, kmeans , method = "silhouette", k.max=15) + 
  theme_minimal() + 
  ggtitle("Silhouette Method")
# k = 5

# Calinski-Harabasz Index (CH)
ch_scores <- c()
for (k in 2:10) {
  km <- kmeans(scaled_df, centers = k, nstart = 25)
  cl_stats <- cluster.stats(dist(scaled_df), km$cluster) # from fpc library
  ch_scores[k] <- cl_stats$ch
}
# nstart = 25 means it tries 25 random initial configurations and chooses the best one (lowest within-cluster sum of squares).

ch_scores = ch_scores[2:10]

plot(2:10, ch_scores, type = "b", pch = 19,
     xlab = "Número de Clusters K", ylab = "Calinski-Harabasz Index",
     main = "Calinski-Harabasz Index vs K")
# A higher value of CH indicates a better clustering, because it means that the data points are more spread out between clusters than they are within clusters. 
# k = 5

# GAP Statistic
fviz_nbclust(scaled_df, kmeans , method = "gap_stat", 
             k.max = 10, 
             iter.max=100 ) + 
  theme_minimal() + 
  ggtitle("Gap Method") 
# uses  the "1 standard error rule" (firstSEmax) to choose the smallest k such that the gap 
# is within 1 standard error of the maximum observed gap.

# k = 8
# Chosen: 5

# Applying Kmeans

cl_5 <- kmeans(scaled_df, centers = 5, nstart = 25)

fviz_cluster(cl_5, data = scaled_df, geom = "point", ellipse.type = "convex") + 
  theme_minimal() +
  ggtitle(paste("K-means Clustering with K = 5"))

pca <- prcomp(scaled_df, scale. = TRUE)
pca_data <- as.data.frame(pca$x[, 1:3]) 
pca_data$cluster <- as.factor(cl_5$cluster)
summary(pca) # hard to see on 2D or even 3D if the clusters are well separated

res <- cluster.stats(dist(scaled_df), cl_5$cluster)

res$ch
res$avg.silwidth
res$dunn
res$within.cluster.ss



######################  Whitened ########################
# features become uncorrelated + unit variance


pcp = princomp(one_hot_df)
df_whitened = pcp$scores %*% diag(1/pcp$sdev)

fviz_nbclust(df_whitened, kmeans, method = "wss", k.max=15) + 
  theme_minimal() + 
  ggtitle("Elbow Method")
# k = 8

# Silhouette Method
fviz_nbclust(df_whitened, kmeans , method = "silhouette", k.max=15) + 
  theme_minimal() + 
  ggtitle("Silhouette Method") 
# k = 4, 5

# Calinski-Harabasz Index (CH)
ch_scores <- c()
for (k in 2:10) {
  km <- kmeans(df_whitened, centers = k, nstart = 25)
  cl_stats <- cluster.stats(dist(df_whitened), km$cluster) # from fpc library
  ch_scores[k] <- cl_stats$ch
}
ch_scores = ch_scores[2:10]
plot(2:10, ch_scores, type = "b", pch = 19,
     xlab = "Número de Clusters K", ylab = "Calinski-Harabasz Index",
     main = "Calinski-Harabasz Index vs K")
# k = 5

# GAP Statistic
fviz_nbclust(df_whitened, kmeans , method = "gap_stat", 
             k.max = 10, 
             iter.max=100 ) + 
  theme_minimal() + 
  ggtitle("Gap Method") 
# k = 1

# chosen K = 5

# Applying

cl_5_whitened <- kmeans(df_whitened, centers = 5, nstart = 25)

fviz_cluster(cl_5_whitened, data = df_whitened, geom = "point", ellipse.type = "convex") + 
  theme_minimal() +
  ggtitle(paste("K-means Clustering with K = 5"))

pca <- prcomp(df_whitened, scale. = TRUE)
pca_data <- as.data.frame(pca$x[, 1:3]) 
pca_data$cluster <- as.factor(cl_5_whitened$cluster)
summary(pca)
# very bad representation in 2D and 3D

res <- cluster.stats(dist(df_whitened), cl_5_whitened$cluster)

res$ch
res$avg.silwidth
res$dunn
res$within.cluster.ss

# non-whitened performed better

# stability test for non-whitened

results <- replicate(30, kmeans(scaled_df, centers = 5, nstart = 25)$cluster)
ari_matrix <- matrix(NA, ncol = 10, nrow = 10)

for (i in 1:10) {
  for (j in 1:10) {
    ari_matrix[i, j] <- adjustedRandIndex(results[, i], results[, j])
  }
}

round(ari_matrix, 2)
ari_df <- melt(ari_matrix)
names(ari_df) <- c("Run1", "Run2", "ARI")

ggplot(ari_df, aes(x = Run1, y = Run2, fill = ARI)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "red", midpoint = 0, limits = c(-1, 1)) +
  theme_minimal() +
  labs(title = "ARI Heatmap of Clustering Runs",
       x = "Clustering Run",
       y = "Clustering Run",
       fill = "ARI") +
  coord_fixed()


############################ Hierarchical clustering #################################


dist_matrix <- dist(scaled_df)
hc <- hclust(dist_matrix, method = "ward.D2")
plot(hc, main = "Dendrogram of Hierarchical Clustering")

# Choosing k

sil_widths <- sapply(2:10, function(k) {
  clusters <- cutree(hc, k = k)
  mean(silhouette(clusters, dist_matrix)[, 3])
})
plot(2:10, sil_widths, type = "b", xlab = "Number of clusters", ylab = "Avg Silhouette Width")
# 5 or 4

plot(hc, main = "Dendrogram of Hierarchical Clustering")
abline(h = 55, col = "red", lty = 2) # 5 clusters, looks cleaner

clusters_hc <- cutree(hc, k = 5)

res <- cluster.stats(dist_matrix, clusters_hc)

res$ch
res$avg.silwidth
res$dunn
res$within.cluster.ss
# similar to kmeans but slighlty worse

# stability

clusterboot_result <- clusterboot(
  dist_matrix, 
  clustermethod = disthclustCBI,
  method = "ward.D2", 
  k = 5,
  B = 100
)
clusterboot_result$bootmean # mean Jaccard stability
# 0.9692012 0.9587767 0.9657267 0.9668429 1.0000000 -> stable


################### Gaussian Mixture Models (EM algorithm) ###############

#5 clusters
mod <- Mclust(scaled_df, G=5)
summary(mod)

#plot(mod, what = "classification")

df5 <- as.data.frame(scaled_df)
df5$em_cluster <- as.factor(mod$classification)

pc <- princomp(scaled_df)
scores <- as.data.frame(pc$scores)

scores$em_cluster <- df5$em_cluster

ggplot(scores, aes(x = Comp.1, y = Comp.2, color = em_cluster)) +
  geom_point() +
  theme_minimal() +
  labs(title = "EM Clustering com 5 Clusters", x = "PC1", y = "PC2", color = "Cluster")

# com 4 clusters
mod1 <- Mclust(scaled_df, G = 4)
summary(mod1)

df4 <- as.data.frame(scaled_df)
df4$em_cluster1 <- as.factor(mod1$classification)

pc1 <- princomp(scaled_df)
scores1 <- as.data.frame(pc1$scores)  

scores1$em_cluster <- df4$em_cluster1

ggplot(scores1, aes(x = Comp.1, y = Comp.2, color = em_cluster)) +
  geom_point() +
  theme_minimal() +
  labs(title = "EM Clustering com 4 Clusters", x = "PC1", y = "PC2", color = "Cluster")

#sem especificar os clusters
mod2 <- Mclust(scaled_df, G=seq(1:30))
summary(mod2)
mod2$G

plot(mod2, what='BIC')
#plot(mod2, what = "uncertainty")

df <- as.data.frame(scaled_df)
df $em_cluster <- as.factor(mod2$classification)

pc2 <- princomp(scaled_df)
scores2 <- as.data.frame(pc2$scores)  

scores2$em_cluster <- df$em_cluster

ggplot(scores2, aes(x = Comp.1, y = Comp.2, color = em_cluster)) +
  geom_point() +
  theme_minimal() +
  labs(title = "EM Clustering com Clusters", x = "PC1", y = "PC2", color = "Cluster")


# TODO: 
# fix GMM clustering always choosing highest number of clusters