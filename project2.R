
df = read.csv("/home/barbara/MDS/EAD/fertilizer_recommendation_dataset.csv", sep=",")
summary(df)

### Clustering

df = df[,1:9]
df

library(fastDummies)
one_hot_df <- dummy_cols(df, remove_first_dummy = TRUE, remove_selected_columns = TRUE)

one_hot_df
head(one_hot_df)
scaled_df <- scale(one_hot_df)

library(factoextra)
library(cluster)

#### K-Means clustering ####

fviz_nbclust(scaled_df, kmeans, method = "wss") + 
  theme_minimal() + 
  ggtitle("Elbow Method")

# Silhouette Method
fviz_nbclust(scaled_df, kmeans , method = "silhouette") + 
  theme_minimal() + 
  ggtitle("Silhouette Method")

set.seed(123)

cl_5 <- kmeans(scaled_df, centers = 5, nstart = 25)

fviz_cluster(cl_5, data = scaled_df, geom = "point", ellipse.type = "convex") + 
  theme_minimal() +
  ggtitle(paste("K-means Clustering with K =", final_k))

cl_5$tot.withinss
cl_5$betweenss
cl_5$totss
cl_5$betweenss / cl_5$totss

# Whitened
pcp = princomp(scaled_df)
spc = pcp$scores %*% diag(1/pcp$sdev)

cl_5_whitened <- kmeans(spc, centers = 5, nstart = 25)

fviz_cluster(cl_5_whitened, data = scaled_df, geom = "point", ellipse.type = "convex") + 
  theme_minimal() +
  ggtitle(paste("K-means Clustering with K =", final_k))

cl_5_whitened$tot.withinss
cl_5_whitened$betweenss
cl_5_whitened$totss
cl_5_whitened$betweenss / cl_5_whitened$totss

# Using k = 6

cl_6 <- kmeans(scaled_df, centers = 6, nstart = 25)

fviz_cluster(cl_6, data = scaled_df, geom = "point", ellipse.type = "convex") + 
  theme_minimal() +
  ggtitle(paste("K-means Clustering with K =", 6))

cl_6$tot.withinss
cl_6$betweenss
cl_6$totss
cl_6$betweenss / cl_6$totss

# Using k = 5 without whitening

clusters <- cl_5$cluster
sil_score <- silhouette(clusters, dist(scaled_df))
mean(sil_score[, 3])

#### Hierarchical clustering ####

dist_matrix <- dist(scaled_df)
hc <- hclust(dist_matrix)
plot(hc, main = "Dendrogram of Hierarchical Clustering")
clusters <- cutree(hc, k = 5)

sil_score <- silhouette(clusters, dist(scaled_df))
mean(sil_score[, 3])
