df = read.csv("~/Documents/UP/24 25/2ºsemestre/EAD/projeto2/fertilizer_recommendation_dataset.csv", sep=",")
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
library(fpc)

#### K-Means clustering ####

fviz_nbclust(scaled_df, kmeans, method = "wss") + 
  theme_minimal() + 
  ggtitle("Elbow Method")

# Silhouette Method
fviz_nbclust(scaled_df, kmeans , method = "silhouette") + 
  theme_minimal() + 
  ggtitle("Silhouette Method")

### Calinski-Harabasz Index (CH)
ch_scores <- c()
for (k in 2:10) {
  set.seed(123)
  km <- kmeans(scaled_df, centers = k, nstart = 25)
  cl_stats <- cluster.stats(dist(scaled_df), km$cluster)
  ch_scores[k] <- cl_stats$ch
}

# Remove NAs (porque ch_scores[1] = NA)
#ch_scores <- ch_scores[!is.na(ch_scores)]

plot(2:10, ch_scores, type = "b", pch = 19,
     xlab = "Número de Clusters K", ylab = "Calinski-Harabasz Index",
     main = "Calinski-Harabasz Index vs K")
#k=5

### GAP Statistic
set.seed(123)
gap_stat <- clusGap(scaled_df,
                    FUN = function(x, k) kmeans(x, centers = k, nstart = 25, iter.max = 100),
                    K.max = 10, B = 50)

fviz_gap_stat(gap_stat) +
  theme_minimal() +
  ggtitle("GAP Statistic Method")

maxSE(gap_stat$Tab[, "gap"], 
      gap_stat$Tab[, "SE.sim"], 
      method = "firstSEmax")
#k=10

set.seed(123)

cl_5 <- kmeans(scaled_df, centers = 5, nstart = 25)

fviz_cluster(cl_5, data = scaled_df, geom = "point", ellipse.type = "convex") + 
  theme_minimal() +
  ggtitle(paste("K-means Clustering with K = 5"))

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
  ggtitle(paste("K-means Clustering with K = 5"))

cl_5_whitened$tot.withinss
cl_5_whitened$betweenss
cl_5_whitened$totss
cl_5_whitened$betweenss / cl_5_whitened$totss

# Using k = 6

cl_6 <- kmeans(scaled_df, centers = 6, nstart = 25)

fviz_cluster(cl_6, data = scaled_df, geom = "point", ellipse.type = "convex") + 
  theme_minimal() +
  ggtitle(paste("K-means Clustering with K = 6"))

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


### EM algorithm 
library(mclust)
library(ggplot2)

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

df <- as.data.frame(scaled_df)
df $em_cluster <- as.factor(mod2$classification)

pc2 <- princomp(scaled_df)
scores2 <- as.data.frame(pc2$scores)  

scores2$em_cluster <- df$em_cluster

ggplot(scores2, aes(x = Comp.1, y = Comp.2, color = em_cluster)) +
  geom_point() +
  theme_minimal() +
  labs(title = "EM Clustering com Clusters", x = "PC1", y = "PC2", color = "Cluster")

