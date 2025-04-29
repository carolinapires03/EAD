df = read.csv("~/Documents/UP/24 25/2ºsemestre/EAD/projeto2/fertilizer_recommendation_dataset.csv", sep=",")
df = read.csv("/home/barbara/MDS/EAD/EAD/fertilizer_recommendation_dataset.csv", sep=",")
summary(df)

library(factoextra)
library(cluster)
library(fpc)
library(fastDummies)
library(mclust)
library(ggplot2)

set.seed(123)

############################## Preprocessing ################################

df = df[,1:9]
df

one_hot_df <- dummy_cols(df, remove_first_dummy = TRUE, remove_selected_columns = TRUE)

dim(one_hot_df)
head(one_hot_df)
scaled_df <- scale(one_hot_df)
summary(scaled_df)

###################### K-Means clustering ########################

# Choosing K

# Elbow method
fviz_nbclust(scaled_df, kmeans, method = "wss") + 
  theme_minimal() + 
  ggtitle("Elbow Method")
# k = 6

# Silhouette Method
fviz_nbclust(scaled_df, kmeans , method = "silhouette") + 
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
# k = 5

# GAP Statistic
fviz_nbclust(scaled_df, kmeans , method = "gap_stat", 
             k.max = 10, 
             iter.max=100 ) + 
  theme_minimal() + 
  ggtitle("Gap Method") 
# uses  the "1 standard error rule" (firstSEmax) to choose the smallest k such that the gap 
# is within 1 standard error of the maximum observed gap.

# Warning message: Quick-TRANSfer stage steps exceeded maximum (= 155000) means one of the boostrapping did not
# coverge, but it is fine if it is only a few times (it was one)
# k = 8

# Applying Kmeans

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


############################ Hierarchical clustering #################################

dist_matrix <- dist(scaled_df)
hc <- hclust(dist_matrix)
plot(hc, main = "Dendrogram of Hierarchical Clustering")
clusters <- cutree(hc, k = 5)

sil_score <- silhouette(clusters, dist(scaled_df))
mean(sil_score[, 3])


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
# use inertia to choose number of clusters
# results of kmeans, see if theres more metrics
# understand whitening
# hierarchical clustering - add heatmap?, choose the best distance to cut? results and analysis
# maybe do some analysis before applying models, like outlier handling