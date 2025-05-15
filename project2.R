df = read.csv("~/Documents/UP/24 25/2ºsemestre/EAD/projeto2/fertilizer_recommendation_dataset.csv", sep=",")
summary(df)

### Clustering

df = df[,1:9]
df

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


################## Avaliação com várias métricas para diferentes valores de G
G_range <- 2:10
results <- data.frame(
  G = integer(),
  BIC = numeric(),
  CH_Index = numeric(),
  Silhouette = numeric(),
  Dunn_Index = numeric()
)

for (k in G_range) {
  mod <- Mclust(scaled_df, G = k)
  bic_val <- mod$bic
  
  cl <- mod$classification
  dist_mat <- dist(scaled_df)
  stats <- cluster.stats(dist_mat, cl)
  
  sil <- mean(silhouette(cl, dist_mat)[, 3])
  
  results <- rbind(results, data.frame(
    G = k,
    BIC = bic_val,
    CH_Index = stats$ch,
    Silhouette = sil,
    Dunn_Index = stats$dunn
  ))
}

print(results)

#G       BIC CH_Index Silhouette Dunn_Index
#2 -98192.42 310.6080  0.1074625 0.05189542
#3 -97018.96 318.1936  0.1006138 0.03419449
#4 -93797.23 426.9783  0.1552033 0.04504935
#5 -91024.49 492.4574  0.1572013 0.04684956
#6 -89126.94 542.1333  0.1962951 0.04137778
#7 -87903.03 490.3373  0.1914104 0.05770785
#8 -86867.07 486.6964  0.1490868 0.03423736
#9 -85588.22 422.5850  0.1525218 0.03358945
#10 -84899.85 367.4908  0.1543238 0.03140038

#BIC - qt mais alto(menos negativo), melhor : melhor valor G=10 
#CH_Index - qt maior, melhor : G=6 (seguido de G=5)
#Silhouette - qt mais perto de 1, melhor : melhor valor: G=6 (depois G=5).
#Dunn_Index - qt maior, melhor : G=7 (seguido de G=2 e depois G=5)

par(mfrow = c(2, 2))
plot(results$G, results$BIC, type = "b", pch = 19, col = "blue",
     main = "BIC vs G", xlab = "Clusters (G)", ylab = "BIC")
plot(results$G, results$CH_Index, type = "b", pch = 19, col = "green",
     main = "CH Index vs G", xlab = "Clusters (G)", ylab = "Calinski-Harabasz")
plot(results$G, results$Silhouette, type = "b", pch = 19, col = "orange",
     main = "Silhouette vs G", xlab = "Clusters (G)", ylab = "Silhouette")
plot(results$G, results$Dunn_Index, type = "b", pch = 19, col = "purple",
     main = "Dunn Index vs G", xlab = "Clusters (G)", ylab = "Dunn")


############################## LDA ################################

final_df <- as.data.frame(scaled_df)
final_df$Soil <- as.factor(read.csv("~/Documents/UP/24 25/2ºsemestre/EAD/projeto2/fertilizer_recommendation_dataset.csv", sep=",")$Soil)

X <- final_df[, -ncol(final_df)]
Y <- final_df$Soil

set.seed(123)
train_idx <- sample(1:nrow(X), size = 0.7 * nrow(X))
test_idx <- setdiff(1:nrow(X), train_idx)

X_train <- X[train_idx, ]
X_test <- X[test_idx, ]
Y_train <- Y[train_idx]
Y_test <- Y[test_idx]

is_constant_within_any_group <- function(column, group) {
  any(tapply(column, group, function(x) length(unique(x)) == 1))
}

cols_to_remove <- sapply(as.data.frame(X_train), is_constant_within_any_group, group = Y_train)

X_train_clean <- X_train[, !cols_to_remove]
X_test_clean <- X_test[, !cols_to_remove]

library(MASS)
lda_model <- lda(x = X_train_clean, grouping = Y_train)
lda_pred <- predict(lda_model, newdata = X_test_clean)

conf_mat <- table(Predito = lda_pred$class, Real = Y_test)
print(conf_mat)

accuracy <- mean(lda_pred$class == Y_test)
cat("Accuracy - LDA multiclasse:", round(accuracy, 4), "\n")

lda_proj <- as.data.frame(predict(lda_model)$x)
lda_proj$Soil <- Y_train

library(ggplot2)
ggplot(lda_proj, aes(x = LD1, y = LD2, color = Soil)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(title = "LDA Multiclasse",
       x = "LD1", y = "LD2")

#curvas ROC 
library(pROC)
library(caret)
library(reshape2)
library(dplyr)

lda_probs <- predict(lda_model, newdata = X_test_clean)$posterior
true_classes <- Y_test
class_levels <- levels(true_classes)

for (class in class_levels) {
  true_bin <- as.numeric(true_classes == class)
  pred_prob <- lda_probs[, class]
  
  roc_obj <- roc(response = true_bin, predictor = pred_prob)
  
  par(mfrow = c(1, 1))
  plot(roc_obj,
       main = paste("ROC -", class),
       col = "darkblue",
       lwd = 2,
       print.auc = TRUE,
       print.auc.y = 0.4)
  abline(a = 0, b = 1, lty = 2, col = "gray")
  
}

#matriz de confusão
conf_matrix <- confusionMatrix(factor(lda_classes, levels = class_levels),
                               factor(true_classes, levels = class_levels))
print(conf_matrix)

metrics <- as.data.frame(conf_matrix$byClass)
metrics_selected <- metrics[, c("Precision", "Recall", "F1", "Balanced Accuracy")]
round(metrics_selected, 3)

############################## QDA ################################

