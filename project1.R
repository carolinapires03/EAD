library(dplyr)

bgg = read.csv("~/Documents/UP/24 25/2ºsemestre/EAD/projeto1/bgg_db_1806.csv", sep=",")

df <- subset(bgg, select = -c(rank, bgg_url, game_id, names, image_url, designer, year))
df$num_categories <- sapply(strsplit(df$category, ","), function(x) if (is.na(x[1])) 0 else length(x))
df$num_mechanics <- sapply(strsplit(df$mechanic, ","), function(x) if (is.na(x[1])) 0 else length(x))
df <- subset(df, select = -c(mechanic, category))

str(df)
sum(is.na(df)) # no missing values
summary(df)

# remove wrong values
cols_to_check <- c("max_time", "min_time", "avg_time")
df <- df[apply(df[, cols_to_check] > 5, 1, all), ]
cols_to_check <- c("min_players", "max_players")
df <- df[apply(df[, cols_to_check] != 0, 1, all), ]
summary(df)

set.seed(42)
s_df <- df %>% sample_n(200)

summary(s_df)  # to assure the distribution didnt change much


###########################################################
            ## UNIVARIATE ANALYSIS ##
###########################################################


library(e1071)

summary_stats <- data.frame(
  Min = sapply(s_df, min),
  Max = sapply(s_df, max),
  Mean = sapply(s_df, mean),
  Trimmed_Mean = sapply(s_df, mean, trim = 0.1),
  Median = sapply(s_df, median),
  Q1 = sapply(s_df, quantile, probs = 0.25),
  Q3 = sapply(s_df, quantile, probs = 0.75),
  Range = sapply(s_df, function(x) diff(range(x))),
  IQR = sapply(s_df, IQR),
  Variance = sapply(s_df, var),
  SD = sapply(s_df, sd),
  Coef_Variation = sapply(s_df, function(x) sd(x) / mean(x)),
  Skewness = sapply(s_df, skewness),
  Kurtosis = sapply(s_df, kurtosis)
)
rounded_stats <- as.data.frame(lapply(summary_stats, function(x) round(x, 3)))
rounded_stats$Variable <- rownames(summary_stats)
rounded_stats <- rounded_stats[, c(ncol(rounded_stats), 1:(ncol(rounded_stats)-1))]
print(format(rounded_stats, scientific = FALSE))

write.csv(rounded_stats, "summary_stats.csv", row.names = FALSE)


# boxplots
for (col in names(s_df)) {
  par(mfrow = c(1, 1))
  boxplot(s_df[[col]], main = paste("Boxplot of", col))
}


# Boxplots - layout 2x2
#par(mfrow = c(2, 2))
#for (col in names(s_df)) {
#  boxplot(s_df[[col]], main = paste("Boxplot of", col))
#}

# histograms
for (col in names(s_df)) {
  hist(s_df[[col]], main = paste("Histogram of", col), xlab = col)
  par(mfrow = c(1, 1))
}

# Histogramas - layout 2x2
#par(mfrow = c(2, 2))
#for (col in names(s_df)) {
#  hist(s_df[[col]], main = paste("Histogram of", col), xlab = col)
#}


scaled_temp <- scale(s_df[, c("num_votes", "owned", "max_players", "max_time", "min_time", "avg_time")])
valid_rows <- apply(scaled_temp, 1, function(row) all(row < 3 & row > -3))
df_clean <- s_df[valid_rows, ]

cat("Rows removed:", nrow(s_df) - nrow(df_clean), "\n")

# boxplots
for (col in names(df_clean)) {
  par(mfrow = c(1, 1))
  boxplot(df_clean[[col]], main = paste("Boxplot of", col))
}

# histograms
for (col in names(df_clean)) {
  hist(df_clean[[col]], main = paste("Histogram of", col), xlab = col)
  par(mfrow = c(1, 1))
}

scaled_df <- as.data.frame(scale(df_clean))
for (col in names(scaled_df)) {
  hist(scaled_df[[col]], main = paste("Histogram of", col), xlab = col)
  par(mfrow = c(1, 1))
}


###########################################################
              ## BIVARIATE ANALYSIS ##
###########################################################
library(corrplot)
library(ggplot2)


numeric_vars <- names(df_clean) # non-standardized data

for (i in 1:(length(numeric_vars) - 1)) {
  for (j in (i + 1):length(numeric_vars)) {
    x_var <- sym(numeric_vars[i])
    y_var <- sym(numeric_vars[j])
    
    p <- ggplot(df_clean, aes(x = !!x_var, y = !!y_var)) +
      geom_point(alpha = 0.5) +
      labs(title = paste("Scatterplot:", numeric_vars[i], "vs", numeric_vars[j]),
           x = numeric_vars[i],
           y = numeric_vars[j]) +
      geom_smooth(method = "lm", se = TRUE, color = "blue") +
      theme_minimal(base_size = 12)
    
    print(p)
  }
}

# For most of the variables scatterplots dont show a linear relationship, harder to see when one of the variables is discrete
# NUMBER OF MECHANICS
# Number of mechanics and weight are greater with the age restriction (positive relationship)
# OWNED
# Most games owned are for +7 yr old
# num votes and owned have a very strong linear relationship (seems almost perfect)
# AGE
# because of the previous, naturally the num_votes is higher for age +7
# GEEK RATINGS
# while theres low geek ratings for all of the weights, from around rating 7 there seems to be a monotonic positive relationship
# geek rating vs owned (and num votes) seems to be linear related, apart from some extremes
# geek rating also gets higher and more variance for age +7
# AVG RATING
# avg rating is very strongly related to weight, more than geek rating. That means avg rating is more influenced by weight than geek rating
# different from geek rating, avg rating does not seem to have a linear relationship with owned/num_votes
# even with the cited differences, avg rating and geek rating have a strong correlation. However its interisting to note how the dots have a triangular shape, which shows low geek ratings can have either low or high avg ratings
# MAX TIME
# strong correlation to weight
# greater max times are achieved for +10 age games
# MIN TIME
# strong correlation to weight again
# min time and max time highly correlated, which is expected
# AVG TIME
# perfect correlation to max time (they are equivalent)
# therefore, all of the same conclusions as max time
# MAX and MIN PLAYERS
# number of players (min or max) doesnt seem to have relevant relationships with other variables
# for the rest, there is no linear/no strong linear relationship or further insight that could be obtained


# !! REMOVING PERFECT CORRELATION VARIABLE (avg_time = max_time) !!
scaled_df <- subset(scaled_df, select = -c(avg_time))
dim(scaled_df)

# Pearson
cor_matrix <- cor(scaled_df)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.6, addCoef.col = "black", 
         cl.cex = 0.6,    
         number.cex = 0.5)

# Spearman
cor_matrix_sp <- cor(scaled_df, method="s")
corrplot(cor_matrix_sp, method = "color", type = "upper", tl.cex = 0.6, addCoef.col = "black", 
         cl.cex = 0.6,    
         number.cex = 0.5)


# Tests for Correlation

library(ggcorrplot)

p.mat <- cor_pmat(scaled_df)

ggcorrplot(cor_matrix_sp, p.mat = p.mat, 
           sig.level = 0.05, 
           insig = "pch", 
           lab = TRUE, 
           lab_size=2, 
           tl.cex=6, 
           colors = c("darkred", "white", "darkblue"))


vars <- names(scaled_df)

for (i in 1:(length(vars) - 1)) {
  for (j in (i + 1):length(vars)) {
    result <- cor.test(scaled_df[[vars[i]]], scaled_df[[vars[j]]], method = "spearman", exact = FALSE)
    assoc <- if (round(result$p.value, 4) < 0.05) " -- ASSOCIATED" else ""
    cat(vars[i], "vs", vars[j], 
        "- p-value:", round(result$p.value, 4), 
        "rho:", round(result$estimate, 3),
        assoc, "\n")
  }
}


###########################################################
## PRINCIPAL COMPONENT ANALYSIS ##
###########################################################

library(FactoMineR)
library(factoextra)

PCA_games <- PCA(scaled_df, graph = FALSE)

#eigenvalues
PCA_games$eig
barplot(PCA_games$eig[,1], main = "Eigenvalues", names.arg = 1:nrow(PCA_games$eig),
        xlab = "Principal Component", ylab = "Eigenvalue")

#loadings
PCA_games$var$coord  
#write.csv(PCA_games$var$coord, "pca_coord.csv")

#contribution of the variables to the PCs
PCA_games$var$contrib  
#write.csv(PCA_games$var$contrib, "pca_contrib.csv")

#quality of representation
PCA_games$var$cos2
#write.csv(PCA_games$var$cos2, "pca_cos2.csv")

#scores 
head(PCA_games$ind$coord)
#write.csv(PCA_games$ind$coord, "pca_scores.csv")

fviz_pca_ind(PCA_games, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

fviz_pca_var(PCA_games, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Variable Contributions to PCs")

plot(PCA_games, choix = "var", axes = c(1,2))  

###########################################################
## FACTOR ANALYSIS ##
###########################################################
library(psych)
library(car)

KMO(cor_matrix) # data is suitable for FA overall MSA = 0.7
cortest.bartlett(cor_matrix, n = 190) # p.value << 0.05, data is suitable for FA

# choosing number of factors
pca_result <- principal(cor_matrix, nfactors = ncol(scaled_df), rotate = "none", covar=FALSE)
pca_result$values
# Eigenvalues greater than 1: first 3

fa.parallel(cor_matrix, fa = "fa", n.iter = 100, n.obs=nrow(scaled_df))
# Parallel analysis suggests that the number of factors =  3

# Chosen: 3

# FA using different methods

### PRINCIPAL AXIS
fa_pa <- fa(cor_matrix, nfactors=3, n.obs=nrow(scaled_df), fm="pa", 
            rotate="varimax", SMC=FALSE)

fa_pa$communality # how much of the variable variance is explained by the common factors
# sum of squares of the pattern loadings for each variable
print(fa_pa$loadings, cutoff = 0)
# loadings: correlations between the observed variables and the common factors 
#                 
# SS loadings     # sum of squares of loadings for each factor
# Proportion Var  # proportion of total variance explained by each factor.
# Cumulative Var  # This shows the cumulative variance explained as you add more factors. Total with 3 factors: 55.7%

fa_pa$uniquenesses # specific variance. equal to 1 - communality_i, since all variables have variance = 1 (stardardized bc orthogonal model, done when computing correlation matrix)

# residual correlations, must be small for good fit
corrplot(fa_pa$residual,is.corr = FALSE, method = "color", type = "upper", tl.cex = 0.8, addCoef.col = "black", 
         cl.cex = 1,    
         number.cex = 0.8)  # diagonal = specific variance

### MIN RESIDUALS (unweighted least squares)
fa_minres <- fa(cor_matrix, nfactors=3, n.obs=nrow(scaled_df), fm="minres", 
                rotate="varimax", SMC=FALSE)
fa_minres$communality 
print(fa_minres$loadings, cutoff = 0)
fa_minres$uniquenesses
corrplot(fa_minres$residual,is.corr = FALSE, method = "color", type = "upper", tl.cex = 0.8, addCoef.col = "black", 
         cl.cex = 1,    
         number.cex = 0.8) 

## WEIGHTED LEAST SQUARES

fa_wls <- fa(cor_matrix, nfactors=3, n.obs=nrow(scaled_df), fm="wls", 
             rotate="varimax", SMC=FALSE)
fa_wls$communality 
print(fa_wls$loadings, cutoff = 0)
fa_wls$uniquenesses
corrplot(fa_wls$residual,is.corr = FALSE, method = "color", type = "upper", tl.cex = 0.8, addCoef.col = "black", 
         cl.cex = 1,    
         number.cex = 0.8) 

# won't use maximum likelihood method because some variables distributions have strong deviations from normality


###########################################################
## MULTIDIMENSIONAL SCALING ##
###########################################################

library(smacof)

dist_matrix <- dist(scaled_df)

mds_result <- mds(dist_matrix, type = "interval") # Scaled MDS

summary(mds_result)

#bubbleplot: quanto maior a bolha, pior o ajuste
plot(mds_result, plot.type = "bubbleplot", main = "MDS - Bubbleplot")

#stressplot-mostra o erro por unidade
plot(mds_result, plot.type = "stressplot", main = "MDS - Stress per Point")

#cálculo manual do Normalized Stress
dhat_matrix <- as.matrix(mds_result$dhat)

d_matrix <- as.matrix(mds_result$confdist)

p_ij <- dhat_matrix[upper.tri(dhat_matrix)]
d_ij <- d_matrix[upper.tri(d_matrix)]

nominator <- sum((p_ij - d_ij)^2)
denominator <- sum(p_ij^2)

normalized_stress <- nominator / denominator
cat("Normalized Stress:", round(normalized_stress, 4), "\n")
#Normalized Stress: 0.0304  < 0.15 ---> modelo bem ajustado

# -------------------
dist_matrix <- dist(scaled_df)

# MDS clássico
mds_result <- cmdscale(dist_matrix, k = 2, eig = TRUE)

mds_coords <- as.data.frame(mds_result$points)
colnames(mds_coords) <- c("Dim1", "Dim2")

plot(mds_coords$Dim1, mds_coords$Dim2, 
     xlab = "Dimension 1", ylab = "Dimension 2", 
     main = "MDS Plot (Euclidean Distance)", 
     pch = 19, col = "steelblue")
abline(h = 0, v = 0, col = "gray", lty = 2)

text(mds_coords$Dim1, mds_coords$Dim2, labels = rownames(mds_coords), cex = 0.6, pos = 3)

eig_vals <- mds_result$eig
var_explained <- sum(eig_vals[1:2]) / sum(abs(eig_vals))
cat("Variance explained:", round(var_explained * 100, 2), "%\n")
#Variance explained: 52.7 %
