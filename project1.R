library(dplyr)

bgg = read.csv("/home/barbara/MDS/EAD/EAD/bgg_db_1806.csv", sep=",")

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

summary(s_df)


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

# boxplots
for (col in names(s_df)) {
  par(mfrow = c(1, 1))
  boxplot(s_df[[col]], main = paste("Boxplot of", col))
}

# histograms
for (col in names(s_df)) {
  hist(s_df[[col]], main = paste("Histogram of", col), xlab = col)
  par(mfrow = c(1, 1))
}

scaled_temp <- scale(s_df[, c("num_votes", "owned", "max_players", "max_time", "min_time", "avg_time")])
valid_rows <- apply(scaled_temp, 1, function(row) all(row < 3 & row > -3))
df_clean <- s_df[valid_rows, ]

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

# Pearson
cor_matrix <- cor(scaled_df)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8, addCoef.col = "darkgrey", 
         cl.cex = 1,    
         number.cex = 0.8)

# Spearman
cor_matrix <- cor(scaled_df, method="s")
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8, addCoef.col = "darkgrey", 
         cl.cex = 1,    
         number.cex = 0.8)



